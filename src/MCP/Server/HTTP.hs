{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{- |
Module      : MCP.Server.HTTP
Description : MCP server implementation for HTTP communication
Copyright   : (C) 2025 Matthias Pall Gissurarson
License     : MIT
Maintainer  : mpg@mpg.is
Stability   : experimental
Portability : GHC

This module provides MCP server implementation for HTTP communication.
-}
module MCP.Server.HTTP (
    -- * Server Runner
    runServerHTTP,
    HTTPServerConfig (..),

    -- * Demo Configuration Helpers
    defaultDemoOAuthConfig,
    defaultProtectedResourceMetadata,

    -- * HTML Content Type
    HTML,

    -- * Login Types
    PendingAuthorization (..),
    LoginForm (..),
    LoginError (..),
    LoginResult (..),

    -- * HTML Rendering
    renderLoginPage,
    renderErrorPage,
    scopeToDescription,
    formatScopeDescriptions,
) where

import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVarIO, writeTVar)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Control.Monad.State.Strict (get, put)
import Data.Aeson (encode, fromJSON, object, toJSON, (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Functor.Contravariant (contramap)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import GHC.Generics (Generic)
import Network.HTTP.Media ((//), (/:))
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant (Context (..), Handler, Proxy (..), Server, hoistServerWithContext, serve, serveWithContext, throwError)
import Servant.API (Accept (..), JSON, MimeRender (..), Post, ReqBody, (:<|>) (..), (:>))
import Servant.Auth.Server (Auth, AuthResult (..), CookieSettings, JWT, JWTSettings, defaultCookieSettings, defaultJWTSettings, generateKey)
import Servant.Server (err400, err401, err500, errBody, errHeaders)

import Control.Monad (when)
import MCP.Protocol
import MCP.Server (MCPServer (..), MCPServerM, ServerConfig (..), ServerState (..), initialServerState, runMCPServer)
import MCP.Server.Auth (OAuthConfig (..), OAuthProvider (..), ProtectedResourceMetadata (..), defaultDemoCredentialStore)

-- Import AuthBackend instance for AppM
import MCP.Server.HTTP.AppEnv (AppEnv (..), HTTPServerConfig (..), runAppM)

-- Import OAuthAPI from OAuth.Server (migration from duplication)

import MCP.Server.Auth.Demo (DemoCredentialEnv (..))
import MCP.Server.OAuth.InMemory (OAuthTVarEnv, defaultExpiryConfig, newOAuthTVarEnv)
import MCP.Server.OAuth.Server (LoginForm (..), OAuthAPI)
import MCP.Server.OAuth.Server qualified as OAuthServer
import MCP.Server.OAuth.Store ()

-- Import OAuthStateStore instance for AppM
import MCP.Server.OAuth.Types (AuthUser (..), ClientAuthMethod (..), CodeChallengeMethod (..), GrantType (..), PendingAuthorization (..), RedirectUri (..), ResponseType (..), Scope (..), UserId (..), unUserId)
import MCP.Trace.HTTP (HTTPTrace (..))
import MCP.Trace.Operation (OperationTrace (..))
import MCP.Trace.Server (ServerTrace (..))
import MCP.Types
import Plow.Logging (IOTracer (..), traceWith)

-- | HTML content type for Servant
data HTML

instance Accept HTML where
    contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML Text where
    mimeRender _ = LBS.fromStrict . TE.encodeUtf8

-- Note: LoginForm is now imported from MCP.Server.OAuth.Server
-- Note: OAuthState is now replaced by OAuthTVarEnv from OAuth.InMemory

-- | Login error types
data LoginError
    = InvalidOAuthParameters Text
    | ClientNotRegistered Text
    | InvalidRedirectUri Text
    | SessionNotFound Text
    | SessionExpired
    | AuthenticationFailed
    | CookiesRequired
    | MissingFormFields [Text]
    deriving (Show, Eq)

-- | Login result
data LoginResult
    = LoginSuccess
        { resultAuthCode :: Text
        , resultRedirectUri :: Text
        , resultState :: Maybe Text
        }
    | LoginDenied
        { resultRedirectUri :: Text
        , resultState :: Maybe Text
        }
    | LoginFailure LoginError
    deriving (Show)

-- Note: ClientRegistrationRequest and ClientRegistrationResponse are now imported from MCP.Server.OAuth.Server

-- | Client info stored in server
data ClientInfo = ClientInfo
    { clientName :: Text
    , clientRedirectUris :: [RedirectUri]
    , clientGrantTypes :: [GrantType]
    , clientResponseTypes :: [ResponseType]
    , clientAuthMethod :: ClientAuthMethod
    }
    deriving (Show, Generic)

-- Note: TokenResponse is now imported from MCP.Server.OAuth.Server

-- | MCP API definition for HTTP server (following the MCP transport spec)
type MCPAPI auths = Auth auths AuthUser :> "mcp" :> ReqBody '[JSON] Aeson.Value :> Post '[JSON] Aeson.Value

-- | Unprotected MCP API (for backward compatibility)
type UnprotectedMCPAPI = "mcp" :> ReqBody '[JSON] Aeson.Value :> Post '[JSON] Aeson.Value

-- Note: OAuthAPI, ProtectedResourceAPI, and LoginAPI are now imported from MCP.Server.OAuth.Server

-- | Complete API with OAuth
type CompleteAPI auths = OAuthAPI :<|> MCPAPI auths

-- | Create a WAI Application for the MCP HTTP server
mcpApp :: (MCPServer MCPServerM) => HTTPServerConfig -> IOTracer HTTPTrace -> TVar ServerState -> OAuthTVarEnv -> JWTSettings -> Application
mcpApp config tracer stateVar oauthEnv jwtSettings =
    let cookieSettings = defaultCookieSettings
        authContext = cookieSettings :. jwtSettings :. EmptyContext
        baseApp = case httpOAuthConfig config of
            Just oauthCfg
                | oauthEnabled oauthCfg ->
                    serveWithContext
                        (Proxy :: Proxy (CompleteAPI '[JWT]))
                        authContext
                        (oauthServerNew config tracer oauthEnv jwtSettings :<|> mcpServerAuth config tracer stateVar)
            _ ->
                serve (Proxy :: Proxy UnprotectedMCPAPI) (mcpServerNoAuth config tracer stateVar)
     in if httpEnableLogging config
            then logStdoutDev baseApp
            else baseApp
  where
    -- NEW: Use polymorphic server via hoistServerWithContext
    oauthServerNew :: HTTPServerConfig -> IOTracer HTTPTrace -> OAuthTVarEnv -> JWTSettings -> Server OAuthAPI
    oauthServerNew cfg httpTracer oauth jwtSet =
        let authEnv = DemoCredentialEnv defaultDemoCredentialStore
            appEnv =
                AppEnv
                    { envOAuth = oauth
                    , envAuth = authEnv
                    , envConfig = cfg
                    , envTracer = httpTracer
                    , envJWT = jwtSet
                    , envTimeProvider = Nothing -- Use real IO time for production
                    }
         in hoistServerWithContext
                (Proxy :: Proxy OAuthAPI)
                (Proxy :: Proxy '[CookieSettings, JWTSettings])
                (runAppM appEnv)
                OAuthServer.oauthServer

    mcpServerAuth :: HTTPServerConfig -> IOTracer HTTPTrace -> TVar ServerState -> AuthResult AuthUser -> Aeson.Value -> Handler Aeson.Value
    mcpServerAuth httpConfig httpTracer stateTVar authResult requestValue =
        case authResult of
            Authenticated user -> do
                liftIO $ traceWith httpTracer $ HTTPAuthSuccess (unUserId $ userUserId user)
                handleHTTPRequest httpConfig httpTracer stateTVar (Just user) requestValue
            BadPassword -> do
                liftIO $ traceWith httpTracer $ HTTPAuthFailure "Bad password"
                liftIO $ traceWith httpTracer $ HTTPAuthRequired "/mcp"
                throwError $
                    err401
                        { errHeaders = [("WWW-Authenticate", wwwAuthenticateValue)]
                        , errBody = encode $ object ["error" .= ("Authentication required" :: Text)]
                        }
            NoSuchUser -> do
                liftIO $ traceWith httpTracer $ HTTPAuthFailure "No such user"
                liftIO $ traceWith httpTracer $ HTTPAuthRequired "/mcp"
                throwError $
                    err401
                        { errHeaders = [("WWW-Authenticate", wwwAuthenticateValue)]
                        , errBody = encode $ object ["error" .= ("Authentication required" :: Text)]
                        }
            Indefinite -> do
                liftIO $ traceWith httpTracer $ HTTPAuthFailure "Authentication indefinite"
                liftIO $ traceWith httpTracer $ HTTPAuthRequired "/mcp"
                throwError $
                    err401
                        { errHeaders = [("WWW-Authenticate", wwwAuthenticateValue)]
                        , errBody = encode $ object ["error" .= ("Authentication required" :: Text)]
                        }
      where
        metadataUrl = httpBaseUrl httpConfig <> "/.well-known/oauth-protected-resource"
        wwwAuthenticateValue = TE.encodeUtf8 $ "Bearer resource_metadata=\"" <> metadataUrl <> "\""

    mcpServerNoAuth :: HTTPServerConfig -> IOTracer HTTPTrace -> TVar ServerState -> Aeson.Value -> Handler Aeson.Value
    mcpServerNoAuth httpConfig httpTracer stateTVar = handleHTTPRequest httpConfig httpTracer stateTVar Nothing

-- | Handle HTTP MCP requests following the MCP transport protocol
handleHTTPRequest :: (MCPServer MCPServerM) => HTTPServerConfig -> IOTracer HTTPTrace -> TVar ServerState -> Maybe AuthUser -> Aeson.Value -> Handler Aeson.Value
handleHTTPRequest httpConfig tracer stateVar mAuthUser requestValue = do
    -- Emit trace for received request
    liftIO $ traceWith tracer $ HTTPRequestReceived "/mcp" "POST" (isJust mAuthUser)

    -- Parse the incoming JSON-RPC message
    case fromJSON requestValue of
        Aeson.Success (msg :: JSONRPCMessage) -> do
            case msg of
                RequestMessage req -> do
                    -- Process the JSON-RPC request
                    result <- liftIO $ processHTTPRequest httpConfig tracer stateVar req
                    case result of
                        Left err -> throwError err500{errBody = encode $ object ["error" .= err]}
                        Right response -> return response
                NotificationMessage notif -> do
                    -- Process notifications (no response expected)
                    _ <- liftIO $ processHTTPNotification httpConfig stateVar notif
                    return $ object [] -- Empty response for notifications
                _ -> throwError err400{errBody = encode $ object ["error" .= ("Invalid JSON-RPC message type" :: Text)]}
        Aeson.Error e -> throwError err400{errBody = encode $ object ["error" .= ("Invalid JSON-RPC message" :: Text), "error_description" .= T.pack e]}

-- | Process an HTTP MCP notification
processHTTPNotification :: HTTPServerConfig -> TVar ServerState -> JSONRPCNotification -> IO ()
processHTTPNotification _ _ _ = do
    -- For now, just ignore notifications since they don't need responses
    -- In a more complete implementation, this would handle logging/setLevel notifications
    return ()

-- | Process an HTTP MCP request
processHTTPRequest :: (MCPServer MCPServerM) => HTTPServerConfig -> IOTracer HTTPTrace -> TVar ServerState -> JSONRPCRequest -> IO (Either Text Aeson.Value)
processHTTPRequest httpConfig tracer stateVar req = do
    -- Read the current state
    currentState <- readTVarIO stateVar
    let dummyConfig =
            ServerConfig
                { configInput = undefined -- Not used in HTTP mode
                , configOutput = undefined -- Not used in HTTP mode
                , configServerInfo = httpServerInfo httpConfig
                , configCapabilities = httpCapabilities httpConfig
                }
        serverTracer = contramap HTTPServer tracer
        opTracer = contramap HTTPOperation tracer

    result <- runMCPServer dummyConfig currentState (handleHTTPRequestInner serverTracer opTracer (httpProtocolVersion httpConfig) req)
    case result of
        Left err -> return $ Left err
        Right (response, newState) -> do
            -- Update the state atomically
            atomically $ writeTVar stateVar newState
            return $ Right response

-- | Handle HTTP request within the MCP monad, returning proper JSON-RPC responses
handleHTTPRequestInner :: (MCPServer MCPServerM) => IOTracer ServerTrace -> IOTracer OperationTrace -> Text -> JSONRPCRequest -> MCPServerM Aeson.Value
handleHTTPRequestInner serverTracer opTracer protocolVersion (JSONRPCRequest _ reqId method params) = do
    config <- ask
    state <- get

    case method of
        "initialize" -> case params of
            Just p -> case fromJSON p of
                Aeson.Success initParams -> do
                    handleInitializeHTTP serverTracer reqId initParams
                    let result =
                            InitializeResult
                                { protocolVersion = protocolVersion
                                , capabilities = configCapabilities config
                                , serverInfo = configServerInfo config
                                , instructions = Nothing
                                , _meta = Nothing
                                }
                    return $ toJSON $ JSONRPCResponse "2.0" reqId (toJSON result)
                Aeson.Error e ->
                    return $
                        toJSON $
                            JSONRPCError "2.0" reqId $
                                JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
            Nothing ->
                return $
                    toJSON $
                        JSONRPCError "2.0" reqId $
                            JSONRPCErrorInfo (-32602) "Missing params" Nothing
        "ping" -> return $ toJSON $ JSONRPCResponse "2.0" reqId (object [])
        "resources/list" -> do
            liftIO $ traceWith opTracer OperationResourcesList
            if not (serverInitialized state)
                then
                    return $
                        toJSON $
                            JSONRPCError "2.0" reqId $
                                JSONRPCErrorInfo (-32002) "Server not initialized" Nothing
                else case params of
                    Just p -> case fromJSON p of
                        Aeson.Success listParams -> do
                            result <- handleListResources listParams
                            return $ toJSON $ JSONRPCResponse "2.0" reqId (toJSON result)
                        Aeson.Error e ->
                            return $
                                toJSON $
                                    JSONRPCError "2.0" reqId $
                                        JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
                    Nothing -> do
                        result <- handleListResources (ListResourcesParams Nothing)
                        return $ toJSON $ JSONRPCResponse "2.0" reqId (toJSON result)
        "resources/read" -> do
            if not (serverInitialized state)
                then
                    return $
                        toJSON $
                            JSONRPCError "2.0" reqId $
                                JSONRPCErrorInfo (-32002) "Server not initialized" Nothing
                else case params of
                    Just p -> case fromJSON p of
                        Aeson.Success readParams@(ReadResourceParams uri) -> do
                            liftIO $ traceWith opTracer $ OperationResourceRead uri
                            result <- handleReadResource readParams
                            return $ toJSON $ JSONRPCResponse "2.0" reqId (toJSON result)
                        Aeson.Error e ->
                            return $
                                toJSON $
                                    JSONRPCError "2.0" reqId $
                                        JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
                    Nothing ->
                        return $
                            toJSON $
                                JSONRPCError "2.0" reqId $
                                    JSONRPCErrorInfo (-32602) "Missing params" Nothing
        "tools/list" -> do
            liftIO $ traceWith opTracer OperationToolsList
            if not (serverInitialized state)
                then
                    return $
                        toJSON $
                            JSONRPCError "2.0" reqId $
                                JSONRPCErrorInfo (-32002) "Server not initialized" Nothing
                else case params of
                    Just p -> case fromJSON p of
                        Aeson.Success listParams -> do
                            result <- handleListTools listParams
                            return $ toJSON $ JSONRPCResponse "2.0" reqId (toJSON result)
                        Aeson.Error e ->
                            return $
                                toJSON $
                                    JSONRPCError "2.0" reqId $
                                        JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
                    Nothing -> do
                        result <- handleListTools (ListToolsParams Nothing)
                        return $ toJSON $ JSONRPCResponse "2.0" reqId (toJSON result)
        "tools/call" -> do
            if not (serverInitialized state)
                then
                    return $
                        toJSON $
                            JSONRPCError "2.0" reqId $
                                JSONRPCErrorInfo (-32002) "Server not initialized" Nothing
                else case params of
                    Just p -> case fromJSON p of
                        Aeson.Success callParams@(CallToolParams name args) -> do
                            result <- handleCallTool callParams
                            let argsJson = maybe (toJSON Aeson.Null) toJSON args
                                resultJson = toJSON result
                                success = maybe True not (MCP.Protocol.isError result)
                            liftIO $ traceWith opTracer $ OperationToolCall name argsJson resultJson Nothing success
                            return $ toJSON $ JSONRPCResponse "2.0" reqId (toJSON result)
                        Aeson.Error e ->
                            return $
                                toJSON $
                                    JSONRPCError "2.0" reqId $
                                        JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
                    Nothing ->
                        return $
                            toJSON $
                                JSONRPCError "2.0" reqId $
                                    JSONRPCErrorInfo (-32602) "Missing params" Nothing
        "prompts/list" -> do
            liftIO $ traceWith opTracer OperationPromptsList
            if not (serverInitialized state)
                then
                    return $
                        toJSON $
                            JSONRPCError "2.0" reqId $
                                JSONRPCErrorInfo (-32002) "Server not initialized" Nothing
                else case params of
                    Just p -> case fromJSON p of
                        Aeson.Success listParams -> do
                            result <- handleListPrompts listParams
                            return $ toJSON $ JSONRPCResponse "2.0" reqId (toJSON result)
                        Aeson.Error e ->
                            return $
                                toJSON $
                                    JSONRPCError "2.0" reqId $
                                        JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
                    Nothing -> do
                        result <- handleListPrompts (ListPromptsParams Nothing)
                        return $ toJSON $ JSONRPCResponse "2.0" reqId (toJSON result)
        "prompts/get" -> do
            if not (serverInitialized state)
                then
                    return $
                        toJSON $
                            JSONRPCError "2.0" reqId $
                                JSONRPCErrorInfo (-32002) "Server not initialized" Nothing
                else case params of
                    Just p -> case fromJSON p of
                        Aeson.Success getParams@(GetPromptParams name args) -> do
                            liftIO $ traceWith opTracer $ OperationPromptGet name (toJSON <$> args)
                            result <- handleGetPrompt getParams
                            return $ toJSON $ JSONRPCResponse "2.0" reqId (toJSON result)
                        Aeson.Error e ->
                            return $
                                toJSON $
                                    JSONRPCError "2.0" reqId $
                                        JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
                    Nothing ->
                        return $
                            toJSON $
                                JSONRPCError "2.0" reqId $
                                    JSONRPCErrorInfo (-32602) "Missing params" Nothing
        "completion/complete" -> do
            if not (serverInitialized state)
                then
                    return $
                        toJSON $
                            JSONRPCError "2.0" reqId $
                                JSONRPCErrorInfo (-32002) "Server not initialized" Nothing
                else case params of
                    Just p -> case fromJSON p of
                        Aeson.Success completeParams@(CompleteParams ref (CompletionArgument argName argValue) _context) -> do
                            let refType = case ref of
                                    PromptRef _ -> "prompt"
                                    ResourceTemplateRef _ -> "resource_template"
                            liftIO $ traceWith opTracer $ OperationComplete refType (argName <> "=" <> argValue)
                            result <- handleComplete completeParams
                            return $ toJSON $ JSONRPCResponse "2.0" reqId (toJSON result)
                        Aeson.Error e ->
                            return $
                                toJSON $
                                    JSONRPCError "2.0" reqId $
                                        JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
                    Nothing ->
                        return $
                            toJSON $
                                JSONRPCError "2.0" reqId $
                                    JSONRPCErrorInfo (-32602) "Missing params" Nothing
        "logging/setLevel" -> case params of
            Just p -> case fromJSON p of
                Aeson.Success setLevelParams -> do
                    _ <- handleSetLevel setLevelParams
                    return $ toJSON $ JSONRPCResponse "2.0" reqId (object [])
                Aeson.Error e ->
                    return $
                        toJSON $
                            JSONRPCError "2.0" reqId $
                                JSONRPCErrorInfo (-32602) ("Invalid params: " <> T.pack e) Nothing
            Nothing ->
                return $
                    toJSON $
                        JSONRPCError "2.0" reqId $
                            JSONRPCErrorInfo (-32602) "Missing params" Nothing
        _ ->
            return $
                toJSON $
                    JSONRPCError "2.0" reqId $
                        JSONRPCErrorInfo (-32601) "Method not found" Nothing

-- | Handle HTTP initialize request
handleInitializeHTTP :: IOTracer ServerTrace -> RequestId -> InitializeParams -> MCPServerM ()
handleInitializeHTTP tracer _ params = do
    config <- ask
    state <- get

    let InitializeParams{capabilities = clientCaps, clientInfo = clientImpl} = params

    put
        state
            { serverInitialized = True
            , clientCapabilities = Just clientCaps
            , serverInfo = Just (configServerInfo config)
            }

    -- Emit ServerInitialized trace
    let Implementation clientName _ _ = clientImpl
    liftIO $ traceWith tracer (ServerInitialized (Just clientName))

    -- Emit ServerCapabilityNegotiation trace
    let caps = extractCapabilityNames (serverCapabilities state)
    liftIO $ traceWith tracer (ServerCapabilityNegotiation caps)

-- | Extract capability names from ServerCapabilities
extractCapabilityNames :: ServerCapabilities -> [Text]
extractCapabilityNames (ServerCapabilities res prpts tls comps logCap _exp) =
    concat
        [ maybe [] (const ["resources"]) res
        , maybe [] (const ["prompts"]) prpts
        , maybe [] (const ["tools"]) tls
        , maybe [] (const ["completions"]) comps
        , maybe [] (const ["logging"]) logCap
        ]

-- | Default demo OAuth configuration for testing purposes
defaultDemoOAuthConfig :: OAuthConfig
defaultDemoOAuthConfig =
    OAuthConfig
        { oauthEnabled = True
        , oauthProviders = []
        , tokenValidationEndpoint = Nothing
        , requireHTTPS = False -- For demo only
        -- Default timing parameters
        , authCodeExpirySeconds = 600 -- 10 minutes
        , accessTokenExpirySeconds = 3600 -- 1 hour
        -- Default OAuth parameters
        , supportedScopes = [Scope "mcp:read", Scope "mcp:write"]
        , supportedResponseTypes = [ResponseCode]
        , supportedGrantTypes = [GrantAuthorizationCode, GrantRefreshToken]
        , supportedAuthMethods = [AuthNone]
        , supportedCodeChallengeMethods = [S256]
        , -- Demo mode settings (no longer auto-approve, now requires login)
          autoApproveAuth = False
        , demoUserIdTemplate = Nothing
        , demoEmailDomain = "example.com"
        , demoUserName = "Test User"
        , publicClientSecret = Just ""
        , -- Default token prefixes
          authCodePrefix = "code_"
        , refreshTokenPrefix = "rt_"
        , clientIdPrefix = "client_"
        , -- Default response template
          authorizationSuccessTemplate = Nothing
        , -- Credential management
          credentialStore = defaultDemoCredentialStore
        , loginSessionExpirySeconds = 600 -- 10 minutes
        }

-- | Default protected resource metadata for a given base URL
defaultProtectedResourceMetadata :: Text -> ProtectedResourceMetadata
defaultProtectedResourceMetadata baseUrl =
    ProtectedResourceMetadata
        { resource = baseUrl
        , authorizationServers = [baseUrl]
        , scopesSupported = Just [Scope "mcp:read", Scope "mcp:write"]
        , bearerMethodsSupported = Just ["header"]
        , resourceName = Nothing
        , resourceDocumentation = Nothing
        }

-- | Map scope to human-readable description
scopeToDescription :: Text -> Text
scopeToDescription "mcp:read" = "Read MCP resources"
scopeToDescription "mcp:write" = "Write MCP resources"
scopeToDescription "mcp:tools" = "Execute MCP tools"
scopeToDescription other = other -- fallback to raw scope

-- | Format scopes as human-readable descriptions
formatScopeDescriptions :: Text -> Text
formatScopeDescriptions scopes =
    let scopeList = T.splitOn " " scopes
        descriptions = map scopeToDescription scopeList
     in T.intercalate ", " descriptions

-- | Render login page HTML
renderLoginPage :: Text -> Text -> Maybe Text -> Text -> Text
renderLoginPage clientName scopes mResource sessionId =
    T.unlines
        [ "<!DOCTYPE html>"
        , "<html>"
        , "<head>"
        , "  <meta charset=\"utf-8\">"
        , "  <title>Sign In - MCP Server</title>"
        , "  <style>"
        , "    body { font-family: system-ui, sans-serif; max-width: 500px; margin: 50px auto; padding: 20px; }"
        , "    h1 { color: #333; }"
        , "    form { margin-top: 20px; }"
        , "    label { display: block; margin: 15px 0 5px; }"
        , "    input[type=text], input[type=password] { width: 100%; padding: 8px; box-sizing: border-box; }"
        , "    button { margin-top: 20px; margin-right: 10px; padding: 10px 20px; }"
        , "    .info { background: #f0f0f0; padding: 15px; border-radius: 5px; margin: 20px 0; }"
        , "  </style>"
        , "</head>"
        , "<body>"
        , "  <h1>Sign In</h1>"
        , "  <div class=\"info\">"
        , "    <p>Application <strong>" <> escapeHtml clientName <> "</strong> is requesting access.</p>"
        , "    <p>Permissions requested: " <> escapeHtml (formatScopeDescriptions scopes) <> "</p>"
        , case mResource of
            Just res -> "    <p>Resource: " <> escapeHtml res <> "</p>"
            Nothing -> ""
        , "  </div>"
        , "  <form method=\"POST\" action=\"/login\">"
        , "    <input type=\"hidden\" name=\"session_id\" value=\"" <> escapeHtml sessionId <> "\">"
        , "    <label>Username:"
        , "      <input type=\"text\" name=\"username\" required autofocus>"
        , "    </label>"
        , "    <label>Password:"
        , "      <input type=\"password\" name=\"password\" required>"
        , "    </label>"
        , "    <button type=\"submit\" name=\"action\" value=\"login\">Sign In</button>"
        , "    <button type=\"submit\" name=\"action\" value=\"deny\">Deny</button>"
        , "  </form>"
        , "</body>"
        , "</html>"
        ]

-- | Render error page HTML
renderErrorPage :: Text -> Text -> Text
renderErrorPage errorTitle errorMessage =
    T.unlines
        [ "<!DOCTYPE html>"
        , "<html>"
        , "<head>"
        , "  <meta charset=\"utf-8\">"
        , "  <title>Error - MCP Server</title>"
        , "  <style>"
        , "    body { font-family: system-ui, sans-serif; max-width: 500px; margin: 50px auto; padding: 20px; }"
        , "    h1 { color: #d32f2f; }"
        , "    .error { background: #ffebee; padding: 15px; border-radius: 5px; border-left: 4px solid #d32f2f; }"
        , "  </style>"
        , "</head>"
        , "<body>"
        , "  <h1>" <> escapeHtml errorTitle <> "</h1>"
        , "  <div class=\"error\">"
        , "    <p>" <> escapeHtml errorMessage <> "</p>"
        , "  </div>"
        , "  <p>Please contact the application developer.</p>"
        , "</body>"
        , "</html>"
        ]

-- | Escape HTML special characters
escapeHtml :: Text -> Text
escapeHtml = T.replace "&" "&amp;" . T.replace "<" "&lt;" . T.replace ">" "&gt;" . T.replace "\"" "&quot;" . T.replace "'" "&#39;"

-- | Run the MCP server as an HTTP server
runServerHTTP :: (MCPServer MCPServerM) => HTTPServerConfig -> IOTracer HTTPTrace -> IO ()
runServerHTTP config tracer = do
    -- Initialize JWT settings if OAuth is enabled
    jwtSettings <- case httpJWK config of
        Just jwk -> return $ defaultJWTSettings jwk
        Nothing -> defaultJWTSettings <$> generateKey

    -- Initialize the server state
    stateVar <- newTVarIO $ initialServerState (httpCapabilities config)

    -- Initialize OAuth state (NEW: using typeclass-based OAuthTVarEnv)
    oauthEnv <- newOAuthTVarEnv defaultExpiryConfig

    traceWith tracer $ HTTPServerStarting (httpPort config) (httpBaseUrl config)

    when (maybe False oauthEnabled (httpOAuthConfig config)) $ do
        let authEp = httpBaseUrl config <> "/authorize"
            tokenEp = httpBaseUrl config <> "/token"
        traceWith tracer $ HTTPOAuthEnabled authEp tokenEp
        case httpOAuthConfig config >>= \cfg -> if null (oauthProviders cfg) then Nothing else Just (oauthProviders cfg) of
            Just providers -> do
                traceWith tracer $ HTTPOAuthProviders (map providerName providers)
                when (any requiresPKCE providers) $ traceWith tracer HTTPPKCEEnabled
            Nothing -> return ()

    traceWith tracer HTTPServerStarted
    run (httpPort config) (mcpApp config tracer stateVar oauthEnv jwtSettings)
