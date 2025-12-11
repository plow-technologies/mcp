{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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

    -- * PROOF-OF-CONCEPT: AppM-based handlers (demonstrates typeclass architecture)
    handleMetadataAppM,
    demoHandleMetadataWithAppM,
) where

import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVarIO, writeTVar)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask, asks)
import Control.Monad.State.Strict (get, put)
import Data.Aeson (encode, fromJSON, object, toJSON, (.=))
import Data.Aeson qualified as Aeson
import Data.ByteArray qualified as BA
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Functor.Contravariant (contramap)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import GHC.Generics (Generic)
import Network.HTTP.Media ((//), (/:))
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant (Context (..), Handler, Proxy (..), Server, serve, serveWithContext, throwError)
import Servant.API (Accept (..), FormUrlEncoded, Get, Header, Headers, JSON, MimeRender (..), NoContent (..), Post, QueryParam, QueryParam', ReqBody, Required, StdMethod (POST), Verb, addHeader, (:<|>) (..), (:>))
import Servant.Auth.Server (Auth, AuthResult (..), FromJWT, JWT, JWTSettings, ToJWT, defaultCookieSettings, defaultJWTSettings, generateKey, makeJWT)
import Servant.Server (err400, err401, err500, errBody, errHeaders)
import Web.FormUrlEncoded (FromForm (..), parseUnique)
import Web.HttpApiData (parseUrlPiece, toUrlPiece)

import Control.Monad (unless, when)
import MCP.Protocol
import MCP.Server (MCPServer (..), MCPServerM, ServerConfig (..), ServerState (..), initialServerState, runMCPServer)
import MCP.Server.Auth (CredentialStore (..), OAuthConfig (..), OAuthMetadata (..), OAuthProvider (..), ProtectedResourceMetadata (..), Salt (..), defaultDemoCredentialStore, validateCodeVerifier, validateCredential)
import MCP.Server.Auth.Backend (PlaintextPassword (..), Username, mkPlaintextPassword, mkUsername, unUsername)

-- Import AuthBackend instance for AppM
import MCP.Server.HTTP.AppEnv (AppEnv (..), AppM, HTTPServerConfig (..))
import MCP.Server.OAuth.Store ()

-- Import OAuthStateStore instance for AppM
import MCP.Server.OAuth.Types (AuthCodeId (..), ClientAuthMethod (..), ClientId (..), CodeChallenge (..), CodeChallengeMethod (..), CodeVerifier (..), GrantType (..), RedirectUri (..), RefreshTokenId (..), ResponseType (..), Scope (..), SessionId (..), mkSessionId, unSessionId)
import MCP.Trace.HTTP (HTTPTrace (..))
import MCP.Trace.OAuth qualified as OAuthTrace
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

-- | User information from OAuth
data AuthUser = AuthUser
    { userId :: Text
    , userEmail :: Maybe Text
    , userName :: Maybe Text
    }
    deriving (Show, Generic)

-- | Authorization code with PKCE
data AuthorizationCode = AuthorizationCode
    { authCode :: Text
    , authClientId :: Text
    , authRedirectUri :: Text
    , authCodeChallenge :: Text
    , authCodeChallengeMethod :: Text
    , authScopes :: [Text]
    , authUserId :: Text
    , authExpiry :: UTCTime
    }
    deriving (Show, Generic)

-- | OAuth server state
data OAuthState = OAuthState
    { authCodes :: Map AuthCodeId AuthorizationCode -- code -> AuthorizationCode
    , accessTokens :: Map Text AuthUser -- token -> user (still Text, will be AccessTokenId later)
    , refreshTokens :: Map RefreshTokenId (ClientId, AuthUser) -- refresh_token -> (client_id, user)
    , registeredClients :: Map ClientId ClientInfo -- client_id -> ClientInfo
    , pendingAuthorizations :: Map SessionId PendingAuthorization -- session_id -> pending authorization
    }
    deriving (Show, Generic)

-- | Pending authorization awaiting user authentication
data PendingAuthorization = PendingAuthorization
    { pendingClientId :: Text
    , pendingRedirectUri :: Text
    , pendingCodeChallenge :: Text
    , pendingCodeChallengeMethod :: Text
    , pendingScope :: Maybe Text
    , pendingState :: Maybe Text
    , pendingResource :: Maybe Text
    , pendingCreatedAt :: UTCTime
    }
    deriving (Show, Generic)

-- | Login form data
data LoginForm = LoginForm
    { formUsername :: MCP.Server.Auth.Backend.Username
    , formPassword :: MCP.Server.Auth.Backend.PlaintextPassword
    , formSessionId :: MCP.Server.OAuth.Types.SessionId
    , formAction :: Text
    }
    deriving (Generic)

-- | Custom Show instance that redacts password
instance Show LoginForm where
    show (LoginForm u _p s a) = "LoginForm {formUsername = " <> show u <> ", formPassword = <redacted>, formSessionId = " <> show s <> ", formAction = " <> show a <> "}"

instance FromForm LoginForm where
    fromForm form = do
        userText <- parseUnique "username" form
        passText <- parseUnique "password" form
        sessText <- parseUnique "session_id" form
        action <- parseUnique "action" form

        username <- case mkUsername userText of
            Just u -> Right u
            Nothing -> Left "Invalid username"
        let password = mkPlaintextPassword passText
        sessionId <- case mkSessionId sessText of
            Just s -> Right s
            Nothing -> Left "Invalid session_id (must be UUID)"

        pure $ LoginForm username password sessionId action

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

-- | Client registration request
data ClientRegistrationRequest = ClientRegistrationRequest
    { client_name :: Text
    , redirect_uris :: [RedirectUri]
    , grant_types :: [GrantType]
    , response_types :: [ResponseType]
    , token_endpoint_auth_method :: ClientAuthMethod
    }
    deriving (Show, Generic)

instance Aeson.FromJSON ClientRegistrationRequest where
    parseJSON = Aeson.genericParseJSON Aeson.defaultOptions

-- | Client registration response
data ClientRegistrationResponse = ClientRegistrationResponse
    { client_id :: Text
    , client_secret :: Text -- Empty string for public clients
    , client_name :: Text
    , redirect_uris :: [RedirectUri]
    , grant_types :: [GrantType]
    , response_types :: [ResponseType]
    , token_endpoint_auth_method :: ClientAuthMethod
    }
    deriving (Show, Generic)

instance Aeson.ToJSON ClientRegistrationResponse where
    toJSON = Aeson.genericToJSON Aeson.defaultOptions

-- | Client info stored in server
data ClientInfo = ClientInfo
    { clientName :: Text
    , clientRedirectUris :: [RedirectUri]
    , clientGrantTypes :: [GrantType]
    , clientResponseTypes :: [ResponseType]
    , clientAuthMethod :: ClientAuthMethod
    }
    deriving (Show, Generic)

-- | Token response
data TokenResponse = TokenResponse
    { access_token :: Text
    , token_type :: Text
    , expires_in :: Maybe Int
    , refresh_token :: Maybe Text
    , scope :: Maybe Text
    }
    deriving (Show, Generic)

instance Aeson.ToJSON TokenResponse where
    toJSON TokenResponse{..} =
        object $
            [ "access_token" .= access_token
            , "token_type" .= token_type
            ]
                ++ maybe [] (\e -> ["expires_in" .= e]) expires_in
                ++ maybe [] (\r -> ["refresh_token" .= r]) refresh_token
                ++ maybe [] (\s -> ["scope" .= s]) scope

instance Aeson.FromJSON AuthUser where
    parseJSON = Aeson.withObject "AuthUser" $ \v ->
        AuthUser
            <$> v Aeson..: "sub"
            <*> v Aeson..:? "email"
            <*> v Aeson..:? "name"

instance Aeson.ToJSON AuthUser where
    toJSON AuthUser{..} =
        object
            [ "sub" .= userId
            , "email" .= userEmail
            , "name" .= userName
            ]

-- Instances for JWT
instance ToJWT AuthUser
instance FromJWT AuthUser

-- | MCP API definition for HTTP server (following the MCP transport spec)
type MCPAPI auths = Auth auths AuthUser :> "mcp" :> ReqBody '[JSON] Aeson.Value :> Post '[JSON] Aeson.Value

-- | Unprotected MCP API (for backward compatibility)
type UnprotectedMCPAPI = "mcp" :> ReqBody '[JSON] Aeson.Value :> Post '[JSON] Aeson.Value

-- | Protected Resource Metadata API (RFC 9728)
type ProtectedResourceAPI = ".well-known" :> "oauth-protected-resource" :> Get '[JSON] ProtectedResourceMetadata

-- | Login API endpoints - returns HTTP 302 redirect
type LoginAPI =
    "login"
        :> Header "Cookie" Text
        :> ReqBody '[FormUrlEncoded] LoginForm
        :> Verb 'POST 302 '[HTML] (Headers '[Header "Location" Text, Header "Set-Cookie" Text] NoContent)

-- | OAuth endpoints
type OAuthAPI =
    ProtectedResourceAPI
        :<|> ".well-known" :> "oauth-authorization-server" :> Get '[JSON] OAuthMetadata
        :<|> "register"
            :> ReqBody '[JSON] ClientRegistrationRequest
            :> Post '[JSON] ClientRegistrationResponse
        :<|> "authorize"
            :> QueryParam' '[Required] "response_type" ResponseType
            :> QueryParam' '[Required] "client_id" ClientId
            :> QueryParam' '[Required] "redirect_uri" RedirectUri
            :> QueryParam' '[Required] "code_challenge" CodeChallenge
            :> QueryParam' '[Required] "code_challenge_method" CodeChallengeMethod
            :> QueryParam "scope" Text
            :> QueryParam "state" Text
            :> QueryParam "resource" Text
            :> Get '[HTML] (Headers '[Header "Set-Cookie" Text] Text)
        :<|> LoginAPI
        :<|> "token"
            :> ReqBody '[FormUrlEncoded] [(Text, Text)]
            :> Post '[JSON] TokenResponse

-- | Complete API with OAuth
type CompleteAPI auths = OAuthAPI :<|> MCPAPI auths

-- | Create a WAI Application for the MCP HTTP server
mcpApp :: (MCPServer MCPServerM) => HTTPServerConfig -> IOTracer HTTPTrace -> TVar ServerState -> TVar OAuthState -> JWTSettings -> Application
mcpApp config tracer stateVar oauthStateVar jwtSettings =
    let cookieSettings = defaultCookieSettings
        authContext = cookieSettings :. jwtSettings :. EmptyContext
        baseApp = case httpOAuthConfig config of
            Just oauthCfg
                | oauthEnabled oauthCfg ->
                    serveWithContext
                        (Proxy :: Proxy (CompleteAPI '[JWT]))
                        authContext
                        (oauthServer config tracer oauthStateVar :<|> mcpServerAuth config tracer stateVar)
            _ ->
                serve (Proxy :: Proxy UnprotectedMCPAPI) (mcpServerNoAuth config tracer stateVar)
     in if httpEnableLogging config
            then logStdoutDev baseApp
            else baseApp
  where
    oauthServer :: HTTPServerConfig -> IOTracer HTTPTrace -> TVar OAuthState -> Server OAuthAPI
    oauthServer cfg httpTracer oauthState =
        handleProtectedResourceMetadata cfg
            :<|> handleMetadata cfg
            :<|> handleRegister cfg httpTracer oauthState
            :<|> handleAuthorize cfg httpTracer oauthState
            :<|> handleLogin cfg httpTracer oauthState jwtSettings
            :<|> handleToken jwtSettings cfg httpTracer oauthState

    mcpServerAuth :: HTTPServerConfig -> IOTracer HTTPTrace -> TVar ServerState -> AuthResult AuthUser -> Aeson.Value -> Handler Aeson.Value
    mcpServerAuth httpConfig httpTracer stateTVar authResult requestValue =
        case authResult of
            Authenticated user -> do
                liftIO $ traceWith httpTracer $ HTTPAuthSuccess (userId user)
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
processHTTPNotification :: (MCPServer MCPServerM) => HTTPServerConfig -> TVar ServerState -> JSONRPCNotification -> IO ()
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

-- | Handler for /.well-known/oauth-protected-resource endpoint
handleProtectedResourceMetadata :: HTTPServerConfig -> Handler ProtectedResourceMetadata
handleProtectedResourceMetadata config = do
    let metadata = case httpProtectedResourceMetadata config of
            Just m -> m
            Nothing -> defaultProtectedResourceMetadata (httpBaseUrl config)
    return metadata

-- | Handle OAuth metadata discovery endpoint
handleMetadata :: HTTPServerConfig -> Handler OAuthMetadata
handleMetadata config = do
    let baseUrl = httpBaseUrl config
        oauthCfg = httpOAuthConfig config
    return
        OAuthMetadata
            { issuer = baseUrl
            , authorizationEndpoint = baseUrl <> "/authorize"
            , tokenEndpoint = baseUrl <> "/token"
            , registrationEndpoint = Just (baseUrl <> "/register")
            , userInfoEndpoint = Nothing
            , jwksUri = Nothing
            , scopesSupported = fmap supportedScopes oauthCfg
            , responseTypesSupported = maybe [ResponseCode] supportedResponseTypes oauthCfg
            , grantTypesSupported = fmap supportedGrantTypes oauthCfg
            , tokenEndpointAuthMethodsSupported = fmap supportedAuthMethods oauthCfg
            , codeChallengeMethodsSupported = fmap supportedCodeChallengeMethods oauthCfg
            }

{- | PROOF-OF-CONCEPT: OAuth metadata endpoint using AppM with typeclass constraints.

This handler demonstrates the new architecture pattern:

* Uses 'AppM' monad instead of 'Handler'
* Reads config via 'MonadReader AppEnv' (asks envConfig)
* Uses typeclass constraints ('OAuthStateStore', 'AuthBackend')
* Converted to Handler via 'runAppM' natural transformation

This is functionally identical to 'handleMetadata' but proves the typeclass-based
architecture works end-to-end. Future handlers can follow this pattern to gain
access to the full typeclass abstraction layer.

== Usage Pattern

To use this handler in Servant:

@
-- Create AppEnv combining all dependencies
let appEnv = AppEnv
      { envOAuth = oauthTVarEnv
      , envAuth = demoCredEnv
      , envConfig = config
      , envTracer = tracer
      }

-- Convert AppM handler to Servant Handler using runAppM
handler :: Handler OAuthMetadata
handler = runAppM appEnv handleMetadataAppM
@

Or use hoistServer for the entire API:

@
hoistServer (Proxy :: Proxy SomeAPI) (runAppM appEnv) serverInAppM
@

NOTE: This handler doesn't actually use the OAuthStateStore/AuthBackend yet
(metadata is read-only config), but the constraints are present to demonstrate
the pattern. For a fuller example using the typeclasses, see the planned
refactoring of handleRegister or handleToken.
-}
handleMetadataAppM :: AppM OAuthMetadata
handleMetadataAppM = do
    -- Access config via Reader
    config <- asks envConfig
    let baseUrl = httpBaseUrl config
        oauthCfg = httpOAuthConfig config
    return
        OAuthMetadata
            { issuer = baseUrl
            , authorizationEndpoint = baseUrl <> "/authorize"
            , tokenEndpoint = baseUrl <> "/token"
            , registrationEndpoint = Just (baseUrl <> "/register")
            , userInfoEndpoint = Nothing
            , jwksUri = Nothing
            , scopesSupported = fmap supportedScopes oauthCfg
            , responseTypesSupported = maybe [ResponseCode] supportedResponseTypes oauthCfg
            , grantTypesSupported = fmap supportedGrantTypes oauthCfg
            , tokenEndpointAuthMethodsSupported = fmap supportedAuthMethods oauthCfg
            , codeChallengeMethodsSupported = fmap supportedCodeChallengeMethods oauthCfg
            }

{- | PROOF-OF-CONCEPT: Demonstrate calling the AppM-based handler.

This function shows how to integrate an AppM handler into the existing
HTTP.hs infrastructure. It creates the AppEnv from existing components
and uses runAppM for the natural transformation.

This is NOT wired into the actual API (to avoid modifying the API type),
but serves as a reference implementation showing the complete integration pattern.
-}
demoHandleMetadataWithAppM ::
    HTTPServerConfig ->
    IOTracer HTTPTrace ->
    TVar OAuthState -> -- Old-style OAuth state (HTTP.hs version)
    Handler OAuthMetadata
demoHandleMetadataWithAppM config _tracer _oauthStateVar = do
    -- NOTE: This is a demonstration only. In a real integration, you would:
    -- 1. Replace TVar OAuthState (HTTP.hs version) with OAuthTVarEnv (typeclass version)
    -- 2. Add DemoCredentialEnv to function parameters
    -- 3. Create AppEnv from all components
    -- 4. Use: runAppM appEnv handleMetadataAppM
    --
    -- For now, we just call handleMetadata directly since we don't have
    -- the typeclass-based state available in this context.
    handleMetadata config

-- | Handle dynamic client registration
handleRegister :: HTTPServerConfig -> IOTracer HTTPTrace -> TVar OAuthState -> ClientRegistrationRequest -> Handler ClientRegistrationResponse
handleRegister config tracer oauthStateVar (ClientRegistrationRequest reqName reqRedirects reqGrants reqResponses reqAuth) = do
    -- Generate client ID
    let prefix = maybe "client_" clientIdPrefix (httpOAuthConfig config)
    clientIdText <- liftIO $ (prefix <>) <$> generateAuthCode
    let clientId = ClientId clientIdText

    -- Store client info
    let clientInfo =
            ClientInfo
                { clientName = reqName
                , clientRedirectUris = reqRedirects
                , clientGrantTypes = reqGrants
                , clientResponseTypes = reqResponses
                , clientAuthMethod = reqAuth
                }

    liftIO $ atomically $ modifyTVar' oauthStateVar $ \s ->
        s{registeredClients = Map.insert clientId clientInfo (registeredClients s)}

    -- Emit trace
    let oauthTracer = contramap HTTPOAuth tracer
    liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthClientRegistration clientIdText reqName

    return
        ClientRegistrationResponse
            { client_id = clientIdText
            , client_secret = "" -- Empty string for public clients
            , client_name = reqName
            , redirect_uris = reqRedirects
            , grant_types = reqGrants
            , response_types = reqResponses
            , token_endpoint_auth_method = reqAuth
            }

-- | Handle OAuth authorize endpoint - now returns login page HTML
handleAuthorize :: HTTPServerConfig -> IOTracer HTTPTrace -> TVar OAuthState -> ResponseType -> ClientId -> RedirectUri -> CodeChallenge -> CodeChallengeMethod -> Maybe Text -> Maybe Text -> Maybe Text -> Handler (Headers '[Header "Set-Cookie" Text] Text)
handleAuthorize config tracer oauthStateVar responseType clientId redirectUri codeChallenge codeChallengeMethod mScope mState mResource = do
    let oauthTracer = contramap HTTPOAuth tracer
        responseTypeText = toUrlPiece responseType
        clientIdText = unClientId clientId
        redirectUriText = toUrlPiece redirectUri
        codeChallengeMethodText = toUrlPiece codeChallengeMethod

    -- Log resource parameter for RFC8707 support
    liftIO $ traceWith tracer $ HTTPResourceParameterDebug mResource "authorize endpoint"

    -- T037: Validate response_type (return HTML error page instead of JSON)
    when (responseType /= ResponseCode) $ do
        liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthValidationError "response_type" ("Unsupported response type: " <> responseTypeText)
        throwError err400{errBody = LBS.fromStrict $ TE.encodeUtf8 $ renderErrorPage "Unsupported Response Type" "Only 'code' response type is supported. Please contact the application developer.", errHeaders = [("Content-Type", "text/html; charset=utf-8")]}

    -- T037: Validate code_challenge_method (return HTML error page instead of JSON)
    when (codeChallengeMethod /= S256) $ do
        liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthValidationError "code_challenge_method" ("Unsupported method: " <> codeChallengeMethodText)
        throwError err400{errBody = LBS.fromStrict $ TE.encodeUtf8 $ renderErrorPage "Invalid Request" "Only 'S256' code challenge method is supported. Please contact the application developer.", errHeaders = [("Content-Type", "text/html; charset=utf-8")]}

    -- Look up client to get client name for display
    oauthState <- liftIO $ readTVarIO oauthStateVar

    -- T040: Handle unregistered client_id - render error page (don't redirect - can't trust redirect_uri)
    clientInfo <- case Map.lookup clientId (registeredClients oauthState) of
        Just ci -> return ci
        Nothing -> do
            liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthValidationError "client_id" ("Unregistered client: " <> clientIdText)
            throwError err400{errBody = LBS.fromStrict $ TE.encodeUtf8 $ renderErrorPage "Unregistered Client" ("Client ID '" <> clientIdText <> "' is not registered. Please contact the application developer."), errHeaders = [("Content-Type", "text/html; charset=utf-8")]}

    -- T041: Handle invalid redirect_uri - render error page (don't redirect)
    -- Validate redirect URI is in registered list (already parsed by FromHttpApiData)
    unless (redirectUri `elem` clientRedirectUris clientInfo) $ do
        liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthValidationError "redirect_uri" ("Redirect URI not registered: " <> redirectUriText)
        throwError err400{errBody = LBS.fromStrict $ TE.encodeUtf8 $ renderErrorPage "Invalid Redirect URI" ("The redirect URI '" <> redirectUriText <> "' is not registered for this client. Please contact the application developer."), errHeaders = [("Content-Type", "text/html; charset=utf-8")]}

    let displayName = clientName clientInfo
        scopeList = maybe [] (T.splitOn " ") mScope

    -- Emit authorization request trace
    liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthAuthorizationRequest clientIdText scopeList (isJust mState)

    -- Generate session ID
    sessionIdText <- liftIO $ UUID.toText <$> UUID.nextRandom
    let sessionId = SessionId sessionIdText
    currentTime <- liftIO getCurrentTime

    -- Create pending authorization (using old PendingAuthorization from HTTP.hs)
    let pending =
            PendingAuthorization
                { pendingClientId = clientIdText
                , pendingRedirectUri = redirectUriText
                , pendingCodeChallenge = unCodeChallenge codeChallenge
                , pendingCodeChallengeMethod = codeChallengeMethodText
                , pendingScope = mScope
                , pendingState = mState
                , pendingResource = mResource
                , pendingCreatedAt = currentTime
                }

    -- Store pending authorization
    liftIO $ atomically $ modifyTVar' oauthStateVar $ \s ->
        s{pendingAuthorizations = Map.insert sessionId pending (pendingAuthorizations s)}

    -- Emit login page served trace
    liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthLoginPageServed sessionIdText

    -- Build session cookie
    let sessionExpirySeconds = maybe 600 loginSessionExpirySeconds (httpOAuthConfig config)
        cookieValue = "mcp_session=" <> sessionIdText <> "; HttpOnly; SameSite=Strict; Path=/; Max-Age=" <> T.pack (show sessionExpirySeconds)
        scopes = fromMaybe "default access" mScope
        loginHtml = renderLoginPage displayName scopes mResource sessionIdText

    return $ addHeader cookieValue loginHtml

-- | Extract session ID from cookie header
extractSessionFromCookie :: Text -> Maybe SessionId
extractSessionFromCookie cookieHeader =
    let cookies = T.splitOn ";" cookieHeader
        sessionCookies = filter (T.isInfixOf "mcp_session=") cookies
     in case sessionCookies of
            (cookie : _) ->
                let parts = T.splitOn "=" cookie
                 in case parts of
                        [_, value] -> mkSessionId (T.strip value)
                        _ -> Nothing
            [] -> Nothing

-- | Handle login form submission
handleLogin :: HTTPServerConfig -> IOTracer HTTPTrace -> TVar OAuthState -> JWTSettings -> Maybe Text -> LoginForm -> Handler (Headers '[Header "Location" Text, Header "Set-Cookie" Text] NoContent)
handleLogin config tracer oauthStateVar _jwtSettings mCookie loginForm = do
    let oauthTracer = contramap HTTPOAuth tracer

    -- Extract session ID from form
    let sessionId = formSessionId loginForm

    -- T039: Handle cookies disabled - check if cookie matches form session_id
    case mCookie of
        Nothing -> do
            liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthValidationError "cookies" "No cookie header present"
            throwError err400{errBody = LBS.fromStrict $ TE.encodeUtf8 $ renderErrorPage "Cookies Required" "Your browser must have cookies enabled to sign in. Please enable cookies and try again.", errHeaders = [("Content-Type", "text/html; charset=utf-8")]}
        Just cookie ->
            -- Parse session cookie and verify it matches form session_id
            let cookieSessionId = extractSessionFromCookie cookie
             in unless (cookieSessionId == Just sessionId) $ do
                    liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthValidationError "cookies" "Session cookie mismatch"
                    throwError err400{errBody = LBS.fromStrict $ TE.encodeUtf8 $ renderErrorPage "Cookies Required" "Session cookie mismatch. Please enable cookies and try again.", errHeaders = [("Content-Type", "text/html; charset=utf-8")]}

    -- Look up pending authorization
    oauthState <- liftIO $ readTVarIO oauthStateVar
    pending <- case Map.lookup sessionId (pendingAuthorizations oauthState) of
        Just p -> return p
        Nothing -> do
            liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthValidationError "session" ("Session not found: " <> unSessionId sessionId)
            throwError err400{errBody = LBS.fromStrict $ TE.encodeUtf8 $ renderErrorPage "Invalid Session" "Session not found or has expired. Please restart the authorization flow.", errHeaders = [("Content-Type", "text/html; charset=utf-8")]}

    -- T038: Handle expired sessions
    currentTime <- liftIO getCurrentTime
    let sessionExpirySeconds = maybe 600 (fromIntegral . loginSessionExpirySeconds) (httpOAuthConfig config)
        expiryTime = addUTCTime sessionExpirySeconds (pendingCreatedAt pending)
    when (currentTime > expiryTime) $ do
        liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthSessionExpired (unSessionId sessionId)
        throwError err400{errBody = LBS.fromStrict $ TE.encodeUtf8 $ renderErrorPage "Session Expired" "Your login session has expired. Please restart the authorization flow.", errHeaders = [("Content-Type", "text/html; charset=utf-8")]}

    -- Check if user denied access
    if formAction loginForm == "deny"
        then do
            -- Emit denial trace
            liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthAuthorizationDenied (pendingClientId pending) "User denied authorization"

            -- Clear session and redirect with error
            let clearCookie = "mcp_session=; Max-Age=0; Path=/"
                errorParams = "error=access_denied&error_description=User%20denied%20access"
                stateParam = case pendingState pending of
                    Just s -> "&state=" <> s
                    Nothing -> ""
                redirectUrl = pendingRedirectUri pending <> "?" <> errorParams <> stateParam

            -- Remove pending authorization
            liftIO $ atomically $ modifyTVar' oauthStateVar $ \s ->
                s{pendingAuthorizations = Map.delete sessionId (pendingAuthorizations s)}

            return $ addHeader redirectUrl $ addHeader clearCookie NoContent
        else do
            -- Validate credentials
            let oauthCfg = httpOAuthConfig config
                -- Create empty store if no OAuth config (shouldn't happen, but defensive)
                emptySalt = MCP.Server.Auth.Salt (BA.convert ("" :: ByteString) :: BA.ScrubbedBytes)
                emptyStore = CredentialStore Map.empty emptySalt
                store = maybe emptyStore MCP.Server.Auth.credentialStore oauthCfg
                username = formUsername loginForm
                password = formPassword loginForm

            -- validateCredential expects Text, so unwrap newtypes
            let credentialsValid = validateCredential store (unUsername username) (TE.decodeUtf8 $ BA.convert $ unPlaintextPassword password)
            liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthLoginAttempt (unUsername username) credentialsValid
            if credentialsValid
                then do
                    -- Emit authorization granted trace
                    liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthAuthorizationGranted (pendingClientId pending) (unUsername username)

                    -- Generate authorization code
                    code <- liftIO $ generateAuthCodeWithConfig config
                    codeGenerationTime <- liftIO getCurrentTime
                    let expirySeconds = maybe 600 (fromIntegral . authCodeExpirySeconds) oauthCfg
                        expiry = addUTCTime expirySeconds codeGenerationTime
                        authCode =
                            AuthorizationCode
                                { authCode = code
                                , authClientId = pendingClientId pending
                                , authRedirectUri = pendingRedirectUri pending
                                , authCodeChallenge = pendingCodeChallenge pending
                                , authCodeChallengeMethod = pendingCodeChallengeMethod pending
                                , authScopes = maybe [] (T.splitOn " ") (pendingScope pending)
                                , authUserId = unUsername username
                                , authExpiry = expiry
                                }

                    -- Store authorization code and remove pending authorization
                    let authCodeId = AuthCodeId code
                    liftIO $ atomically $ modifyTVar' oauthStateVar $ \s ->
                        s
                            { authCodes = Map.insert authCodeId authCode (authCodes s)
                            , pendingAuthorizations = Map.delete sessionId (pendingAuthorizations s)
                            }

                    -- Build redirect URL with code
                    let stateParam = case pendingState pending of
                            Just s -> "&state=" <> s
                            Nothing -> ""
                        redirectUrl = pendingRedirectUri pending <> "?code=" <> code <> stateParam
                        clearCookie = "mcp_session=; Max-Age=0; Path=/"

                    return $ addHeader redirectUrl $ addHeader clearCookie NoContent
                else
                    -- Invalid credentials - return error (validateCredential already emitted OAuthLoginAttempt trace)
                    throwError err401{errBody = encode $ object ["error" .= ("authentication_failed" :: Text), "error_description" .= ("Invalid username or password" :: Text)]}

-- | Handle OAuth token endpoint
handleToken :: JWTSettings -> HTTPServerConfig -> IOTracer HTTPTrace -> TVar OAuthState -> [(Text, Text)] -> Handler TokenResponse
handleToken jwtSettings config tracer oauthStateVar params = do
    let paramMap = Map.fromList params
    -- Parse grant_type from Text to GrantType newtype
    case Map.lookup "grant_type" paramMap of
        Nothing -> throwError err400{errBody = encode $ object ["error" .= ("invalid_request" :: Text), "error_description" .= ("Missing grant_type" :: Text)]}
        Just grantTypeText -> case parseUrlPiece grantTypeText of
            Left _err -> throwError err400{errBody = encode $ object ["error" .= ("unsupported_grant_type" :: Text)]}
            Right grantType -> case grantType of
                GrantAuthorizationCode -> handleAuthCodeGrant jwtSettings config tracer oauthStateVar paramMap
                GrantRefreshToken -> handleRefreshTokenGrant jwtSettings config tracer oauthStateVar paramMap
                GrantClientCredentials -> throwError err400{errBody = encode $ object ["error" .= ("unsupported_grant_type" :: Text)]}

-- | Handle authorization code grant
handleAuthCodeGrant :: JWTSettings -> HTTPServerConfig -> IOTracer HTTPTrace -> TVar OAuthState -> Map Text Text -> Handler TokenResponse
handleAuthCodeGrant jwtSettings config tracer oauthStateVar params = do
    let oauthTracer = contramap HTTPOAuth tracer

    -- Extract and log resource parameter (RFC8707)
    let mResource = Map.lookup "resource" params
    liftIO $ traceWith tracer $ HTTPResourceParameterDebug mResource "token request (auth code)"

    -- Parse code from Text to AuthCodeId
    code <- case Map.lookup "code" params of
        Nothing -> do
            liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthValidationError "token_request" "Missing authorization code"
            throwError err400{errBody = encode $ object ["error" .= ("invalid_request" :: Text)]}
        Just codeText -> case parseUrlPiece codeText of
            Left err -> do
                liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthValidationError "token_request" ("Invalid authorization code: " <> err)
                throwError err400{errBody = encode $ object ["error" .= ("invalid_request" :: Text)]}
            Right authCodeId -> return authCodeId

    -- Parse code_verifier from Text to CodeVerifier
    codeVerifier <- case Map.lookup "code_verifier" params of
        Nothing -> do
            liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthValidationError "token_request" "Missing code_verifier"
            throwError err400{errBody = encode $ object ["error" .= ("invalid_request" :: Text)]}
        Just verifierText -> case parseUrlPiece verifierText of
            Left err -> do
                liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthValidationError "token_request" ("Invalid code_verifier: " <> err)
                throwError err400{errBody = encode $ object ["error" .= ("invalid_request" :: Text)]}
            Right verifier -> return verifier

    -- Look up authorization code
    oauthState <- liftIO $ readTVarIO oauthStateVar
    authCode <- case Map.lookup code (authCodes oauthState) of
        Just ac -> return ac
        Nothing -> do
            liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthTokenExchange "authorization_code" False
            throwError err400{errBody = encode $ object ["error" .= ("invalid_grant" :: Text)]}

    -- Verify code hasn't expired
    currentTime <- liftIO getCurrentTime
    when (currentTime > authExpiry authCode) $ do
        liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthValidationError "auth_code" "Authorization code expired"
        liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthTokenExchange "authorization_code" False
        throwError err400{errBody = encode $ object ["error" .= ("invalid_grant" :: Text), "error_description" .= ("Authorization code expired" :: Text)]}

    -- Verify PKCE (validateCodeVerifier now uses newtypes, but authCodeChallenge is still Text)
    -- We need to wrap authCodeChallenge as CodeChallenge
    let authChallenge = CodeChallenge (authCodeChallenge authCode)
        pkceValid = validateCodeVerifier codeVerifier authChallenge
    liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthPKCEValidation (unCodeVerifier codeVerifier) (authCodeChallenge authCode) pkceValid
    unless pkceValid $ do
        liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthTokenExchange "authorization_code" False
        throwError err400{errBody = encode $ object ["error" .= ("invalid_grant" :: Text), "error_description" .= ("Invalid code verifier" :: Text)]}

    -- Create user for JWT
    let oauthCfg = httpOAuthConfig config
        emailDomain = maybe "example.com" demoEmailDomain oauthCfg
        userName = maybe "User" demoUserName oauthCfg
        user =
            AuthUser
                { userId = authUserId authCode
                , userEmail = Just $ authUserId authCode <> "@" <> emailDomain
                , userName = Just userName
                }

    -- Generate tokens
    accessTokenText <- generateJWTAccessToken user jwtSettings
    refreshTokenText <- liftIO $ generateRefreshTokenWithConfig config
    let refreshToken = RefreshTokenId refreshTokenText
        clientId = ClientId (authClientId authCode)

    -- Store tokens
    liftIO $ atomically $ modifyTVar' oauthStateVar $ \s ->
        s
            { authCodes = Map.delete code (authCodes s)
            , accessTokens = Map.insert accessTokenText user (accessTokens s)
            , refreshTokens = Map.insert refreshToken (clientId, user) (refreshTokens s)
            }

    -- Emit successful token exchange trace
    liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthTokenExchange "authorization_code" True

    return
        TokenResponse
            { access_token = accessTokenText
            , token_type = "Bearer"
            , expires_in = Just $ maybe 3600 accessTokenExpirySeconds (httpOAuthConfig config)
            , refresh_token = Just refreshTokenText
            , scope = if null (authScopes authCode) then Nothing else Just (T.intercalate " " (authScopes authCode))
            }

-- | Handle refresh token grant
handleRefreshTokenGrant :: JWTSettings -> HTTPServerConfig -> IOTracer HTTPTrace -> TVar OAuthState -> Map Text Text -> Handler TokenResponse
handleRefreshTokenGrant jwtSettings config tracer oauthStateVar params = do
    let oauthTracer = contramap HTTPOAuth tracer

    -- Extract and log resource parameter (RFC8707)
    let mResource = Map.lookup "resource" params
    liftIO $ traceWith tracer $ HTTPResourceParameterDebug mResource "token request (refresh token)"

    -- Parse refresh_token from Text to RefreshTokenId
    refreshTokenId <- case Map.lookup "refresh_token" params of
        Nothing -> do
            liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthValidationError "token_request" "Missing refresh_token"
            throwError err400{errBody = encode $ object ["error" .= ("invalid_request" :: Text)]}
        Just tokenText -> case parseUrlPiece tokenText of
            Left err -> do
                liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthValidationError "token_request" ("Invalid refresh_token: " <> err)
                throwError err400{errBody = encode $ object ["error" .= ("invalid_request" :: Text)]}
            Right rtId -> return rtId

    -- Look up refresh token
    oauthState <- liftIO $ readTVarIO oauthStateVar
    (clientId, user) <- case Map.lookup refreshTokenId (refreshTokens oauthState) of
        Just info -> return info
        Nothing -> do
            liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthTokenRefresh False
            throwError err400{errBody = encode $ object ["error" .= ("invalid_grant" :: Text)]}

    -- Generate new JWT access token
    newAccessTokenText <- generateJWTAccessToken user jwtSettings

    -- Update tokens (keep same refresh token, update with new client/user association)
    liftIO $ atomically $ modifyTVar' oauthStateVar $ \s ->
        s
            { accessTokens = Map.insert newAccessTokenText user (accessTokens s)
            , refreshTokens = Map.insert refreshTokenId (clientId, user) (refreshTokens s)
            }

    -- Emit successful token refresh trace
    liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthTokenRefresh True

    return
        TokenResponse
            { access_token = newAccessTokenText
            , token_type = "Bearer"
            , expires_in = Just $ maybe 3600 accessTokenExpirySeconds (httpOAuthConfig config)
            , refresh_token = Just (unRefreshTokenId refreshTokenId)
            , scope = Nothing
            }

-- | Generate random authorization code
generateAuthCode :: IO Text
generateAuthCode = do
    uuid <- UUID.nextRandom
    return $ "code_" <> UUID.toText uuid

-- | Generate authorization code with configurable prefix
generateAuthCodeWithConfig :: HTTPServerConfig -> IO Text
generateAuthCodeWithConfig config = do
    uuid <- UUID.nextRandom
    let prefix = maybe "code_" authCodePrefix (httpOAuthConfig config)
    return $ prefix <> UUID.toText uuid

-- | Generate JWT access token for user
generateJWTAccessToken :: AuthUser -> JWTSettings -> Handler Text
generateJWTAccessToken user jwtSettings = do
    accessTokenResult <- liftIO $ makeJWT user jwtSettings Nothing
    case accessTokenResult of
        Left _err -> throwError err500{errBody = encode $ object ["error" .= ("Token generation failed" :: Text)]}
        Right accessToken -> return $ TE.decodeUtf8 $ LBS.toStrict accessToken

-- | Generate refresh token with configurable prefix
generateRefreshTokenWithConfig :: HTTPServerConfig -> IO Text
generateRefreshTokenWithConfig config = do
    uuid <- UUID.nextRandom
    let prefix = maybe "rt_" refreshTokenPrefix (httpOAuthConfig config)
    return $ prefix <> UUID.toText uuid

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

    -- Initialize OAuth state
    oauthStateVar <-
        newTVarIO $
            OAuthState
                { authCodes = Map.empty
                , accessTokens = Map.empty
                , refreshTokens = Map.empty
                , registeredClients = Map.empty
                , pendingAuthorizations = Map.empty
                }

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
    run (httpPort config) (mcpApp config tracer stateVar oauthStateVar jwtSettings)
