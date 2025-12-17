{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

{- |
Module      : Servant.OAuth2.IDP.Handlers
Description : Polymorphic OAuth 2.1 handler implementations
Copyright   : (C) 2025 Matthias Pall Gissurarson
License     : MIT
Maintainer  : mpg@mpg.is
Stability   : experimental
Portability : GHC

This module provides polymorphic OAuth 2.1 handler implementations using
the OAuthStateStore and AuthBackend typeclasses for pluggable backends.

All handlers are polymorphic over the monad @m@ with typeclass constraints,
allowing them to work with different storage and auth backends:

* In-memory (demo/testing)
* PostgreSQL (production)
* Redis (caching)
* LDAP/Active Directory (authentication)

= Usage

@
-- Use handlers directly in your server implementation
oauthServer :: ServerT OAuthAPI m
oauthServer =
    handleProtectedResourceMetadata
        :<|> handleMetadata
        :<|> handleRegister
        :<|> handleAuthorize
        :<|> handleLogin
        :<|> handleToken
@
-}
module Servant.OAuth2.IDP.Handlers (
    -- * Polymorphic Handlers
    handleMetadata,
    handleProtectedResourceMetadata,
    handleRegister,
    handleAuthorize,
    handleLogin,
    handleToken,
    handleAuthCodeGrant,
    handleRefreshTokenGrant,

    -- * Helper Functions
    extractSessionFromCookie,
    generateAuthCode,
    renderErrorPage,
    renderLoginPage,
    scopeToDescription,
    formatScopeDescriptions,
    escapeHtml,
    generateJWTAccessToken,
    generateRefreshTokenWithConfig,
) where

import Control.Monad (unless, when)
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, asks)
import Data.ByteString.Lazy qualified as LBS
import Data.Functor.Contravariant (contramap)
import Data.Generics.Product (HasType)
import Data.Generics.Product.Typed (getTyped)
import Data.Generics.Sum.Typed (AsType, injectTyped)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time.Clock (addUTCTime)
import Data.UUID.V4 qualified as UUID
import Network.URI (parseURI)
import Servant (
    Header,
    Headers,
    NoContent (..),
    addHeader,
 )
import Servant.Auth.Server (JWTSettings, ToJWT, makeJWT)
import Web.HttpApiData (ToHttpApiData (..), parseUrlPiece)

import Data.UUID qualified as UUID
import MCP.Server.Auth (
    OAuthConfig (..),
    OAuthMetadata (..),
    ProtectedResourceMetadata (..),
    authCodePrefix,
    clientIdPrefix,
    refreshTokenPrefix,
    validateCodeVerifier,
 )
import MCP.Server.HTTP.AppEnv (HTTPServerConfig (..))
import MCP.Server.Time (MonadTime (..))
import MCP.Trace.HTTP (HTTPTrace (..))
import MCP.Trace.OAuth qualified as OAuthTrace
import Plow.Logging (IOTracer, traceWith)
import Servant.OAuth2.IDP.API (
    ClientRegistrationRequest (..),
    ClientRegistrationResponse (..),
    LoginForm (..),
    TokenResponse (..),
 )
import Servant.OAuth2.IDP.Auth.Backend (AuthBackend (..), Username (..))
import Servant.OAuth2.IDP.Auth.Demo () -- Instance only
import Servant.OAuth2.IDP.Store (OAuthStateStore (..))
import Servant.OAuth2.IDP.Types (
    AccessTokenId (..),
    AuthCodeId (..),
    AuthorizationCode (..),
    AuthorizationError (..),
    ClientId (..),
    ClientInfo (..),
    CodeChallenge,
    CodeChallengeMethod (..),
    CodeVerifier (..),
    GrantType (..),
    PendingAuthorization (..),
    RedirectTarget (..),
    RedirectUri,
    RefreshTokenId (..),
    ResponseType (..),
    Scope (..),
    SessionCookie (..),
    SessionId (..),
    ValidationError (..),
    mkScope,
    mkSessionId,
    unClientId,
    unCodeChallenge,
    unCodeVerifier,
    unRefreshTokenId,
    unScope,
    unSessionId,
 )

-- -----------------------------------------------------------------------------
-- Metadata Handlers
-- -----------------------------------------------------------------------------

{- | OAuth authorization server metadata endpoint (polymorphic).

Returns discovery metadata per RFC 8414 and MCP OAuth specification.

This handler is polymorphic over the monad @m@, requiring only:

* 'MonadReader env m': Access to environment containing config
* 'HasType HTTPServerConfig env': Config can be extracted via generic-lens

The handler extracts the HTTPServerConfig from the environment using
@typed \@HTTPServerConfig@ and builds the metadata response.

== Usage

@
-- In AppM (with AppEnv)
metadata <- handleMetadata  -- Extracts config from AppEnv

-- In custom monad
metadata <- handleMetadata  -- Extracts config from custom env
@

== Migration Note

This is ported from HTTP.hs as part of the typeclass-based architecture
migration. The shim pattern is used: HTTP.hs maintains the old signature
by calling this handler via runAppM.
-}
handleMetadata ::
    (MonadReader env m, HasType HTTPServerConfig env) =>
    m OAuthMetadata
handleMetadata = do
    config <- asks (getTyped @HTTPServerConfig)
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

{- | Protected resource metadata endpoint (polymorphic).

Returns protected resource metadata per RFC 9728.

This handler is polymorphic over the monad @m@, requiring only:

* 'MonadReader env m': Access to environment containing config
* 'HasType HTTPServerConfig env': Config can be extracted via generic-lens

The handler extracts the HTTPServerConfig from the environment and returns
either the custom metadata (if provided) or auto-generated metadata based
on the base URL.

== Usage

@
-- In AppM (with AppEnv)
metadata <- handleProtectedResourceMetadata

-- In custom monad
metadata <- handleProtectedResourceMetadata
@

== Migration Note

This is ported from HTTP.hs as part of the typeclass-based architecture
migration. The shim pattern is used: HTTP.hs maintains the old signature
by calling this handler via runAppM.
-}
handleProtectedResourceMetadata ::
    (MonadReader env m, HasType HTTPServerConfig env) =>
    m ProtectedResourceMetadata
handleProtectedResourceMetadata = do
    config <- asks (getTyped @HTTPServerConfig)
    let metadata = case httpProtectedResourceMetadata config of
            Just m -> m
            Nothing -> defaultProtectedResourceMetadata (httpBaseUrl config)
    return metadata

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

-- -----------------------------------------------------------------------------
-- Registration Handler
-- -----------------------------------------------------------------------------

{- | Dynamic client registration endpoint (polymorphic).

Handles client registration per RFC 7591 and MCP OAuth specification.

This handler is polymorphic over the monad @m@, requiring:

* 'OAuthStateStore m': Storage for registered clients
* 'MonadIO m': Ability to generate UUIDs
* 'MonadReader env m': Access to environment containing config and tracer
* 'HasType HTTPServerConfig env': Config can be extracted via generic-lens
* 'HasType (IOTracer HTTPTrace) env': Tracer can be extracted via generic-lens

The handler:

1. Generates a client ID with configurable prefix
2. Stores the client information via OAuthStateStore
3. Emits structured trace for registration
4. Returns client credentials (empty secret for public clients)

== Usage

@
-- In AppM (with AppEnv)
response <- handleRegister request

-- In custom monad
response <- handleRegister request
@

== Migration Note

This is ported from HTTP.hs as part of the typeclass-based architecture
migration. The shim pattern is used: HTTP.hs maintains the old signature
by calling this handler via runAppM.
-}
handleRegister ::
    ( OAuthStateStore m
    , MonadIO m
    , MonadReader env m
    , MonadError e m
    , AsType AuthorizationError e
    , HasType HTTPServerConfig env
    , HasType (IOTracer HTTPTrace) env
    ) =>
    ClientRegistrationRequest ->
    m ClientRegistrationResponse
handleRegister (ClientRegistrationRequest reqName reqRedirects reqGrants reqResponses reqAuth) = do
    config <- asks (getTyped @HTTPServerConfig)
    tracer <- asks (getTyped @(IOTracer HTTPTrace))

    -- Validate redirect_uris is not empty
    when (null reqRedirects) $
        throwError $
            injectTyped @AuthorizationError $
                InvalidRequest "redirect_uris must not be empty"

    -- Generate client ID
    let prefix = maybe "client_" clientIdPrefix (httpOAuthConfig config)
    uuid <- liftIO UUID.nextRandom
    let clientIdText = prefix <> UUID.toText uuid
        clientId = ClientId clientIdText

    -- Convert lists to NonEmpty and Set for ClientInfo
    -- Note: ClientInfo from OAuth.Types requires NonEmpty and Set
    redirectsNE <- case NE.nonEmpty reqRedirects of
        Just ne -> pure ne
        Nothing -> throwError $ injectTyped @AuthorizationError $ InvalidRequest "redirect_uris must not be empty"

    let grantsSet = Set.fromList reqGrants
        responsesSet = Set.fromList reqResponses
        clientInfo =
            ClientInfo
                { clientName = reqName
                , clientRedirectUris = redirectsNE
                , clientGrantTypes = grantsSet
                , clientResponseTypes = responsesSet
                , clientAuthMethod = reqAuth
                }

    storeClient clientId clientInfo

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

-- -----------------------------------------------------------------------------
-- Authorization Handler
-- -----------------------------------------------------------------------------

{- | Authorization endpoint handler (polymorphic).

Handles OAuth authorization requests and returns an interactive login page.

This handler is polymorphic over the monad @m@, requiring:

* 'OAuthStateStore m': Storage for pending authorizations
* 'MonadIO m': Ability to generate UUIDs and get current time
* 'MonadReader env m': Access to environment containing config and tracer
* 'HasType HTTPServerConfig env': Config can be extracted via generic-lens
* 'HasType (IOTracer HTTPTrace) env': Tracer can be extracted via generic-lens

The handler:

1. Validates response_type (only "code" supported)
2. Validates code_challenge_method (only "S256" supported)
3. Looks up the client to verify it's registered
4. Validates the redirect_uri is registered for this client
5. Generates a session ID and stores pending authorization
6. Returns login page HTML with session cookie

== Usage

@
-- In AppM (with AppEnv)
loginPage <- handleAuthorize responseType clientId redirectUri codeChallenge method scope state resource

-- In custom monad
loginPage <- handleAuthorize responseType clientId redirectUri codeChallenge method scope state resource
@

== Migration Note

This is ported from HTTP.hs as part of the typeclass-based architecture
migration. The shim pattern is used: HTTP.hs maintains the old signature
by calling this handler via runAppM.
-}
handleAuthorize ::
    ( OAuthStateStore m
    , MonadIO m
    , MonadReader env m
    , MonadError e m
    , AsType ValidationError e
    , AsType AuthorizationError e
    , HasType HTTPServerConfig env
    , HasType (IOTracer HTTPTrace) env
    ) =>
    ResponseType ->
    ClientId ->
    RedirectUri ->
    CodeChallenge ->
    CodeChallengeMethod ->
    Maybe Text ->
    Maybe Text ->
    Maybe Text ->
    m (Headers '[Header "Set-Cookie" SessionCookie] Text)
handleAuthorize responseType clientId redirectUri codeChallenge codeChallengeMethod mScope mState mResource = do
    config <- asks (getTyped @HTTPServerConfig)
    tracer <- asks (getTyped @(IOTracer HTTPTrace))

    let oauthTracer = contramap HTTPOAuth tracer
        responseTypeText = toUrlPiece responseType
        clientIdText = unClientId clientId
        redirectUriText = toUrlPiece redirectUri
        codeChallengeMethodText = toUrlPiece codeChallengeMethod

    -- Log resource parameter for RFC8707 support
    liftIO $ traceWith tracer $ HTTPResourceParameterDebug mResource "authorize endpoint"

    -- Validate response_type (only "code" supported)
    when (responseType /= ResponseCode) $ do
        liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthValidationError "response_type" ("Unsupported response type: " <> responseTypeText)
        throwError $ injectTyped @ValidationError $ UnsupportedResponseType responseTypeText

    -- Validate code_challenge_method (only "S256" supported)
    when (codeChallengeMethod /= S256) $ do
        liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthValidationError "code_challenge_method" ("Unsupported method: " <> codeChallengeMethodText)
        throwError $ injectTyped @AuthorizationError $ InvalidRequest ("Unsupported code_challenge_method: " <> codeChallengeMethodText)

    -- Look up client to verify it's registered
    mClientInfo <- lookupClient clientId
    clientInfo <- case mClientInfo of
        Just ci -> return ci
        Nothing -> do
            liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthValidationError "client_id" ("Unregistered client: " <> clientIdText)
            throwError $ injectTyped @ValidationError $ ClientNotRegistered clientId

    -- Validate redirect_uri is registered for this client
    unless (redirectUri `elem` NE.toList (clientRedirectUris clientInfo)) $ do
        liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthValidationError "redirect_uri" ("Redirect URI not registered: " <> redirectUriText)
        throwError $ injectTyped @ValidationError $ RedirectUriMismatch clientId redirectUri

    let displayName = clientName clientInfo
        scopeList = maybe [] (T.splitOn " ") mScope

    -- Emit authorization request trace
    liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthAuthorizationRequest clientIdText scopeList (isJust mState)

    -- Generate session ID
    sessionIdText <- liftIO $ UUID.toText <$> UUID.nextRandom
    let sessionId = case mkSessionId sessionIdText of
            Just sid -> sid
            Nothing -> error "Generated invalid session UUID"
    now <- currentTime

    -- Convert mScope from Maybe Text to Maybe (Set Scope)
    let scopesSet =
            mScope >>= \scopeText ->
                let scopeTexts = T.splitOn " " scopeText
                    scopesMaybe = traverse (mkScope . T.strip) scopeTexts
                 in fmap Set.fromList scopesMaybe
        -- Convert mResource from Maybe Text to Maybe URI
        resourceUri = mResource >>= (parseURI . T.unpack)

    -- Create pending authorization
    let pending =
            PendingAuthorization
                { pendingClientId = clientId
                , pendingRedirectUri = redirectUri
                , pendingCodeChallenge = codeChallenge
                , pendingCodeChallengeMethod = codeChallengeMethod
                , pendingScope = scopesSet
                , pendingState = mState
                , pendingResource = resourceUri
                , pendingCreatedAt = now
                }

    -- Store pending authorization
    storePendingAuth sessionId pending

    -- Emit login page served trace
    liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthLoginPageServed sessionIdText

    -- Build session cookie
    let sessionExpirySeconds = maybe 600 loginSessionExpirySeconds (httpOAuthConfig config)
        cookieValue = SessionCookie $ "mcp_session=" <> sessionIdText <> "; HttpOnly; SameSite=Strict; Path=/; Max-Age=" <> T.pack (show sessionExpirySeconds)
        scopes = fromMaybe "default access" mScope
        loginHtml = renderLoginPage displayName scopes mResource sessionIdText

    return $ addHeader cookieValue loginHtml

-- -----------------------------------------------------------------------------
-- Login Handler
-- -----------------------------------------------------------------------------

{- | Login form submission handler (polymorphic).

Handles user credential validation and authorization code generation.

This handler is polymorphic over the monad @m@, requiring:

* 'OAuthStateStore m': Storage for pending authorizations and authorization codes
* 'AuthBackend m': Credential validation backend
* 'MonadTime m': Access to current time for expiry checks
* 'MonadIO m': Ability to generate UUIDs and perform IO
* 'MonadReader env m': Access to environment containing config and tracer
* 'HasType HTTPServerConfig env': Config can be extracted via generic-lens
* 'HasType (IOTracer HTTPTrace) env': Tracer can be extracted via generic-lens

The handler:

1. Validates session cookie matches form session_id
2. Looks up pending authorization by session ID
3. Checks if session has expired
4. Handles "deny" action by redirecting with error
5. Validates credentials via AuthBackend
6. Generates authorization code and stores it
7. Redirects with authorization code or error

== Usage

@
-- In AppM (with AppEnv)
response <- handleLogin mCookie form

-- In custom monad
response <- handleLogin mCookie form
@

== Migration Note

This is ported from HTTP.hs as part of the typeclass-based architecture
migration. The shim pattern is used: HTTP.hs maintains the old signature
by calling this handler via runAppM.
-}
handleLogin ::
    forall m env e.
    ( OAuthStateStore m
    , AuthBackend m
    , AuthBackendUser m ~ OAuthUser m
    , AuthBackendUserId m ~ OAuthUserId m
    , MonadIO m
    , MonadReader env m
    , MonadError e m
    , AsType AuthorizationError e
    , HasType HTTPServerConfig env
    , HasType (IOTracer HTTPTrace) env
    ) =>
    Maybe Text ->
    LoginForm ->
    m (Headers '[Header "Location" RedirectTarget, Header "Set-Cookie" SessionCookie] NoContent)
handleLogin mCookie loginForm = do
    config <- asks (getTyped @HTTPServerConfig)
    tracer <- asks (getTyped @(IOTracer HTTPTrace))

    let oauthTracer = contramap HTTPOAuth tracer
        sessionId = formSessionId loginForm

    -- T039: Handle cookies disabled - check if cookie matches form session_id
    case mCookie of
        Nothing -> do
            liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthValidationError "cookies" "No cookie header present"
            throwError $ injectTyped @AuthorizationError $ InvalidRequest $ renderErrorPage "Cookies Required" "Your browser must have cookies enabled to sign in. Please enable cookies and try again."
        Just cookie ->
            -- Parse session cookie and verify it matches form session_id
            let cookieSessionId = extractSessionFromCookie cookie
             in unless (cookieSessionId == Just sessionId) $ do
                    liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthValidationError "cookies" "Session cookie mismatch"
                    throwError $ injectTyped @AuthorizationError $ InvalidRequest $ renderErrorPage "Cookies Required" "Session cookie mismatch. Please enable cookies and try again."

    -- Look up pending authorization
    mPending <- lookupPendingAuth sessionId
    pending <- case mPending of
        Just p -> return p
        Nothing -> do
            liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthValidationError "session" ("Session not found: " <> unSessionId sessionId)
            throwError $ injectTyped @AuthorizationError $ InvalidRequest $ renderErrorPage "Invalid Session" "Session not found or has expired. Please restart the authorization flow."

    -- T038: Handle expired sessions
    now <- currentTime
    let sessionExpirySeconds = fromIntegral $ maybe 600 loginSessionExpirySeconds (httpOAuthConfig config)
        expiryTime = addUTCTime sessionExpirySeconds (pendingCreatedAt pending)
    when (now > expiryTime) $ do
        liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthSessionExpired (unSessionId sessionId)
        throwError $ injectTyped @AuthorizationError $ InvalidRequest $ renderErrorPage "Session Expired" "Your login session has expired. Please restart the authorization flow."

    -- Check if user denied access
    if formAction loginForm == "deny"
        then do
            -- Emit denial trace
            liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthAuthorizationDenied (unClientId $ pendingClientId pending) "User denied authorization"

            -- Clear session and redirect with error
            let clearCookie = SessionCookie "mcp_session=; Max-Age=0; Path=/"
                errorParams = "error=access_denied&error_description=User%20denied%20access"
                stateParam = case pendingState pending of
                    Just s -> "&state=" <> s
                    Nothing -> ""
                redirectUrl = RedirectTarget $ toUrlPiece (pendingRedirectUri pending) <> "?" <> errorParams <> stateParam

            -- Remove pending authorization
            deletePendingAuth sessionId

            return $ addHeader redirectUrl $ addHeader clearCookie NoContent
        else do
            -- Validate credentials via AuthBackend
            let username = formUsername loginForm
                password = formPassword loginForm

            validationResult <- validateCredentials username password
            liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthLoginAttempt (unUsername username) (isJust validationResult)
            case validationResult of
                Just (userId, authUser) -> do
                    -- Cache the user for later lookup during token exchange
                    storeUserInCache userId authUser

                    -- Emit authorization granted trace
                    liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthAuthorizationGranted (unClientId $ pendingClientId pending) (unUsername username)

                    -- Generate authorization code
                    code <- liftIO $ generateAuthCode config
                    codeGenerationTime <- currentTime
                    let oauthCfg = httpOAuthConfig config
                        expirySeconds = fromIntegral $ maybe 600 authCodeExpirySeconds oauthCfg
                        expiry = addUTCTime expirySeconds codeGenerationTime
                        -- Convert pendingScope from Maybe (Set Scope) to Set Scope
                        scopes = fromMaybe Set.empty (pendingScope pending)
                        codeId = AuthCodeId code
                        authCode =
                            AuthorizationCode
                                { authCodeId = codeId
                                , authClientId = pendingClientId pending
                                , authRedirectUri = pendingRedirectUri pending
                                , authCodeChallenge = pendingCodeChallenge pending
                                , authCodeChallengeMethod = pendingCodeChallengeMethod pending
                                , authScopes = scopes
                                , authUserId = userId -- Store user ID, not full user
                                , authExpiry = expiry
                                }

                    -- Store authorization code and remove pending authorization
                    storeAuthCode authCode
                    deletePendingAuth sessionId

                    -- Build redirect URL with code
                    let stateParam = case pendingState pending of
                            Just s -> "&state=" <> s
                            Nothing -> ""
                        redirectUrl = RedirectTarget $ toUrlPiece (pendingRedirectUri pending) <> "?code=" <> code <> stateParam
                        clearCookie = SessionCookie "mcp_session=; Max-Age=0; Path=/"

                    return $ addHeader redirectUrl $ addHeader clearCookie NoContent
                Nothing ->
                    -- Invalid credentials - return 401 OAuth error (validateCredentials already emitted trace)
                    throwError $ injectTyped @AuthorizationError $ InvalidClient "Invalid credentials"

-- -----------------------------------------------------------------------------
-- Token Endpoint Handlers
-- -----------------------------------------------------------------------------

{- | Token endpoint handler (polymorphic).

Handles OAuth token requests, dispatching to appropriate grant type handler.

This handler is polymorphic over the monad @m@, requiring:

* 'OAuthStateStore m': Storage for OAuth state
* 'MonadTime m': Access to current time for expiry checks
* 'MonadIO m': Ability to generate JWTs and perform IO
* 'MonadReader env m': Access to environment containing config, tracer, and JWT settings
* 'MonadError e m': Error handling via MonadError
* 'HasType HTTPServerConfig env': Config can be extracted via generic-lens
* 'HasType (IOTracer HTTPTrace) env': Tracer can be extracted via generic-lens
* 'HasType JWTSettings env': JWT settings can be extracted via generic-lens
* 'AsType OAuthStoreError e': Storage errors can be injected into error type

The handler parses the grant_type parameter and dispatches to:

* 'handleAuthCodeGrant': For authorization_code grant
* 'handleRefreshTokenGrant': For refresh_token grant

== Usage

@
-- In AppM (with AppEnv)
response <- handleToken formParams

-- In custom monad
response <- handleToken formParams
@

== Migration Note

This is ported from HTTP.hs as part of the typeclass-based architecture
migration. The shim pattern is used: HTTP.hs maintains the old signature
by calling this handler via runAppM.
-}
handleToken ::
    ( OAuthStateStore m
    , ToJWT (OAuthUser m)
    , MonadIO m
    , MonadReader env m
    , MonadError e m
    , AsType AuthorizationError e
    , HasType HTTPServerConfig env
    , HasType (IOTracer HTTPTrace) env
    , HasType JWTSettings env
    ) =>
    [(Text, Text)] ->
    m TokenResponse
handleToken params = do
    let paramMap = Map.fromList params
    -- Parse grant_type from Text to GrantType newtype
    case Map.lookup "grant_type" paramMap of
        Nothing -> throwError $ injectTyped @AuthorizationError $ InvalidRequest "Missing grant_type"
        Just grantTypeText -> case parseUrlPiece grantTypeText of
            Left _err -> throwError $ injectTyped @AuthorizationError $ UnsupportedGrantType "Unsupported grant_type"
            Right grantType -> case grantType of
                GrantAuthorizationCode -> handleAuthCodeGrant paramMap
                GrantRefreshToken -> handleRefreshTokenGrant paramMap
                GrantClientCredentials -> throwError $ injectTyped @AuthorizationError $ UnsupportedGrantType "client_credentials not supported"

{- | Authorization code grant handler (polymorphic).

Handles token exchange for authorization code grant type.

This handler is polymorphic over the monad @m@, requiring the same constraints
as 'handleToken'.

The handler:

1. Extracts and validates the authorization code
2. Verifies the code hasn't expired
3. Validates PKCE code_verifier against stored challenge
4. Generates JWT access token and refresh token
5. Stores tokens and removes the used authorization code
6. Returns TokenResponse with access token, refresh token, and scopes

== Usage

@
-- In AppM (with AppEnv)
response <- handleAuthCodeGrant paramMap
@

== Migration Note

This is ported from HTTP.hs as part of the typeclass-based architecture
migration.
-}
handleAuthCodeGrant ::
    ( OAuthStateStore m
    , ToJWT (OAuthUser m)
    , MonadIO m
    , MonadReader env m
    , MonadError e m
    , AsType AuthorizationError e
    , HasType HTTPServerConfig env
    , HasType (IOTracer HTTPTrace) env
    , HasType JWTSettings env
    ) =>
    Map Text Text ->
    m TokenResponse
handleAuthCodeGrant params = do
    config <- asks (getTyped @HTTPServerConfig)
    tracer <- asks (getTyped @(IOTracer HTTPTrace))
    jwtSettings <- asks (getTyped @JWTSettings)

    let oauthTracer = contramap HTTPOAuth tracer

    -- Extract and log resource parameter (RFC8707)
    let mResource = Map.lookup "resource" params
    liftIO $ traceWith tracer $ HTTPResourceParameterDebug mResource "token request (auth code)"

    -- Parse code from Text to AuthCodeId
    code <- case Map.lookup "code" params of
        Nothing -> do
            liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthValidationError "token_request" "Missing authorization code"
            throwError $ injectTyped @AuthorizationError $ InvalidRequest "Missing authorization code"
        Just codeText -> case parseUrlPiece codeText of
            Left err -> do
                liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthValidationError "token_request" ("Invalid authorization code: " <> err)
                throwError $ injectTyped @AuthorizationError $ InvalidGrant "Invalid authorization code"
            Right authCodeId -> return authCodeId

    -- Parse code_verifier from Text to CodeVerifier
    codeVerifier <- case Map.lookup "code_verifier" params of
        Nothing -> do
            liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthValidationError "token_request" "Missing code_verifier"
            throwError $ injectTyped @AuthorizationError $ InvalidRequest "Missing code_verifier"
        Just verifierText -> case parseUrlPiece verifierText of
            Left err -> do
                liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthValidationError "token_request" ("Invalid code_verifier: " <> err)
                throwError $ injectTyped @AuthorizationError $ InvalidRequest "Invalid code_verifier"
            Right verifier -> return verifier

    -- Look up authorization code
    mAuthCode <- lookupAuthCode code
    authCode <- case mAuthCode of
        Just ac -> return ac
        Nothing -> do
            liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthTokenExchange "authorization_code" False
            throwError $ injectTyped @AuthorizationError $ InvalidGrant "Invalid authorization code"

    -- Verify code hasn't expired
    now <- currentTime
    when (now > authExpiry authCode) $ do
        liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthValidationError "auth_code" "Authorization code expired"
        liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthTokenExchange "authorization_code" False
        throwError $ injectTyped @AuthorizationError ExpiredCode

    -- Verify PKCE
    let authChallenge = authCodeChallenge authCode
        pkceValid = validateCodeVerifier codeVerifier authChallenge
    liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthPKCEValidation (unCodeVerifier codeVerifier) (unCodeChallenge authChallenge) pkceValid
    unless pkceValid $ do
        liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthTokenExchange "authorization_code" False
        throwError $ injectTyped @AuthorizationError PKCEVerificationFailed

    -- Extract userId from auth code and lookup full user
    let userId = authUserId authCode
        clientId = authClientId authCode

    -- Lookup full user by ID (needed for JWT generation)
    mUser <- lookupUserById userId
    user <- case mUser of
        Nothing -> throwError $ injectTyped @AuthorizationError $ InvalidGrant "User not found for authorization code"
        Just u -> pure u

    -- Generate tokens
    accessTokenText <- generateJWTAccessToken user jwtSettings
    refreshTokenText <- liftIO $ generateRefreshTokenWithConfig config
    let refreshToken = RefreshTokenId refreshTokenText

    -- Store tokens
    storeAccessToken (AccessTokenId accessTokenText) user
    storeRefreshToken refreshToken (clientId, user)
    deleteAuthCode code

    -- Emit successful token exchange trace
    liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthTokenExchange "authorization_code" True

    return
        TokenResponse
            { access_token = accessTokenText
            , token_type = "Bearer"
            , expires_in = Just $ maybe 3600 accessTokenExpirySeconds (httpOAuthConfig config)
            , refresh_token = Just refreshTokenText
            , scope = if Set.null (authScopes authCode) then Nothing else Just (T.intercalate " " (map unScope $ Set.toList $ authScopes authCode))
            }

{- | Refresh token grant handler (polymorphic).

Handles token refresh for refresh_token grant type.

This handler is polymorphic over the monad @m@, requiring the same constraints
as 'handleToken'.

The handler:

1. Extracts and validates the refresh token
2. Looks up the associated user and client
3. Generates a new JWT access token
4. Updates the access token mapping
5. Returns TokenResponse with new access token (keeps same refresh token)

== Usage

@
-- In AppM (with AppEnv)
response <- handleRefreshTokenGrant paramMap
@

== Migration Note

This is ported from HTTP.hs as part of the typeclass-based architecture
migration.
-}
handleRefreshTokenGrant ::
    ( OAuthStateStore m
    , ToJWT (OAuthUser m)
    , MonadIO m
    , MonadReader env m
    , MonadError e m
    , AsType AuthorizationError e
    , HasType HTTPServerConfig env
    , HasType (IOTracer HTTPTrace) env
    , HasType JWTSettings env
    ) =>
    Map Text Text ->
    m TokenResponse
handleRefreshTokenGrant params = do
    config <- asks (getTyped @HTTPServerConfig)
    tracer <- asks (getTyped @(IOTracer HTTPTrace))
    jwtSettings <- asks (getTyped @JWTSettings)

    let oauthTracer = contramap HTTPOAuth tracer

    -- Extract and log resource parameter (RFC8707)
    let mResource = Map.lookup "resource" params
    liftIO $ traceWith tracer $ HTTPResourceParameterDebug mResource "token request (refresh token)"

    -- Parse refresh_token from Text to RefreshTokenId
    refreshTokenId <- case Map.lookup "refresh_token" params of
        Nothing -> do
            liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthValidationError "token_request" "Missing refresh_token"
            throwError $ injectTyped @AuthorizationError $ InvalidRequest "Missing refresh_token"
        Just tokenText -> case parseUrlPiece tokenText of
            Left err -> do
                liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthValidationError "token_request" ("Invalid refresh_token: " <> err)
                throwError $ injectTyped @AuthorizationError $ InvalidGrant "Invalid refresh_token"
            Right rtId -> return rtId

    -- Look up refresh token
    mTokenInfo <- lookupRefreshToken refreshTokenId
    (clientId, user) <- case mTokenInfo of
        Just info -> return info
        Nothing -> do
            liftIO $ traceWith oauthTracer $ OAuthTrace.OAuthTokenRefresh False
            throwError $ injectTyped @AuthorizationError $ InvalidGrant "Invalid refresh_token"

    -- Generate new JWT access token
    newAccessTokenText <- generateJWTAccessToken user jwtSettings

    -- Update tokens (keep same refresh token, update with new client/user association)
    storeAccessToken (AccessTokenId newAccessTokenText) user
    updateRefreshToken refreshTokenId (clientId, user)

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

-- -----------------------------------------------------------------------------
-- Helper Functions
-- -----------------------------------------------------------------------------

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

-- | Generate authorization code with config prefix
generateAuthCode :: HTTPServerConfig -> IO Text
generateAuthCode config = do
    uuid <- UUID.nextRandom
    let prefix = maybe "code_" authCodePrefix (httpOAuthConfig config)
    return $ prefix <> UUID.toText uuid

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

-- | Escape HTML special characters
escapeHtml :: Text -> Text
escapeHtml = T.replace "&" "&amp;" . T.replace "<" "&lt;" . T.replace ">" "&gt;" . T.replace "\"" "&quot;" . T.replace "'" "&#39;"

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

-- | Generate JWT access token for user
generateJWTAccessToken :: (OAuthStateStore m, ToJWT (OAuthUser m), MonadIO m, MonadError e m, AsType AuthorizationError e) => OAuthUser m -> JWTSettings -> m Text
generateJWTAccessToken user jwtSettings = do
    accessTokenResult <- liftIO $ makeJWT user jwtSettings Nothing
    case accessTokenResult of
        Left _err -> throwError $ injectTyped @AuthorizationError $ InvalidRequest "Token generation failed"
        Right accessToken -> case TE.decodeUtf8' $ LBS.toStrict accessToken of
            Left _decodeErr -> throwError $ injectTyped @AuthorizationError $ InvalidRequest "Token encoding failed"
            Right tokenText -> return tokenText

-- | Generate refresh token with configurable prefix
generateRefreshTokenWithConfig :: HTTPServerConfig -> IO Text
generateRefreshTokenWithConfig config = do
    uuid <- UUID.nextRandom
    let prefix = maybe "rt_" refreshTokenPrefix (httpOAuthConfig config)
    return $ prefix <> UUID.toText uuid
