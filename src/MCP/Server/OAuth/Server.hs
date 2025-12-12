{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

{- |
Module      : MCP.Server.OAuth.Server
Description : Polymorphic OAuth 2.1 server using typeclass architecture
Copyright   : (C) 2025 Matthias Pall Gissurarson
License     : MIT
Maintainer  : mpg@mpg.is
Stability   : experimental
Portability : GHC

This module provides a polymorphic OAuth 2.1 server implementation using
the OAuthStateStore and AuthBackend typeclasses for pluggable backends.

= Architecture

The server is polymorphic over the monad @m@, requiring:

* 'OAuthStateStore m': Storage for OAuth state (codes, tokens, clients)
* 'AuthBackend m': Credential validation backend
* 'MonadIO m': Ability to perform IO operations

This allows the server to work with different storage and auth backends:

* In-memory (demo/testing)
* PostgreSQL (production)
* Redis (caching)
* LDAP/Active Directory (authentication)

= Usage

@
-- Create server with specific monad
server :: Server OAuthAPI
server = hoistServer (Proxy :: Proxy OAuthAPI) (runAppM appEnv) oauthServer

-- Or use directly with AppM from MCP.Server.HTTP.AppEnv
app :: Application
app = serve (Proxy :: Proxy OAuthAPI) oauthServer
@

= Migration Status

This module is the target architecture for the OAuth implementation.
Handlers are being progressively migrated from HTTP.hs to this module
to use the typeclass-based design.

Current status: Skeleton created, handlers not yet ported.
-}
module MCP.Server.OAuth.Server (
    -- * API Definition
    OAuthAPI,
    ProtectedResourceAPI,
    LoginAPI,

    -- * Request/Response Types
    ClientRegistrationRequest (..),
    ClientRegistrationResponse (..),
    LoginForm (..),
    TokenResponse (..),

    -- * Constraint Alias
    OAuthConstraints,

    -- * Server Implementation
    oauthServer,

    -- * Polymorphic Handlers
    handleMetadata,
    handleProtectedResourceMetadata,
) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, asks)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Generics.Product (HasType)
import Data.Generics.Product.Typed (getTyped)
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import GHC.Generics (Generic)
import Network.HTTP.Media ((//), (/:))
import Servant (
    FormUrlEncoded,
    Get,
    Header,
    Headers,
    JSON,
    NoContent,
    Post,
    QueryParam,
    QueryParam',
    ReqBody,
    Required,
    Server,
    StdMethod (POST),
    Verb,
    (:<|>),
    (:>),
 )
import Servant.API (Accept (..), MimeRender (..))
import Web.FormUrlEncoded (FromForm (..), parseUnique)

import MCP.Server.Auth (
    OAuthConfig (..),
    OAuthMetadata (..),
    ProtectedResourceMetadata (..),
 )
import MCP.Server.Auth.Backend (AuthBackend, PlaintextPassword, Username, mkPlaintextPassword, mkUsername)
import MCP.Server.HTTP.AppEnv (HTTPServerConfig (..))
import MCP.Server.OAuth.Store (OAuthStateStore)
import MCP.Server.OAuth.Types (
    ClientAuthMethod,
    ClientId,
    CodeChallenge,
    CodeChallengeMethod,
    GrantType,
    RedirectTarget,
    RedirectUri,
    ResponseType (..),
    Scope (..),
    SessionCookie,
    SessionId,
    mkSessionId,
 )

-- -----------------------------------------------------------------------------
-- HTML Content Type (imported from HTTP.hs for now)
-- -----------------------------------------------------------------------------

-- | HTML content type for Servant (will be moved to separate module later)
data HTML

instance Accept HTML where
    contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML Text where
    mimeRender _ = LBS.fromStrict . TE.encodeUtf8

-- -----------------------------------------------------------------------------
-- API Types
-- -----------------------------------------------------------------------------

{- | Protected Resource Metadata API (RFC 9728).

Provides metadata about OAuth-protected resources including:

* Resource identifier
* List of authorization servers
* Supported scopes
* Supported bearer token methods
-}
type ProtectedResourceAPI =
    ".well-known" :> "oauth-protected-resource" :> Get '[JSON] ProtectedResourceMetadata

{- | Login API endpoints.

Handles the interactive login flow:

1. User submits credentials via form
2. Server validates credentials
3. On success: redirects with authorization code
4. On failure: returns error page

Returns HTTP 302 redirect with:

* @Location@ header: redirect URI with code or error
* @Set-Cookie@ header: session cookie (cleared after use)
-}
type LoginAPI =
    "login"
        :> Header "Cookie" Text
        :> ReqBody '[FormUrlEncoded] LoginForm
        :> Verb 'POST 302 '[HTML] (Headers '[Header "Location" RedirectTarget, Header "Set-Cookie" SessionCookie] NoContent)

{- | Complete OAuth 2.1 API.

Provides all OAuth endpoints:

* @/.well-known/oauth-protected-resource@: Protected resource metadata
* @/.well-known/oauth-authorization-server@: Authorization server metadata
* @/register@: Dynamic client registration
* @/authorize@: Authorization endpoint (returns login page)
* @/login@: Login form submission endpoint
* @/token@: Token exchange endpoint

All endpoints follow RFC 6749, RFC 8414, and MCP OAuth specification.
-}
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
            :> Get '[HTML] (Headers '[Header "Set-Cookie" SessionCookie] Text)
        :<|> LoginAPI
        :<|> "token"
            :> ReqBody '[FormUrlEncoded] [(Text, Text)]
            :> Post '[JSON] TokenResponse

-- -----------------------------------------------------------------------------
-- Constraint Alias
-- -----------------------------------------------------------------------------

{- | Constraint alias for OAuth server operations.

Captures all the typeclass constraints needed for OAuth handlers:

* 'OAuthStateStore m': Storage for OAuth state
* 'AuthBackend m': Credential validation
* 'MonadIO m': Ability to perform IO operations

Handlers can use this alias to reduce boilerplate:

@
handleSomeRoute :: (OAuthConstraints m) => ... -> m Response
@
-}
type OAuthConstraints m =
    ( OAuthStateStore m
    , AuthBackend m
    , MonadIO m
    )

-- -----------------------------------------------------------------------------
-- Server Implementation
-- -----------------------------------------------------------------------------

{- | OAuth server implementation (polymorphic over monad).

This is the main entry point for the OAuth server. It provides handlers
for all OAuth endpoints, polymorphic over the monad @m@.

The handlers are not yet ported from HTTP.hs. This is a skeleton to
establish the module structure and build dependency.

== Migration Plan

Handlers will be progressively migrated from HTTP.hs:

1. handleProtectedResourceMetadata
2. handleMetadata (OAuth discovery)
3. handleRegister (client registration)
4. handleAuthorize (login page)
5. handleLogin (credential validation)
6. handleToken (token exchange)

Each handler will be refactored to use the typeclass-based architecture
instead of direct TVar manipulation.

== Usage After Migration

@
-- With AppM from MCP.Server.HTTP.AppEnv
handler :: AppEnv -> Server OAuthAPI
handler env = hoistServer (Proxy :: Proxy OAuthAPI) (runAppM env) oauthServer

-- Or with custom monad
customHandler :: MyMonad (Server OAuthAPI)
customHandler = do
  env <- ask
  pure $ hoistServer (Proxy :: Proxy OAuthAPI) (runMyMonad env) oauthServer
@
-}
oauthServer :: Server OAuthAPI
oauthServer = error "oauthServer: handlers not yet ported from HTTP.hs"

-- -----------------------------------------------------------------------------
-- Request/Response Types (moved from HTTP.hs)
-- -----------------------------------------------------------------------------

-- | Login form data
data LoginForm = LoginForm
    { formUsername :: Username
    , formPassword :: PlaintextPassword
    , formSessionId :: SessionId
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
        Aeson.object $
            [ "access_token" .= access_token
            , "token_type" .= token_type
            ]
                ++ maybe [] (\e -> ["expires_in" .= e]) expires_in
                ++ maybe [] (\r -> ["refresh_token" .= r]) refresh_token
                ++ maybe [] (\s -> ["scope" .= s]) scope

-- -----------------------------------------------------------------------------
-- Polymorphic Handlers
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
