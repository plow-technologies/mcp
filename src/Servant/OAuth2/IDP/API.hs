{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

{- |
Module      : Servant.OAuth2.IDP.API
Description : OAuth 2.1 API type definitions
Copyright   : (C) 2025 Matthias Pall Gissurarson
License     : MIT
Maintainer  : mpg@mpg.is
Stability   : experimental
Portability : GHC

This module provides the OAuth 2.1 API type definitions for use in
Servant route composition. The API types are separated from the handler
implementations to enable modular API composition.

= API Structure

The OAuth API is composed of several sub-APIs:

* 'ProtectedResourceAPI': RFC 9728 protected resource metadata
* 'OAuthAPI': Complete OAuth 2.1 server API (metadata, registration, authorization, token)

= Content Types

This module also provides the 'HTML' content type for serving HTML pages
in OAuth flows (login pages, error pages).
-}
module Servant.OAuth2.IDP.API (
    -- * OAuth API Types
    OAuthAPI,
    ProtectedResourceAPI,
    LoginAPI,

    -- * Content Types
    HTML,

    -- * Request/Response Types
    LoginForm (..),
    ClientRegistrationRequest (..),
    ClientRegistrationResponse (..),
    TokenResponse (..),
) where

import Control.Monad (when)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
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
    StdMethod (POST),
    Verb,
    (:<|>),
    (:>),
 )
import Servant.API (Accept (..), MimeRender (..))
import Web.FormUrlEncoded (FromForm (..), parseUnique)

import MCP.Server.Auth (OAuthMetadata, ProtectedResourceMetadata)
import Servant.OAuth2.IDP.Auth.Backend (PlaintextPassword, Username, mkPlaintextPassword, mkUsername)
import Servant.OAuth2.IDP.Types (
    ClientAuthMethod,
    ClientId,
    CodeChallenge,
    CodeChallengeMethod,
    GrantType,
    RedirectTarget,
    RedirectUri,
    ResponseType,
    SessionCookie,
    SessionId,
    mkSessionId,
 )

-- FIXME: Use HTML from servant-lucid and provide a lucid template for the login page
-- -----------------------------------------------------------------------------
-- HTML Content Type
-- -----------------------------------------------------------------------------

{- | HTML content type for Servant.

This content type is used to serve HTML pages in OAuth flows, such as
login pages and error pages. The content type is @text/html; charset=utf-8@.

= Usage

@
type MyAPI = "login" :> Get '[HTML] Text

handleLogin :: Handler Text
handleLogin = return "<html><body>Login Page</body></html>"
@
-}
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

This endpoint is served at @/.well-known/oauth-protected-resource@
per RFC 9728 specification.

= Response Example

@
{
  "resource": "https://api.example.com",
  "authorization_servers": ["https://api.example.com"],
  "scopes_supported": ["mcp:read", "mcp:write"],
  "bearer_methods_supported": ["header"]
}
@
-}
type ProtectedResourceAPI =
    ".well-known" :> "oauth-protected-resource" :> Get '[JSON] ProtectedResourceMetadata

{- | Login API endpoint.

Handles the interactive login flow for OAuth authorization:

1. User submits credentials via form
2. Server validates credentials
3. On success: redirects with authorization code
4. On failure: returns error page

The endpoint returns an HTTP 302 redirect with:

* @Location@ header: redirect URI with code or error
* @Set-Cookie@ header: session cookie (cleared after use)

This endpoint is typically reached after the user is shown a login page
by the authorization endpoint.
-}
type LoginAPI =
    "login"
        :> Header "Cookie" Text
        :> ReqBody '[FormUrlEncoded] LoginForm
        :> Verb 'POST 302 '[HTML] (Headers '[Header "Location" RedirectTarget, Header "Set-Cookie" SessionCookie] NoContent)

{- | Complete OAuth 2.1 API.

Provides all OAuth endpoints required by the MCP OAuth specification:

* @/.well-known/oauth-protected-resource@: Protected resource metadata (RFC 9728)
* @/.well-known/oauth-authorization-server@: Authorization server metadata (RFC 8414)
* @/register@: Dynamic client registration (RFC 7591)
* @/authorize@: Authorization endpoint (RFC 6749)
* @/login@: Login form submission endpoint (custom)
* @/token@: Token exchange endpoint (RFC 6749)

All endpoints follow their respective RFCs and the MCP OAuth specification.

= API Composition

The API can be composed with other Servant APIs:

@
type FullAPI = OAuthAPI :<|> McpAPI

server :: ServerT FullAPI m
server = oauthServer :<|> mcpServer
@

= Metadata Discovery

Clients should start by querying the metadata endpoints:

1. @GET /.well-known/oauth-authorization-server@ - Server capabilities
2. @GET /.well-known/oauth-protected-resource@ - Resource metadata

This provides automatic configuration of OAuth clients per RFC 8414.
-}
type OAuthAPI =
    ProtectedResourceAPI
        :<|> ".well-known" :> "oauth-authorization-server" :> Get '[JSON] OAuthMetadata
        :<|> "register"
            :> ReqBody '[JSON] ClientRegistrationRequest
            :> Verb 'POST 201 '[JSON] ClientRegistrationResponse
        :<|> "authorize"
            :> QueryParam' '[Required] "response_type" ResponseType
            :> QueryParam' '[Required] "client_id" ClientId
            :> QueryParam' '[Required] "redirect_uri" RedirectUri
            :> QueryParam' '[Required] "code_challenge" CodeChallenge
            :> QueryParam' '[Required] "code_challenge_method" CodeChallengeMethod
            :> QueryParam "scope" Text
            :> QueryParam "state" Text
            :> QueryParam "resource" Text
            -- FIXME: Must return a richer type that implements ToHtml from lucid
            :> Get '[HTML] (Headers '[Header "Set-Cookie" SessionCookie] Text)
        :<|> LoginAPI
        :<|> "token"
            -- FIXME: MUST use a sum-type that captures the expected form payload PRECISELY
            :> ReqBody '[FormUrlEncoded] [(Text, Text)]
            :> Post '[JSON] TokenResponse

-- -----------------------------------------------------------------------------
-- Request/Response Types
-- -----------------------------------------------------------------------------

{- | Login form data.

Contains user credentials submitted via the login page, plus the session ID
to correlate with the pending authorization request.
-}
data LoginForm = LoginForm
    { formUsername :: Username
    , formPassword :: PlaintextPassword
    , formSessionId :: SessionId
    , formAction :: Text -- "login" or "deny"
    }
    deriving (Generic)

-- | Custom Show instance that redacts password
instance Show LoginForm where
    show (LoginForm u _p s a) =
        "LoginForm {formUsername = "
            <> show u
            <> ", formPassword = <redacted>, formSessionId = "
            <> show s
            <> ", formAction = "
            <> show a
            <> "}"

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

{- | Client registration request.

Submitted by OAuth clients to register dynamically per RFC 7591.
-}
data ClientRegistrationRequest = ClientRegistrationRequest
    { client_name :: Text
    , redirect_uris :: [RedirectUri]
    , grant_types :: [GrantType]
    , response_types :: [ResponseType]
    , token_endpoint_auth_method :: ClientAuthMethod
    }
    deriving (Show, Generic)

instance Aeson.FromJSON ClientRegistrationRequest where
    parseJSON = Aeson.withObject "ClientRegistrationRequest" $ \v -> do
        name <- v Aeson..: "client_name"
        uris <- v Aeson..: "redirect_uris"
        when (null uris) $
            fail "redirect_uris must contain at least one URI"
        grants <- v Aeson..: "grant_types"
        when (null grants) $
            fail "grant_types must not be empty"
        responses <- v Aeson..: "response_types"
        when (null responses) $
            fail "response_types must not be empty"
        authMethod <- v Aeson..: "token_endpoint_auth_method"
        pure $ ClientRegistrationRequest name uris grants responses authMethod

{- | Client registration response.

Returned after successful client registration. Contains client credentials
and registered metadata.
-}
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

{- | Token endpoint response.

Returned from the token endpoint after successful token exchange.
Contains access token, optional refresh token, and metadata.
-}
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
