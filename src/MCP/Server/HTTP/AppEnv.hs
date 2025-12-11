{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : MCP.Server.HTTP.AppEnv
Description : Composite environment and error types for OAuth HTTP server
Copyright   : (c) 2025
License     : MIT
Maintainer  : maintainer@example.com

This module provides composite environment and error types that unify
OAuth state storage, credential authentication, server configuration,
and tracing into a single environment for the HTTP server handlers.

= Design

The composite types enable use of @generic-lens@ and @HasType@/@AsType@
for accessing components without boilerplate. This allows handlers to
use constraints like:

@
handleSomeRoute
  :: (MonadReader env m, HasType OAuthTVarEnv env, MonadIO m)
  => m Response
@

Instead of passing the entire @AppEnv@ or writing custom has-field typeclasses.

= Usage

@
import MCP.Server.HTTP.AppEnv
import Control.Monad.Reader
import GHC.Generics (Generic)
import Data.Generics.Product (HasType)

-- Access a component via HasType
getOAuthEnv :: (MonadReader env m, HasType OAuthTVarEnv env) => m OAuthTVarEnv
getOAuthEnv = view (typed @OAuthTVarEnv)

-- Handle errors with toServerError
handleAppError :: AppError -> Handler a
handleAppError err = throwError (toServerError err)
@
-}
module MCP.Server.HTTP.AppEnv (
    -- * HTTP Server Configuration
    HTTPServerConfig (..),

    -- * Composite Environment
    AppEnv (..),

    -- * Composite Error
    AppError (..),

    -- * Error Translation
    toServerError,
) where

import Crypto.JOSE (JWK)
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (Port)
import Servant (ServerError, err400, err401, err500, errBody)

import MCP.Server.Auth (OAuthConfig, ProtectedResourceMetadata)
import MCP.Server.Auth.Demo (DemoAuthError (..), DemoCredentialEnv)
import MCP.Server.OAuth.InMemory (OAuthStoreError (..), OAuthTVarEnv)
import MCP.Trace.HTTP (HTTPTrace)
import MCP.Types (Implementation, ServerCapabilities)
import Plow.Logging (IOTracer)

-- -----------------------------------------------------------------------------
-- HTTP Server Configuration
-- -----------------------------------------------------------------------------

{- | Configuration for running an MCP HTTP server

This type was moved from MCP.Server.HTTP to avoid circular imports.
-}
data HTTPServerConfig = HTTPServerConfig
    { httpPort :: Port
    , httpBaseUrl :: Text -- Base URL for OAuth endpoints (e.g., "https://api.example.com")
    , httpServerInfo :: Implementation
    , httpCapabilities :: ServerCapabilities
    , httpEnableLogging :: Bool
    , httpOAuthConfig :: Maybe OAuthConfig
    , httpJWK :: Maybe JWK -- JWT signing key
    , httpProtocolVersion :: Text -- MCP protocol version
    , httpProtectedResourceMetadata :: Maybe ProtectedResourceMetadata
    {- ^ Custom protected resource metadata.
    When Nothing, auto-generated from httpBaseUrl.
    -}
    }
    deriving (Generic)

-- -----------------------------------------------------------------------------
-- Composite Environment
-- -----------------------------------------------------------------------------

{- | Composite environment for the HTTP OAuth server.

Combines all dependencies needed by HTTP handlers:

* 'envOAuth': OAuth state storage (authorization codes, tokens, clients)
* 'envAuth': Credential authentication backend
* 'envConfig': HTTP server configuration (port, base URL, OAuth settings)
* 'envTracer': Structured tracing for HTTP and OAuth events

Derives 'Generic' to enable @generic-lens@ access via 'HasType'.
-}
data AppEnv = AppEnv
    { envOAuth :: OAuthTVarEnv
    -- ^ OAuth state storage environment
    , envAuth :: DemoCredentialEnv
    -- ^ Credential authentication environment
    , envConfig :: HTTPServerConfig
    -- ^ HTTP server configuration
    , envTracer :: IOTracer HTTPTrace
    -- ^ Tracer for HTTP and OAuth events
    }
    deriving (Generic)

-- -----------------------------------------------------------------------------
-- Composite Error
-- -----------------------------------------------------------------------------

{- | Composite error type for the HTTP OAuth server.

Unifies all possible error types from different components:

* 'OAuthStoreErr': Storage backend errors (unavailable, internal errors)
* 'AuthBackendErr': Authentication failures (invalid credentials, user not found)
* 'ValidationErr': Input validation errors (malformed parameters, missing fields)
* 'ServerErr': Servant-level errors (already a ServerError, pass through)

Derives 'Generic' to enable @generic-lens@ projection via 'AsType'.
-}
data AppError
    = -- | Error from OAuth state storage
      OAuthStoreErr OAuthStoreError
    | -- | Error from credential authentication
      AuthBackendErr DemoAuthError
    | -- | Input validation error
      ValidationErr Text
    | -- | Servant server error (pass through)
      ServerErr ServerError
    deriving (Generic)

-- -----------------------------------------------------------------------------
-- Error Translation
-- -----------------------------------------------------------------------------

{- | Translate application errors to Servant ServerError.

Maps domain-specific errors to appropriate HTTP status codes:

* 'OAuthStoreErr': Storage errors map to 500 Internal Server Error
* 'AuthBackendErr': Authentication errors map to 401 Unauthorized
* 'ValidationErr': Validation errors map to 400 Bad Request
* 'ServerErr': Already a ServerError, pass through as-is

This function is used at the Servant boundary to convert domain errors
into HTTP responses.

== Examples

@
toServerError (OAuthStoreErr (StoreUnavailable "Database connection failed"))
-- err500 { errBody = "Storage error: Database connection failed" }

toServerError (AuthBackendErr InvalidCredentials)
-- err401 { errBody = "Authentication failed: Invalid credentials" }

toServerError (ValidationErr "Missing required parameter: client_id")
-- err400 { errBody = "Validation error: Missing required parameter: client_id" }
@
-}
toServerError :: AppError -> ServerError
toServerError (OAuthStoreErr storeErr) =
    case storeErr of
        StoreUnavailable msg ->
            err500{errBody = "Storage error: " <> toLBS msg}
        StoreInternalError msg ->
            err500{errBody = "Internal storage error: " <> toLBS msg}
toServerError (AuthBackendErr authErr) =
    case authErr of
        InvalidCredentials ->
            err401{errBody = "Authentication failed: Invalid credentials"}
        UserNotFound _ ->
            -- Don't leak user existence to unauthorized clients
            err401{errBody = "Authentication failed: Invalid credentials"}
toServerError (ValidationErr msg) =
    err400{errBody = "Validation error: " <> toLBS msg}
toServerError (ServerErr serverErr) =
    -- Already a ServerError, pass through
    serverErr

-- -----------------------------------------------------------------------------
-- Utilities
-- -----------------------------------------------------------------------------

-- | Convert Text to lazy ByteString for errBody
toLBS :: Text -> LBS.ByteString
toLBS = LBS.fromStrict . TE.encodeUtf8
