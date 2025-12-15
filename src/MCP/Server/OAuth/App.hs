{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : MCP.Server.OAuth.App
Description : Polymorphic OAuth application entry points
Copyright   : (c) 2025
License     : MIT
Maintainer  : maintainer@example.com

This module provides polymorphic entry points for creating OAuth WAI Applications.

= Design Philosophy

These functions are **library code**: pure, polymorphic, and composable. They:

1. Accept a natural transformation (@forall a. m a -> Handler a@) from callers
2. Require all typeclass constraints on @m@ in the function signature
3. Use `hoistServer` and `hoistServerWithContext` from Servant to apply the nat-trans
4. Return opaque WAI `Application` values

Callers provide:

* The monad @m@ (must satisfy all typeclass constraints)
* The natural transformation that interprets @m@ in `Handler`
* The Servant context (for `mcpAppWithContext`)

= Usage

@
-- Example 1: Using AppM from MCP.Server.HTTP.AppEnv
import MCP.Server.HTTP.AppEnv (AppM, runAppM)
import MCP.Server.OAuth.App (mcpApp)

myApp :: AppEnv -> Application
myApp env = mcpApp (runAppM env)

-- Example 2: Using mcpAppWithContext with custom context
import Servant.Auth.Server (CookieSettings, JWTSettings)

myAppWithContext :: AppEnv -> CookieSettings -> JWTSettings -> Application
myAppWithContext env cookieSettings jwtSettings =
  let ctx = cookieSettings :. jwtSettings :. EmptyContext
   in mcpAppWithContext ctx (runAppM env)
@

= Migration from HTTP.hs

This module replaces the hardcoded `mcpApp` function in HTTP.hs with
polymorphic versions that work with any monad satisfying the required
typeclass constraints.

Old approach (monomorphic):
@
mcpApp :: HTTPServerConfig -> IOTracer HTTPTrace -> TVar ServerState -> OAuthTVarEnv -> JWTSettings -> Application
mcpApp config tracer stateVar oauthEnv jwtSettings = ...
@

New approach (polymorphic):
@
mcpApp :: (OAuthStateStore m, AuthBackend m, ...) => (forall a. m a -> Handler a) -> Application
mcpApp runM = ...
@
-}
module MCP.Server.OAuth.App (
    -- * Polymorphic Entry Points
    mcpApp,
    mcpAppWithContext,
) where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import Data.Generics.Product (HasType)
import Network.Wai (Application)
import Servant (Context, Handler, HasServer, Proxy (..), serve, serveWithContext)
import Servant.Auth.Server (JWTSettings, ToJWT)
import Servant.Server (hoistServer, hoistServerWithContext)
import Servant.Server.Internal.Context (HasContextEntry, type (.++))
import Servant.Server.Internal.ErrorFormatter (DefaultErrorFormatters, ErrorFormatters)

import MCP.Server.Auth.Backend (AuthBackend (..))
import MCP.Server.HTTP.AppEnv (AppError, HTTPServerConfig)
import MCP.Server.OAuth.Server (OAuthAPI, oauthServer)
import MCP.Server.OAuth.Store (OAuthStateStore (..))
import MCP.Trace.HTTP (HTTPTrace)
import Plow.Logging (IOTracer)

{- | Polymorphic OAuth application entry point.

Creates a WAI Application for the OAuth API using a caller-provided
natural transformation to interpret the monad @m@ in Servant's `Handler`.

= Type Parameters

* @m@: The monad in which OAuth handlers run (e.g., AppM, ReaderT AppEnv Handler)

= Constraints on m

The monad @m@ must satisfy all of:

* 'OAuthStateStore m': Storage for OAuth codes, tokens, clients, sessions
* 'AuthBackend m': Credential validation backend
* 'MonadTime m': Access to current time (for expiry checks)
* 'MonadIO m': Ability to perform IO operations
* 'OAuthUser m ~ AuthBackendUser m': User types must match
* 'OAuthUserId m ~ AuthBackendUserId m': User ID types must match
* 'ToJWT (OAuthUser m)': User can be encoded into JWT tokens
* 'FromJWT (OAuthUser m)': User can be decoded from JWT tokens

= Parameters

* @runM@: Natural transformation from @m@ to `Handler` (rank-2 type)

= Return Value

An opaque WAI `Application` that serves the OAuth API.

= Usage Example

@
import MCP.Server.HTTP.AppEnv (AppM, AppEnv, runAppM)
import MCP.Server.OAuth.App (mcpApp)

createOAuthApp :: AppEnv -> Application
createOAuthApp env = mcpApp (runAppM env)
@

= Implementation Notes

Uses `hoistServer` from Servant to apply the natural transformation
to the polymorphic `oauthServer` from MCP.Server.OAuth.Server.

This function is pure and polymorphic - it doesn't perform IO or depend
on concrete types. All configuration and state is provided via the
typeclass constraints on @m@.
-}
mcpApp ::
    forall m env.
    ( OAuthStateStore m
    , AuthBackend m
    , MonadIO m
    , MonadReader env m
    , MonadError AppError m
    , HasType HTTPServerConfig env
    , HasType (IOTracer HTTPTrace) env
    , HasType JWTSettings env
    , OAuthUser m ~ AuthBackendUser m
    , OAuthUserId m ~ AuthBackendUserId m
    , ToJWT (OAuthUser m)
    ) =>
    (forall a. m a -> Handler a) ->
    Application
mcpApp runM = serve (Proxy :: Proxy OAuthAPI) (hoistServer (Proxy :: Proxy OAuthAPI) runM oauthServer)

{- | Polymorphic OAuth application entry point with Servant Context.

Similar to `mcpApp`, but accepts a Servant `Context` for additional
configuration (e.g., JWT settings, cookie settings, custom auth handlers).

= Type Parameters

* @m@: The monad in which OAuth handlers run
* @ctx@: The Servant context type list (e.g., '[CookieSettings, JWTSettings])

= Additional Constraints

* 'HasServer OAuthAPI ctx': The API must be servable with the given context

= Parameters

* @ctx@: Servant context containing JWT settings, cookie settings, etc.
* @runM@: Natural transformation from @m@ to `Handler`

= Return Value

An opaque WAI `Application` that serves the OAuth API with the given context.

= Usage Example

@
import MCP.Server.HTTP.AppEnv (AppM, AppEnv, runAppM)
import MCP.Server.OAuth.App (mcpAppWithContext)
import Servant (Context (..), EmptyContext)
import Servant.Auth.Server (CookieSettings, JWTSettings, defaultCookieSettings)

createOAuthAppWithAuth :: AppEnv -> JWTSettings -> Application
createOAuthAppWithAuth env jwtSettings =
  let cookieSettings = defaultCookieSettings
      ctx = cookieSettings :. jwtSettings :. EmptyContext
   in mcpAppWithContext ctx (runAppM env)
@

= When to Use This Instead of mcpApp

Use this function when you need to:

* Configure JWT authentication settings
* Configure cookie settings for sessions
* Add custom authentication handlers
* Provide other context-dependent configuration

For simple applications without custom context, use `mcpApp` instead.

= Implementation Notes

Uses `hoistServerWithContext` from Servant to apply the natural transformation
while preserving the context for auth handlers and other context-dependent
operations.

The context type must match the server's expectations (enforced by the
'HasServer OAuthAPI ctx' constraint).
-}
mcpAppWithContext ::
    forall m ctx env.
    ( OAuthStateStore m
    , AuthBackend m
    , MonadIO m
    , MonadReader env m
    , MonadError AppError m
    , HasType HTTPServerConfig env
    , HasType (IOTracer HTTPTrace) env
    , HasType JWTSettings env
    , OAuthUser m ~ AuthBackendUser m
    , OAuthUserId m ~ AuthBackendUserId m
    , ToJWT (OAuthUser m)
    , HasServer OAuthAPI ctx
    , HasContextEntry (ctx .++ DefaultErrorFormatters) ErrorFormatters
    ) =>
    Context ctx ->
    (forall a. m a -> Handler a) ->
    Application
mcpAppWithContext ctx runM =
    serveWithContext
        (Proxy :: Proxy OAuthAPI)
        ctx
        (hoistServerWithContext (Proxy :: Proxy OAuthAPI) (Proxy :: Proxy ctx) runM oauthServer)
