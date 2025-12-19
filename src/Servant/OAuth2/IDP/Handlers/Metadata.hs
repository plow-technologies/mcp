{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : Servant.OAuth2.IDP.Handlers.Metadata
Description : OAuth metadata discovery handlers
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com
Stability   : experimental
Portability : GHC

OAuth metadata discovery endpoint handlers (RFC 8414, RFC 9728).
-}
module Servant.OAuth2.IDP.Handlers.Metadata (
    handleMetadata,
    handleProtectedResourceMetadata,
    defaultProtectedResourceMetadata,
) where

import Control.Monad.Reader (MonadReader, asks)
import Data.Generics.Product (HasType)
import Data.Generics.Product.Typed (getTyped)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)

import MCP.Server.Auth (
    OAuthMetadata (..),
    ProtectedResourceMetadata (..),
 )
import MCP.Server.HTTP.AppEnv (HTTPServerConfig (..))
import Servant.OAuth2.IDP.Config (OAuthEnv (..))
import Servant.OAuth2.IDP.Types (
    oauthGrantTypeToGrantType,
 )
import Servant.OAuth2.IDP.Types.Internal (unsafeScope)

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
-}
handleMetadata ::
    (MonadReader env m, HasType HTTPServerConfig env, HasType OAuthEnv env) =>
    m OAuthMetadata
handleMetadata = do
    config <- asks (getTyped @HTTPServerConfig)
    oauthEnv <- asks (getTyped @OAuthEnv)
    let baseUrl = httpBaseUrl config
    return
        OAuthMetadata
            { issuer = baseUrl
            , authorizationEndpoint = baseUrl <> "/authorize"
            , tokenEndpoint = baseUrl <> "/token"
            , registrationEndpoint = Just (baseUrl <> "/register")
            , userInfoEndpoint = Nothing
            , jwksUri = Nothing
            , scopesSupported = Just (oauthSupportedScopes oauthEnv)
            , responseTypesSupported = NE.toList (oauthSupportedResponseTypes oauthEnv)
            , grantTypesSupported = Just (NE.toList (fmap oauthGrantTypeToGrantType (oauthSupportedGrantTypes oauthEnv)))
            , tokenEndpointAuthMethodsSupported = Just (NE.toList (oauthSupportedAuthMethods oauthEnv))
            , codeChallengeMethodsSupported = Just (NE.toList (oauthSupportedCodeChallengeMethods oauthEnv))
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
        , scopesSupported = Just [unsafeScope "mcp:read", unsafeScope "mcp:write"]
        , bearerMethodsSupported = Just ["header"]
        , resourceName = Nothing
        , resourceDocumentation = Nothing
        }
