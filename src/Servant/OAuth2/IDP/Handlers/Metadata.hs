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
) where

import Control.Monad.Reader (MonadReader, asks)
import Data.Generics.Product (HasType)
import Data.Generics.Product.Typed (getTyped)
import Data.List.NonEmpty qualified as NE

import MCP.Server.Auth (
    OAuthMetadata (..),
 )
import MCP.Server.HTTP.AppEnv (HTTPServerConfig (..))
import Servant.OAuth2.IDP.Config (OAuthEnv (..))
import Servant.OAuth2.IDP.Metadata (ProtectedResourceMetadata)
import Servant.OAuth2.IDP.Types (
    oauthGrantTypeToGrantType,
 )

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
* 'HasType OAuthEnv env': OAuthEnv can be extracted via generic-lens

The handler extracts the OAuthEnv from the environment and returns
the resource server metadata configured in it.

== Usage

@
-- In AppM (with AppEnv)
metadata <- handleProtectedResourceMetadata

-- In custom monad
metadata <- handleProtectedResourceMetadata
@
-}
handleProtectedResourceMetadata ::
    (MonadReader env m, HasType OAuthEnv env) =>
    m ProtectedResourceMetadata
handleProtectedResourceMetadata = do
    oauthEnv <- asks (getTyped @OAuthEnv)
    return (resourceServerMetadata oauthEnv)
