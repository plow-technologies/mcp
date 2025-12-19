{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Servant.OAuth2.IDP.Metadata
Description : OAuth metadata types per RFC 8414 and RFC 9728
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com
Stability   : experimental
Portability : GHC

This module provides OAuth metadata types for discovery endpoints:
- RFC 8414: OAuth Authorization Server Metadata
- RFC 9728: OAuth Protected Resource Metadata

Both types use custom JSON instances with snake_case field names per the RFCs.
-}
module Servant.OAuth2.IDP.Metadata (
    -- * OAuth Authorization Server Metadata (RFC 8414)
    OAuthMetadata (..),
    mkOAuthMetadata,

    -- * OAuth Protected Resource Metadata (RFC 9728)
    ProtectedResourceMetadata (..),
    mkProtectedResourceMetadata,
) where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.:?), (.=))
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant.OAuth2.IDP.Types (
    ClientAuthMethod,
    CodeChallengeMethod,
    GrantType,
    ResponseType,
    Scope,
 )

-- -----------------------------------------------------------------------------
-- OAuth Authorization Server Metadata (RFC 8414)
-- -----------------------------------------------------------------------------

{- | OAuth Authorization Server Metadata per RFC 8414.

This type represents the discovery metadata returned by the
@.well-known/oauth-authorization-server@ endpoint.

Required fields:
- issuer: Authorization server issuer identifier (MUST be absolute URI with https)
- authorization_endpoint: URL of the authorization endpoint
- token_endpoint: URL of the token endpoint
- response_types_supported: List of supported OAuth 2.0 response_type values

Optional fields provide additional server capabilities and endpoints.
-}
data OAuthMetadata = OAuthMetadata
    { issuer :: Text
    -- ^ Authorization server issuer identifier (MUST be absolute URI with https)
    , authorizationEndpoint :: Text
    -- ^ URL of the authorization endpoint
    , tokenEndpoint :: Text
    -- ^ URL of the token endpoint
    , registrationEndpoint :: Maybe Text
    -- ^ URL of the client registration endpoint (RFC 7591)
    , userInfoEndpoint :: Maybe Text
    -- ^ URL of the UserInfo endpoint (OpenID Connect)
    , jwksUri :: Maybe Text
    -- ^ URL of the JSON Web Key Set document
    , scopesSupported :: Maybe [Scope]
    -- ^ List of OAuth 2.0 scope values supported
    , responseTypesSupported :: [ResponseType]
    -- ^ List of OAuth 2.0 response_type values supported
    , grantTypesSupported :: Maybe [GrantType]
    -- ^ List of OAuth 2.0 grant_type values supported
    , tokenEndpointAuthMethodsSupported :: Maybe [ClientAuthMethod]
    -- ^ List of client authentication methods supported at token endpoint
    , codeChallengeMethodsSupported :: Maybe [CodeChallengeMethod]
    -- ^ List of PKCE code challenge methods supported
    }
    deriving (Eq, Show, Generic)

-- | Custom ToJSON instance with snake_case field names per RFC 8414
instance ToJSON OAuthMetadata where
    toJSON OAuthMetadata{..} =
        object $
            [ "issuer" .= issuer
            , "authorization_endpoint" .= authorizationEndpoint
            , "token_endpoint" .= tokenEndpoint
            , "response_types_supported" .= responseTypesSupported
            ]
                ++ optionalFields
      where
        optionalFields =
            concat
                [ maybe [] (\v -> ["registration_endpoint" .= v]) registrationEndpoint
                , maybe [] (\v -> ["userinfo_endpoint" .= v]) userInfoEndpoint
                , maybe [] (\v -> ["jwks_uri" .= v]) jwksUri
                , maybe [] (\v -> ["scopes_supported" .= v]) scopesSupported
                , maybe [] (\v -> ["grant_types_supported" .= v]) grantTypesSupported
                , maybe [] (\v -> ["token_endpoint_auth_methods_supported" .= v]) tokenEndpointAuthMethodsSupported
                , maybe [] (\v -> ["code_challenge_methods_supported" .= v]) codeChallengeMethodsSupported
                ]

-- | Custom FromJSON instance with snake_case field names per RFC 8414
instance FromJSON OAuthMetadata where
    parseJSON = withObject "OAuthMetadata" $ \v ->
        OAuthMetadata
            <$> v .: "issuer"
            <*> v .: "authorization_endpoint"
            <*> v .: "token_endpoint"
            <*> v .:? "registration_endpoint"
            <*> v .:? "userinfo_endpoint"
            <*> v .:? "jwks_uri"
            <*> v .:? "scopes_supported"
            <*> v .: "response_types_supported"
            <*> v .:? "grant_types_supported"
            <*> v .:? "token_endpoint_auth_methods_supported"
            <*> v .:? "code_challenge_methods_supported"

{- | Smart constructor for OAuthMetadata.

Currently performs no validation - fields are validated by their types.
Provided for API consistency and future validation needs.
-}
mkOAuthMetadata ::
    Text ->
    Text ->
    Text ->
    Maybe Text ->
    Maybe Text ->
    Maybe Text ->
    Maybe [Scope] ->
    [ResponseType] ->
    Maybe [GrantType] ->
    Maybe [ClientAuthMethod] ->
    Maybe [CodeChallengeMethod] ->
    Maybe OAuthMetadata
mkOAuthMetadata
    iss
    authzEndpoint
    tokEndpoint
    regEndpoint
    userInfoEp
    jwksU
    scopesSupp
    responseTypesSupp
    grantTypesSupp
    tokenAuthMethodsSupp
    challengeMethodsSupp =
        Just $
            OAuthMetadata
                { issuer = iss
                , authorizationEndpoint = authzEndpoint
                , tokenEndpoint = tokEndpoint
                , registrationEndpoint = regEndpoint
                , userInfoEndpoint = userInfoEp
                , jwksUri = jwksU
                , scopesSupported = scopesSupp
                , responseTypesSupported = responseTypesSupp
                , grantTypesSupported = grantTypesSupp
                , tokenEndpointAuthMethodsSupported = tokenAuthMethodsSupp
                , codeChallengeMethodsSupported = challengeMethodsSupp
                }

-- -----------------------------------------------------------------------------
-- OAuth Protected Resource Metadata (RFC 9728)
-- -----------------------------------------------------------------------------

{- | OAuth Protected Resource Metadata per RFC 9728.

This type represents the discovery metadata returned by the
@.well-known/oauth-protected-resource@ endpoint.

Required fields:
- resource: Protected resource identifier (MUST be absolute URI with https)
- authorization_servers: List of authorization server issuer identifiers

Optional fields provide additional resource server information.
-}
data ProtectedResourceMetadata = ProtectedResourceMetadata
    { prResource :: Text
    -- ^ Protected resource identifier (MUST be absolute URI with https)
    , prAuthorizationServers :: [Text]
    -- ^ List of authorization server issuer identifiers
    , prScopesSupported :: Maybe [Scope]
    -- ^ Scope values the resource server understands
    , prBearerMethodsSupported :: Maybe [Text]
    -- ^ Token presentation methods (default: ["header"])
    , prResourceName :: Maybe Text
    -- ^ Human-readable name for display
    , prResourceDocumentation :: Maybe Text
    -- ^ URL of developer documentation
    }
    deriving (Eq, Show, Generic)

-- | Custom ToJSON instance with snake_case field names per RFC 9728
instance ToJSON ProtectedResourceMetadata where
    toJSON ProtectedResourceMetadata{..} =
        object $
            [ "resource" .= prResource
            , "authorization_servers" .= prAuthorizationServers
            ]
                ++ optionalFields
      where
        optionalFields =
            concat
                [ maybe [] (\v -> ["scopes_supported" .= v]) prScopesSupported
                , maybe [] (\v -> ["bearer_methods_supported" .= v]) prBearerMethodsSupported
                , maybe [] (\v -> ["resource_name" .= v]) prResourceName
                , maybe [] (\v -> ["resource_documentation" .= v]) prResourceDocumentation
                ]

-- | Custom FromJSON instance with snake_case field names per RFC 9728
instance FromJSON ProtectedResourceMetadata where
    parseJSON = withObject "ProtectedResourceMetadata" $ \v ->
        ProtectedResourceMetadata
            <$> v .: "resource"
            <*> v .: "authorization_servers"
            <*> v .:? "scopes_supported"
            <*> v .:? "bearer_methods_supported"
            <*> v .:? "resource_name"
            <*> v .:? "resource_documentation"

{- | Smart constructor for ProtectedResourceMetadata.

Currently performs no validation - fields are validated by their types.
Provided for API consistency and future validation needs.
-}
mkProtectedResourceMetadata ::
    Text ->
    [Text] ->
    Maybe [Scope] ->
    Maybe [Text] ->
    Maybe Text ->
    Maybe Text ->
    Maybe ProtectedResourceMetadata
mkProtectedResourceMetadata
    res
    authzServers
    scopesSupp
    bearerMethodsSupp
    resName
    resDoc =
        Just $
            ProtectedResourceMetadata
                { prResource = res
                , prAuthorizationServers = authzServers
                , prScopesSupported = scopesSupp
                , prBearerMethodsSupported = bearerMethodsSupp
                , prResourceName = resName
                , prResourceDocumentation = resDoc
                }
