{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : MCP.Server.Auth
Description : MCP-compliant OAuth 2.1 authentication with pluggable credential backends
Copyright   : (C) 2025 Matthias Pall Gissurarson
License     : MIT
Maintainer  : mpg@mpg.is
Stability   : experimental
Portability : GHC

This module provides MCP-compliant OAuth 2.1 authentication with PKCE support,
and re-exports the credential authentication backend types.

== Re-exports

* "Servant.OAuth2.IDP.Auth.Backend" - AuthBackend typeclass for pluggable credential validation
* "Servant.OAuth2.IDP.Auth.Demo" - Demo implementation with hardcoded credentials
-}
module MCP.Server.Auth (
    -- * AuthBackend typeclass (re-exported from Servant.OAuth2.IDP.Auth.Backend)
    module Servant.OAuth2.IDP.Auth.Backend,

    -- * OAuth Configuration
    OAuthConfig (..),
    OAuthProvider (..),
    OAuthGrantType (..),

    -- * Token Validation
    TokenInfo (..),
    validateBearerToken,
    extractBearerToken,

    -- * PKCE Support
    PKCEChallenge (..),
    generateCodeVerifier,
    generateCodeChallenge,
    validateCodeVerifier,

    -- * Metadata Discovery
    OAuthMetadata (..),

    -- * Protected Resource Metadata (RFC 9728)
    ProtectedResourceMetadata (..),
    ProtectedResourceAuth,
    ProtectedResourceAuthConfig (..),
) where

-- Re-exports
import Servant.OAuth2.IDP.Auth.Backend

import Control.Monad.IO.Class (MonadIO, liftIO)
import Crypto.Hash (hashWith)
import Crypto.Hash.Algorithms (SHA256 (..))
import Crypto.Random (getRandomBytes)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Aeson
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import Data.ByteString.Base64.URL qualified as B64URL
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import GHC.Generics (Generic)
import Plow.Logging (IOTracer)

import MCP.Trace.OAuth (OAuthTrace (..))
import Servant.OAuth2.IDP.Types (
    ClientAuthMethod (..),
    CodeChallenge (..),
    CodeChallengeMethod (..),
    CodeVerifier (..),
    GrantType (..),
    ResponseType (..),
    Scope (..),
 )

-- | OAuth grant types supported by MCP
data OAuthGrantType
    = AuthorizationCode -- For user-based scenarios
    | ClientCredentials -- For application-to-application
    deriving (Show, Eq, Generic)

-- | OAuth provider configuration (MCP-compliant)
data OAuthProvider = OAuthProvider
    { providerName :: Text
    , clientId :: Text
    , clientSecret :: Maybe Text -- Optional for public clients
    , authorizationEndpoint :: Text
    , tokenEndpoint :: Text
    , userInfoEndpoint :: Maybe Text
    , scopes :: [Text]
    , grantTypes :: [OAuthGrantType]
    , requiresPKCE :: Bool -- MCP requires PKCE for all clients
    , metadataEndpoint :: Maybe Text -- For OAuth metadata discovery
    }
    deriving (Show, Generic)

-- | OAuth configuration for the MCP server
data OAuthConfig = OAuthConfig
    { oauthEnabled :: Bool
    , oauthProviders :: [OAuthProvider]
    , requireHTTPS :: Bool -- MCP requires HTTPS for OAuth
    -- Configurable timing parameters
    , authCodeExpirySeconds :: Int
    , accessTokenExpirySeconds :: Int
    , -- Configurable OAuth parameters
      supportedScopes :: [Scope]
    , supportedResponseTypes :: [ResponseType]
    , supportedGrantTypes :: [GrantType]
    , supportedAuthMethods :: [ClientAuthMethod]
    , supportedCodeChallengeMethods :: [CodeChallengeMethod]
    , -- Demo mode settings
      autoApproveAuth :: Bool
    , demoUserIdTemplate :: Maybe Text -- Nothing means no demo mode
    , demoEmailDomain :: Text
    , demoUserName :: Text
    , publicClientSecret :: Maybe Text
    , -- Token prefixes
      authCodePrefix :: Text
    , refreshTokenPrefix :: Text
    , clientIdPrefix :: Text
    , -- Response templates
      authorizationSuccessTemplate :: Maybe Text
    , -- Credential management
      credentialStore :: CredentialStore
    , loginSessionExpirySeconds :: Int
    }
    deriving (Generic)

-- Note: No Show instance because CredentialStore contains ScrubbedBytes (no Show)

-- | PKCE challenge data
data PKCEChallenge = PKCEChallenge
    { codeVerifier :: Text
    , codeChallenge :: Text
    , challengeMethod :: Text -- Always "S256" for MCP
    }
    deriving (Show, Generic)

-- | OAuth metadata (from discovery endpoint)
data OAuthMetadata = OAuthMetadata
    { issuer :: Text
    , authorizationEndpoint :: Text
    , tokenEndpoint :: Text
    , registrationEndpoint :: Maybe Text
    , userInfoEndpoint :: Maybe Text
    , jwksUri :: Maybe Text
    , scopesSupported :: Maybe [Scope]
    , responseTypesSupported :: [ResponseType]
    , grantTypesSupported :: Maybe [GrantType]
    , tokenEndpointAuthMethodsSupported :: Maybe [ClientAuthMethod]
    , codeChallengeMethodsSupported :: Maybe [CodeChallengeMethod]
    }
    deriving (Show, Generic)

-- | Protected Resource Metadata (RFC 9728)
data ProtectedResourceMetadata = ProtectedResourceMetadata
    { resource :: Text
    {- ^ The protected resource's identifier (MCP server URL)
    Required. MUST be an absolute URI with https scheme.
    -}
    , authorizationServers :: [Text]
    {- ^ List of authorization server issuer identifiers
    Required for MCP. At least one entry.
    -}
    , scopesSupported :: Maybe [Scope]
    {- ^ Scope values the resource server understands
    Optional. e.g., ["mcp:read", "mcp:write"]
    -}
    , bearerMethodsSupported :: Maybe [Text]
    {- ^ Token presentation methods supported
    Optional. Default: ["header"] per RFC9728
    -}
    , resourceName :: Maybe Text
    {- ^ Human-readable name for end-user display
    Optional. e.g., "My MCP Server"
    -}
    , resourceDocumentation :: Maybe Text
    {- ^ URL of developer documentation
    Optional.
    -}
    }
    deriving (Show, Generic)

instance FromJSON OAuthMetadata where
    parseJSON = Aeson.withObject "OAuthMetadata" $ \v ->
        OAuthMetadata
            <$> v Aeson..: "issuer"
            <*> v Aeson..: "authorization_endpoint"
            <*> v Aeson..: "token_endpoint"
            <*> v Aeson..:? "registration_endpoint"
            <*> v Aeson..:? "userinfo_endpoint"
            <*> v Aeson..:? "jwks_uri"
            <*> v Aeson..:? "scopes_supported"
            <*> v Aeson..: "response_types_supported"
            <*> v Aeson..:? "grant_types_supported"
            <*> v Aeson..:? "token_endpoint_auth_methods_supported"
            <*> v Aeson..:? "code_challenge_methods_supported"

instance ToJSON OAuthMetadata where
    toJSON OAuthMetadata{..} =
        Aeson.object $
            [ "issuer" Aeson..= issuer
            , "authorization_endpoint" Aeson..= authorizationEndpoint
            , "token_endpoint" Aeson..= tokenEndpoint
            , "response_types_supported" Aeson..= responseTypesSupported
            ]
                ++ maybe [] (\x -> ["registration_endpoint" Aeson..= x]) registrationEndpoint
                ++ maybe [] (\x -> ["userinfo_endpoint" Aeson..= x]) userInfoEndpoint
                ++ maybe [] (\x -> ["jwks_uri" Aeson..= x]) jwksUri
                ++ maybe [] (\x -> ["scopes_supported" Aeson..= x]) scopesSupported
                ++ maybe [] (\x -> ["grant_types_supported" Aeson..= x]) grantTypesSupported
                ++ maybe [] (\x -> ["token_endpoint_auth_methods_supported" Aeson..= x]) tokenEndpointAuthMethodsSupported
                ++ maybe [] (\x -> ["code_challenge_methods_supported" Aeson..= x]) codeChallengeMethodsSupported

instance FromJSON ProtectedResourceMetadata where
    parseJSON = Aeson.withObject "ProtectedResourceMetadata" $ \v ->
        ProtectedResourceMetadata
            <$> v Aeson..: "resource"
            <*> v Aeson..: "authorization_servers"
            <*> v Aeson..:? "scopes_supported"
            <*> v Aeson..:? "bearer_methods_supported"
            <*> v Aeson..:? "resource_name"
            <*> v Aeson..:? "resource_documentation"

instance ToJSON ProtectedResourceMetadata where
    toJSON ProtectedResourceMetadata{..} =
        Aeson.object $
            [ "resource" Aeson..= resource
            , "authorization_servers" Aeson..= authorizationServers
            ]
                ++ maybe [] (\x -> ["scopes_supported" Aeson..= x]) scopesSupported
                ++ maybe [] (\x -> ["bearer_methods_supported" Aeson..= x]) bearerMethodsSupported
                ++ maybe [] (\x -> ["resource_name" Aeson..= x]) resourceName
                ++ maybe [] (\x -> ["resource_documentation" Aeson..= x]) resourceDocumentation

-- | Token introspection response
data TokenInfo = TokenInfo
    { active :: Bool
    , scope :: Maybe Text
    , clientId :: Maybe Text
    , username :: Maybe Text
    , tokenType :: Maybe Text
    , exp :: Maybe Integer -- Expiration time (Unix timestamp)
    , iat :: Maybe Integer -- Issued at time (Unix timestamp)
    , nbf :: Maybe Integer -- Not before time (Unix timestamp)
    , sub :: Maybe Text -- Subject
    , aud :: Maybe [Text] -- Audience
    , iss :: Maybe Text -- Issuer
    }
    deriving (Show, Generic)

instance FromJSON TokenInfo where
    parseJSON = Aeson.withObject "TokenInfo" $ \v ->
        TokenInfo
            <$> v Aeson..: "active"
            <*> v Aeson..:? "scope"
            <*> v Aeson..:? "client_id"
            <*> v Aeson..:? "username"
            <*> v Aeson..:? "token_type"
            <*> v Aeson..:? "exp"
            <*> v Aeson..:? "iat"
            <*> v Aeson..:? "nbf"
            <*> v Aeson..:? "sub"
            <*> v Aeson..:? "aud"
            <*> v Aeson..:? "iss"

-- | Extract Bearer token from Authorization header
extractBearerToken :: Text -> Maybe Text
extractBearerToken authHeader =
    case T.words authHeader of
        ["Bearer", token] -> Just token
        _ -> Nothing

{- | Validate a bearer token

Note: The @_config@ parameter is intentionally kept for future JWT validation improvements.
Currently unused, but will be needed for proper validation (see lines 293-297):
- Verify JWT signature using JWK from jwks_uri
- Check expiration using accessTokenExpirySeconds
- Validate issuer and audience
-}
validateBearerToken :: (MonadIO m) => IOTracer OAuthTrace -> OAuthConfig -> Text -> m (Either Text TokenInfo)
validateBearerToken _tracer _config token = do
    -- Basic validation
    if T.null token
        then return $ Left "Empty token"
        else do
            -- Perform basic JWT validation
            -- In production, this should:
            -- 1. Verify JWT signature using JWK from jwks_uri
            -- 2. Check expiration time
            -- 3. Validate issuer and audience
            -- 4. Check token type is "Bearer"

            -- For now, decode JWT payload (middle part) for basic validation
            case T.splitOn "." token of
                [_header, payload, _signature] -> do
                    currentTime <- liftIO getCurrentTime
                    case decodeJWTPayload payload of
                        Right tokenInfo ->
                            case validateTokenClaims tokenInfo currentTime of
                                Right _ -> return $ Right tokenInfo
                                Left err -> return $ Left err
                        Left err -> return $ Left $ "Invalid JWT format: " <> err
                _ -> return $ Left "Invalid JWT structure"

-- | Decode JWT payload (base64url encoded JSON)
decodeJWTPayload :: Text -> Either Text TokenInfo
decodeJWTPayload payload =
    case B64URL.decodeUnpadded (TE.encodeUtf8 payload) of
        Right decodedBytes ->
            case Aeson.decode' (LBS.fromStrict decodedBytes) of
                Just info -> Right info{active = True} -- JWT is implicitly active
                Nothing -> Left "Failed to parse JWT payload"
        Left _ -> Left "Invalid base64url encoding"

-- | Validate token claims (expiration, not-before, etc.)
validateTokenClaims :: TokenInfo -> UTCTime -> Either Text ()
validateTokenClaims tokenInfo currentTime = do
    let currentTimestamp = floor (realToFrac (utcTimeToPOSIXSeconds currentTime) :: Double) :: Integer

    -- Check expiration
    case MCP.Server.Auth.exp tokenInfo of
        Just expTime ->
            if currentTimestamp > expTime
                then Left "Token has expired"
                else Right ()
        Nothing -> Right ()

    -- Check not-before
    case MCP.Server.Auth.nbf tokenInfo of
        Just nbfTime ->
            if currentTimestamp < nbfTime
                then Left "Token not yet valid"
                else Right ()
        Nothing -> Right ()

    return ()

-- | Generate a cryptographically secure code verifier for PKCE
generateCodeVerifier :: IO Text
generateCodeVerifier = do
    bytes <- getRandomBytes 32 -- 32 bytes = 256 bits entropy
    pure $ TE.decodeUtf8 $ B64URL.encodeUnpadded bytes -- 43 chars

-- | Generate code challenge from verifier using SHA256 (S256 method)
generateCodeChallenge :: Text -> Text
generateCodeChallenge verifier =
    let verifierBytes = TE.encodeUtf8 verifier
        challengeHash = hashWith SHA256 verifierBytes
        challengeBytes = convert challengeHash :: ByteString
     in TE.decodeUtf8 $ B64URL.encodeUnpadded challengeBytes

-- | Validate PKCE code verifier against challenge
validateCodeVerifier :: CodeVerifier -> CodeChallenge -> Bool
validateCodeVerifier (CodeVerifier verifier) (CodeChallenge challenge) = generateCodeChallenge verifier == challenge

-- | Type-level tag for MCP protected resource authentication
data ProtectedResourceAuth

-- | Configuration for ProtectedResourceAuth
newtype ProtectedResourceAuthConfig = ProtectedResourceAuthConfig
    { resourceMetadataUrl :: Text
    {- ^ URL to the protected resource metadata endpoint
    e.g., "https://mcp.example.com/.well-known/oauth-protected-resource"
    -}
    }
    deriving (Show, Generic)
