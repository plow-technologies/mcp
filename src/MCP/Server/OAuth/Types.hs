{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : MCP.Server.OAuth.Types
Description : OAuth 2.1 domain types for MCP server
Copyright   : (C) 2025 Matthias Pall Gissurarson
License     : MIT
Maintainer  : mpg@mpg.is
Stability   : experimental
Portability : GHC

This module provides type-safe OAuth 2.1 domain types with smart constructors
for validation. These types form the foundation for the OAuth typeclass
refactoring.
-}
module MCP.Server.OAuth.Types (
    -- * Identity Newtypes
    AuthCodeId (..),
    mkAuthCodeId,
    ClientId (..),
    mkClientId,
    SessionId (..),
    mkSessionId,
    AccessTokenId (..),
    RefreshTokenId (..),
    mkRefreshTokenId,
    UserId (..),
    mkUserId,

    -- * Value Newtypes
    RedirectUri (..),
    mkRedirectUri,
    Scope (..),
    mkScope,
    CodeChallenge (..),
    mkCodeChallenge,
    CodeVerifier (..),
    mkCodeVerifier,

    -- * HTTP Response Newtypes
    RedirectTarget (..),
    SessionCookie (..),

    -- * ADTs
    CodeChallengeMethod (..),
    GrantType (..),
    ResponseType (..),
    ClientAuthMethod (..),

    -- * Domain Entities
    AuthorizationCode (..),
    ClientInfo (..),
    PendingAuthorization (..),
    AuthUser (..),
) where

import Control.Monad (forM_, when)
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, withText, (.:), (.:?), (.=))
import Data.Aeson.Types (Parser)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit, isHexDigit, isSpace)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Network.URI (URI, parseURI, uriScheme, uriToString)
import Servant.Auth.Server (FromJWT, ToJWT)
import Web.HttpApiData (FromHttpApiData (..), ToHttpApiData (..))

-- -----------------------------------------------------------------------------
-- Identity Newtypes
-- -----------------------------------------------------------------------------

-- | Authorization code identifier
newtype AuthCodeId = AuthCodeId {unAuthCodeId :: Text}
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (FromJSON, ToJSON)

-- | Smart constructor for AuthCodeId
mkAuthCodeId :: Text -> Maybe AuthCodeId
mkAuthCodeId t
    | T.null t = Nothing
    | otherwise = Just (AuthCodeId t)

instance FromHttpApiData AuthCodeId where
    parseUrlPiece t
        | T.null t = Left "AuthCodeId cannot be empty"
        | otherwise = Right (AuthCodeId t)

instance ToHttpApiData AuthCodeId where
    toUrlPiece = unAuthCodeId

-- | OAuth client identifier
newtype ClientId = ClientId {unClientId :: Text}
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (FromJSON, ToJSON)

-- | Smart constructor for ClientId
mkClientId :: Text -> Maybe ClientId
mkClientId t
    | T.null t = Nothing
    | otherwise = Just (ClientId t)

instance FromHttpApiData ClientId where
    parseUrlPiece t
        | T.null t = Left "ClientId cannot be empty"
        | otherwise = Right (ClientId t)

instance ToHttpApiData ClientId where
    toUrlPiece = unClientId

-- | Session identifier for pending authorizations
newtype SessionId = SessionId {unSessionId :: Text}
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (FromJSON, ToJSON)

-- | Smart constructor for SessionId (validates UUID format)
mkSessionId :: Text -> Maybe SessionId
mkSessionId t
    | isValidUUID t = Just (SessionId t)
    | otherwise = Nothing
  where
    -- Check for UUID format: 8-4-4-4-12 hex pattern
    isValidUUID uuid =
        let parts = T.splitOn "-" uuid
         in case parts of
                [p1, p2, p3, p4, p5]
                    | T.length p1 == 8
                        && T.length p2 == 4
                        && T.length p3 == 4
                        && T.length p4 == 4
                        && T.length p5 == 12
                        && all (T.all isHexDigit) parts ->
                        True
                _ -> False

instance FromHttpApiData SessionId where
    parseUrlPiece t = case mkSessionId t of
        Just sid -> Right sid
        Nothing -> Left "SessionId must be a valid UUID"

instance ToHttpApiData SessionId where
    toUrlPiece = unSessionId

-- | Access token identifier (JWT-generated, no smart constructor needed)
newtype AccessTokenId = AccessTokenId {unAccessTokenId :: Text}
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (FromJSON, ToJSON)

instance FromHttpApiData AccessTokenId where
    parseUrlPiece t
        | T.null t = Left "AccessTokenId cannot be empty"
        | otherwise = Right (AccessTokenId t)

instance ToHttpApiData AccessTokenId where
    toUrlPiece = unAccessTokenId

-- | Refresh token identifier
newtype RefreshTokenId = RefreshTokenId {unRefreshTokenId :: Text}
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (FromJSON, ToJSON)

-- | Smart constructor for RefreshTokenId
mkRefreshTokenId :: Text -> Maybe RefreshTokenId
mkRefreshTokenId t
    | T.null t = Nothing
    | otherwise = Just (RefreshTokenId t)

instance FromHttpApiData RefreshTokenId where
    parseUrlPiece t
        | T.null t = Left "RefreshTokenId cannot be empty"
        | otherwise = Right (RefreshTokenId t)

instance ToHttpApiData RefreshTokenId where
    toUrlPiece = unRefreshTokenId

-- | User identifier
newtype UserId = UserId {unUserId :: Text}
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (FromJSON, ToJSON)

-- | Smart constructor for UserId
mkUserId :: Text -> Maybe UserId
mkUserId t
    | T.null t = Nothing
    | otherwise = Just (UserId t)

instance FromHttpApiData UserId where
    parseUrlPiece t
        | T.null t = Left "UserId cannot be empty"
        | otherwise = Right (UserId t)

instance ToHttpApiData UserId where
    toUrlPiece = unUserId

-- -----------------------------------------------------------------------------
-- Value Newtypes
-- -----------------------------------------------------------------------------

-- | OAuth redirect URI
newtype RedirectUri = RedirectUri {unRedirectUri :: URI}
    deriving stock (Eq, Ord, Show, Generic)

instance FromJSON RedirectUri where
    parseJSON = withText "RedirectUri" $ \t ->
        case mkRedirectUri t of
            Just uri -> pure uri
            Nothing -> fail $ "Invalid redirect URI: " ++ T.unpack t

instance ToJSON RedirectUri where
    toJSON (RedirectUri uri) = toJSON (show uri)

instance FromHttpApiData RedirectUri where
    parseUrlPiece t = case parseURI (T.unpack t) of
        Nothing -> Left "Invalid URI format"
        Just uri
            | uriScheme uri == "https:" -> Right (RedirectUri uri)
            | uriScheme uri == "http:" && ("localhost" `T.isInfixOf` t || "127.0.0.1" `T.isInfixOf` t) ->
                Right (RedirectUri uri)
            | otherwise -> Left "Redirect URI must use https or http://localhost"

instance ToHttpApiData RedirectUri where
    toUrlPiece (RedirectUri uri) = T.pack (uriToString id uri "")

-- | Smart constructor for RedirectUri (validates https:// or http://localhost)
mkRedirectUri :: Text -> Maybe RedirectUri
mkRedirectUri t = do
    uri <- parseURI (T.unpack t)
    let scheme = uriScheme uri
    -- Allow https:// or http://localhost for development
    if scheme == "https:"
        || (scheme == "http:" && ("localhost" `T.isInfixOf` t || "127.0.0.1" `T.isInfixOf` t))
        then Just (RedirectUri uri)
        else Nothing

-- | OAuth scope value
newtype Scope = Scope {unScope :: Text}
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (FromJSON, ToJSON)

-- | Smart constructor for Scope (non-empty, no whitespace)
mkScope :: Text -> Maybe Scope
mkScope t
    | T.null t = Nothing
    | T.any isSpace t = Nothing
    | otherwise = Just (Scope t)

instance FromHttpApiData Scope where
    parseUrlPiece t
        | T.null t = Left "Scope cannot be empty"
        | T.any isSpace t = Left "Scope cannot contain whitespace"
        | otherwise = Right (Scope t)

instance ToHttpApiData Scope where
    toUrlPiece = unScope

-- | PKCE code challenge
newtype CodeChallenge = CodeChallenge {unCodeChallenge :: Text}
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (FromJSON, ToJSON)

-- | Smart constructor for CodeChallenge (base64url charset, 43-128 chars)
mkCodeChallenge :: Text -> Maybe CodeChallenge
mkCodeChallenge t
    | len < 43 || len > 128 = Nothing
    | not (T.all isBase64UrlChar t) = Nothing
    | otherwise = Just (CodeChallenge t)
  where
    len = T.length t
    isBase64UrlChar c =
        isAsciiUpper c
            || isAsciiLower c
            || isDigit c
            || c == '-'
            || c == '_'

instance FromHttpApiData CodeChallenge where
    parseUrlPiece t = case mkCodeChallenge t of
        Just cc -> Right cc
        Nothing -> Left "CodeChallenge must be base64url (43-128 chars)"

instance ToHttpApiData CodeChallenge where
    toUrlPiece = unCodeChallenge

-- | PKCE code verifier
newtype CodeVerifier = CodeVerifier {unCodeVerifier :: Text}
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (FromJSON, ToJSON)

-- | Smart constructor for CodeVerifier (unreserved chars per RFC 7636, 43-128 chars)
mkCodeVerifier :: Text -> Maybe CodeVerifier
mkCodeVerifier t
    | len < 43 || len > 128 = Nothing
    | not (T.all isUnreservedChar t) = Nothing
    | otherwise = Just (CodeVerifier t)
  where
    len = T.length t
    -- RFC 7636: unreserved = ALPHA / DIGIT / "-" / "." / "_" / "~"
    isUnreservedChar c =
        isAsciiUpper c
            || isAsciiLower c
            || isDigit c
            || c == '-'
            || c == '.'
            || c == '_'
            || c == '~'

instance FromHttpApiData CodeVerifier where
    parseUrlPiece t = case mkCodeVerifier t of
        Just cv -> Right cv
        Nothing -> Left "CodeVerifier must contain unreserved chars (43-128 chars)"

instance ToHttpApiData CodeVerifier where
    toUrlPiece = unCodeVerifier

-- -----------------------------------------------------------------------------
-- HTTP Response Newtypes
-- -----------------------------------------------------------------------------

-- | Semantic wrapper for HTTP redirect target (Location header)
newtype RedirectTarget = RedirectTarget {unRedirectTarget :: Text}
    deriving stock (Eq, Show)
    deriving newtype (ToHttpApiData)

-- | Semantic wrapper for HTTP session cookie (Set-Cookie header)
newtype SessionCookie = SessionCookie {unSessionCookie :: Text}
    deriving stock (Eq, Show)
    deriving newtype (ToHttpApiData)

-- -----------------------------------------------------------------------------
-- ADTs
-- -----------------------------------------------------------------------------

-- | PKCE code challenge method
data CodeChallengeMethod
    = S256
    | Plain
    deriving stock (Eq, Ord, Show, Generic)

instance FromJSON CodeChallengeMethod where
    parseJSON = withText "CodeChallengeMethod" $ \case
        "S256" -> pure S256
        "plain" -> pure Plain
        other -> fail $ "Invalid code_challenge_method: " ++ T.unpack other

instance ToJSON CodeChallengeMethod where
    toJSON S256 = toJSON ("S256" :: Text)
    toJSON Plain = toJSON ("plain" :: Text)

instance FromHttpApiData CodeChallengeMethod where
    parseUrlPiece = \case
        "S256" -> Right S256
        "plain" -> Right Plain
        other -> Left $ "Invalid code_challenge_method: " <> other

instance ToHttpApiData CodeChallengeMethod where
    toUrlPiece S256 = "S256"
    toUrlPiece Plain = "plain"

-- | OAuth grant type
data GrantType
    = GrantAuthorizationCode
    | GrantRefreshToken
    | GrantClientCredentials
    deriving stock (Eq, Ord, Show, Generic)

instance FromJSON GrantType where
    parseJSON = withText "GrantType" $ \case
        "authorization_code" -> pure GrantAuthorizationCode
        "refresh_token" -> pure GrantRefreshToken
        "client_credentials" -> pure GrantClientCredentials
        other -> fail $ "Invalid grant_type: " ++ T.unpack other

instance ToJSON GrantType where
    toJSON GrantAuthorizationCode = toJSON ("authorization_code" :: Text)
    toJSON GrantRefreshToken = toJSON ("refresh_token" :: Text)
    toJSON GrantClientCredentials = toJSON ("client_credentials" :: Text)

instance FromHttpApiData GrantType where
    parseUrlPiece = \case
        "authorization_code" -> Right GrantAuthorizationCode
        "refresh_token" -> Right GrantRefreshToken
        "client_credentials" -> Right GrantClientCredentials
        other -> Left $ "Invalid grant_type: " <> other

instance ToHttpApiData GrantType where
    toUrlPiece GrantAuthorizationCode = "authorization_code"
    toUrlPiece GrantRefreshToken = "refresh_token"
    toUrlPiece GrantClientCredentials = "client_credentials"

-- | OAuth response type
data ResponseType
    = ResponseCode
    | ResponseToken
    deriving stock (Eq, Ord, Show, Generic)

instance FromJSON ResponseType where
    parseJSON = withText "ResponseType" $ \case
        "code" -> pure ResponseCode
        "token" -> pure ResponseToken
        other -> fail $ "Invalid response_type: " ++ T.unpack other

instance ToJSON ResponseType where
    toJSON ResponseCode = toJSON ("code" :: Text)
    toJSON ResponseToken = toJSON ("token" :: Text)

instance FromHttpApiData ResponseType where
    parseUrlPiece = \case
        "code" -> Right ResponseCode
        "token" -> Right ResponseToken
        other -> Left $ "Invalid response_type: " <> other

instance ToHttpApiData ResponseType where
    toUrlPiece ResponseCode = "code"
    toUrlPiece ResponseToken = "token"

-- | Client authentication method
data ClientAuthMethod
    = AuthNone
    | AuthClientSecretPost
    | AuthClientSecretBasic
    deriving stock (Eq, Ord, Show, Generic)

instance FromJSON ClientAuthMethod where
    parseJSON = withText "ClientAuthMethod" $ \case
        "none" -> pure AuthNone
        "client_secret_post" -> pure AuthClientSecretPost
        "client_secret_basic" -> pure AuthClientSecretBasic
        other -> fail $ "Invalid token_endpoint_auth_method: " ++ T.unpack other

instance ToJSON ClientAuthMethod where
    toJSON AuthNone = toJSON ("none" :: Text)
    toJSON AuthClientSecretPost = toJSON ("client_secret_post" :: Text)
    toJSON AuthClientSecretBasic = toJSON ("client_secret_basic" :: Text)

instance FromHttpApiData ClientAuthMethod where
    parseUrlPiece = \case
        "none" -> Right AuthNone
        "client_secret_post" -> Right AuthClientSecretPost
        "client_secret_basic" -> Right AuthClientSecretBasic
        other -> Left $ "Invalid token_endpoint_auth_method: " <> other

instance ToHttpApiData ClientAuthMethod where
    toUrlPiece AuthNone = "none"
    toUrlPiece AuthClientSecretPost = "client_secret_post"
    toUrlPiece AuthClientSecretBasic = "client_secret_basic"

-- -----------------------------------------------------------------------------
-- Domain Entities
-- -----------------------------------------------------------------------------

-- | Authorization code with PKCE
data AuthorizationCode userId = AuthorizationCode
    { authCodeId :: AuthCodeId
    , authClientId :: ClientId
    , authRedirectUri :: RedirectUri
    , authCodeChallenge :: CodeChallenge
    , authCodeChallengeMethod :: CodeChallengeMethod
    , authScopes :: Set Scope
    , authUserId :: userId
    , authExpiry :: UTCTime
    }
    deriving stock (Eq, Show, Generic, Functor)

instance (FromJSON userId) => FromJSON (AuthorizationCode userId) where
    parseJSON = withObject "AuthorizationCode" $ \v -> do
        codeId <- v .: "auth_code_id"
        -- Validate AuthCodeId is non-empty
        when (T.null (unAuthCodeId codeId)) $
            fail "auth_code_id must not be empty"

        clientId <- v .: "auth_client_id"
        -- Validate ClientId is non-empty
        when (T.null (unClientId clientId)) $
            fail "auth_client_id must not be empty"

        redirectUri <- v .: "auth_redirect_uri"

        challengeText <- v .: "auth_code_challenge"
        challenge <- case mkCodeChallenge challengeText of
            Just c -> pure c
            Nothing -> fail "auth_code_challenge must be base64url (43-128 chars)"

        challengeMethod <- v .: "auth_code_challenge_method"

        scopesSet <- v .: "auth_scopes"
        -- Validate all scopes are valid
        forM_ (Set.toList scopesSet) $ \scope ->
            when (T.null (unScope scope) || T.any isSpace (unScope scope)) $
                fail "auth_scopes must contain valid scopes (non-empty, no whitespace)"

        userId <- v .: "auth_user_id"

        expiry <- v .: "auth_expiry"

        pure $ AuthorizationCode codeId clientId redirectUri challenge challengeMethod scopesSet userId expiry

instance (ToJSON userId) => ToJSON (AuthorizationCode userId) where
    toJSON AuthorizationCode{..} =
        object
            [ "auth_code_id" .= authCodeId
            , "auth_client_id" .= authClientId
            , "auth_redirect_uri" .= authRedirectUri
            , "auth_code_challenge" .= authCodeChallenge
            , "auth_code_challenge_method" .= authCodeChallengeMethod
            , "auth_scopes" .= authScopes
            , "auth_user_id" .= authUserId
            , "auth_expiry" .= authExpiry
            ]

-- | Registered OAuth client information
data ClientInfo = ClientInfo
    { clientName :: Text
    , clientRedirectUris :: NonEmpty RedirectUri
    , clientGrantTypes :: Set GrantType
    , clientResponseTypes :: Set ResponseType
    , clientAuthMethod :: ClientAuthMethod
    }
    deriving stock (Eq, Show, Generic)

instance FromJSON ClientInfo where
    parseJSON = withObject "ClientInfo" $ \v -> do
        name <- v .: "client_name"

        uriList <- v .: "client_redirect_uris"
        uris <- case nonEmpty uriList of
            Nothing -> fail "client_redirect_uris must contain at least one URI"
            Just ne -> pure ne

        grantTypes <- v .: "client_grant_types"
        when (Set.null grantTypes) $
            fail "client_grant_types must not be empty"

        responseTypes <- v .: "client_response_types"
        when (Set.null responseTypes) $
            fail "client_response_types must not be empty"

        authMethod <- v .: "client_auth_method"

        pure $ ClientInfo name uris grantTypes responseTypes authMethod

instance ToJSON ClientInfo where
    toJSON ClientInfo{..} =
        object
            [ "client_name" .= clientName
            , "client_redirect_uris" .= clientRedirectUris
            , "client_grant_types" .= clientGrantTypes
            , "client_response_types" .= clientResponseTypes
            , "client_auth_method" .= clientAuthMethod
            ]

-- | Pending authorization awaiting user authentication
data PendingAuthorization = PendingAuthorization
    { pendingClientId :: ClientId
    , pendingRedirectUri :: RedirectUri
    , pendingCodeChallenge :: CodeChallenge
    , pendingCodeChallengeMethod :: CodeChallengeMethod
    , pendingScope :: Maybe (Set Scope)
    , pendingState :: Maybe Text
    , pendingResource :: Maybe URI
    , pendingCreatedAt :: UTCTime
    }
    deriving stock (Eq, Show, Generic)

instance FromJSON PendingAuthorization where
    parseJSON = withObject "PendingAuthorization" $ \v -> do
        clientId <- v .: "pending_client_id"
        -- Validate ClientId is non-empty
        when (T.null (unClientId clientId)) $
            fail "pending_client_id must not be empty"

        redirectUri <- v .: "pending_redirect_uri"

        challengeText <- v .: "pending_code_challenge"
        challenge <- case mkCodeChallenge challengeText of
            Just c -> pure c
            Nothing -> fail "pending_code_challenge must be base64url (43-128 chars)"

        challengeMethod <- v .: "pending_code_challenge_method"

        scopeMaybe <- v .:? "pending_scope"
        -- Validate scopes if present
        case scopeMaybe of
            Just scopesSet ->
                forM_ (Set.toList scopesSet) $ \scope ->
                    when (T.null (unScope scope) || T.any isSpace (unScope scope)) $
                        fail "pending_scope must contain valid scopes (non-empty, no whitespace)"
            Nothing -> pure ()

        state <- v .:? "pending_state"
        resource <- v .:? "pending_resource" >>= traverse parseURIText
        createdAt <- v .: "pending_created_at"

        pure $ PendingAuthorization clientId redirectUri challenge challengeMethod scopeMaybe state resource createdAt
      where
        parseURIText :: Text -> Parser URI
        parseURIText t = case parseURI (T.unpack t) of
            Just uri -> pure uri
            Nothing -> fail $ "Invalid URI: " ++ T.unpack t

instance ToJSON PendingAuthorization where
    toJSON PendingAuthorization{..} =
        object
            [ "pending_client_id" .= pendingClientId
            , "pending_redirect_uri" .= pendingRedirectUri
            , "pending_code_challenge" .= pendingCodeChallenge
            , "pending_code_challenge_method" .= pendingCodeChallengeMethod
            , "pending_scope" .= pendingScope
            , "pending_state" .= pendingState
            , "pending_resource" .= fmap (T.pack . show) pendingResource
            , "pending_created_at" .= pendingCreatedAt
            ]

-- | Authenticated user information
data AuthUser = AuthUser
    { userUserId :: UserId
    , userUserEmail :: Maybe Text
    , userUserName :: Maybe Text
    }
    deriving stock (Eq, Show, Generic)

instance FromJSON AuthUser where
    parseJSON = withObject "AuthUser" $ \v ->
        AuthUser
            <$> v .: "user_id"
            <*> v .:? "user_email"
            <*> v .:? "user_name"

instance ToJSON AuthUser where
    toJSON AuthUser{..} =
        object
            [ "user_id" .= userUserId
            , "user_email" .= userUserEmail
            , "user_name" .= userUserName
            ]

-- | JWT instances for AuthUser (rely on JSON instances above)
instance ToJWT AuthUser

instance FromJWT AuthUser
