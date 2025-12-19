{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Servant.OAuth2.IDP.Errors
Description : Consolidated OAuth error types
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com
Stability   : experimental
Portability : GHC

Consolidated error types for OAuth 2.1 implementation. This module centralizes
all error types previously scattered across Types and LoginFlowError modules.

Per FR-004b requirements:
- ValidationError: semantic validation errors for OAuth handler logic
- AuthorizationError: OAuth 2.0 protocol errors per RFC 6749
- LoginFlowError: semantic errors for login flow
- OAuthErrorCode: RFC 6749 compliant error codes (snake_case JSON)
- TokenParameter: token endpoint parameter identification
- OAuthErrorResponse: OAuth error response structure
-}
module Servant.OAuth2.IDP.Errors (
    -- * OAuth Error Codes
    OAuthErrorCode (..),

    -- * Token Parameters
    TokenParameter (..),

    -- * Validation Errors
    ValidationError (..),
    validationErrorToResponse,

    -- * Authorization Errors
    AuthorizationError (..),
    authorizationErrorToResponse,
    renderAuthorizationError,

    -- * Authorization Error Reason ADTs (FR-004c)
    InvalidRequestReason (..),
    InvalidClientReason (..),
    InvalidGrantReason (..),
    UnauthorizedClientReason (..),
    UnsupportedGrantTypeReason (..),
    InvalidScopeReason (..),
    AccessDeniedReason (..),

    -- * Login Flow Errors
    LoginFlowError (..),

    -- * Error Response
    OAuthErrorResponse (..),
) where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, withText, (.:), (.:?), (.=))
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Lucid (
    ToHtml (..),
    body_,
    charset_,
    class_,
    div_,
    doctypehtml_,
    h1_,
    head_,
    meta_,
    p_,
    style_,
    title_,
 )
import Network.HTTP.Types.Status (Status, status400, status401, status403)
import Servant.OAuth2.IDP.Types (
    AuthCodeId,
    ClientId (..),
    CodeChallengeMethod,
    OAuthGrantType (..),
    RedirectUri,
    RefreshTokenId,
    Scope (..),
    SessionId (..),
    unAuthCodeId,
    unClientId,
    unRefreshTokenId,
    unScope,
 )
import Web.HttpApiData (ToHttpApiData (..))

-- -----------------------------------------------------------------------------
-- OAuth Error Codes (RFC 6749)
-- -----------------------------------------------------------------------------

{- | OAuth 2.0 error codes per RFC 6749.
These are the standardized error codes that appear in OAuth error responses.
ToJSON instance outputs snake_case as required by RFC 6749.
-}
data OAuthErrorCode
    = ErrInvalidRequest
    | ErrInvalidClient
    | ErrInvalidGrant
    | ErrUnauthorizedClient
    | ErrUnsupportedGrantType
    | ErrInvalidScope
    | ErrAccessDenied
    | ErrUnsupportedResponseType
    | ErrServerError
    | ErrTemporarilyUnavailable
    deriving stock (Eq, Show, Generic)

instance ToJSON OAuthErrorCode where
    toJSON = \case
        ErrInvalidRequest -> "invalid_request"
        ErrInvalidClient -> "invalid_client"
        ErrInvalidGrant -> "invalid_grant"
        ErrUnauthorizedClient -> "unauthorized_client"
        ErrUnsupportedGrantType -> "unsupported_grant_type"
        ErrInvalidScope -> "invalid_scope"
        ErrAccessDenied -> "access_denied"
        ErrUnsupportedResponseType -> "unsupported_response_type"
        ErrServerError -> "server_error"
        ErrTemporarilyUnavailable -> "temporarily_unavailable"

instance FromJSON OAuthErrorCode where
    parseJSON = withText "OAuthErrorCode" $ \code -> case code of
        "invalid_request" -> pure ErrInvalidRequest
        "invalid_client" -> pure ErrInvalidClient
        "invalid_grant" -> pure ErrInvalidGrant
        "unauthorized_client" -> pure ErrUnauthorizedClient
        "unsupported_grant_type" -> pure ErrUnsupportedGrantType
        "invalid_scope" -> pure ErrInvalidScope
        "access_denied" -> pure ErrAccessDenied
        "unsupported_response_type" -> pure ErrUnsupportedResponseType
        "server_error" -> pure ErrServerError
        "temporarily_unavailable" -> pure ErrTemporarilyUnavailable
        _ -> fail $ "Unknown error code: " <> T.unpack code

-- -----------------------------------------------------------------------------
-- Token Parameters
-- -----------------------------------------------------------------------------

{- | Token endpoint parameter identification.
Used in error reporting to indicate which parameter is missing or malformed.
-}
data TokenParameter
    = TokenParamCode
    | TokenParamCodeVerifier
    | TokenParamRefreshToken
    deriving stock (Eq, Show, Generic)

-- -----------------------------------------------------------------------------
-- OAuth Error Response
-- -----------------------------------------------------------------------------

{- | OAuth 2.0 error response per RFC 6749 Section 5.2.
Uses OAuthErrorCode ADT for type-safe error codes (FR-004b).
-}
data OAuthErrorResponse = OAuthErrorResponse
    { oauthErrorCode :: OAuthErrorCode
    , oauthErrorDescription :: Maybe Text
    }
    deriving stock (Eq, Show, Generic)

instance ToJSON OAuthErrorResponse where
    toJSON OAuthErrorResponse{..} =
        object $
            ("error" .= oauthErrorCode)
                : case oauthErrorDescription of
                    Just desc -> ["error_description" .= desc]
                    Nothing -> []

instance FromJSON OAuthErrorResponse where
    parseJSON = withObject "OAuthErrorResponse" $ \v ->
        OAuthErrorResponse
            <$> v .: "error"
            <*> v .:? "error_description"

-- -----------------------------------------------------------------------------
-- Validation Errors
-- -----------------------------------------------------------------------------

{- | Semantic validation errors for OAuth handler logic.
Fixed type (not an associated type) - safe to expose to clients.
These are validation failures that pass parsing but violate business rules.

Extended in FR-004b with:
- UnsupportedCodeChallengeMethod: PKCE method not supported
- MissingTokenParameter: required token parameter absent
- InvalidTokenParameterFormat: token parameter has invalid format
- EmptyRedirectUris: client registration with no redirect URIs
-}
data ValidationError
    = -- | redirect_uri doesn't match registered client
      RedirectUriMismatch ClientId RedirectUri
    | -- | response_type not supported
      UnsupportedResponseType Text
    | -- | client_id not found in registry
      ClientNotRegistered ClientId
    | -- | required scope not present
      MissingRequiredScope Scope
    | -- | state parameter validation failed
      InvalidStateParameter Text
    | -- | code_challenge_method not supported (FR-004b)
      UnsupportedCodeChallengeMethod CodeChallengeMethod
    | -- | required token parameter missing (FR-004b)
      MissingTokenParameter TokenParameter
    | -- | token parameter format invalid (FR-004b)
      InvalidTokenParameterFormat TokenParameter Text
    | -- | client registration with no redirect URIs (FR-004b)
      EmptyRedirectUris
    deriving stock (Eq, Show, Generic)

{- | Map ValidationError to HTTP 400 status with descriptive message.
All validation errors are semantic failures (not parse errors) and map to 400.
-}
validationErrorToResponse :: ValidationError -> (Status, Text)
validationErrorToResponse = \case
    RedirectUriMismatch clientId redirectUri ->
        ( status400
        , "redirect_uri does not match registered URIs for client_id: "
            <> unClientId clientId
            <> " (provided: "
            <> toUrlPiece redirectUri
            <> ")"
        )
    UnsupportedResponseType responseType ->
        (status400, "response_type not supported: " <> responseType)
    ClientNotRegistered clientId ->
        (status400, "client_id not registered: " <> unClientId clientId)
    MissingRequiredScope scope ->
        (status400, "Missing required scope: " <> unScope scope)
    InvalidStateParameter stateValue ->
        (status400, "Invalid state parameter: " <> stateValue)
    UnsupportedCodeChallengeMethod method ->
        (status400, "code_challenge_method not supported: " <> toUrlPiece method)
    MissingTokenParameter param ->
        ( status400
        , "Missing required parameter: " <> case param of
            TokenParamCode -> "code"
            TokenParamCodeVerifier -> "code_verifier"
            TokenParamRefreshToken -> "refresh_token"
        )
    InvalidTokenParameterFormat param detail ->
        ( status400
        , "Invalid parameter format for "
            <> ( case param of
                    TokenParamCode -> "code"
                    TokenParamCodeVerifier -> "code_verifier"
                    TokenParamRefreshToken -> "refresh_token"
               )
            <> ": "
            <> detail
        )
    EmptyRedirectUris ->
        (status400, "Client registration must include at least one redirect_uri")

-- -----------------------------------------------------------------------------
-- Authorization Error Reason ADTs (FR-004c)
-- -----------------------------------------------------------------------------

{- | Reasons for InvalidRequest errors.
Replaces Text payload with precise ADT for exhaustive pattern matching.
-}
data InvalidRequestReason
    = MissingParameter TokenParameter
    | InvalidParameterFormat TokenParameter
    | MalformedRequest
    deriving stock (Eq, Show, Generic)

{- | Reasons for InvalidClient errors.
Replaces Text payload with precise ADT for exhaustive pattern matching.
-}
data InvalidClientReason
    = ClientNotFound ClientId
    | InvalidClientCredentials
    | ClientSecretMismatch
    deriving stock (Eq, Show, Generic)

{- | Reasons for InvalidGrant errors.
Replaces Text payload with precise ADT for exhaustive pattern matching.
-}
data InvalidGrantReason
    = CodeNotFound AuthCodeId
    | CodeExpired AuthCodeId
    | CodeAlreadyUsed AuthCodeId
    | RefreshTokenNotFound RefreshTokenId
    | RefreshTokenExpired RefreshTokenId
    | RefreshTokenRevoked RefreshTokenId
    deriving stock (Eq, Show, Generic)

{- | Reasons for UnauthorizedClient errors.
Replaces Text payload with precise ADT for exhaustive pattern matching.
-}
data UnauthorizedClientReason
    = GrantTypeNotAllowed OAuthGrantType
    | ScopeNotAllowed Scope
    | RedirectUriNotRegistered RedirectUri
    deriving stock (Eq, Show, Generic)

{- | Reasons for UnsupportedGrantType errors.
Replaces Text payload with precise ADT for exhaustive pattern matching.
-}
data UnsupportedGrantTypeReason
    = UnknownGrantType Text
    | GrantTypeDisabled OAuthGrantType
    deriving stock (Eq, Show, Generic)

{- | Reasons for InvalidScope errors.
Replaces Text payload with precise ADT for exhaustive pattern matching.
-}
data InvalidScopeReason
    = UnknownScope Text
    | ScopeNotPermitted Scope
    deriving stock (Eq, Show, Generic)

{- | Reasons for AccessDenied errors.
Replaces Text payload with precise ADT for exhaustive pattern matching.
-}
data AccessDeniedReason
    = UserDenied
    | ResourceOwnerDenied
    | ConsentRequired
    deriving stock (Eq, Show, Generic)

-- -----------------------------------------------------------------------------
-- Authorization Errors
-- -----------------------------------------------------------------------------

{- | OAuth 2.0 authorization errors per RFC 6749 Section 4.1.2.1 and 5.2.
Fixed type (protocol-defined), NOT an associated type.
Safe to expose to clients in OAuth error response format.

Updated in FR-004c to use ADT payloads instead of Text for exhaustive pattern matching.
-}
data AuthorizationError
    = -- | 400: Missing/invalid parameter
      InvalidRequest InvalidRequestReason
    | -- | 401: Client authentication failed
      InvalidClient InvalidClientReason
    | -- | 400: Invalid authorization code/refresh token
      InvalidGrant InvalidGrantReason
    | -- | 401: Client not authorized for grant type
      UnauthorizedClient UnauthorizedClientReason
    | -- | 400: Grant type not supported
      UnsupportedGrantType UnsupportedGrantTypeReason
    | -- | 400: Invalid/unknown scope
      InvalidScope InvalidScopeReason
    | -- | 403: Resource owner denied request
      AccessDenied AccessDeniedReason
    | -- | 400: Authorization code expired
      ExpiredCode
    | -- | 400: Redirect URI doesn't match registered
      InvalidRedirectUri
    | -- | 400: Code verifier doesn't match challenge
      PKCEVerificationFailed
    deriving stock (Eq, Show, Generic)

{- | Render AuthorizationError to human-readable error description (FR-004c).
Converts ADT payloads to Text for UI layer.
-}
renderAuthorizationError :: AuthorizationError -> Text
renderAuthorizationError = \case
    InvalidRequest reason -> case reason of
        MissingParameter param -> "Missing required parameter: " <> renderTokenParam param
        InvalidParameterFormat param -> "Invalid parameter format: " <> renderTokenParam param
        MalformedRequest -> "Malformed request"
    InvalidClient reason -> case reason of
        ClientNotFound clientId -> "Client not found: " <> unClientId clientId
        InvalidClientCredentials -> "Invalid client credentials"
        ClientSecretMismatch -> "Client secret mismatch"
    InvalidGrant reason -> case reason of
        CodeNotFound codeId -> "Authorization code not found: " <> unAuthCodeId codeId
        CodeExpired codeId -> "Authorization code expired: " <> unAuthCodeId codeId
        CodeAlreadyUsed codeId -> "Authorization code already used: " <> unAuthCodeId codeId
        RefreshTokenNotFound rtId -> "Refresh token not found: " <> unRefreshTokenId rtId
        RefreshTokenExpired rtId -> "Refresh token expired: " <> unRefreshTokenId rtId
        RefreshTokenRevoked rtId -> "Refresh token revoked: " <> unRefreshTokenId rtId
    UnauthorizedClient reason -> case reason of
        GrantTypeNotAllowed grantType -> "Grant type not allowed: " <> renderGrantType grantType
        ScopeNotAllowed scope -> "Scope not allowed: " <> unScope scope
        RedirectUriNotRegistered _uri -> "Redirect URI not registered"
    UnsupportedGrantType reason -> case reason of
        UnknownGrantType gt -> "Unknown grant type: " <> gt
        GrantTypeDisabled gt -> "Grant type disabled: " <> renderGrantType gt
    InvalidScope reason -> case reason of
        UnknownScope s -> "Unknown scope: " <> s
        ScopeNotPermitted scope -> "Scope not permitted: " <> unScope scope
    AccessDenied reason -> case reason of
        UserDenied -> "User denied authorization"
        ResourceOwnerDenied -> "Resource owner denied authorization"
        ConsentRequired -> "Consent required"
    ExpiredCode -> "Authorization code has expired"
    InvalidRedirectUri -> "Invalid redirect_uri"
    PKCEVerificationFailed -> "PKCE verification failed"
  where
    renderTokenParam :: TokenParameter -> Text
    renderTokenParam TokenParamCode = "code"
    renderTokenParam TokenParamCodeVerifier = "code_verifier"
    renderTokenParam TokenParamRefreshToken = "refresh_token"

    renderGrantType :: OAuthGrantType -> Text
    renderGrantType OAuthAuthorizationCode = "authorization_code"
    renderGrantType OAuthClientCredentials = "client_credentials"

{- | Map AuthorizationError to HTTP status and OAuth error response.
Per RFC 6749 Section 4.1.2.1 (authorization endpoint errors) and Section 5.2 (token endpoint errors).
Uses OAuthErrorCode ADT for type-safe error codes (FR-004b).
Uses renderAuthorizationError for human-readable descriptions (FR-004c).
-}
authorizationErrorToResponse :: AuthorizationError -> (Status, OAuthErrorResponse)
authorizationErrorToResponse err =
    let desc = renderAuthorizationError err
     in case err of
            InvalidRequest _ -> (status400, OAuthErrorResponse ErrInvalidRequest (Just desc))
            InvalidClient _ -> (status401, OAuthErrorResponse ErrInvalidClient (Just desc))
            InvalidGrant _ -> (status400, OAuthErrorResponse ErrInvalidGrant (Just desc))
            UnauthorizedClient _ -> (status401, OAuthErrorResponse ErrUnauthorizedClient (Just desc))
            UnsupportedGrantType _ -> (status400, OAuthErrorResponse ErrUnsupportedGrantType (Just desc))
            InvalidScope _ -> (status400, OAuthErrorResponse ErrInvalidScope (Just desc))
            AccessDenied _ -> (status403, OAuthErrorResponse ErrAccessDenied (Just desc))
            ExpiredCode -> (status400, OAuthErrorResponse ErrInvalidGrant (Just desc))
            InvalidRedirectUri -> (status400, OAuthErrorResponse ErrInvalidRequest (Just desc))
            PKCEVerificationFailed -> (status400, OAuthErrorResponse ErrInvalidGrant (Just desc))

-- -----------------------------------------------------------------------------
-- Login Flow Errors
-- -----------------------------------------------------------------------------

{- | Semantic errors that can occur during the OAuth login flow.

Each constructor represents a specific failure mode with enough
information to render a user-friendly error page.

Moved from Servant.OAuth2.IDP.LoginFlowError module (FR-004b).
-}
data LoginFlowError
    = -- | Browser does not have cookies enabled
      CookiesRequired
    | -- | Session cookie doesn't match the form session ID
      SessionCookieMismatch
    | -- | Session not found in storage
      SessionNotFound SessionId
    | -- | Login session has expired
      SessionExpired SessionId
    deriving (Show, Eq)

{- | Render login flow errors as user-friendly HTML pages.

Each error type produces a styled error page with appropriate
title and message. HTML special characters are automatically
escaped by Lucid.
-}
instance ToHtml LoginFlowError where
    toHtmlRaw = toHtml
    toHtml err = doctypehtml_ $ do
        head_ $ do
            meta_ [charset_ "utf-8"]
            title_ $ toHtml (errorTitle err)
            style_ $
                T.unlines
                    [ "body { font-family: system-ui, sans-serif; max-width: 500px; margin: 50px auto; padding: 20px; }"
                    , "h1 { color: #d32f2f; }"
                    , ".error { background: #ffebee; padding: 15px; border-radius: 5px; border-left: 4px solid #d32f2f; }"
                    ]
        body_ $ do
            h1_ $ toHtml (errorTitle err)
            div_ [class_ "error"] $ do
                p_ $ toHtml (errorMessage err)
            p_ "Please contact the application developer."

-- | Get the error title for a LoginFlowError
errorTitle :: LoginFlowError -> Text
errorTitle CookiesRequired = "Cookies Required"
errorTitle SessionCookieMismatch = "Cookies Required"
errorTitle (SessionNotFound _) = "Invalid Session"
errorTitle (SessionExpired _) = "Session Expired"

-- | Get the user-friendly error message for a LoginFlowError
errorMessage :: LoginFlowError -> Text
errorMessage CookiesRequired =
    "Your browser must have cookies enabled to sign in. Please enable cookies and try again."
errorMessage SessionCookieMismatch =
    "Session cookie mismatch. Please enable cookies and try again."
errorMessage (SessionNotFound _) =
    "Session not found or has expired. Please restart the authorization flow."
errorMessage (SessionExpired _) =
    "Your login session has expired. Please restart the authorization flow."
