{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Servant.OAuth2.IDP.ErrorsSpec
Description : Tests for consolidated OAuth error types
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com
Stability   : experimental
Portability : GHC

Test suite for consolidated OAuth error types in Servant.OAuth2.IDP.Errors.
-}
module Servant.OAuth2.IDP.ErrorsSpec (spec) where

import Data.Aeson (decode, encode)
import Data.Text qualified as T
import Network.HTTP.Types.Status (status400, status401, status403)
import Servant.OAuth2.IDP.Errors
import Servant.OAuth2.IDP.Types (
    AuthCodeId,
    ClientId,
    CodeChallengeMethod (..),
    OAuthGrantType (..),
    RedirectUri,
    RefreshTokenId,
    Scope,
    SessionId,
    mkRedirectUri,
    mkScope,
 )
import Servant.OAuth2.IDP.Types.Internal (unsafeAuthCodeId, unsafeClientId, unsafeRefreshTokenId, unsafeSessionId)
import Test.Hspec

-- Test fixture helpers
testClientId :: ClientId
testClientId = unsafeClientId "test_client_123"

testRedirectUri :: RedirectUri
testRedirectUri = case mkRedirectUri "https://example.com/callback" of
    Just uri -> uri
    Nothing -> error "Test fixture: invalid redirect URI"

testScope :: Scope
testScope = case mkScope "read" of
    Just s -> s
    Nothing -> error "Test fixture: invalid scope"

testSessionId :: SessionId
testSessionId = unsafeSessionId "12345678-1234-1234-1234-123456789abc"

spec :: Spec
spec = do
    describe "OAuthErrorCode" $ do
        it "ErrInvalidRequest serializes to snake_case JSON" $ do
            let json = encode ErrInvalidRequest
            decode json `shouldBe` Just ("invalid_request" :: T.Text)

        it "ErrInvalidClient serializes to snake_case JSON" $ do
            let json = encode ErrInvalidClient
            decode json `shouldBe` Just ("invalid_client" :: T.Text)

        it "ErrInvalidGrant serializes to snake_case JSON" $ do
            let json = encode ErrInvalidGrant
            decode json `shouldBe` Just ("invalid_grant" :: T.Text)

        it "ErrUnauthorizedClient serializes to snake_case JSON" $ do
            let json = encode ErrUnauthorizedClient
            decode json `shouldBe` Just ("unauthorized_client" :: T.Text)

        it "ErrUnsupportedGrantType serializes to snake_case JSON" $ do
            let json = encode ErrUnsupportedGrantType
            decode json `shouldBe` Just ("unsupported_grant_type" :: T.Text)

        it "ErrInvalidScope serializes to snake_case JSON" $ do
            let json = encode ErrInvalidScope
            decode json `shouldBe` Just ("invalid_scope" :: T.Text)

        it "ErrAccessDenied serializes to snake_case JSON" $ do
            let json = encode ErrAccessDenied
            decode json `shouldBe` Just ("access_denied" :: T.Text)

        it "ErrUnsupportedResponseType serializes to snake_case JSON" $ do
            let json = encode ErrUnsupportedResponseType
            decode json `shouldBe` Just ("unsupported_response_type" :: T.Text)

        it "ErrServerError serializes to snake_case JSON" $ do
            let json = encode ErrServerError
            decode json `shouldBe` Just ("server_error" :: T.Text)

        it "ErrTemporarilyUnavailable serializes to snake_case JSON" $ do
            let json = encode ErrTemporarilyUnavailable
            decode json `shouldBe` Just ("temporarily_unavailable" :: T.Text)

    describe "TokenParameter" $ do
        it "TokenParamCode has Eq instance" $ do
            TokenParamCode `shouldBe` TokenParamCode

        it "TokenParamCodeVerifier has Eq instance" $ do
            TokenParamCodeVerifier `shouldBe` TokenParamCodeVerifier

        it "TokenParamRefreshToken has Eq instance" $ do
            TokenParamRefreshToken `shouldBe` TokenParamRefreshToken

        it "different constructors are not equal" $ do
            TokenParamCode `shouldNotBe` TokenParamCodeVerifier

        it "has Show instance" $ do
            show TokenParamCode `shouldContain` "Code"

    describe "ValidationError" $ do
        describe "existing constructors" $ do
            it "RedirectUriMismatch can be constructed" $ do
                let err = RedirectUriMismatch testClientId testRedirectUri
                case err of
                    RedirectUriMismatch cid uri -> do
                        cid `shouldBe` testClientId
                        uri `shouldBe` testRedirectUri
                    _ -> expectationFailure "Pattern match failed"

            it "UnsupportedResponseType can be constructed" $ do
                let err = UnsupportedResponseType "implicit"
                case err of
                    UnsupportedResponseType rt -> rt `shouldBe` "implicit"
                    _ -> expectationFailure "Pattern match failed"

            it "ClientNotRegistered can be constructed" $ do
                let err = ClientNotRegistered testClientId
                case err of
                    ClientNotRegistered cid -> cid `shouldBe` testClientId
                    _ -> expectationFailure "Pattern match failed"

            it "MissingRequiredScope can be constructed" $ do
                let err = MissingRequiredScope testScope
                case err of
                    MissingRequiredScope s -> s `shouldBe` testScope
                    _ -> expectationFailure "Pattern match failed"

            it "InvalidStateParameter can be constructed" $ do
                let err = InvalidStateParameter "bad state"
                case err of
                    InvalidStateParameter st -> st `shouldBe` "bad state"
                    _ -> expectationFailure "Pattern match failed"

        describe "new constructors from FR-004b" $ do
            it "UnsupportedCodeChallengeMethod can be constructed" $ do
                let err = UnsupportedCodeChallengeMethod Plain
                case err of
                    UnsupportedCodeChallengeMethod method -> method `shouldBe` Plain
                    _ -> expectationFailure "Pattern match failed"

            it "MissingTokenParameter can be constructed" $ do
                let err = MissingTokenParameter TokenParamCode
                case err of
                    MissingTokenParameter param -> param `shouldBe` TokenParamCode
                    _ -> expectationFailure "Pattern match failed"

            it "InvalidTokenParameterFormat can be constructed" $ do
                let err = InvalidTokenParameterFormat TokenParamCodeVerifier "invalid chars"
                case err of
                    InvalidTokenParameterFormat param detail -> do
                        param `shouldBe` TokenParamCodeVerifier
                        detail `shouldBe` "invalid chars"
                    _ -> expectationFailure "Pattern match failed"

            it "EmptyRedirectUris can be constructed" $ do
                let err = EmptyRedirectUris
                err `shouldBe` EmptyRedirectUris

        describe "validationErrorToResponse" $ do
            it "RedirectUriMismatch returns 400 with descriptive message" $ do
                let err = RedirectUriMismatch testClientId testRedirectUri
                    (status, message) = validationErrorToResponse err
                status `shouldBe` status400
                T.unpack message `shouldContain` "redirect_uri does not match"
                T.unpack message `shouldContain` "test_client_123"

            it "UnsupportedResponseType returns 400 with response type" $ do
                let err = UnsupportedResponseType "implicit"
                    (status, message) = validationErrorToResponse err
                status `shouldBe` status400
                T.unpack message `shouldContain` "response_type not supported"
                T.unpack message `shouldContain` "implicit"

            it "ClientNotRegistered returns 400 with client_id" $ do
                let err = ClientNotRegistered testClientId
                    (status, message) = validationErrorToResponse err
                status `shouldBe` status400
                T.unpack message `shouldContain` "client_id not registered"
                T.unpack message `shouldContain` "test_client_123"

            it "MissingRequiredScope returns 400 with scope" $ do
                let err = MissingRequiredScope testScope
                    (status, message) = validationErrorToResponse err
                status `shouldBe` status400
                T.unpack message `shouldContain` "Missing required scope"
                T.unpack message `shouldContain` "read"

            it "InvalidStateParameter returns 400 with state value" $ do
                let err = InvalidStateParameter "tampered"
                    (status, message) = validationErrorToResponse err
                status `shouldBe` status400
                T.unpack message `shouldContain` "Invalid state parameter"
                T.unpack message `shouldContain` "tampered"

            it "UnsupportedCodeChallengeMethod returns 400 with method" $ do
                let err = UnsupportedCodeChallengeMethod Plain
                    (status, message) = validationErrorToResponse err
                status `shouldBe` status400
                T.unpack message `shouldContain` "code_challenge_method not supported"
                T.unpack message `shouldContain` "plain"

            it "MissingTokenParameter returns 400 with parameter name" $ do
                let err = MissingTokenParameter TokenParamCode
                    (status, message) = validationErrorToResponse err
                status `shouldBe` status400
                T.unpack message `shouldContain` "Missing required parameter"
                T.unpack message `shouldContain` "code"

            it "InvalidTokenParameterFormat returns 400 with parameter and detail" $ do
                let err = InvalidTokenParameterFormat TokenParamCodeVerifier "invalid chars"
                    (status, message) = validationErrorToResponse err
                status `shouldBe` status400
                T.unpack message `shouldContain` "Invalid parameter format"
                T.unpack message `shouldContain` "code_verifier"
                T.unpack message `shouldContain` "invalid chars"

            it "EmptyRedirectUris returns 400 with descriptive message" $ do
                let err = EmptyRedirectUris
                    (status, message) = validationErrorToResponse err
                status `shouldBe` status400
                T.unpack message `shouldContain` "redirect_uri"
                T.unpack message `shouldContain` "at least one"

    describe "AuthorizationError" $ do
        describe "authorizationErrorToResponse" $ do
            it "InvalidRequest maps to 400 with invalid_request code" $ do
                let err = InvalidRequest "Missing client_id"
                    (status, resp) = authorizationErrorToResponse err
                status `shouldBe` status400
                oauthErrorCode resp `shouldBe` ErrInvalidRequest
                oauthErrorDescription resp `shouldBe` Just "Missing client_id"

            it "InvalidClient maps to 401 with invalid_client code" $ do
                let err = InvalidClient "Authentication failed"
                    (status, resp) = authorizationErrorToResponse err
                status `shouldBe` status401
                oauthErrorCode resp `shouldBe` ErrInvalidClient
                oauthErrorDescription resp `shouldBe` Just "Authentication failed"

            it "InvalidGrant maps to 400 with invalid_grant code" $ do
                let err = InvalidGrant "Code already used"
                    (status, resp) = authorizationErrorToResponse err
                status `shouldBe` status400
                oauthErrorCode resp `shouldBe` ErrInvalidGrant
                oauthErrorDescription resp `shouldBe` Just "Code already used"

            it "UnauthorizedClient maps to 401 with unauthorized_client code" $ do
                let err = UnauthorizedClient "Not allowed"
                    (status, resp) = authorizationErrorToResponse err
                status `shouldBe` status401
                oauthErrorCode resp `shouldBe` ErrUnauthorizedClient
                oauthErrorDescription resp `shouldBe` Just "Not allowed"

            it "UnsupportedGrantType maps to 400 with unsupported_grant_type code" $ do
                let err = UnsupportedGrantType "password"
                    (status, resp) = authorizationErrorToResponse err
                status `shouldBe` status400
                oauthErrorCode resp `shouldBe` ErrUnsupportedGrantType
                oauthErrorDescription resp `shouldBe` Just "password"

            it "InvalidScope maps to 400 with invalid_scope code" $ do
                let err = InvalidScope "unknown scope"
                    (status, resp) = authorizationErrorToResponse err
                status `shouldBe` status400
                oauthErrorCode resp `shouldBe` ErrInvalidScope
                oauthErrorDescription resp `shouldBe` Just "unknown scope"

            it "AccessDenied maps to 403 with access_denied code" $ do
                let err = AccessDenied "User rejected"
                    (status, resp) = authorizationErrorToResponse err
                status `shouldBe` status403
                oauthErrorCode resp `shouldBe` ErrAccessDenied
                oauthErrorDescription resp `shouldBe` Just "User rejected"

            it "ExpiredCode maps to 400 with invalid_grant code" $ do
                let (status, resp) = authorizationErrorToResponse ExpiredCode
                status `shouldBe` status400
                oauthErrorCode resp `shouldBe` ErrInvalidGrant
                oauthErrorDescription resp `shouldBe` Just "Authorization code has expired"

            it "InvalidRedirectUri maps to 400 with invalid_request code" $ do
                let (status, resp) = authorizationErrorToResponse InvalidRedirectUri
                status `shouldBe` status400
                oauthErrorCode resp `shouldBe` ErrInvalidRequest
                oauthErrorDescription resp `shouldBe` Just "Invalid redirect_uri"

            it "PKCEVerificationFailed maps to 400 with invalid_grant code" $ do
                let (status, resp) = authorizationErrorToResponse PKCEVerificationFailed
                status `shouldBe` status400
                oauthErrorCode resp `shouldBe` ErrInvalidGrant
                oauthErrorDescription resp `shouldBe` Just "PKCE verification failed"

        describe "constructors" $ do
            it "InvalidRequest can be pattern matched" $ do
                let err = InvalidRequest "test"
                case err of
                    InvalidRequest msg -> msg `shouldBe` "test"
                    _ -> expectationFailure "Pattern match failed"

            it "InvalidClient can be pattern matched" $ do
                let err = InvalidClient "test"
                case err of
                    InvalidClient msg -> msg `shouldBe` "test"
                    _ -> expectationFailure "Pattern match failed"

            it "InvalidGrant can be pattern matched" $ do
                let err = InvalidGrant "test"
                case err of
                    InvalidGrant msg -> msg `shouldBe` "test"
                    _ -> expectationFailure "Pattern match failed"

            it "UnauthorizedClient can be pattern matched" $ do
                let err = UnauthorizedClient "test"
                case err of
                    UnauthorizedClient msg -> msg `shouldBe` "test"
                    _ -> expectationFailure "Pattern match failed"

            it "UnsupportedGrantType can be pattern matched" $ do
                let err = UnsupportedGrantType "test"
                case err of
                    UnsupportedGrantType msg -> msg `shouldBe` "test"
                    _ -> expectationFailure "Pattern match failed"

            it "InvalidScope can be pattern matched" $ do
                let err = InvalidScope "test"
                case err of
                    InvalidScope msg -> msg `shouldBe` "test"
                    _ -> expectationFailure "Pattern match failed"

            it "AccessDenied can be pattern matched" $ do
                let err = AccessDenied "test"
                case err of
                    AccessDenied msg -> msg `shouldBe` "test"
                    _ -> expectationFailure "Pattern match failed"

            it "ExpiredCode can be pattern matched" $ do
                ExpiredCode `shouldBe` ExpiredCode

            it "InvalidRedirectUri can be pattern matched" $ do
                InvalidRedirectUri `shouldBe` InvalidRedirectUri

            it "PKCEVerificationFailed can be pattern matched" $ do
                PKCEVerificationFailed `shouldBe` PKCEVerificationFailed

    describe "AuthorizationError with ADT payloads (Phase F.3)" $ do
        describe "InvalidRequestReason" $ do
            it "MissingParameter with TokenParamCode constructs correctly" $ do
                let reason = MissingParameter TokenParamCode
                    err = InvalidRequest reason
                case err of
                    InvalidRequest (MissingParameter param) -> param `shouldBe` TokenParamCode
                    _ -> expectationFailure "Pattern match failed"

            it "InvalidParameterFormat with details constructs correctly" $ do
                let reason = InvalidParameterFormat TokenParamCodeVerifier
                    err = InvalidRequest reason
                case err of
                    InvalidRequest (InvalidParameterFormat param) -> param `shouldBe` TokenParamCodeVerifier
                    _ -> expectationFailure "Pattern match failed"

            it "MalformedRequest constructs correctly" $ do
                let reason = MalformedRequest
                    err = InvalidRequest reason
                case err of
                    InvalidRequest MalformedRequest -> return ()
                    _ -> expectationFailure "Pattern match failed"

        describe "InvalidClientReason" $ do
            it "ClientNotFound constructs correctly" $ do
                let reason = ClientNotFound testClientId
                    err = InvalidClient reason
                case err of
                    InvalidClient (ClientNotFound cid) -> cid `shouldBe` testClientId
                    _ -> expectationFailure "Pattern match failed"

            it "InvalidClientCredentials constructs correctly" $ do
                let reason = InvalidClientCredentials
                    err = InvalidClient reason
                case err of
                    InvalidClient InvalidClientCredentials -> return ()
                    _ -> expectationFailure "Pattern match failed"

            it "ClientSecretMismatch constructs correctly" $ do
                let reason = ClientSecretMismatch
                    err = InvalidClient reason
                case err of
                    InvalidClient ClientSecretMismatch -> return ()
                    _ -> expectationFailure "Pattern match failed"

        describe "InvalidGrantReason" $ do
            it "CodeNotFound constructs correctly" $ do
                let codeId = unsafeAuthCodeId "code_123"
                    reason = CodeNotFound codeId
                    err = InvalidGrant reason
                case err of
                    InvalidGrant (CodeNotFound aid) -> aid `shouldBe` codeId
                    _ -> expectationFailure "Pattern match failed"

            it "CodeExpired constructs correctly" $ do
                let codeId = unsafeAuthCodeId "code_123"
                    reason = CodeExpired codeId
                    err = InvalidGrant reason
                case err of
                    InvalidGrant (CodeExpired aid) -> aid `shouldBe` codeId
                    _ -> expectationFailure "Pattern match failed"

            it "CodeAlreadyUsed constructs correctly" $ do
                let codeId = unsafeAuthCodeId "code_123"
                    reason = CodeAlreadyUsed codeId
                    err = InvalidGrant reason
                case err of
                    InvalidGrant (CodeAlreadyUsed aid) -> aid `shouldBe` codeId
                    _ -> expectationFailure "Pattern match failed"

            it "RefreshTokenNotFound constructs correctly" $ do
                let rtId = unsafeRefreshTokenId "rt_123"
                    reason = RefreshTokenNotFound rtId
                    err = InvalidGrant reason
                case err of
                    InvalidGrant (RefreshTokenNotFound rid) -> rid `shouldBe` rtId
                    _ -> expectationFailure "Pattern match failed"

            it "RefreshTokenExpired constructs correctly" $ do
                let rtId = unsafeRefreshTokenId "rt_123"
                    reason = RefreshTokenExpired rtId
                    err = InvalidGrant reason
                case err of
                    InvalidGrant (RefreshTokenExpired rid) -> rid `shouldBe` rtId
                    _ -> expectationFailure "Pattern match failed"

            it "RefreshTokenRevoked constructs correctly" $ do
                let rtId = unsafeRefreshTokenId "rt_123"
                    reason = RefreshTokenRevoked rtId
                    err = InvalidGrant reason
                case err of
                    InvalidGrant (RefreshTokenRevoked rid) -> rid `shouldBe` rtId
                    _ -> expectationFailure "Pattern match failed"

        describe "UnauthorizedClientReason" $ do
            it "GrantTypeNotAllowed constructs correctly" $ do
                let reason = GrantTypeNotAllowed OAuthAuthorizationCode
                    err = UnauthorizedClient reason
                case err of
                    UnauthorizedClient (GrantTypeNotAllowed gt) -> gt `shouldBe` OAuthAuthorizationCode
                    _ -> expectationFailure "Pattern match failed"

            it "ScopeNotAllowed constructs correctly" $ do
                let reason = ScopeNotAllowed testScope
                    err = UnauthorizedClient reason
                case err of
                    UnauthorizedClient (ScopeNotAllowed s) -> s `shouldBe` testScope
                    _ -> expectationFailure "Pattern match failed"

            it "RedirectUriNotRegistered constructs correctly" $ do
                let reason = RedirectUriNotRegistered testRedirectUri
                    err = UnauthorizedClient reason
                case err of
                    UnauthorizedClient (RedirectUriNotRegistered uri) -> uri `shouldBe` testRedirectUri
                    _ -> expectationFailure "Pattern match failed"

        describe "UnsupportedGrantTypeReason" $ do
            it "UnknownGrantType constructs correctly" $ do
                let reason = UnknownGrantType "password"
                    err = UnsupportedGrantType reason
                case err of
                    UnsupportedGrantType (UnknownGrantType gt) -> gt `shouldBe` "password"
                    _ -> expectationFailure "Pattern match failed"

            it "GrantTypeDisabled constructs correctly" $ do
                let reason = GrantTypeDisabled OAuthClientCredentials
                    err = UnsupportedGrantType reason
                case err of
                    UnsupportedGrantType (GrantTypeDisabled gt) -> gt `shouldBe` OAuthClientCredentials
                    _ -> expectationFailure "Pattern match failed"

        describe "InvalidScopeReason" $ do
            it "UnknownScope constructs correctly" $ do
                let reason = UnknownScope "undefined_scope"
                    err = InvalidScope reason
                case err of
                    InvalidScope (UnknownScope s) -> s `shouldBe` "undefined_scope"
                    _ -> expectationFailure "Pattern match failed"

            it "ScopeNotPermitted constructs correctly" $ do
                let reason = ScopeNotPermitted testScope
                    err = InvalidScope reason
                case err of
                    InvalidScope (ScopeNotPermitted s) -> s `shouldBe` testScope
                    _ -> expectationFailure "Pattern match failed"

        describe "AccessDeniedReason" $ do
            it "UserDenied constructs correctly" $ do
                let reason = UserDenied
                    err = AccessDenied reason
                case err of
                    AccessDenied UserDenied -> return ()
                    _ -> expectationFailure "Pattern match failed"

            it "ResourceOwnerDenied constructs correctly" $ do
                let reason = ResourceOwnerDenied
                    err = AccessDenied reason
                case err of
                    AccessDenied ResourceOwnerDenied -> return ()
                    _ -> expectationFailure "Pattern match failed"

            it "ConsentRequired constructs correctly" $ do
                let reason = ConsentRequired
                    err = AccessDenied reason
                case err of
                    AccessDenied ConsentRequired -> return ()
                    _ -> expectationFailure "Pattern match failed"

        describe "renderAuthorizationError" $ do
            it "renders InvalidRequest with MissingParameter" $ do
                let err = InvalidRequest (MissingParameter TokenParamCode)
                    rendered = renderAuthorizationError err
                T.unpack rendered `shouldContain` "code"
                T.unpack rendered `shouldContain` "missing"

            it "renders InvalidClient with ClientNotFound" $ do
                let err = InvalidClient (ClientNotFound testClientId)
                    rendered = renderAuthorizationError err
                T.unpack rendered `shouldContain` "test_client_123"
                T.unpack rendered `shouldContain` "not found"

            it "renders InvalidGrant with CodeExpired" $ do
                let codeId = unsafeAuthCodeId "code_123"
                    err = InvalidGrant (CodeExpired codeId)
                    rendered = renderAuthorizationError err
                T.unpack rendered `shouldContain` "code_123"
                T.unpack rendered `shouldContain` "expired"

            it "renders UnauthorizedClient with GrantTypeNotAllowed" $ do
                let err = UnauthorizedClient (GrantTypeNotAllowed OAuthAuthorizationCode)
                    rendered = renderAuthorizationError err
                T.unpack rendered `shouldContain` "authorization_code"
                T.unpack rendered `shouldContain` "not allowed"

            it "renders UnsupportedGrantType with UnknownGrantType" $ do
                let err = UnsupportedGrantType (UnknownGrantType "password")
                    rendered = renderAuthorizationError err
                T.unpack rendered `shouldContain` "password"
                T.unpack rendered `shouldContain` "unknown"

            it "renders InvalidScope with ScopeNotPermitted" $ do
                let err = InvalidScope (ScopeNotPermitted testScope)
                    rendered = renderAuthorizationError err
                T.unpack rendered `shouldContain` "read"
                T.unpack rendered `shouldContain` "not permitted"

            it "renders AccessDenied with UserDenied" $ do
                let err = AccessDenied UserDenied
                    rendered = renderAuthorizationError err
                T.unpack rendered `shouldContain` "user"
                T.unpack rendered `shouldContain` "denied"

    describe "LoginFlowError" $ do
        it "CookiesRequired can be constructed" $ do
            CookiesRequired `shouldBe` CookiesRequired

        it "SessionCookieMismatch can be constructed" $ do
            SessionCookieMismatch `shouldBe` SessionCookieMismatch

        it "SessionNotFound can be constructed" $ do
            let err = SessionNotFound testSessionId
            case err of
                SessionNotFound sid -> sid `shouldBe` testSessionId
                _ -> expectationFailure "Pattern match failed"

        it "SessionExpired can be constructed" $ do
            let err = SessionExpired testSessionId
            case err of
                SessionExpired sid -> sid `shouldBe` testSessionId
                _ -> expectationFailure "Pattern match failed"

        it "has Eq instance" $ do
            CookiesRequired `shouldBe` CookiesRequired
            SessionCookieMismatch `shouldBe` SessionCookieMismatch
            let sid = testSessionId
            SessionNotFound sid `shouldBe` SessionNotFound sid
            SessionExpired sid `shouldBe` SessionExpired sid

        it "has Show instance" $ do
            show CookiesRequired `shouldContain` "CookiesRequired"
            show SessionCookieMismatch `shouldContain` "SessionCookieMismatch"
            show (SessionNotFound testSessionId) `shouldContain` "SessionNotFound"
            show (SessionExpired testSessionId) `shouldContain` "SessionExpired"
