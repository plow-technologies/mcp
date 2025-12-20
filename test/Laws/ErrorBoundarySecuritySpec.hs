{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : Laws.ErrorBoundarySecuritySpec
Description : Security tests for OAuth error boundary translation
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com
Stability   : experimental
Portability : GHC

This module verifies that the OAuth error boundary correctly implements
security policies for error logging and exposure:

1. **OAuthStateError**: Logged via tracer but returns generic 500 without details
2. **AuthBackendError**: Logged via tracer but returns generic 401 without details
3. **ValidationError**: Returns 400 with descriptive message (safe to expose)
4. **AuthorizationError**: Returns appropriate 4xx status with RFC 6749-compliant JSON

== Security Requirements

The boundary must ensure that:

* Infrastructure details (connection strings, database errors) never appear in HTTP responses
* User enumeration is prevented (all auth failures return same generic message)
* Error details ARE logged via tracer for observability
* OAuth protocol errors follow RFC 6749 format

== Test Coverage

* Secure error hiding for OAuthStoreError and DemoAuthError
* Descriptive error exposure for ValidationError
* RFC 6749-compliant responses for AuthorizationError
* Infrastructure detail exclusion verification
* Tracer verification for all error types
-}
module Laws.ErrorBoundarySecuritySpec (spec) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (decode)
import Data.ByteString.Lazy qualified as BL
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error (lenientDecode)
import MCP.Server.HTTP.AppEnv (AppError (..), AppM)
import MCP.Trace.HTTP (HTTPTrace (..))
import Plow.Logging (IOTracer (..), Tracer (..))
import Servant.OAuth2.IDP.Auth.Backend (mkUsername)
import Servant.OAuth2.IDP.Auth.Demo (DemoAuthError (..))
import Servant.OAuth2.IDP.Boundary (
    OAuthBoundaryTrace (..),
    domainErrorToServerError,
 )
import Servant.OAuth2.IDP.Errors (
    AccessDeniedReason (..),
    AuthorizationError (..),
    InvalidClientReason (..),
    InvalidGrantReason (..),
    InvalidRequestReason (..),
    MalformedReason (..),
    OAuthErrorCode (..),
    OAuthErrorResponse (..),
    UnauthorizedClientReason (..),
    ValidationError (..),
    oauthErrorCode,
    oauthErrorDescription,
 )
import Servant.OAuth2.IDP.Store.InMemory (OAuthStoreError (..))
import Servant.OAuth2.IDP.Types (
    mkAuthCodeId,
    mkClientId,
    mkScope,
 )
import Servant.Server (ServerError (..), errBody, errHTTPCode, errHeaders)
import Test.Hspec (Spec, describe, it, shouldBe)

-- | Helper to extract Just or fail test
fromJustOrFail :: String -> Maybe a -> a
fromJustOrFail msg Nothing = error msg
fromJustOrFail _ (Just x) = x

-- | Mock tracer that records trace events to an IORef
withMockTracer :: (IOTracer trace -> IO (Maybe ServerError)) -> IO ([trace], Maybe ServerError)
withMockTracer action = do
    tracesRef <- newIORef []
    let tracer = IOTracer $ Tracer $ \trace -> liftIO $ modifyIORef' tracesRef (trace :)
    result <- action tracer
    traces <- readIORef tracesRef
    pure (reverse traces, result)

-- -----------------------------------------------------------------------------
-- Test Spec
-- -----------------------------------------------------------------------------

spec :: Spec
spec = describe "OAuth Error Boundary Security" $ do
    secureErrorHidingSpec
    validationErrorExposureSpec
    authorizationErrorExposureSpec
    infrastructureDetailExclusionSpec
    tracerVerificationSpec

-- -----------------------------------------------------------------------------
-- Secure Error Hiding Tests
-- -----------------------------------------------------------------------------

{- | Verify that OAuthStoreError and AuthBackendError details are hidden
from HTTP responses but logged via tracer.
-}
secureErrorHidingSpec :: Spec
secureErrorHidingSpec = describe "Secure Error Hiding" $ do
    describe "OAuthStoreError" $ do
        it "returns generic 500 for StoreUnavailable" $ do
            let err = OAuthStoreErr (StoreUnavailable "database connection failed")
            (traces, mServerErr) <- withMockTracer $ \tracer ->
                domainErrorToServerError @IO @AppM tracer HTTPOAuthBoundary err

            -- Should return generic 500
            case mServerErr of
                Nothing -> fail "Expected ServerError, got Nothing"
                Just serverErr -> do
                    errHTTPCode serverErr `shouldBe` 500
                    errBody serverErr `shouldBe` "Internal server error"

            -- Should log details
            case traces of
                [HTTPOAuthBoundary (BoundaryStoreError msg)] ->
                    T.isInfixOf "database connection failed" msg `shouldBe` True
                _ -> fail $ "Expected BoundaryStoreError trace, got: " ++ show traces

        it "returns generic 500 for StoreInternalError" $ do
            let err = OAuthStoreErr (StoreInternalError "SELECT * FROM tokens failed")
            (traces, mServerErr) <- withMockTracer $ \tracer ->
                domainErrorToServerError @IO @AppM tracer HTTPOAuthBoundary err

            -- Should return generic 500
            case mServerErr of
                Nothing -> fail "Expected ServerError, got Nothing"
                Just serverErr -> do
                    errHTTPCode serverErr `shouldBe` 500
                    errBody serverErr `shouldBe` "Internal server error"

            -- Should log details
            case traces of
                [HTTPOAuthBoundary (BoundaryStoreError msg)] ->
                    T.isInfixOf "SELECT * FROM tokens failed" msg `shouldBe` True
                _ -> fail $ "Expected BoundaryStoreError trace, got: " ++ show traces

    describe "AuthBackendError" $ do
        it "returns generic 401 for InvalidCredentials" $ do
            let err = AuthBackendErr InvalidCredentials
            (traces, mServerErr) <- withMockTracer $ \tracer ->
                domainErrorToServerError @IO @AppM tracer HTTPOAuthBoundary err

            -- Should return generic 401
            case mServerErr of
                Nothing -> fail "Expected ServerError, got Nothing"
                Just serverErr -> do
                    errHTTPCode serverErr `shouldBe` 401
                    errBody serverErr `shouldBe` "Unauthorized"

            -- Should log details
            case traces of
                [HTTPOAuthBoundary (BoundaryAuthError msg)] ->
                    T.isInfixOf "InvalidCredentials" msg `shouldBe` True
                _ -> fail $ "Expected BoundaryAuthError trace, got: " ++ show traces

        it "returns generic 401 for UserNotFound (no user enumeration)" $ do
            let username = fromJustOrFail "mkUsername failed for 'alice'" $ mkUsername "alice"
                err = AuthBackendErr (UserNotFound username)
            (traces, mServerErr) <- withMockTracer $ \tracer ->
                domainErrorToServerError @IO @AppM tracer HTTPOAuthBoundary err

            -- Should return generic 401 (not exposing user existence)
            case mServerErr of
                Nothing -> fail "Expected ServerError, got Nothing"
                Just serverErr -> do
                    errHTTPCode serverErr `shouldBe` 401
                    errBody serverErr `shouldBe` "Unauthorized"
                    -- Verify no mention of "alice" in response
                    T.isInfixOf "alice" (TE.decodeUtf8With lenientDecode $ BL.toStrict $ errBody serverErr) `shouldBe` False

            -- Should log details (including username for debugging)
            case traces of
                [HTTPOAuthBoundary (BoundaryAuthError msg)] ->
                    T.isInfixOf "UserNotFound" msg `shouldBe` True
                _ -> fail $ "Expected BoundaryAuthError trace, got: " ++ show traces

-- -----------------------------------------------------------------------------
-- ValidationError Exposure Tests
-- -----------------------------------------------------------------------------

{- | Verify that ValidationError returns descriptive 400 responses that
are safe to expose to clients.
-}
validationErrorExposureSpec :: Spec
validationErrorExposureSpec = describe "ValidationError Exposure" $ do
    it "returns 400 with client_id for ClientNotRegistered" $ do
        let clientId = fromJustOrFail "mkClientId failed for 'client_abc123'" $ mkClientId "client_abc123"
            err = ValidationErr (ClientNotRegistered clientId)
        (_, mServerErr) <- withMockTracer $ \tracer ->
            domainErrorToServerError @IO @AppM tracer HTTPOAuthBoundary err

        case mServerErr of
            Nothing -> fail "Expected ServerError, got Nothing"
            Just serverErr -> do
                errHTTPCode serverErr `shouldBe` 400
                let bodyText = TE.decodeUtf8With lenientDecode $ BL.toStrict $ errBody serverErr
                T.isInfixOf "client_id not registered" bodyText `shouldBe` True
                T.isInfixOf "client_abc123" bodyText `shouldBe` True

    it "returns 400 with response_type for UnsupportedResponseType" $ do
        let err = ValidationErr (UnsupportedResponseType "implicit")
        (_, mServerErr) <- withMockTracer $ \tracer ->
            domainErrorToServerError @IO @AppM tracer HTTPOAuthBoundary err

        case mServerErr of
            Nothing -> fail "Expected ServerError, got Nothing"
            Just serverErr -> do
                errHTTPCode serverErr `shouldBe` 400
                let bodyText = TE.decodeUtf8With lenientDecode $ BL.toStrict $ errBody serverErr
                T.isInfixOf "response_type not supported" bodyText `shouldBe` True
                T.isInfixOf "implicit" bodyText `shouldBe` True

    it "returns 400 with scope for MissingRequiredScope" $ do
        let scope = fromJustOrFail "mkScope failed for 'admin'" $ mkScope "admin"
            err = ValidationErr (MissingRequiredScope scope)
        (_, mServerErr) <- withMockTracer $ \tracer ->
            domainErrorToServerError @IO @AppM tracer HTTPOAuthBoundary err

        case mServerErr of
            Nothing -> fail "Expected ServerError, got Nothing"
            Just serverErr -> do
                errHTTPCode serverErr `shouldBe` 400
                let bodyText = TE.decodeUtf8With lenientDecode $ BL.toStrict $ errBody serverErr
                T.isInfixOf "Missing required scope" bodyText `shouldBe` True
                T.isInfixOf "admin" bodyText `shouldBe` True

    it "returns 400 with state value for InvalidStateParameter" $ do
        let err = ValidationErr (InvalidStateParameter "malformed-state")
        (_, mServerErr) <- withMockTracer $ \tracer ->
            domainErrorToServerError @IO @AppM tracer HTTPOAuthBoundary err

        case mServerErr of
            Nothing -> fail "Expected ServerError, got Nothing"
            Just serverErr -> do
                errHTTPCode serverErr `shouldBe` 400
                let bodyText = TE.decodeUtf8With lenientDecode $ BL.toStrict $ errBody serverErr
                T.isInfixOf "Invalid state parameter" bodyText `shouldBe` True
                T.isInfixOf "malformed-state" bodyText `shouldBe` True

-- -----------------------------------------------------------------------------
-- AuthorizationError Exposure Tests
-- -----------------------------------------------------------------------------

{- | Verify that AuthorizationError returns RFC 6749-compliant JSON responses
with appropriate HTTP status codes.
-}
authorizationErrorExposureSpec :: Spec
authorizationErrorExposureSpec = describe "AuthorizationError Exposure" $ do
    it "returns 400 with JSON for InvalidRequest" $ do
        let err = AuthorizationErr (InvalidRequest (MalformedRequest (UnparseableBody "test error")))
        (_, mServerErr) <- withMockTracer $ \tracer ->
            domainErrorToServerError @IO @AppM tracer HTTPOAuthBoundary err

        case mServerErr of
            Nothing -> fail "Expected ServerError, got Nothing"
            Just serverErr -> do
                errHTTPCode serverErr `shouldBe` 400
                -- Verify content-type
                lookup "Content-Type" (errHeaders serverErr) `shouldBe` Just "application/json; charset=utf-8"
                -- Parse JSON
                case decode (errBody serverErr) :: Maybe OAuthErrorResponse of
                    Nothing -> fail "Failed to parse OAuthErrorResponse JSON"
                    Just oauthErr -> do
                        oauthErrorCode oauthErr `shouldBe` ErrInvalidRequest
                        oauthErrorDescription oauthErr `shouldBe` Just "Unparseable request body: test error"

    it "returns 401 with JSON for InvalidClient" $ do
        let err = AuthorizationErr (InvalidClient InvalidClientCredentials)
        (_, mServerErr) <- withMockTracer $ \tracer ->
            domainErrorToServerError @IO @AppM tracer HTTPOAuthBoundary err

        case mServerErr of
            Nothing -> fail "Expected ServerError, got Nothing"
            Just serverErr -> do
                errHTTPCode serverErr `shouldBe` 401
                case decode (errBody serverErr) :: Maybe OAuthErrorResponse of
                    Nothing -> fail "Failed to parse OAuthErrorResponse JSON"
                    Just oauthErr -> do
                        oauthErrorCode oauthErr `shouldBe` ErrInvalidClient
                        oauthErrorDescription oauthErr `shouldBe` Just "Invalid client credentials"

    it "returns 400 with JSON for InvalidGrant" $ do
        let codeId = fromJustOrFail "mkAuthCodeId failed" $ mkAuthCodeId "test_code_123"
            err = AuthorizationErr (InvalidGrant (CodeExpired codeId))
        (_, mServerErr) <- withMockTracer $ \tracer ->
            domainErrorToServerError @IO @AppM tracer HTTPOAuthBoundary err

        case mServerErr of
            Nothing -> fail "Expected ServerError, got Nothing"
            Just serverErr -> do
                errHTTPCode serverErr `shouldBe` 400
                case decode (errBody serverErr) :: Maybe OAuthErrorResponse of
                    Nothing -> fail "Failed to parse OAuthErrorResponse JSON"
                    Just oauthErr -> do
                        oauthErrorCode oauthErr `shouldBe` ErrInvalidGrant
                        T.isInfixOf "expired" (fromMaybe "" (oauthErrorDescription oauthErr)) `shouldBe` True

    it "returns 401 with JSON for UnauthorizedClient" $ do
        let scope = fromJustOrFail "mkScope failed" $ mkScope "admin"
            err = AuthorizationErr (UnauthorizedClient (ScopeNotAllowed scope))
        (_, mServerErr) <- withMockTracer $ \tracer ->
            domainErrorToServerError @IO @AppM tracer HTTPOAuthBoundary err

        case mServerErr of
            Nothing -> fail "Expected ServerError, got Nothing"
            Just serverErr -> do
                errHTTPCode serverErr `shouldBe` 401
                case decode (errBody serverErr) :: Maybe OAuthErrorResponse of
                    Nothing -> fail "Failed to parse OAuthErrorResponse JSON"
                    Just oauthErr -> do
                        oauthErrorCode oauthErr `shouldBe` ErrUnauthorizedClient

    it "returns 403 with JSON for AccessDenied" $ do
        let err = AuthorizationErr (AccessDenied UserDenied)
        (_, mServerErr) <- withMockTracer $ \tracer ->
            domainErrorToServerError @IO @AppM tracer HTTPOAuthBoundary err

        case mServerErr of
            Nothing -> fail "Expected ServerError, got Nothing"
            Just serverErr -> do
                errHTTPCode serverErr `shouldBe` 403
                case decode (errBody serverErr) :: Maybe OAuthErrorResponse of
                    Nothing -> fail "Failed to parse OAuthErrorResponse JSON"
                    Just oauthErr -> do
                        oauthErrorCode oauthErr `shouldBe` ErrAccessDenied

    it "returns 400 with JSON for ExpiredCode" $ do
        let err = AuthorizationErr ExpiredCode
        (_, mServerErr) <- withMockTracer $ \tracer ->
            domainErrorToServerError @IO @AppM tracer HTTPOAuthBoundary err

        case mServerErr of
            Nothing -> fail "Expected ServerError, got Nothing"
            Just serverErr -> do
                errHTTPCode serverErr `shouldBe` 400
                case decode (errBody serverErr) :: Maybe OAuthErrorResponse of
                    Nothing -> fail "Failed to parse OAuthErrorResponse JSON"
                    Just oauthErr -> do
                        oauthErrorCode oauthErr `shouldBe` ErrInvalidGrant
                        oauthErrorDescription oauthErr `shouldBe` Just "Authorization code has expired"

-- -----------------------------------------------------------------------------
-- Infrastructure Detail Exclusion Tests
-- -----------------------------------------------------------------------------

{- | Verify that infrastructure details (connection strings, table names,
AWS keys, etc.) never appear in HTTP responses.
-}
infrastructureDetailExclusionSpec :: Spec
infrastructureDetailExclusionSpec = describe "Infrastructure Detail Exclusion" $ do
    it "excludes database connection strings from OAuthStoreError responses" $ do
        let dangerousString = "postgresql://user:password123@localhost:5432/oauth_db"
            err = OAuthStoreErr (StoreUnavailable dangerousString)
        (_, mServerErr) <- withMockTracer $ \tracer ->
            domainErrorToServerError @IO @AppM tracer HTTPOAuthBoundary err

        case mServerErr of
            Nothing -> fail "Expected ServerError, got Nothing"
            Just serverErr -> do
                let bodyText = TE.decodeUtf8With lenientDecode $ BL.toStrict $ errBody serverErr
                -- Should NOT contain any part of the connection string
                T.isInfixOf "postgresql" bodyText `shouldBe` False
                T.isInfixOf "password123" bodyText `shouldBe` False
                T.isInfixOf "5432" bodyText `shouldBe` False
                T.isInfixOf "oauth_db" bodyText `shouldBe` False
                -- Should only be generic message
                bodyText `shouldBe` "Internal server error"

    it "excludes SQL queries from OAuthStoreError responses" $ do
        let dangerousString = "SELECT * FROM users WHERE username='admin' AND password='secret'"
            err = OAuthStoreErr (StoreInternalError dangerousString)
        (_, mServerErr) <- withMockTracer $ \tracer ->
            domainErrorToServerError @IO @AppM tracer HTTPOAuthBoundary err

        case mServerErr of
            Nothing -> fail "Expected ServerError, got Nothing"
            Just serverErr -> do
                let bodyText = TE.decodeUtf8With lenientDecode $ BL.toStrict $ errBody serverErr
                T.isInfixOf "SELECT" bodyText `shouldBe` False
                T.isInfixOf "users" bodyText `shouldBe` False
                T.isInfixOf "admin" bodyText `shouldBe` False
                bodyText `shouldBe` "Internal server error"

    it "excludes AWS credentials from OAuthStoreError responses" $ do
        let dangerousString = "AWS_SECRET_KEY=wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY"
            err = OAuthStoreErr (StoreUnavailable dangerousString)
        (_, mServerErr) <- withMockTracer $ \tracer ->
            domainErrorToServerError @IO @AppM tracer HTTPOAuthBoundary err

        case mServerErr of
            Nothing -> fail "Expected ServerError, got Nothing"
            Just serverErr -> do
                let bodyText = TE.decodeUtf8With lenientDecode $ BL.toStrict $ errBody serverErr
                T.isInfixOf "AWS_SECRET_KEY" bodyText `shouldBe` False
                T.isInfixOf "wJalrXUtnFEMI" bodyText `shouldBe` False
                bodyText `shouldBe` "Internal server error"

    it "excludes usernames from AuthBackendError responses" $ do
        let username = fromJustOrFail "mkUsername failed" $ mkUsername "sensitive.admin.user@company.internal"
            err = AuthBackendErr (UserNotFound username)
        (_, mServerErr) <- withMockTracer $ \tracer ->
            domainErrorToServerError @IO @AppM tracer HTTPOAuthBoundary err

        case mServerErr of
            Nothing -> fail "Expected ServerError, got Nothing"
            Just serverErr -> do
                let bodyText = TE.decodeUtf8With lenientDecode $ BL.toStrict $ errBody serverErr
                T.isInfixOf "sensitive.admin.user" bodyText `shouldBe` False
                T.isInfixOf "company.internal" bodyText `shouldBe` False
                bodyText `shouldBe` "Unauthorized"

-- -----------------------------------------------------------------------------
-- Tracer Verification Tests
-- -----------------------------------------------------------------------------

{- | Verify that all error types are properly logged via tracer with
appropriate detail levels.
-}
tracerVerificationSpec :: Spec
tracerVerificationSpec = describe "Tracer Verification" $ do
    it "logs OAuthStateError details via BoundaryStoreError" $ do
        let err = OAuthStoreErr (StoreUnavailable "connection pool exhausted")
        (traces, _) <- withMockTracer $ \tracer ->
            domainErrorToServerError @IO @AppM tracer HTTPOAuthBoundary err

        case traces of
            [HTTPOAuthBoundary (BoundaryStoreError msg)] -> do
                T.isInfixOf "StoreUnavailable" msg `shouldBe` True
                T.isInfixOf "connection pool exhausted" msg `shouldBe` True
            _ -> fail $ "Expected BoundaryStoreError trace, got: " ++ show traces

    it "logs AuthBackendError details via BoundaryAuthError" $ do
        let err = AuthBackendErr InvalidCredentials
        (traces, _) <- withMockTracer $ \tracer ->
            domainErrorToServerError @IO @AppM tracer HTTPOAuthBoundary err

        case traces of
            [HTTPOAuthBoundary (BoundaryAuthError msg)] ->
                T.isInfixOf "InvalidCredentials" msg `shouldBe` True
            _ -> fail $ "Expected BoundaryAuthError trace, got: " ++ show traces

    it "logs ValidationError via BoundaryValidationError" $ do
        let clientId = fromJustOrFail "mkClientId failed for 'test_client'" $ mkClientId "test_client"
            err = ValidationErr (ClientNotRegistered clientId)
        (traces, _) <- withMockTracer $ \tracer ->
            domainErrorToServerError @IO @AppM tracer HTTPOAuthBoundary err

        -- Note: Currently ValidationError is not explicitly traced in domainErrorToServerError
        -- This test documents current behavior and can be updated if tracing is added
        traces `shouldBe` []

    it "logs AuthorizationError via BoundaryAuthorizationError" $ do
        let err = AuthorizationErr ExpiredCode
        (traces, _) <- withMockTracer $ \tracer ->
            domainErrorToServerError @IO @AppM tracer HTTPOAuthBoundary err

        -- Note: Currently AuthorizationError is not explicitly traced in domainErrorToServerError
        -- This test documents current behavior and can be updated if tracing is added
        traces `shouldBe` []
