{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.ByteArray qualified as BA
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)

-- Old Auth module (for legacy tests - DEPRECATED API)
import MCP.Server.Auth qualified as Auth
import Servant.OAuth2.IDP.Auth.Demo qualified as Auth

-- New typeclass modules (for law tests)
import Servant.OAuth2.IDP.Auth.Backend (Username (..), mkPlaintextPassword)

import Test.Hspec

-- Test monad with controlled time
import TestMonad (TestM, addTestCredential, mkTestEnv, runTestM)

-- Law tests
import Laws.AuthBackendAssociatedTypesSpec qualified as AuthBackendAssociatedTypesSpec
import Laws.AuthBackendSignatureSpec qualified as AuthBackendSignatureSpec
import Laws.AuthBackendSpec (authBackendKnownCredentials, authBackendLaws)
import Laws.AuthCodeFunctorSpec qualified as AuthCodeFunctorSpec
import Laws.AuthCodeUserIdSpec qualified as AuthCodeUserIdSpec
import Laws.BoundarySpec qualified as BoundarySpec
import Laws.ConsumeAuthCodeSpec (consumeAuthCodeConcurrencySpec, consumeAuthCodeSpec)
import Laws.ErrorBoundarySecuritySpec qualified as ErrorBoundarySecuritySpec
import Laws.OAuthStateStoreSpec (oauthStateStoreLaws)
import Laws.OAuthUserTypeSpec qualified as OAuthUserTypeSpec

-- Existing specs
import Trace.FilterSpec qualified as FilterSpec
import Trace.GoldenSpec qualified as GoldenSpec
import Trace.OAuthSpec qualified as OAuthSpec
import Trace.RenderSpec qualified as RenderSpec

-- Unit tests
import MCP.Server.OAuth.TypesSpec qualified as TypesSpec

-- OAuth App tests
import MCP.Server.OAuth.AppSpec qualified as AppSpec

-- HTTP endpoint tests
import MCP.Server.HTTP.McpAuthSpec qualified as McpAuthSpec

-- Functional tests
import Functional.OAuthFlowSpec qualified as OAuthFlowSpec

main :: IO ()
main = hspec spec

{- | Fixed base time for all tests (year 2020, start of the test data range)
The Arbitrary instance for UTCTime generates times within 10 years from 2020-01-01,
so times range from 2020 to 2030. We set currentTime to 2020-01-01 so that ALL
generated timestamps appear to be in the future. The law tests then manipulate
timestamps relative to the generated values (e.g., subtract 1 day to make expired).
With currentTime at the minimum and expiry durations measured in hours, this ensures:
- Round-trip tests (subtract 60s) remain valid (not expired)
- Expiry tests (subtract 1 day) become expired
-}
baseTestTime :: UTCTime
baseTestTime = case parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S %Z" "2020-01-01 00:00:00 UTC" of
    Just t -> t
    Nothing -> error "Failed to parse base test time"

-- | Runner for TestM monad (provides both OAuthStateStore and AuthBackend)
runTestM' :: TestM a -> IO a
runTestM' action = do
    env <- mkTestEnv baseTestTime Map.empty
    runTestM env action

-- | Runner for TestM with demo credentials pre-loaded
runTestMWithDemoCreds :: TestM a -> IO a
runTestMWithDemoCreds action = do
    env <- mkTestEnv baseTestTime Map.empty
    runTestM env $ do
        -- Add demo credentials
        addTestCredential (Username "demo") (mkPlaintextPassword "demo123")
        addTestCredential (Username "admin") (mkPlaintextPassword "admin456")
        action

spec :: Spec
spec = do
    FilterSpec.spec
    GoldenSpec.spec
    OAuthSpec.spec
    RenderSpec.spec

    -- Unit tests
    TypesSpec.spec

    -- OAuth App tests
    AppSpec.spec

    -- HTTP endpoint tests
    McpAuthSpec.spec

    -- Boundary layer tests (Servant FromHttpApiData/ToHttpApiData)
    BoundarySpec.spec

    -- Error boundary security tests
    ErrorBoundarySecuritySpec.spec

    -- Functional tests
    describe "Functional" OAuthFlowSpec.spec

    -- Typeclass law tests (using TestM with controlled time)
    describe "TestM OAuthStateStore" $ do
        oauthStateStoreLaws runTestM'
        consumeAuthCodeSpec runTestM'
        consumeAuthCodeConcurrencySpec
        OAuthUserTypeSpec.spec
        AuthCodeUserIdSpec.spec

    -- Functor laws
    AuthCodeFunctorSpec.spec

    describe "TestM AuthBackend" $ do
        AuthBackendAssociatedTypesSpec.spec
        AuthBackendSignatureSpec.spec
        authBackendLaws runTestMWithDemoCreds
        authBackendKnownCredentials
            runTestMWithDemoCreds
            (Username "demo")
            (mkPlaintextPassword "demo123")
            (mkPlaintextPassword "wrongpassword")

    -- Existing tests for old Auth module
    describe "MCP.Server.Auth" $ do
        describe "validateCredential" $ do
            it "validates correct demo credentials" $ do
                Auth.validateCredential Auth.defaultDemoCredentialStore "demo" "demo123" `shouldBe` True

            it "validates correct admin credentials" $ do
                Auth.validateCredential Auth.defaultDemoCredentialStore "admin" "admin456" `shouldBe` True

            it "rejects invalid password for demo user" $ do
                Auth.validateCredential Auth.defaultDemoCredentialStore "demo" "wrongpassword" `shouldBe` False

            it "rejects invalid password for admin user" $ do
                Auth.validateCredential Auth.defaultDemoCredentialStore "admin" "wrongpass" `shouldBe` False

            it "rejects invalid username" $ do
                Auth.validateCredential Auth.defaultDemoCredentialStore "nonexistent" "demo123" `shouldBe` False

        describe "mkHashedPassword" $ do
            it "produces consistent hashes for same inputs" $ do
                let saltBytes = BA.convert (TE.encodeUtf8 ("test-salt" :: Text)) :: BA.ScrubbedBytes
                    salt = Auth.Salt saltBytes
                    password = Auth.mkPlaintextPassword "test-password"
                    hash1 = Auth.mkHashedPassword salt password
                    hash2 = Auth.mkHashedPassword salt password
                (hash1 == hash2) `shouldBe` True

            it "produces different hashes for different passwords" $ do
                let saltBytes = BA.convert (TE.encodeUtf8 ("test-salt" :: Text)) :: BA.ScrubbedBytes
                    salt = Auth.Salt saltBytes
                    hash1 = Auth.mkHashedPassword salt (Auth.mkPlaintextPassword "test-password")
                    hash2 = Auth.mkHashedPassword salt (Auth.mkPlaintextPassword "different-password")
                (hash1 /= hash2) `shouldBe` True

            it "produces different hashes for different salts" $ do
                let password = Auth.mkPlaintextPassword "test-password"
                    saltBytes1 = BA.convert (TE.encodeUtf8 ("test-salt" :: Text)) :: BA.ScrubbedBytes
                    saltBytes2 = BA.convert (TE.encodeUtf8 ("different-salt" :: Text)) :: BA.ScrubbedBytes
                    hash1 = Auth.mkHashedPassword (Auth.Salt saltBytes1) password
                    hash2 = Auth.mkHashedPassword (Auth.Salt saltBytes2) password
                (hash1 /= hash2) `shouldBe` True
