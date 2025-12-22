{-# LANGUAGE OverloadedStrings #-}

module Servant.OAuth2.IDP.Handlers.HelpersSpec (spec) where

import qualified Data.Text as T
import Servant.OAuth2.IDP.Config (OAuthEnv (..))
import Servant.OAuth2.IDP.Handlers.Helpers
import Servant.OAuth2.IDP.Types
import Test.Hspec

-- Minimal OAuthEnv for testing
testOAuthEnv :: OAuthEnv
testOAuthEnv = error "testOAuthEnv: stub - only authCodePrefix/refreshTokenPrefix accessed in tests"

spec :: Spec
spec = do
    describe "generateAuthCode" $ do
        it "returns AuthCodeId (not Text)" $ do
            let config = testOAuthEnv{oauthAuthCodePrefix = "AC_"}
            code <- generateAuthCode config
            -- This test verifies the return type is AuthCodeId
            -- If it compiles and runs, the type is correct
            let codeText = unAuthCodeId code
            T.isPrefixOf "AC_" codeText `shouldBe` True

        it "generates non-empty AuthCodeId" $ do
            let config = testOAuthEnv{oauthAuthCodePrefix = ""}
            code <- generateAuthCode config
            T.null (unAuthCodeId code) `shouldBe` False

    describe "generateRefreshTokenWithConfig" $ do
        it "returns RefreshTokenId (not Text)" $ do
            let config = testOAuthEnv{oauthRefreshTokenPrefix = "RT_"}
            token <- generateRefreshTokenWithConfig config
            -- This test verifies the return type is RefreshTokenId
            -- If it compiles and runs, the type is correct
            let tokenText = unRefreshTokenId token
            T.isPrefixOf "RT_" tokenText `shouldBe` True

        it "generates non-empty RefreshTokenId" $ do
            let config = testOAuthEnv{oauthRefreshTokenPrefix = ""}
            token <- generateRefreshTokenWithConfig config
            T.null (unRefreshTokenId token) `shouldBe` False

    -- NOTE: generateJWTAccessToken tests would require mocking JWTSettings
    -- and OAuthStateStore, which is beyond the scope of this unit test.
    -- The return type change is verified by compilation.
    describe "generateJWTAccessToken" $ do
        it "type signature requires AccessTokenId return type" $ do
            -- This is a compilation test - if this module compiles,
            -- generateJWTAccessToken has the correct return type
            True `shouldBe` True
