{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text (Text)
import MCP.Server.Auth (HashedPassword (..), defaultDemoCredentialStore, mkHashedPassword, validateCredential)
import Test.Hspec
import Trace.FilterSpec qualified as FilterSpec
import Trace.GoldenSpec qualified as GoldenSpec
import Trace.OAuthSpec qualified as OAuthSpec
import Trace.RenderSpec qualified as RenderSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    FilterSpec.spec
    GoldenSpec.spec
    OAuthSpec.spec
    RenderSpec.spec

    describe "MCP.Server.Auth" $ do
        describe "validateCredential" $ do
            it "validates correct demo credentials" $ do
                validateCredential defaultDemoCredentialStore "demo" "demo123" `shouldBe` True

            it "validates correct admin credentials" $ do
                validateCredential defaultDemoCredentialStore "admin" "admin456" `shouldBe` True

            it "rejects invalid password for demo user" $ do
                validateCredential defaultDemoCredentialStore "demo" "wrongpassword" `shouldBe` False

            it "rejects invalid password for admin user" $ do
                validateCredential defaultDemoCredentialStore "admin" "wrongpass" `shouldBe` False

            it "rejects invalid username" $ do
                validateCredential defaultDemoCredentialStore "nonexistent" "demo123" `shouldBe` False

        describe "mkHashedPassword" $ do
            it "produces consistent hashes for same inputs" $ do
                let salt = "test-salt" :: Text
                    password = "test-password" :: Text
                    hash1 = mkHashedPassword salt password
                    hash2 = mkHashedPassword salt password
                unHashedPassword hash1 `shouldBe` unHashedPassword hash2

            it "produces different hashes for different passwords" $ do
                let salt = "test-salt" :: Text
                    hash1 = mkHashedPassword salt "test-password"
                    hash2 = mkHashedPassword salt "different-password"
                unHashedPassword hash1 `shouldNotBe` unHashedPassword hash2

            it "produces different hashes for different salts" $ do
                let password = "test-password" :: Text
                    hash1 = mkHashedPassword "test-salt" password
                    hash2 = mkHashedPassword "different-salt" password
                unHashedPassword hash1 `shouldNotBe` unHashedPassword hash2
