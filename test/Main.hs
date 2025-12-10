{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text (Text)
import MCP.Server.Auth (HashedPassword (..), defaultDemoCredentialStore, mkHashedPassword, validateCredential)
import MCP.Trace.OAuth (OAuthTrace)
import Plow.Logging (IOTracer (..), Tracer (..))
import Test.Hspec

-- | A no-op tracer that discards all trace events
nullIOTracer :: IOTracer a
nullIOTracer = IOTracer (Tracer (\_ -> pure ()))

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "MCP.Server.Auth" $ do
        describe "validateCredential" $ do
            it "validates correct demo credentials" $ do
                result <- validateCredential (nullIOTracer :: IOTracer OAuthTrace) defaultDemoCredentialStore "demo" "demo123"
                result `shouldBe` True

            it "validates correct admin credentials" $ do
                result <- validateCredential (nullIOTracer :: IOTracer OAuthTrace) defaultDemoCredentialStore "admin" "admin456"
                result `shouldBe` True

            it "rejects invalid password for demo user" $ do
                result <- validateCredential (nullIOTracer :: IOTracer OAuthTrace) defaultDemoCredentialStore "demo" "wrongpassword"
                result `shouldBe` False

            it "rejects invalid password for admin user" $ do
                result <- validateCredential (nullIOTracer :: IOTracer OAuthTrace) defaultDemoCredentialStore "admin" "wrongpass"
                result `shouldBe` False

            it "rejects invalid username" $ do
                result <- validateCredential (nullIOTracer :: IOTracer OAuthTrace) defaultDemoCredentialStore "nonexistent" "demo123"
                result `shouldBe` False

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
