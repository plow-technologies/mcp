{-# LANGUAGE OverloadedStrings #-}

module Servant.OAuth2.IDP.TypesSpec (spec) where

import Data.Either (isLeft)
import Data.Maybe (isJust)
import Data.Set qualified as Set
import Data.Text (Text)
import Servant.OAuth2.IDP.Types (Scope (..), mkRedirectUri, parseScopeList, serializeScopeSet)
import Test.Hspec
import Web.HttpApiData (parseUrlPiece, toUrlPiece)

spec :: Spec
spec = do
    describe "mkRedirectUri" $ do
        context "FR-050: Exact hostname matching" $ do
            it "rejects substring localhost bypass in query param" $
                mkRedirectUri "http://evil.com/callback?localhost=bypass" `shouldBe` Nothing

            it "rejects substring localhost bypass in path" $
                mkRedirectUri "http://evil.com/localhost/callback" `shouldBe` Nothing

            it "rejects localhost as subdomain" $
                mkRedirectUri "http://localhost.evil.com/callback" `shouldBe` Nothing

        context "Allowlist acceptance" $ do
            it "accepts http://localhost:3000/callback" $
                mkRedirectUri "http://localhost:3000/callback" `shouldSatisfy` isJust

            it "accepts http://127.0.0.1:8080/callback" $
                mkRedirectUri "http://127.0.0.1:8080/callback" `shouldSatisfy` isJust

            it "accepts http://[::1]:3000/callback" $
                mkRedirectUri "http://[::1]:3000/callback" `shouldSatisfy` isJust

        context "HTTPS always accepted (non-private IPs)" $ do
            it "accepts https://example.com/callback" $
                mkRedirectUri "https://example.com/callback" `shouldSatisfy` isJust

        context "HTTP rejected for non-localhost" $ do
            it "rejects http://example.com/callback (non-localhost, non-HTTPS)" $
                mkRedirectUri "http://example.com/callback" `shouldBe` Nothing

        context "FR-051: Private IP range blocking" $ do
            it "rejects https://10.0.0.1/callback (Class A private)" $
                mkRedirectUri "https://10.0.0.1/callback" `shouldBe` Nothing

            it "rejects https://10.255.255.255/callback (Class A private boundary)" $
                mkRedirectUri "https://10.255.255.255/callback" `shouldBe` Nothing

            it "rejects https://172.16.0.1/callback (Class B private start)" $
                mkRedirectUri "https://172.16.0.1/callback" `shouldBe` Nothing

            it "rejects https://172.31.255.255/callback (Class B private end)" $
                mkRedirectUri "https://172.31.255.255/callback" `shouldBe` Nothing

            it "rejects https://192.168.1.1/callback (Class C private)" $
                mkRedirectUri "https://192.168.1.1/callback" `shouldBe` Nothing

            it "rejects https://169.254.169.254/latest/meta-data (cloud metadata SSRF)" $
                mkRedirectUri "https://169.254.169.254/latest/meta-data" `shouldBe` Nothing

            it "accepts https://example.com/callback (public domain)" $
                mkRedirectUri "https://example.com/callback" `shouldSatisfy` isJust

            it "accepts https://8.8.8.8/callback (public IP)" $
                mkRedirectUri "https://8.8.8.8/callback" `shouldSatisfy` isJust

            it "accepts http://localhost:3000/callback (localhost still allowed)" $
                mkRedirectUri "http://localhost:3000/callback" `shouldSatisfy` isJust

        context "FR-051: Security bypass vectors" $ do
            it "rejects overflow bypass 172.288.0.1" $
                mkRedirectUri "https://172.288.0.1/callback" `shouldBe` Nothing

            it "rejects decimal IP notation 167772161 (10.0.0.1)" $
                mkRedirectUri "https://167772161/callback" `shouldBe` Nothing

            it "rejects hex IP notation 0xa000001" $
                mkRedirectUri "https://0xa000001/callback" `shouldBe` Nothing

            it "rejects octal IP notation 012.0.0.1" $
                mkRedirectUri "https://012.0.0.1/callback" `shouldBe` Nothing

            -- Boundary tests for public IPs (should ACCEPT)
            it "accepts public IP 172.32.0.0 (just outside Class B private)" $
                mkRedirectUri "https://172.32.0.0/callback" `shouldSatisfy` isJust

            it "accepts public IP 172.15.255.255 (just below Class B private)" $
                mkRedirectUri "https://172.15.255.255/callback" `shouldSatisfy` isJust

        context "FR-051: IPv6 private range blocking" $ do
            it "rejects fe80::1 (IPv6 link-local)" $
                mkRedirectUri "https://[fe80::1]/callback" `shouldBe` Nothing

            it "rejects fd00::1 (IPv6 unique local)" $
                mkRedirectUri "https://[fd00::1]/callback" `shouldBe` Nothing

            it "rejects fc00::1 (IPv6 unique local alternative)" $
                mkRedirectUri "https://[fc00::1]/callback" `shouldBe` Nothing

    describe "FR-060: Scope parsing and serialization" $ do
        context "Scope newtype single value" $ do
            it "accepts valid single scope via FromHttpApiData" $
                parseUrlPiece "openid" `shouldBe` Right (Scope "openid")

            it "rejects empty scope" $
                (parseUrlPiece "" :: Either Text Scope) `shouldSatisfy` isLeft

            it "rejects scope with whitespace" $
                (parseUrlPiece "open id" :: Either Text Scope) `shouldSatisfy` isLeft

            it "round-trips through ToHttpApiData" $
                let scope = Scope "profile"
                 in parseUrlPiece (toUrlPiece scope) `shouldBe` Right scope

        context "Space-delimited scope list parsing (RFC 6749 Section 3.3)" $ do
            it "parses space-delimited scopes into Set" $
                parseScopeList "openid profile email"
                    `shouldBe` Just (Set.fromList [Scope "openid", Scope "profile", Scope "email"])

            it "handles single scope" $
                parseScopeList "openid" `shouldBe` Just (Set.fromList [Scope "openid"])

            it "handles empty string" $
                parseScopeList "" `shouldBe` Just Set.empty

            it "filters out empty scopes from consecutive spaces" $
                parseScopeList "openid  profile" `shouldBe` Just (Set.fromList [Scope "openid", Scope "profile"])

            it "trims whitespace around scopes" $
                parseScopeList "  openid   profile  " `shouldBe` Just (Set.fromList [Scope "openid", Scope "profile"])

        context "Set Scope serialization to space-delimited string" $ do
            it "serializes Set to space-delimited string" $
                let scopes = Set.fromList [Scope "email", Scope "openid", Scope "profile"]
                 in serializeScopeSet scopes `shouldSatisfy` (\s -> s `elem` ["email openid profile", "email profile openid", "openid email profile", "openid profile email", "profile email openid", "profile openid email"])

            it "handles empty Set" $
                serializeScopeSet Set.empty `shouldBe` ""

            it "handles single scope Set" $
                serializeScopeSet (Set.fromList [Scope "openid"]) `shouldBe` "openid"
