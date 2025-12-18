{-# LANGUAGE OverloadedStrings #-}

module Servant.OAuth2.IDP.TypesSpec (spec) where

import Data.Maybe (isJust)
import Servant.OAuth2.IDP.Types (mkRedirectUri)
import Test.Hspec

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
