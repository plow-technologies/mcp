{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : MCP.Server.AuthSpec
Description : Tests for MCP.Server.Auth module
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com
Stability   : experimental
Portability : GHC

Tests for MCP-specific OAuth configuration types.
-}
module MCP.Server.AuthSpec (spec) where

import Data.Text (Text)
import Test.Hspec

import MCP.Server.Auth

spec :: Spec
spec = do
    describe "MCPOAuthConfig" $ do
        describe "record construction" $ do
            it "creates config with autoApproveAuth disabled" $ do
                let config =
                        MCPOAuthConfig
                            { autoApproveAuth = False
                            , demoUserIdTemplate = "user-{id}"
                            , demoEmailDomain = "example.com"
                            , authorizationSuccessTemplate = "<html>Success</html>"
                            }
                autoApproveAuth config `shouldBe` False

            it "creates config with demo user ID template" $ do
                let template = "demo-user-{id}" :: Text
                    config =
                        MCPOAuthConfig
                            { autoApproveAuth = True
                            , demoUserIdTemplate = template
                            , demoEmailDomain = "test.org"
                            , authorizationSuccessTemplate = ""
                            }
                demoUserIdTemplate config `shouldBe` template

            it "creates config with demo email domain" $ do
                let domain = "demo.example.com" :: Text
                    config =
                        MCPOAuthConfig
                            { autoApproveAuth = True
                            , demoUserIdTemplate = "user-{id}"
                            , demoEmailDomain = domain
                            , authorizationSuccessTemplate = ""
                            }
                demoEmailDomain config `shouldBe` domain

            it "creates config with authorization success template" $ do
                let template = "<html><body>Authorization successful!</body></html>" :: Text
                    config =
                        MCPOAuthConfig
                            { autoApproveAuth = False
                            , demoUserIdTemplate = "user-{id}"
                            , demoEmailDomain = "example.com"
                            , authorizationSuccessTemplate = template
                            }
                authorizationSuccessTemplate config `shouldBe` template
