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
                            { mcpAutoApproveAuth = False
                            , mcpDemoUserIdTemplate = "user-{id}"
                            , mcpDemoEmailDomain = "example.com"
                            , mcpAuthorizationSuccessTemplate = "<html>Success</html>"
                            }
                mcpAutoApproveAuth config `shouldBe` False

            it "creates config with demo user ID template" $ do
                let template = "demo-user-{id}" :: Text
                    config =
                        MCPOAuthConfig
                            { mcpAutoApproveAuth = True
                            , mcpDemoUserIdTemplate = template
                            , mcpDemoEmailDomain = "test.org"
                            , mcpAuthorizationSuccessTemplate = ""
                            }
                mcpDemoUserIdTemplate config `shouldBe` template

            it "creates config with demo email domain" $ do
                let domain = "demo.example.com" :: Text
                    config =
                        MCPOAuthConfig
                            { mcpAutoApproveAuth = True
                            , mcpDemoUserIdTemplate = "user-{id}"
                            , mcpDemoEmailDomain = domain
                            , mcpAuthorizationSuccessTemplate = ""
                            }
                mcpDemoEmailDomain config `shouldBe` domain

            it "creates config with authorization success template" $ do
                let template = "<html><body>Authorization successful!</body></html>" :: Text
                    config =
                        MCPOAuthConfig
                            { mcpAutoApproveAuth = False
                            , mcpDemoUserIdTemplate = "user-{id}"
                            , mcpDemoEmailDomain = "example.com"
                            , mcpAuthorizationSuccessTemplate = template
                            }
                mcpAuthorizationSuccessTemplate config `shouldBe` template
