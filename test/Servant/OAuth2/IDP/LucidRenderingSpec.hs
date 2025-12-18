{-# LANGUAGE OverloadedStrings #-}

module Servant.OAuth2.IDP.LucidRenderingSpec (spec) where

import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Lucid (Html, renderText, toHtml)
import Test.Hspec

import Servant.OAuth2.IDP.Handlers.HTML (
    ErrorPage (..),
    LoginPage (..),
 )

-- | Test suite for Lucid-based HTML rendering
spec :: Spec
spec = do
    describe "LoginPage ToHtml instance" $ do
        it "renders a login page with client name and scopes" $ do
            let page =
                    LoginPage
                        { loginClientName = "Test Client"
                        , loginScopes = "mcp:read mcp:write"
                        , loginResource = Nothing
                        , loginSessionId = "test-session-123"
                        }
            let html = TL.toStrict $ renderText (toHtml page)

            -- Verify essential HTML structure
            html `shouldSatisfy` T.isInfixOf "<!DOCTYPE HTML>"
            html `shouldSatisfy` T.isInfixOf "<html>"
            html `shouldSatisfy` T.isInfixOf "</html>"

        it "includes the client name in the rendered HTML" $ do
            let page =
                    LoginPage
                        { loginClientName = "MyApp"
                        , loginScopes = "mcp:read"
                        , loginResource = Nothing
                        , loginSessionId = "session-456"
                        }
            let html = TL.toStrict $ renderText (toHtml page)

            html `shouldSatisfy` T.isInfixOf "MyApp"

        it "includes scope descriptions in the rendered HTML" $ do
            let page =
                    LoginPage
                        { loginClientName = "Test"
                        , loginScopes = "mcp:read mcp:write"
                        , loginResource = Nothing
                        , loginSessionId = "session-789"
                        }
            let html = TL.toStrict $ renderText (toHtml page)

            -- Should contain human-readable scope descriptions
            html `shouldSatisfy` T.isInfixOf "Read MCP resources"
            html `shouldSatisfy` T.isInfixOf "Write MCP resources"

        it "includes session ID as hidden field" $ do
            let page =
                    LoginPage
                        { loginClientName = "Test"
                        , loginScopes = "mcp:read"
                        , loginResource = Nothing
                        , loginSessionId = "hidden-session-id"
                        }
            let html = TL.toStrict $ renderText (toHtml page)

            html `shouldSatisfy` T.isInfixOf "hidden-session-id"
            html `shouldSatisfy` T.isInfixOf "name=\"session_id\""

        it "includes optional resource parameter when present" $ do
            let page =
                    LoginPage
                        { loginClientName = "Test"
                        , loginScopes = "mcp:read"
                        , loginResource = Just "https://api.example.com"
                        , loginSessionId = "session-with-resource"
                        }
            let html = TL.toStrict $ renderText (toHtml page)

            html `shouldSatisfy` T.isInfixOf "https://api.example.com"

        it "escapes HTML special characters in client name" $ do
            let page =
                    LoginPage
                        { loginClientName = "<script>alert('xss')</script>"
                        , loginScopes = "mcp:read"
                        , loginResource = Nothing
                        , loginSessionId = "session-xss-test"
                        }
            let html = TL.toStrict $ renderText (toHtml page)

            -- Lucid should auto-escape, so literal <script> should not appear
            html `shouldNotSatisfy` T.isInfixOf "<script>alert('xss')</script>"
            -- But the escaped version should be present
            html `shouldSatisfy` T.isInfixOf "&lt;script&gt;"

    describe "ErrorPage ToHtml instance" $ do
        it "renders an error page with title and message" $ do
            let page = ErrorPage "Invalid Request" "The client_id is missing"
            let html = TL.toStrict $ renderText (toHtml page)

            html `shouldSatisfy` T.isInfixOf "Invalid Request"
            html `shouldSatisfy` T.isInfixOf "The client_id is missing"

        it "includes DOCTYPE and html tags" $ do
            let page = ErrorPage "Error" "Something went wrong"
            let html = TL.toStrict $ renderText (toHtml page)

            html `shouldSatisfy` T.isInfixOf "<!DOCTYPE HTML>"
            html `shouldSatisfy` T.isInfixOf "<html>"
            html `shouldSatisfy` T.isInfixOf "</html>"

        it "escapes HTML special characters in error messages" $ do
            let page = ErrorPage "Error" "<script>malicious()</script>"
            let html = TL.toStrict $ renderText (toHtml page)

            html `shouldNotSatisfy` T.isInfixOf "<script>malicious()</script>"
            html `shouldSatisfy` T.isInfixOf "&lt;script&gt;"

    describe "HTML content type integration" $ do
        it "can render LoginPage to Html type" $ do
            let page =
                    LoginPage
                        { loginClientName = "Integration Test"
                        , loginScopes = "mcp:read"
                        , loginResource = Nothing
                        , loginSessionId = "int-session"
                        }
            -- This test verifies the type signature works
            let _htmlValue :: Html () = toHtml page
            -- If it compiles and runs, the integration works
            True `shouldBe` True

        it "can render ErrorPage to Html type" $ do
            let page = ErrorPage "Test Error" "Test message"
            let _htmlValue :: Html () = toHtml page
            True `shouldBe` True
