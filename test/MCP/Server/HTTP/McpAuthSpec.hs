{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{- |
Module      : MCP.Server.HTTP.McpAuthSpec
Description : Tests for MCP endpoint JWT authentication
Copyright   : (C) 2025 Matthias Pall Gissurarson
License     : MIT
Maintainer  : mpg@mpg.is
Stability   : experimental
Portability : GHC

Tests that mcpServerAuth properly translates JWT authentication failures
to domain errors (AuthorizationError) which are then translated to 401
responses at the boundary.
-}
module MCP.Server.HTTP.McpAuthSpec (spec) where

import Control.Concurrent.STM (newTVarIO)
import Data.Aeson (Value, object, (.=))
import Data.ByteString.Char8 qualified as BS
import Data.List (isInfixOf)
import Data.Text qualified as T
import Plow.Logging (IOTracer (..), Tracer (..))
import Servant.Auth.Server (AuthResult (..))
import Servant.Server (ServerError (..))
import Servant.Server.Internal.Handler (runHandler)
import Test.Hspec

import MCP.Server (MCPServer (..), MCPServerM, initialServerState)
import MCP.Server.Auth.Demo (AuthUser (..))
import MCP.Server.HTTP (HTTPServerConfig (..), defaultDemoOAuthConfig, defaultProtectedResourceMetadata)
import MCP.Server.HTTP qualified as HTTP
import Servant.OAuth2.IDP.Types (UserId (..))
import MCP.Trace.HTTP (HTTPTrace)
import MCP.Types (Implementation (..), ServerCapabilities (..))

-- | Minimal MCPServer instance for testing (uses default implementations)
instance MCPServer MCPServerM

-- | Minimal test HTTP server configuration
testConfig :: HTTPServerConfig
testConfig =
    HTTPServerConfig
        { httpPort = 8080
        , httpBaseUrl = "http://localhost:8080"
        , httpServerInfo = Implementation "test-server" (Just "1.0.0") ""
        , httpCapabilities = ServerCapabilities Nothing Nothing Nothing Nothing Nothing Nothing
        , httpEnableLogging = False
        , httpOAuthConfig = Just defaultDemoOAuthConfig
        , httpJWK = Nothing
        , httpProtocolVersion = "2025-06-18"
        , httpProtectedResourceMetadata = Just (defaultProtectedResourceMetadata "http://localhost:8080")
        }

-- | Null tracer for tests (discards all traces)
testTracer :: IOTracer HTTPTrace
testTracer = IOTracer (Tracer (\_ -> pure ()))

-- | Test AuthUser for authenticated test cases
testAuthUser :: AuthUser
testAuthUser =
    AuthUser
        { userUserId = UserId "test-user-123"
        , userUserEmail = Just "test@example.com"
        , userUserName = Just "Test User"
        }

-- | Test request value (minimal valid JSON-RPC request)
testRequest :: Value
testRequest =
    object
        [ "jsonrpc" .= ("2.0" :: T.Text)
        , "method" .= ("initialize" :: T.Text)
        , "id" .= (1 :: Int)
        , "params" .= object []
        ]

spec :: Spec
spec = do
    describe "mcpServerAuth" $ do
        describe "JWT authentication failure handling" $ do
            it "returns 401 with WWW-Authenticate header for BadPassword" $ do
                stateTVar <- newTVarIO (initialServerState (httpCapabilities testConfig))
                result <- runHandler $ HTTP.mcpServerAuth testConfig testTracer stateTVar BadPassword testRequest
                case result of
                    Left err -> do
                        -- Check status code is 401
                        show (errHTTPCode err) `shouldBe` "401"
                        -- Check WWW-Authenticate header is present
                        let headers = Servant.Server.errHeaders err
                            hasWWWAuth = any (\(name, _) -> name == "WWW-Authenticate") headers
                        hasWWWAuth `shouldBe` True
                        -- Check header value contains Bearer and resource_metadata
                        let wwwAuthValue = lookup "WWW-Authenticate" headers
                        case wwwAuthValue of
                            Just value -> do
                                let valueStr = BS.unpack value
                                valueStr `shouldSatisfy` \v -> "Bearer" `elem` words v
                                valueStr `shouldSatisfy` \v -> "resource_metadata" `isInfixOf` v
                            Nothing -> expectationFailure "WWW-Authenticate header missing"
                    Right _ -> expectationFailure "Expected ServerError (401), got success"

            it "returns 401 with WWW-Authenticate header for NoSuchUser" $ do
                stateTVar <- newTVarIO (initialServerState (httpCapabilities testConfig))
                result <- runHandler $ HTTP.mcpServerAuth testConfig testTracer stateTVar NoSuchUser testRequest
                case result of
                    Left err -> do
                        -- Check status code is 401
                        show (Servant.Server.errHTTPCode err) `shouldBe` "401"
                        -- Check WWW-Authenticate header is present
                        let headers = Servant.Server.errHeaders err
                            hasWWWAuth = any (\(name, _) -> name == "WWW-Authenticate") headers
                        hasWWWAuth `shouldBe` True
                        -- Check header value contains Bearer and resource_metadata
                        let wwwAuthValue = lookup "WWW-Authenticate" headers
                        case wwwAuthValue of
                            Just value -> do
                                let valueStr = BS.unpack value
                                valueStr `shouldSatisfy` \v -> "Bearer" `elem` words v
                                valueStr `shouldSatisfy` \v -> "resource_metadata" `isInfixOf` v
                            Nothing -> expectationFailure "WWW-Authenticate header missing"
                    Right _ -> expectationFailure "Expected ServerError (401), got success"

            it "returns 401 with WWW-Authenticate header for Indefinite" $ do
                stateTVar <- newTVarIO (initialServerState (httpCapabilities testConfig))
                result <- runHandler $ HTTP.mcpServerAuth testConfig testTracer stateTVar Indefinite testRequest
                case result of
                    Left err -> do
                        -- Check status code is 401
                        show (Servant.Server.errHTTPCode err) `shouldBe` "401"
                        -- Check WWW-Authenticate header is present
                        let headers = Servant.Server.errHeaders err
                            hasWWWAuth = any (\(name, _) -> name == "WWW-Authenticate") headers
                        hasWWWAuth `shouldBe` True
                        -- Check header value contains Bearer and resource_metadata
                        let wwwAuthValue = lookup "WWW-Authenticate" headers
                        case wwwAuthValue of
                            Just value -> do
                                let valueStr = BS.unpack value
                                valueStr `shouldSatisfy` \v -> "Bearer" `elem` words v
                                valueStr `shouldSatisfy` \v -> "resource_metadata" `isInfixOf` v
                            Nothing -> expectationFailure "WWW-Authenticate header missing"
                    Right _ -> expectationFailure "Expected ServerError (401), got success"

            it "processes request successfully for Authenticated user" $ do
                stateTVar <- newTVarIO (initialServerState (httpCapabilities testConfig))
                result <- runHandler $ HTTP.mcpServerAuth testConfig testTracer stateTVar (Authenticated testAuthUser) testRequest
                case result of
                    Left err -> expectationFailure $ "Expected success, got error: " ++ show err
                    Right value -> do
                        -- Just verify we got a value back (the actual response content is tested elsewhere)
                        value `shouldSatisfy` const True

    describe "demoMcpApp" $ do
        it "creates a non-bottom WAI Application" $ do
            -- This tests that demoMcpApp can be called and returns a WAI Application
            -- We verify it's non-bottom by evaluating to WHNF with seq
            app <- HTTP.demoMcpApp
            app `seq` True `shouldBe` True
