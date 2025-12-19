{-# LANGUAGE OverloadedStrings #-}

module MCP.Server.HTTP.AppEnvSpec (spec) where

import Control.Monad.IO.Class (liftIO)
import Data.Functor.Contravariant (contramap)
import Data.IORef (modifyIORef, newIORef, readIORef)
import Plow.Logging (IOTracer (..), Tracer (..))
import Servant.OAuth2.IDP.Auth.Backend (mkUsername)
import Servant.OAuth2.IDP.Trace (OAuthTrace (..), OperationResult (..))
import Test.Hspec

import MCP.Trace.HTTP (HTTPTrace (..))

spec :: Spec
spec = do
    describe "AppEnv" $ do
        describe "envOAuthTracer" $ do
            it "should route OAuthTrace events through HTTPTrace using contramap HTTPOAuth" $ do
                -- Track captured traces
                capturedRef <- newIORef []

                -- Create HTTPTrace tracer that captures events
                let httpTracer = IOTracer $ Tracer $ \trace ->
                        liftIO $ modifyIORef capturedRef (trace :)

                -- Create OAuthTrace tracer using contramap (this is what we're testing)
                let oauthTracer = contramap HTTPOAuth httpTracer

                -- Create test user with smart constructor
                let testUser = case mkUsername "testuser" of
                        Just u -> u
                        Nothing -> error "Test fixture: invalid username"

                -- Emit an OAuthTrace event
                let oauthEvent = TraceLoginAttempt testUser Success
                case oauthTracer of
                    IOTracer (Tracer f) -> f oauthEvent

                -- Verify the event was wrapped in HTTPOAuth and captured
                captured <- readIORef capturedRef
                captured `shouldBe` [HTTPOAuth oauthEvent]
