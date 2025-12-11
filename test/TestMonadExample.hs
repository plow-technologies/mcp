{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : TestMonadExample
Description : Example usage of TestMonad for deterministic OAuth testing
Copyright   : (C) 2025 Matthias Pall Gissurarson
License     : MIT
Maintainer  : mpg@mpg.is

This module demonstrates how to use the TestMonad for writing deterministic
tests for OAuth operations.
-}
module TestMonadExample (spec) where

import Data.Map.Strict qualified as Map
import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, parseTimeOrError)
import Test.Hspec
import TestMonad

-- | Helper to parse time strings
parseTime :: String -> UTCTime
parseTime = parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M:%S"

-- | Hspec test suite demonstrating TestMonad usage
spec :: Spec
spec = describe "TestMonad Examples" $ do
    describe "Credential validation" $ do
        it "validates correct credentials" $ do
            let initialTime = parseTime "2025-01-01 00:00:00"
            env <- mkTestEnv initialTime Map.empty

            result <- runTestM env $ do
                addTestCredential (Username "alice") (mkPlaintextPassword "secret123")
                validateCredentials (Username "alice") (mkPlaintextPassword "secret123")

            result `shouldBe` True

        it "rejects incorrect credentials" $ do
            let initialTime = parseTime "2025-01-01 00:00:00"
            env <- mkTestEnv initialTime Map.empty

            result <- runTestM env $ do
                addTestCredential (Username "alice") (mkPlaintextPassword "secret123")
                validateCredentials (Username "alice") (mkPlaintextPassword "wrongpass")

            result `shouldBe` False

    describe "Time control" $ do
        it "allows setting and advancing time" $ do
            let initialTime = parseTime "2025-01-01 00:00:00"
            env <- mkTestEnv initialTime Map.empty

            (time1, time2, time3) <- runTestM env $ do
                t1 <- currentTime
                advanceTime 3600 -- 1 hour
                t2 <- currentTime
                setTime (parseTime "2025-12-31 23:59:59")
                t3 <- currentTime
                pure (t1, t2, t3)

            time1 `shouldBe` parseTime "2025-01-01 00:00:00"
            time2 `shouldBe` parseTime "2025-01-01 01:00:00"
            time3 `shouldBe` parseTime "2025-12-31 23:59:59"
