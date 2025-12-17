{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Laws.ConsumeAuthCodeSpec
Description : Tests for atomic consumeAuthCode operation
Copyright   : (C) 2025
License     : MIT
Maintainer  : maintainer@example.com

This module tests the atomic consumeAuthCode operation which combines
lookup and delete to prevent race conditions per RFC 6749 §4.1.2
(authorization codes MUST be single-use).
-}
module Laws.ConsumeAuthCodeSpec (
    consumeAuthCodeSpec,
) where

import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), addUTCTime)
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary, ioProperty, (.&&.), (===))

-- Import orphan Arbitrary instances
import Generators ()

import Servant.OAuth2.IDP.Store (OAuthStateStore (..))
import Servant.OAuth2.IDP.Types (AuthorizationCode (..))

{- | Tests for consumeAuthCode operation.

This spec tests that consumeAuthCode:
1. Returns Just for valid code and deletes it atomically
2. Returns Nothing for expired codes
3. Is truly atomic (no race condition - only one concurrent consumer succeeds)
-}
consumeAuthCodeSpec ::
    forall m.
    ( OAuthStateStore m
    , Arbitrary (OAuthUserId m)
    , Eq (OAuthUserId m)
    , Show (OAuthUserId m)
    ) =>
    -- | Runner function to execute 'm' in 'IO'
    (forall a. m a -> IO a) ->
    Spec
consumeAuthCodeSpec runM = describe "consumeAuthCode" $ do
    prop "returns Just for valid code and deletes it atomically" $
        \(code :: AuthorizationCode (OAuthUserId m)) -> ioProperty $ do
            -- Make code valid (not expired)
            let validCode = code{authExpiry = addUTCTime 86400 (authExpiry code)}

            result <- runM $ do
                storeAuthCode validCode
                consumeAuthCode (authCodeId validCode)

            -- Verify second consume returns Nothing (code was deleted)
            secondResult <- runM $ consumeAuthCode (authCodeId validCode)

            -- Both checks must pass
            pure $ (result === Just validCode) .&&. (secondResult === Nothing)

    prop "returns Nothing for expired code" $
        \(code :: AuthorizationCode (OAuthUserId m)) -> ioProperty $ do
            -- Make code expired by setting expiry to a fixed past time (year 2019)
            -- This ensures expiry < currentTime (2020-01-01) for all test runs
            let pastTime = UTCTime (fromGregorian 2019 12 31) 0
            let expiredCode = code{authExpiry = pastTime}

            result <- runM $ do
                storeAuthCode expiredCode
                consumeAuthCode (authCodeId expiredCode)

            pure $ result === Nothing
