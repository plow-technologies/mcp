{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Laws.AuthCodeFunctorSpec
Description : Property tests for AuthorizationCode Functor laws
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com
Stability   : experimental
Portability : GHC
-}
module Laws.AuthCodeFunctorSpec (spec) where

import Data.Set qualified as Set
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Network.URI qualified
import Test.Hspec
import Test.Hspec.QuickCheck (prop)

import Generators ()
import Servant.OAuth2.IDP.Types (
    AuthorizationCode (..),
    CodeChallengeMethod (..),
    UserId,
    mkCodeChallenge,
 )
import Servant.OAuth2.IDP.Types.Internal (
    unsafeAuthCodeId,
    unsafeClientId,
    unsafeRedirectUri,
    unsafeScope,
    unsafeUserId,
 )

-- | Test that AuthorizationCode is a Functor
spec :: Spec
spec = describe "AuthorizationCode Functor" $ do
    -- Functor law 1: fmap id = id
    prop "fmap id = id (first functor law)" $ \authCode ->
        let authCode' = authCode :: AuthorizationCode UserId
         in fmap id authCode' == authCode'

    -- Functor law 2: fmap (f . g) = fmap f . fmap g
    prop "fmap (f . g) = fmap f . fmap g (second functor law)" $ \authCode ->
        let authCode' = authCode :: AuthorizationCode Int
            f = (* 2) :: Int -> Int
            g = (+ 1) :: Int -> Int
         in fmap (f . g) authCode' == (fmap f . fmap g) authCode'

    -- Practical use case: map userId to a different type
    it "can map UserId to String" $ do
        let authCode = mkTestAuthCode (unsafeUserId "user123")
            mapped = fmap (const "mapped") authCode
        authUserId mapped `shouldBe` ("mapped" :: String)

-- | Helper to create a test AuthorizationCode
mkTestAuthCode :: userId -> AuthorizationCode userId
mkTestAuthCode userId =
    AuthorizationCode
        { authCodeId = unsafeAuthCodeId "code_test123"
        , authClientId = unsafeClientId "client_test"
        , authRedirectUri = testRedirectUri
        , authCodeChallenge = case mkCodeChallenge "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM" of
            Just cc -> cc
            Nothing -> error "Invalid test CodeChallenge"
        , authCodeChallengeMethod = S256
        , authScopes = Set.fromList [unsafeScope "read", unsafeScope "write"]
        , authUserId = userId
        , authExpiry = testTime
        }
  where
    testRedirectUri = case Network.URI.parseURI "https://example.com/callback" of
        Just uri -> unsafeRedirectUri uri
        Nothing -> error "Invalid test URI"
    testTime = case parseTimeM True defaultTimeLocale "%Y-%m-%d" "2025-01-01" of
        Just t -> t
        Nothing -> error "Invalid test time"
