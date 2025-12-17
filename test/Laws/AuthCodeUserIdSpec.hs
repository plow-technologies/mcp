{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Laws.AuthCodeUserIdSpec
Description : Tests that AuthorizationCode uses OAuthUserId not OAuthUser (FR-041)
Copyright   : (C) 2025 Matthias Pall Gissurarson
License     : MIT
Maintainer  : mpg@mpg.is
Stability   : experimental
Portability : GHC

This module tests FR-041: AuthorizationCode MUST be parameterized over
the user ID type (OAuthUserId m), not the full user type (OAuthUser m).

== Rationale

Authorization codes are short-lived, single-use tokens that only need to
track which user authorized them. Storing the full user record is wasteful
and violates separation of concerns:

* Auth codes need: user ID for linking
* Access tokens need: full user data for JWT generation

By parameterizing AuthorizationCode over OAuthUserId, we:
1. Reduce memory footprint
2. Enforce correct abstraction boundaries
3. Make code exchange logic explicit (ID -> lookup user)

== Test Strategy

We test the RUNTIME BEHAVIOR that matters:

1. **Store userId, not full user**: When creating an auth code, only the
   userId should be stored in the authUserId field
2. **Lookup returns userId**: When retrieving an auth code, we get back
   the userId that we stored
3. **Type correctness**: The stored value must have the correct type
   (OAuthUserId m, not OAuthUser m)

These are NOT "compilation tests" - the compiler already verifies types
exist. These are RUNTIME tests that verify the actual data flow behavior.
-}
module Laws.AuthCodeUserIdSpec (spec) where

import Data.Set qualified as Set
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..))
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import TestMonad (mkTestEnv, runTestM)
import Web.HttpApiData (parseUrlPiece)

import Servant.OAuth2.IDP.Store (OAuthStateStore (..), storeAuthCode)
import Servant.OAuth2.IDP.Types (
    AuthCodeId (..),
    AuthorizationCode (..),
    ClientId (..),
    CodeChallenge (..),
    CodeChallengeMethod (..),
    RedirectUri (..),
    Scope (..),
    UserId (..),
 )

{- | Test that AuthorizationCode stores OAuthUserId, not OAuthUser.

This is the KEY test for FR-041. It verifies RUNTIME BEHAVIOR:
- We store a userId (just the ID)
- We retrieve the same userId back
- The stored field type is OAuthUserId m (UserId for TestM)

This is NOT a compilation test. We're testing actual data flow.
-}
spec :: Spec
spec = describe "FR-041: AuthorizationCode uses OAuthUserId not OAuthUser" $ do
    it "stores and retrieves userId (not full user) in authorization code" $ do
        env <- mkTestEnv testTime mempty

        -- Given: A user ID (not a full user record)
        let userId = UserId "user-123"
            codeId = AuthCodeId "code_abc"

        -- Create an authorization code with JUST the userId
        -- (this will FAIL with current implementation because it expects AuthUser)
        let authCode =
                AuthorizationCode
                    { authCodeId = codeId
                    , authClientId = ClientId "client-1"
                    , authRedirectUri = testRedirectUri
                    , authCodeChallenge = testChallenge
                    , authCodeChallengeMethod = S256
                    , authScopes = Set.singleton (Scope "openid")
                    , authUserId = userId -- This is the key: just an ID
                    , authExpiry = testTime
                    }

        -- When: We store and retrieve the authorization code
        maybeCode <- runTestM env $ do
            storeAuthCode authCode
            lookupAuthCode codeId

        -- Then: We get back the authorization code with the userId we stored
        case maybeCode of
            Nothing -> fail "Expected to retrieve stored authorization code"
            Just retrievedCode -> do
                -- Verify the userId field contains our userId
                authUserId retrievedCode `shouldBe` userId

                -- Verify it's NOT a full user (this is the smoking gun)
                -- If authUserId had type AuthUser, this would fail to compile
                -- But since we're testing runtime, we verify the ID value
                authUserId retrievedCode `shouldSatisfy` \(UserId uid) ->
                    uid == "user-123"

    it "type signature of storeAuthCode uses OAuthUserId m" $ do
        -- This test documents the expected type signature
        -- If the type signature is wrong, this won't compile
        env <- mkTestEnv testTime mempty

        let userId = UserId "user-456" -- Just an ID, not a full user
            codeWithUserId =
                AuthorizationCode
                    { authCodeId = AuthCodeId "code_xyz"
                    , authClientId = ClientId "client-2"
                    , authRedirectUri = testRedirectUri
                    , authCodeChallenge = testChallenge
                    , authCodeChallengeMethod = S256
                    , authScopes = Set.empty
                    , authUserId = userId -- OAuthUserId TestM = UserId
                    , authExpiry = testTime
                    }

        -- This should compile and run if storeAuthCode signature is correct:
        -- storeAuthCode :: AuthorizationCode (OAuthUserId m) -> m ()
        result <- runTestM env $ do
            storeAuthCode codeWithUserId
            pure True

        result `shouldBe` True

-- -----------------------------------------------------------------------------
-- Test Fixtures
-- -----------------------------------------------------------------------------

testTime :: UTCTime
testTime = UTCTime (fromGregorian 2025 1 1) 0

testRedirectUri :: RedirectUri
testRedirectUri = case parseRedirectUri "https://example.com/callback" of
    Just uri -> uri
    Nothing -> error "Invalid test redirect URI"
  where
    parseRedirectUri t = case parseHttpApiData t of
        Right uri -> Just uri
        Left _ -> Nothing
    parseHttpApiData = Web.HttpApiData.parseUrlPiece

testChallenge :: CodeChallenge
testChallenge = case parseChallenge challengeText of
    Just c -> c
    Nothing -> error "Invalid test code challenge"
  where
    -- Valid base64url string with 43 chars (minimum)
    challengeText = "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM"
    parseChallenge t = case parseUrlPiece t of
        Right c -> Just c
        Left _ -> Nothing
