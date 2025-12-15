{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : Laws.OAuthUserTypeSpec
Description : Tests for OAuthUser/OAuthUserId associated types
Copyright   : (C) 2025 Matthias Pall Gissurarson
License     : MIT
Maintainer  : mpg@mpg.is
Stability   : experimental
Portability : GHC

This module tests that the OAuthStateStore typeclass has the required
OAuthUser and OAuthUserId associated types with correct signatures.
-}
module Laws.OAuthUserTypeSpec (spec) where

import Data.Proxy (Proxy (..))
import Data.Typeable (typeRep)
import Test.Hspec (Spec, describe, it, shouldBe)
import TestMonad (TestM)

import MCP.Server.OAuth.Store (OAuthStateStore (OAuthUser, OAuthUserId))
import MCP.Server.OAuth.Types (AuthUser, UserId)

{- | Test that OAuthUser and OAuthUserId associated types exist.

This test verifies that:
1. OAuthStateStore has OAuthUser associated type
2. OAuthStateStore has OAuthUserId associated type
3. For TestM, OAuthUser TestM is AuthUser
4. For TestM, OAuthUserId TestM is UserId
-}
spec :: Spec
spec = describe "OAuthStateStore associated types" $ do
    it "has OAuthUser associated type for TestM" $ do
        let userType = typeRep (Proxy @(OAuthUser TestM))
            expectedType = typeRep (Proxy @AuthUser)
        userType `shouldBe` expectedType

    it "has OAuthUserId associated type for TestM" $ do
        let userIdType = typeRep (Proxy @(OAuthUserId TestM))
            expectedType = typeRep (Proxy @UserId)
        userIdType `shouldBe` expectedType
