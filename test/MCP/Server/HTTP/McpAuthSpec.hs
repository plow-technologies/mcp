{-# LANGUAGE OverloadedStrings #-}

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

import Test.Hspec

spec :: Spec
spec = describe "mcpServerAuth" $ do
    describe "JWT authentication failure handling" $ do
        it "returns 401 with WWW-Authenticate header for BadPassword" $ pending
        it "returns 401 with WWW-Authenticate header for NoSuchUser" $ pending
        it "returns 401 with WWW-Authenticate header for Indefinite" $ pending
        it "processes request successfully for Authenticated user" $ pending
