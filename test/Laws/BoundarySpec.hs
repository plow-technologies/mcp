{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Laws.BoundarySpec
Description : Property-based tests for Servant boundary instances
Copyright   : (C) 2025 Matthias Pall Gissurarson
License     : MIT
Maintainer  : mpg@mpg.is
Stability   : experimental
Portability : GHC

This module provides round-trip property tests for the FromHttpApiData/ToHttpApiData
instances on OAuth domain types. These instances form the boundary layer between
Servant's URL/query parameter parsing and our type-safe domain model.

== What This Tests

The round-trip property ensures that for any value @x@:

@
parseUrlPiece (toUrlPiece x) === Right x
@

This guarantees:

1. **Serialization is reversible**: No information is lost when converting to text
2. **Parsing is consistent**: The parser accepts what the serializer produces
3. **Type safety boundary**: Values that pass through HTTP remain valid

== Tested Types

=== Identity Newtypes

* 'ClientId' - OAuth client identifier
* 'AuthCodeId' - Authorization code identifier
* 'SessionId' - Session identifier (UUID format)
* 'UserId' - User identifier
* 'RefreshTokenId' - Refresh token identifier

=== Value Newtypes

* 'RedirectUri' - OAuth redirect URI (https:// or http://localhost)
* 'Scope' - OAuth scope value (non-empty, no whitespace)

=== ADTs

* 'CodeChallengeMethod' - PKCE challenge method (S256, Plain)
* 'GrantType' - OAuth grant type (authorization_code, refresh_token, client_credentials)
* 'ResponseType' - OAuth response type (code, token)
* 'ClientAuthMethod' - Client authentication method (none, client_secret_post, client_secret_basic)

== Usage

These tests are automatically included in the main test suite:

@
cabal test
@

To run only the boundary tests:

@
cabal test --test-option="-m" --test-option="Servant Boundary Round-trip"
@
-}
module Laws.BoundarySpec (spec) where

import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary, (===))
import Web.HttpApiData (FromHttpApiData (..), ToHttpApiData (..))

-- Import orphan Arbitrary instances
import Generators ()

-- OAuth domain types
import MCP.Server.OAuth.Types (
    AuthCodeId,
    ClientAuthMethod,
    ClientId,
    CodeChallengeMethod,
    GrantType,
    RedirectUri,
    RefreshTokenId,
    ResponseType,
    Scope,
    SessionId,
    UserId,
 )

-- | Main spec: round-trip property tests for all boundary instances
spec :: Spec
spec = describe "Servant Boundary Round-trip Laws" $ do
    describe "Identity Newtypes" $ do
        identityRoundTrip "ClientId" (undefined :: ClientId)
        identityRoundTrip "AuthCodeId" (undefined :: AuthCodeId)
        identityRoundTrip "SessionId" (undefined :: SessionId)
        identityRoundTrip "UserId" (undefined :: UserId)
        identityRoundTrip "RefreshTokenId" (undefined :: RefreshTokenId)

    describe "Value Newtypes" $ do
        identityRoundTrip "RedirectUri" (undefined :: RedirectUri)
        identityRoundTrip "Scope" (undefined :: Scope)

    describe "ADTs" $ do
        identityRoundTrip "CodeChallengeMethod" (undefined :: CodeChallengeMethod)
        identityRoundTrip "GrantType" (undefined :: GrantType)
        identityRoundTrip "ResponseType" (undefined :: ResponseType)
        identityRoundTrip "ClientAuthMethod" (undefined :: ClientAuthMethod)

{- | Generic round-trip property test for any type with FromHttpApiData/ToHttpApiData

This test verifies the fundamental boundary law:

@
parseUrlPiece . toUrlPiece = Right
@

The test is parameterized by:
- A human-readable type name (for test output)
- A type proxy (to fix the type variable)

Example usage:

@
identityRoundTrip "ClientId" (undefined :: ClientId)
@
-}
identityRoundTrip ::
    forall a.
    (Eq a, Show a, Arbitrary a, FromHttpApiData a, ToHttpApiData a) =>
    String ->
    a ->
    Spec
identityRoundTrip typeName (_ :: a) =
    prop (typeName ++ " round-trip") $ \(x :: a) ->
        parseUrlPiece (toUrlPiece x) === Right x
