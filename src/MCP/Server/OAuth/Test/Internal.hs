{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : MCP.Server.OAuth.Test.Internal
Description : Internal test utilities for OAuth testing
Copyright   : (C) 2025 Matthias Pall Gissurarson
License     : MIT
Maintainer  : mpg@mpg.is
Stability   : experimental
Portability : GHC

This module provides test utilities for OAuth flows, particularly PKCE generation
and client registration helpers. These utilities are designed for use with hspec-wai
functional tests.

== Usage

@
import MCP.Server.OAuth.Test.Internal

spec :: Spec
spec = do
  describe "OAuth PKCE flow" $ do
    it "accepts valid PKCE challenge" $ do
      (verifier, challenge) <- generatePKCE
      -- Use verifier and challenge in test...
@

== IMPORTANT

These utilities are for testing only. They generate cryptographically secure
random values but should NEVER be used in production code.
-}
module MCP.Server.OAuth.Test.Internal (
    -- * PKCE Utilities
    generatePKCE,

    -- * HTTP Response Utilities
    extractSessionCookie,
    extractCodeFromRedirect,
) where

import Crypto.Hash (hashWith)
import Crypto.Hash.Algorithms (SHA256 (..))
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import Data.ByteString.Base64.URL qualified as B64URL
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Network.HTTP.Types (hLocation)
import Network.URI (URI (..), parseURI, uriQuery)
import Network.Wai.Test (SResponse, simpleHeaders)
import System.Random (newStdGen, randomRs)

{- | Generate PKCE code verifier and challenge pair.

Returns (verifier, challenge) where:
- verifier: 128-char string using unreserved chars (RFC 7636)
- challenge: S256 hash of verifier (SHA256, base64url-encoded)

The generated values satisfy the server's validation rules:
- Verifier: 43-128 chars, unreserved charset (A-Z, a-z, 0-9, -, ., _, ~)
- Challenge: 43 chars (SHA256 output), base64url charset (A-Z, a-z, 0-9, -, _)

This implementation matches the production code in MCP.Server.Auth to ensure
compatibility.

== Example

>>> (verifier, challenge) <- generatePKCE
>>> T.length verifier
128
>>> T.length challenge
43
-}
generatePKCE :: IO (Text, Text)
generatePKCE = do
    verifier <- generateCodeVerifier
    let challenge = generateCodeChallenge verifier
    return (verifier, challenge)

{- | Generate a cryptographically secure code verifier for PKCE
Produces a 128-character string using unreserved characters per RFC 7636
-}
generateCodeVerifier :: IO Text
generateCodeVerifier = do
    gen <- newStdGen
    let chars = ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9'] ++ "-._~"
    let verifier = take 128 $ randomRs (0, length chars - 1) gen
    return $ T.pack $ map (chars !!) verifier

{- | Generate code challenge from verifier using SHA256 (S256 method)
Uses base64url encoding without padding per RFC 7636
-}
generateCodeChallenge :: Text -> Text
generateCodeChallenge verifier =
    let verifierBytes = TE.encodeUtf8 verifier
        challengeHash = hashWith SHA256 verifierBytes
        challengeBytes = convert challengeHash :: ByteString
     in TE.decodeUtf8 $ B64URL.encodeUnpadded challengeBytes

{- | Extract session cookie value from HTTP response headers.

Searches for the "Set-Cookie" header and parses the "mcp_session=<value>" cookie.

Returns:
- Just the cookie value if found and properly formatted
- Nothing if header missing or malformed

== Example

>>> let response = ... -- SResponse with Set-Cookie: mcp_session=abc123; Path=/
>>> extractSessionCookie response
Just "abc123"
-}
extractSessionCookie :: SResponse -> Maybe Text
extractSessionCookie response = do
    let headers = simpleHeaders response
    -- Look for Set-Cookie header (case-insensitive)
    setCookieValue <- lookup "Set-Cookie" headers
    -- Decode to Text
    cookieText <- either (const Nothing) Just $ TE.decodeUtf8' setCookieValue
    -- Parse "mcp_session=<value>; ..." format
    extractCookieValue "mcp_session=" cookieText
  where
    extractCookieValue :: Text -> Text -> Maybe Text
    extractCookieValue prefix fullCookie =
        case T.breakOn prefix fullCookie of
            (_, rest)
                | T.null rest -> Nothing
                | otherwise ->
                    let withoutPrefix = T.drop (T.length prefix) rest
                        -- Take until semicolon or end
                        value = T.takeWhile (/= ';') withoutPrefix
                     in if T.null value then Nothing else Just value

{- | Extract authorization code from redirect Location header.

Parses the Location header to extract the "code" query parameter from the redirect URI.

Returns:
- Just the authorization code if found
- Nothing if header missing, malformed, or code parameter not present

== Example

>>> let response = ... -- SResponse with Location: http://example.com/callback?code=abc123&state=xyz
>>> extractCodeFromRedirect response
Just "abc123"

>>> let response2 = ... -- SResponse with Location: http://example.com/callback?error=access_denied
>>> extractCodeFromRedirect response2
Nothing
-}
extractCodeFromRedirect :: SResponse -> Maybe Text
extractCodeFromRedirect response = do
    let headers = simpleHeaders response
    -- Look for Location header
    locationValue <- lookup hLocation headers
    -- Decode to String for URI parsing
    locationStr <- either (const Nothing) Just $ TE.decodeUtf8' locationValue
    -- Parse as URI
    uri <- parseURI (T.unpack locationStr)
    -- Extract query string
    let query = uriQuery uri
    -- Parse query parameters (format: ?code=xxx&state=yyy)
    extractCodeParam query
  where
    extractCodeParam :: String -> Maybe Text
    extractCodeParam query =
        case query of
            [] -> Nothing
            '?' : rest ->
                let params = map parseParam $ splitOn '&' rest
                 in lookup "code" params
            _ ->
                let params = map parseParam $ splitOn '&' query
                 in lookup "code" params

    parseParam :: String -> (Text, Text)
    parseParam param =
        case break (== '=') param of
            (key, '=' : value) -> (T.pack key, T.pack value)
            (key, _) -> (T.pack key, "")

    splitOn :: Char -> String -> [String]
    splitOn _ [] = []
    splitOn delimiter str =
        let (chunk, rest) = break (== delimiter) str
         in case rest of
                [] -> [chunk]
                (_ : remainder) -> chunk : splitOn delimiter remainder
