{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

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
    -- * Test Configuration
    TestConfig (..),
    TestCredentials (..),

    -- * PKCE Utilities
    generatePKCE,

    -- * HTTP Response Utilities
    extractSessionCookie,
    extractCodeFromRedirect,

    -- * Client Registration Helpers
    withRegisteredClient,

    -- * Authorization Flow Helpers
    withAuthorizedUser,
) where

import Control.Monad (when)
import Crypto.Hash (hashWith)
import Crypto.Hash.Algorithms (SHA256 (..))
import Data.Aeson (Value (Object, String), eitherDecode, encode, object, (.=))
import Data.Aeson.KeyMap qualified as KM
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import Data.ByteString.Base64.URL qualified as B64URL
import Data.ByteString.Lazy qualified as LBS
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time.Clock (NominalDiffTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUID
import Network.HTTP.Types (hLocation, status201)
import Network.URI (URI (..), parseURI, uriQuery)
import Network.Wai (Application)
import Network.Wai.Test (SResponse, simpleBody, simpleHeaders, simpleStatus)
import System.Random (newStdGen, randomRs)
import Test.Hspec.Wai (WaiSession, get, liftIO, post, postHtmlForm)

import MCP.Server.OAuth.Types (AuthCodeId (..), ClientId (..), mkAuthCodeId)

-- -----------------------------------------------------------------------------
-- Test Configuration Types
-- -----------------------------------------------------------------------------

{- | Polymorphic test configuration for OAuth tests.

This type packages all the necessary components for running OAuth tests:

- 'tcMakeApp': Creates the test application and time advancement function
- 'tcRunM': Runs the monad stack in IO for test execution
- 'tcCredentials': Test user credentials for authentication flows

The type is polymorphic over the monad 'm' to support different implementations
(e.g., in-memory TVar-based, mock database, etc.).

== Example

@
let testConfig = TestConfig
      { tcMakeApp = createTestApp
      , tcRunM = runReaderT \`flip\` testEnv
      , tcCredentials = TestCredentials "demo" "demo123"
      }
@
-}
data TestConfig (m :: Type -> Type) = TestConfig
    { tcMakeApp :: IO (Application, NominalDiffTime -> IO ())
    -- ^ Create test application and time advancement function
    , tcRunM :: forall a. m a -> IO a
    -- ^ Run the monad stack in IO
    , tcCredentials :: TestCredentials
    -- ^ Test credentials for authentication
    }

{- | Test user credentials for OAuth authentication flows.

Contains username and password for test user accounts.

== Example

@
let testCreds = TestCredentials
      { tcUsername = "demo"
      , tcPassword = "demo123"
      }
@
-}
data TestCredentials = TestCredentials
    { tcUsername :: Text
    , tcPassword :: Text
    }
    deriving (Eq, Show)

-- -----------------------------------------------------------------------------
-- PKCE Utilities
-- -----------------------------------------------------------------------------

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

-- -----------------------------------------------------------------------------
-- Client Registration Helpers
-- -----------------------------------------------------------------------------

{- | Register a test client and run an action with its ID.

Issues a real POST /register request within WaiSession to register a new client.
Uses a UUID-based client name to avoid collisions when tests share state.

The function:
1. Generates a unique UUID for the client name
2. POSTs to /register with client_name and redirect_uris
3. Verifies 201 status code response
4. Extracts client_id from the JSON response
5. Passes the ClientId to the continuation action

== Example

@
it "can register and authorize" $ do
  withRegisteredClient config $ \\clientId -> do
    -- clientId is now a valid registered client
    get ("/authorize?client_id=" <> unClientId clientId <> "&...")
      \`shouldRespondWith\` 200
@

== Test Isolation

When using a shared Application across tests, the UUID-based naming ensures
each invocation registers a distinct client, preventing ID collisions.

For strict isolation, wrap tests with @around@ to create fresh Applications.
-}
withRegisteredClient ::
    TestConfig m ->
    (ClientId -> WaiSession st a) ->
    WaiSession st a
withRegisteredClient _config action = do
    -- Generate unique client name using UUID
    uuid <- liftIO UUID.nextRandom
    let clientName = "test-client-" <> UUID.toText uuid
        body =
            object
                [ "client_name" .= clientName
                , "redirect_uris" .= (["http://localhost/callback"] :: [Text])
                ]

    -- POST to /register endpoint
    resp <- post "/register" (encode body)

    -- Verify 201 Created status
    let status = simpleStatus resp
        bodyBytes = simpleBody resp

    -- Use error for failures since this is a test helper combinator
    -- The caller expects a valid ClientId or test failure
    when (status /= status201) $
        liftIO $
            error $
                "Client registration failed: Expected 201 Created, got "
                    <> show status
                    <> ". Body: "
                    <> show bodyBytes

    -- Extract client_id from JSON response
    case eitherDecodeClientId bodyBytes of
        Left err ->
            liftIO $
                error $
                    "Failed to parse client_id from response: "
                        <> err
                        <> ". Body: "
                        <> show bodyBytes
        Right clientId -> action clientId
  where
    -- Helper to decode client_id from JSON response
    eitherDecodeClientId :: LBS.ByteString -> Either String ClientId
    eitherDecodeClientId bs = do
        val <- eitherDecode bs
        case val of
            Object obj ->
                case KM.lookup "client_id" obj of
                    Just (String clientIdText) ->
                        Right (ClientId clientIdText)
                    Just other ->
                        Left $ "client_id was not a string: " <> show other
                    Nothing ->
                        Left "client_id field not found in response"
            _ ->
                Left $ "Response was not a JSON object: " <> show val

-- -----------------------------------------------------------------------------
-- Authorization Flow Helpers
-- -----------------------------------------------------------------------------

{- | Complete OAuth authorization flow and run action with authorization code.

Issues real HTTP requests to complete the full OAuth authorization flow:

1. Generate PKCE verifier and challenge
2. GET /authorize with OAuth parameters (triggers login page)
3. Extract session cookie from response
4. POST /login with credentials and session cookie
5. Extract authorization code from redirect Location header
6. Pass code and verifier to the continuation action

The function:
1. Generates cryptographically secure PKCE pair (verifier, challenge)
2. Constructs /authorize URL with client_id, redirect_uri, response_type, and PKCE
3. GETs /authorize to initiate flow (server returns login page with session cookie)
4. Extracts mcp_session cookie from Set-Cookie header
5. POSTs /login with username, password, session_id (from cookie), and action=approve
6. Follows redirect to extract authorization code from Location header query params
7. Passes AuthCodeId and CodeVerifier to the continuation

Returns:
- The result of the continuation action
- Fails with error if any step fails (missing headers, invalid responses, etc.)

== Example

@
it "can complete full OAuth flow" $ do
  withRegisteredClient config $ \\clientId -> do
    withAuthorizedUser config clientId $ \\code verifier -> do
      -- code and verifier are now available for token exchange
      post "/token" (encode tokenRequest) \`shouldRespondWith\` 200
@

== Error Handling

Uses `error` for failures since this is a test helper combinator. The caller
expects valid AuthCodeId and verifier or test failure. Common failure modes:

- Missing Set-Cookie header → session cookie extraction fails
- Missing Location header → code extraction fails
- Invalid OAuth parameters → server returns error page (no redirect)
- Invalid credentials → server returns login page again (no redirect)

== Security Note

This helper uses the demo credentials from TestConfig. For production testing,
ensure TestCredentials match the configured CredentialStore.
-}
withAuthorizedUser ::
    TestConfig m ->
    ClientId ->
    (AuthCodeId -> Text -> WaiSession st a) ->
    WaiSession st a
withAuthorizedUser config clientId action = do
    -- Generate PKCE verifier and challenge
    (verifier, challenge) <- liftIO generatePKCE

    -- Step 1: GET /authorize to initiate OAuth flow
    let authUrl =
            "/authorize?client_id="
                <> unClientId clientId
                <> "&redirect_uri=http://localhost/callback"
                <> "&response_type=code"
                <> "&code_challenge="
                <> challenge
                <> "&code_challenge_method=S256"

    resp1 <- get (TE.encodeUtf8 authUrl)

    -- Extract session cookie from Set-Cookie header
    sessionCookie <- case extractSessionCookie resp1 of
        Just cookie -> pure cookie
        Nothing ->
            liftIO $
                error $
                    "Failed to extract session cookie from /authorize response. "
                        <> "Response headers: "
                        <> show (simpleHeaders resp1)

    -- Step 2: POST /login with credentials and session cookie
    let loginForm =
            [ ("username", T.unpack $ tcUsername (tcCredentials config))
            , ("password", T.unpack $ tcPassword (tcCredentials config))
            , ("session_id", T.unpack sessionCookie)
            , ("action", "approve")
            ]

    resp2 <- postHtmlForm "/login" loginForm

    -- Extract authorization code from Location redirect header
    code <- case extractCodeFromRedirect resp2 of
        Just codeText ->
            case mkAuthCodeId codeText of
                Just authCodeId -> pure authCodeId
                Nothing ->
                    liftIO $
                        error $
                            "Failed to construct AuthCodeId from code: " <> T.unpack codeText
        Nothing ->
            liftIO $
                error $
                    "Failed to extract authorization code from /login redirect. "
                        <> "Response status: "
                        <> show (simpleStatus resp2)
                        <> ". Response headers: "
                        <> show (simpleHeaders resp2)
                        <> ". Response body: "
                        <> show (simpleBody resp2)

    -- Pass code and verifier to continuation
    action code verifier
