{- |
Module      : MCP.Server.OAuth.Boundary
Description : Boundary conversion functions between Text and typed OAuth newtypes
Copyright   : (C) 2025 Matthias Pall Gissurarson
License     : MIT
Maintainer  : mpg@mpg.is
Stability   : experimental
Portability : GHC

This module provides unsafe constructors and extractors for converting between
Text (used by HTTP.hs and Servant) and type-safe OAuth newtypes. These functions
bypass validation and should ONLY be used at the HTTP boundary where data has
already been validated by the HTTP layer.

= Usage Pattern

HTTP.hs receives Text from Servant endpoints → convert to typed newtypes using
unsafe constructors → call typeclass methods → convert results back to Text for
JSON responses.

= Safety Notes

These functions are marked "unsafe" because they bypass the smart constructors
that perform validation. They should ONLY be used when:

1. The Text value comes from a validated HTTP request
2. You're at the boundary between HTTP.hs and the typeclass layer
3. You need to convert between representations without re-validating

For all other uses, prefer the smart constructors from "MCP.Server.OAuth.Types".
-}
module MCP.Server.OAuth.Boundary (
    -- * Unsafe Constructors (HTTP boundary only)

    -- | WARNING: These bypass validation. Use only for data from validated HTTP requests.
    unsafeAuthCodeId,
    unsafeClientId,
    unsafeSessionId,
    unsafeAccessTokenId,
    unsafeRefreshTokenId,
    unsafeUserId,
    unsafeRedirectUri,
    unsafeScope,
    unsafeCodeChallenge,

    -- * Extractors (typed → Text)
    authCodeIdToText,
    clientIdToText,
    sessionIdToText,
    accessTokenIdToText,
    refreshTokenIdToText,
    userIdToText,
    redirectUriToText,
    scopeToText,
    codeChallengeToText,

    -- * Re-exports from MCP.Server.OAuth.Types
    module MCP.Server.OAuth.Types,
) where

import Data.Text (Text)
import Data.Text qualified as T
import MCP.Server.OAuth.Types
import Network.URI (parseURI)

-- -----------------------------------------------------------------------------
-- Unsafe Constructors
-- -----------------------------------------------------------------------------

{- | Unsafe constructor for AuthCodeId.

WARNING: Bypasses validation. Use only for data from validated HTTP requests.
-}
unsafeAuthCodeId :: Text -> AuthCodeId
unsafeAuthCodeId = AuthCodeId

{- | Unsafe constructor for ClientId.

WARNING: Bypasses validation. Use only for data from validated HTTP requests.
-}
unsafeClientId :: Text -> ClientId
unsafeClientId = ClientId

{- | Unsafe constructor for SessionId.

WARNING: Bypasses validation (does NOT check UUID format).
Use only for data from validated HTTP requests.
-}
unsafeSessionId :: Text -> SessionId
unsafeSessionId = SessionId

{- | Unsafe constructor for AccessTokenId.

WARNING: Bypasses validation. Use only for JWT tokens from validated sources.
-}
unsafeAccessTokenId :: Text -> AccessTokenId
unsafeAccessTokenId = AccessTokenId

{- | Unsafe constructor for RefreshTokenId.

WARNING: Bypasses validation. Use only for data from validated HTTP requests.
-}
unsafeRefreshTokenId :: Text -> RefreshTokenId
unsafeRefreshTokenId = RefreshTokenId

{- | Unsafe constructor for UserId.

WARNING: Bypasses validation. Use only for data from validated HTTP requests.
-}
unsafeUserId :: Text -> UserId
unsafeUserId = UserId

{- | Unsafe constructor for RedirectUri.

WARNING: Bypasses validation (does NOT check https:// scheme or localhost).
Use only for URIs from validated HTTP requests.

Returns Nothing if the URI cannot be parsed at all.
-}
unsafeRedirectUri :: Text -> Maybe RedirectUri
unsafeRedirectUri t = RedirectUri <$> parseURI (T.unpack t)

{- | Unsafe constructor for Scope.

WARNING: Bypasses validation (does NOT check for whitespace).
Use only for data from validated HTTP requests.
-}
unsafeScope :: Text -> Scope
unsafeScope = Scope

{- | Unsafe constructor for CodeChallenge.

WARNING: Bypasses validation (does NOT check base64url charset or length).
Use only for data from validated HTTP requests.
-}
unsafeCodeChallenge :: Text -> CodeChallenge
unsafeCodeChallenge = CodeChallenge

-- -----------------------------------------------------------------------------
-- Extractors
-- -----------------------------------------------------------------------------

-- | Extract Text from AuthCodeId
authCodeIdToText :: AuthCodeId -> Text
authCodeIdToText = unAuthCodeId

-- | Extract Text from ClientId
clientIdToText :: ClientId -> Text
clientIdToText = unClientId

-- | Extract Text from SessionId
sessionIdToText :: SessionId -> Text
sessionIdToText = unSessionId

-- | Extract Text from AccessTokenId
accessTokenIdToText :: AccessTokenId -> Text
accessTokenIdToText = unAccessTokenId

-- | Extract Text from RefreshTokenId
refreshTokenIdToText :: RefreshTokenId -> Text
refreshTokenIdToText = unRefreshTokenId

-- | Extract Text from UserId
userIdToText :: UserId -> Text
userIdToText = unUserId

-- | Extract Text from RedirectUri
redirectUriToText :: RedirectUri -> Text
redirectUriToText (RedirectUri uri) = T.pack (show uri)

-- | Extract Text from Scope
scopeToText :: Scope -> Text
scopeToText = unScope

-- | Extract Text from CodeChallenge
codeChallengeToText :: CodeChallenge -> Text
codeChallengeToText = unCodeChallenge
