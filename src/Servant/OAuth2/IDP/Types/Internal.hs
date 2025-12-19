{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Servant.OAuth2.IDP.Types.Internal
Description : Internal access to OAuth type constructors
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com
Stability   : experimental
Portability : GHC

This module provides access to raw constructors for OAuth types, intended
ONLY for internal use by modules like Boundary.hs that need to create
unsafe constructors at the HTTP boundary.

WARNING: Do NOT import this module in application code. Use the smart
constructors from "Servant.OAuth2.IDP.Types" instead.

Per Constitution Principle II: Raw constructors should not be in public API.
This module exists solely for boundary translation purposes.

IMPLEMENTATION NOTE: This module does NOT define the types. It imports them
from where they are defined in Types.hs and re-exports them with raw
constructors exposed. This ensures there is only one definition of each type.
-}
module Servant.OAuth2.IDP.Types.Internal (
    -- * Unsafe constructor functions
    -- These are the ONLY exports - no raw constructors exported
    unsafeAuthCodeId,
    unsafeClientId,
    unsafeSessionId,
    unsafeRefreshTokenId,
    unsafeUserId,
    unsafeRedirectUri,
    unsafeScope,
    unsafeCodeChallenge,
    unsafeCodeVerifier,
    unsafeClientSecret,
    unsafeClientName,
) where

-- Import types WITH constructors so coerce can work
-- These constructors are NOT re-exported by this module
import Servant.OAuth2.IDP.Types
    ( AuthCodeId (..)
    , ClientId (..)
    , ClientName (..)
    , ClientSecret (..)
    , CodeChallenge (..)
    , CodeVerifier (..)
    , RedirectUri (..)
    , RefreshTokenId (..)
    , Scope (..)
    , SessionId (..)
    , UserId (..)
    )

import Data.Text (Text)
import Network.URI (URI)
import Unsafe.Coerce (unsafeCoerce)

{- | Unsafe constructor wrappers (bypass validation)

These functions use `unsafeCoerce` to convert between the underlying type and
the newtype wrapper, bypassing all smart constructor validation.

This is safe in this specific case because:
1. All these types are newtypes with the same runtime representation as their wrapped type
2. We're only changing the compile-time type, not the runtime value
3. This is ONLY used at HTTP boundaries where validation has already occurred

WARNING: These bypass all validation logic. Use ONLY at HTTP boundaries where
data has already been validated by the HTTP layer (e.g., in Boundary.hs).
-}

unsafeAuthCodeId :: Text -> AuthCodeId
unsafeAuthCodeId = unsafeCoerce

unsafeClientId :: Text -> ClientId
unsafeClientId = unsafeCoerce

unsafeSessionId :: Text -> SessionId
unsafeSessionId = unsafeCoerce

unsafeRefreshTokenId :: Text -> RefreshTokenId
unsafeRefreshTokenId = unsafeCoerce

unsafeUserId :: Text -> UserId
unsafeUserId = unsafeCoerce

unsafeRedirectUri :: URI -> RedirectUri
unsafeRedirectUri = unsafeCoerce

unsafeScope :: Text -> Scope
unsafeScope = unsafeCoerce

unsafeCodeChallenge :: Text -> CodeChallenge
unsafeCodeChallenge = unsafeCoerce

unsafeCodeVerifier :: Text -> CodeVerifier
unsafeCodeVerifier = unsafeCoerce

unsafeClientSecret :: Text -> ClientSecret
unsafeClientSecret = unsafeCoerce

unsafeClientName :: Text -> ClientName
unsafeClientName = unsafeCoerce
