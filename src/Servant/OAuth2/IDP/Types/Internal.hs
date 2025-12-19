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
-}
module Servant.OAuth2.IDP.Types.Internal (
    -- * PKCE Types (with raw constructors)
    CodeChallenge (..),
    CodeVerifier (..),
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Web.HttpApiData (FromHttpApiData (..), ToHttpApiData (..))

-- | PKCE code challenge
newtype CodeChallenge = CodeChallenge {unCodeChallenge :: Text}
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (FromJSON, ToJSON)

instance FromHttpApiData CodeChallenge where
    parseUrlPiece t
        | len < 43 || len > 128 = Left "CodeChallenge must be base64url (43-128 chars)"
        | not (T.all isBase64UrlChar t) = Left "CodeChallenge must be base64url (43-128 chars)"
        | otherwise = Right (CodeChallenge t)
      where
        len = T.length t
        isBase64UrlChar c =
            isAsciiUpper c
                || isAsciiLower c
                || isDigit c
                || c == '-'
                || c == '_'

instance ToHttpApiData CodeChallenge where
    toUrlPiece = unCodeChallenge

-- | PKCE code verifier
newtype CodeVerifier = CodeVerifier {unCodeVerifier :: Text}
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (FromJSON, ToJSON)

instance FromHttpApiData CodeVerifier where
    parseUrlPiece t
        | len < 43 || len > 128 = Left "CodeVerifier must contain unreserved chars (43-128 chars)"
        | not (T.all isUnreservedChar t) = Left "CodeVerifier must contain unreserved chars (43-128 chars)"
        | otherwise = Right (CodeVerifier t)
      where
        len = T.length t
        -- RFC 7636: unreserved = ALPHA / DIGIT / "-" / "." / "_" / "~"
        isUnreservedChar c =
            isAsciiUpper c
                || isAsciiLower c
                || isDigit c
                || c == '-'
                || c == '.'
                || c == '_'
                || c == '~'

instance ToHttpApiData CodeVerifier where
    toUrlPiece = unCodeVerifier
