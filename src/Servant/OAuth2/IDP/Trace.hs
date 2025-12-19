{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Servant.OAuth2.IDP.Trace
Description : OAuth trace events with domain newtypes
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com
Stability   : experimental
Portability : GHC

OAuth-specific trace events using domain newtypes from Servant.OAuth2.IDP.Types
and minimal trace-specific ADTs for operation results and denial reasons.

Per FR-005 requirements:
- OperationResult: type-safe boolean alternative (Success | Failure)
- DenialReason: authorization denial reason ADT
- OAuthTrace: main trace ADT using domain newtypes
-}
module Servant.OAuth2.IDP.Trace (
    -- * Supporting Types
    OperationResult (..),
    DenialReason (..),

    -- * Main Trace ADT
    OAuthTrace (..),
) where

import Data.Text (Text)
import GHC.Generics (Generic)
import Servant.OAuth2.IDP.Auth.Backend (Username)
import Servant.OAuth2.IDP.Errors (ValidationError)
import Servant.OAuth2.IDP.Types (ClientId, OAuthGrantType, RedirectUri, Scope, SessionId)

-- -----------------------------------------------------------------------------
-- Supporting Types
-- -----------------------------------------------------------------------------

-- | Operation result (type-safe boolean alternative)
data OperationResult
    = Success
    | Failure
    deriving stock (Show, Eq, Generic)

-- | Authorization denial reason ADT
data DenialReason
    = UserDenied
    | InvalidRequest
    | UnauthorizedClient
    | ServerError Text
    deriving stock (Show, Eq, Generic)

-- -----------------------------------------------------------------------------
-- Main Trace ADT
-- -----------------------------------------------------------------------------

{- | OAuth trace events using domain newtypes.

All constructors use typed domain values instead of primitives:
- ClientId, SessionId, Username, etc. instead of Text
- OperationResult instead of Bool
- DenialReason ADT instead of Text
- ValidationError domain type instead of Text

This enables type-safe trace construction and exhaustive pattern matching.
-}
data OAuthTrace
    = -- | Client registration with redirect URI
      TraceClientRegistration ClientId RedirectUri
    | -- | Authorization request with scopes
      TraceAuthorizationRequest ClientId [Scope] OperationResult
    | -- | Login page served with session
      TraceLoginPageServed SessionId
    | -- | Login attempt by user
      TraceLoginAttempt Username OperationResult
    | -- | PKCE code challenge validation
      TracePKCEValidation OperationResult
    | -- | Authorization granted to client for user
      TraceAuthorizationGranted ClientId Username
    | -- | Authorization denied with reason
      TraceAuthorizationDenied ClientId DenialReason
    | -- | Token exchange by grant type
      TraceTokenExchange OAuthGrantType OperationResult
    | -- | Refresh token operation
      TraceTokenRefresh OperationResult
    | -- | Login session expired
      TraceSessionExpired SessionId
    | -- | Validation error occurred
      TraceValidationError ValidationError
    deriving stock (Show, Eq, Generic)
