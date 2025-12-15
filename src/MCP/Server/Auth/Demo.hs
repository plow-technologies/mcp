{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : MCP.Server.Auth.Demo
Description : Demo credential AuthBackend implementation
Copyright   : (C) 2025 Matthias Pall Gissurarson
License     : MIT
Maintainer  : mpg@mpg.is
Stability   : experimental
Portability : GHC

This module provides a demo implementation of the AuthBackend typeclass
using an in-memory credential store with hardcoded demo accounts.

== Security Notice

This implementation is for DEMONSTRATION purposes only:

* Uses hardcoded credentials (demo/demo123, admin/admin456)
* Uses simple SHA256 hashing (not suitable for production)
* No rate limiting or account lockout
* No audit logging

Production implementations should:

* Use secure password hashing (Argon2id, bcrypt, PBKDF2)
* Store credentials in a secure database
* Implement rate limiting and account lockout
* Log all authentication attempts
* Support password rotation policies

== Usage

@
import MCP.Server.Auth.Demo
import Control.Monad.Reader

-- Create environment with demo credentials
let env = DemoCredentialEnv defaultDemoCredentialStore

-- Validate credentials
let username = Username "demo"
let password = mkPlaintextPassword "demo123"
result <- runReaderT (validateCredentials username password) env
-- result == True
@
-}
module MCP.Server.Auth.Demo (
    -- * Environment
    DemoCredentialEnv (..),

    -- * Error Type
    DemoAuthError (..),

    -- * Default Credentials
    defaultDemoCredentialStore,
) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT, asks)
import Data.ByteArray qualified as BA
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import GHC.Generics (Generic)

import MCP.Server.Auth.Backend (
    AuthBackend (..),
    CredentialStore (..),
    Salt (..),
    Username (..),
    mkHashedPassword,
    mkPlaintextPassword,
 )
import MCP.Server.OAuth.Types (AuthUser, UserId)

-- -----------------------------------------------------------------------------
-- Environment
-- -----------------------------------------------------------------------------

{- | Environment for demo credential authentication.

Contains the in-memory credential store with demo accounts.
-}
newtype DemoCredentialEnv = DemoCredentialEnv
    { credentialStore :: CredentialStore
    -- ^ In-memory map of username -> hashed password
    }
    deriving (Generic)

-- -----------------------------------------------------------------------------
-- Error Type
-- -----------------------------------------------------------------------------

{- | Authentication errors for demo implementation.

Intentionally minimal to avoid leaking information about which step failed.
-}
data DemoAuthError
    = -- | Username/password combination is invalid (deliberately vague)
      InvalidCredentials
    | -- | User does not exist (only used internally, not exposed to clients)
      UserNotFound Username
    deriving (Eq, Show, Generic)

-- -----------------------------------------------------------------------------
-- AuthBackend Instance
-- -----------------------------------------------------------------------------

instance (MonadIO m) => AuthBackend (ReaderT DemoCredentialEnv m) where
    type AuthBackendError (ReaderT DemoCredentialEnv m) = DemoAuthError
    type AuthBackendEnv (ReaderT DemoCredentialEnv m) = DemoCredentialEnv
    type AuthBackendUser (ReaderT DemoCredentialEnv m) = AuthUser
    type AuthBackendUserId (ReaderT DemoCredentialEnv m) = UserId

    validateCredentials username password = do
        store <- asks credentialStore
        let storedHash = Map.lookup username (storeCredentials store)
        case storedHash of
            Nothing -> pure False -- User not found (same as invalid password)
            Just hash -> do
                let candidateHash = mkHashedPassword (storeSalt store) password
                -- ScrubbedBytes Eq is constant-time
                pure $ hash == candidateHash

-- -----------------------------------------------------------------------------
-- Default Credentials
-- -----------------------------------------------------------------------------

{- | Default demo credential store with hardcoded test accounts.

Credentials:

* Username: @demo@, Password: @demo123@
* Username: @admin@, Password: @admin456@

== Example

@
import MCP.Server.Auth.Demo
import Control.Monad.Reader

let env = DemoCredentialEnv defaultDemoCredentialStore
let username = Username "demo"
let password = mkPlaintextPassword "demo123"
result <- runReaderT (validateCredentials username password) env
-- result == True
@
-}
defaultDemoCredentialStore :: CredentialStore
defaultDemoCredentialStore =
    let saltText = "mcp-demo-salt" :: Text
        saltBytes = BA.convert (TE.encodeUtf8 saltText) :: BA.ScrubbedBytes
        salt = Salt saltBytes
        demoHash = mkHashedPassword salt (mkPlaintextPassword "demo123")
        adminHash = mkHashedPassword salt (mkPlaintextPassword "admin456")
     in CredentialStore
            { storeCredentials =
                Map.fromList
                    [ (Username "demo", demoHash)
                    , (Username "admin", adminHash)
                    ]
            , storeSalt = salt
            }
