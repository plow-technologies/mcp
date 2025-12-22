{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : Servant.OAuth2.IDP.Handlers.Helpers
Description : Helper functions for OAuth handlers
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com
Stability   : experimental
Portability : GHC

Helper functions used across OAuth handler implementations.
-}
module Servant.OAuth2.IDP.Handlers.Helpers (
    extractSessionFromCookie,
    generateAuthCode,
    generateJWTAccessToken,
    generateRefreshTokenWithConfig,
) where

import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString.Lazy qualified as LBS
import Data.Generics.Sum.Typed (AsType, injectTyped)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Servant.Auth.Server (JWTSettings, ToJWT, makeJWT)

import Servant.OAuth2.IDP.Config (OAuthEnv (..))
import Servant.OAuth2.IDP.Errors (
    AuthorizationError (..),
    InvalidRequestReason (..),
    MalformedReason (..),
 )
import Servant.OAuth2.IDP.Store (OAuthStateStore (..))
import Servant.OAuth2.IDP.Types (
    AccessTokenId,
    AuthCodeId,
    RefreshTokenId,
    SessionId (..),
    generateAuthCodeId,
    generateRefreshTokenId,
    mkAccessTokenId,
    mkSessionId,
 )

-- | Extract session ID from cookie header
extractSessionFromCookie :: Text -> Maybe SessionId
extractSessionFromCookie cookieHeader =
    let cookies = T.splitOn ";" cookieHeader
        sessionCookies = filter (T.isInfixOf "mcp_session=") cookies
     in case sessionCookies of
            (cookie : _) ->
                let parts = T.splitOn "=" cookie
                 in case parts of
                        [_, value] -> mkSessionId (T.strip value)
                        _ -> Nothing
            [] -> Nothing

-- | Generate authorization code with config prefix
generateAuthCode :: OAuthEnv -> IO AuthCodeId
generateAuthCode config = do
    let prefix = oauthAuthCodePrefix config
    generateAuthCodeId prefix

{- | Generate JWT access token for user

Uses TypeApplications to specify the monad context (and thus the OAuthUser type).
Call with: @generateJWTAccessToken \@m user jwtSettings@
-}
generateJWTAccessToken :: forall m e. (OAuthStateStore m, ToJWT (OAuthUser m), MonadIO m, MonadError e m, AsType AuthorizationError e) => OAuthUser m -> JWTSettings -> m AccessTokenId
generateJWTAccessToken user jwtSettings = do
    accessTokenResult <- liftIO $ makeJWT user jwtSettings Nothing
    case accessTokenResult of
        Left err -> throwError $ injectTyped @AuthorizationError $ InvalidRequest $ MalformedRequest $ UnparseableBody $ T.pack $ show err
        Right accessToken -> case TE.decodeUtf8' $ LBS.toStrict accessToken of
            Left decodeErr -> throwError $ injectTyped @AuthorizationError $ InvalidRequest $ MalformedRequest $ UnparseableBody $ T.pack $ show decodeErr
            Right tokenText -> case mkAccessTokenId tokenText of
                Just tokenId -> return tokenId
                Nothing -> error "generateJWTAccessToken: JWT generation produced empty text (impossible)"

-- | Generate refresh token with configurable prefix
generateRefreshTokenWithConfig :: OAuthEnv -> IO RefreshTokenId
generateRefreshTokenWithConfig config = do
    let prefix = oauthRefreshTokenPrefix config
    generateRefreshTokenId prefix
