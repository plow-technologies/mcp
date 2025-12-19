{-# LANGUAGE OverloadedStrings #-}

module Servant.OAuth2.IDP.ConfigSpec (spec) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Network.URI (URI, parseURI)
import Test.Hspec (Spec, describe, it, shouldBe)

import Servant.OAuth2.IDP.Config (OAuthEnv (..))
import Servant.OAuth2.IDP.Types (
    ClientAuthMethod (..),
    CodeChallengeMethod (..),
    OAuthGrantType (..),
    ResponseType (..),
    Scope,
    mkScope,
 )

-- Helper to get test URI (pattern match safe in test context)
testUri :: URI
testUri = case parseURI "https://example.com" of
    Just uri -> uri
    Nothing -> error "Failed to parse test URI"

-- Helper to get test scope (pattern match safe in test context)
testScope :: Scope
testScope = case mkScope "read" of
    Just scope -> scope
    Nothing -> error "Failed to create test scope"

spec :: Spec
spec = do
    describe "OAuthEnv" $ do
        it "constructs valid configuration with all required fields" $ do
            let
                env =
                    OAuthEnv
                        { oauthRequireHTTPS = True
                        , oauthBaseUrl = testUri
                        , oauthAuthCodeExpiry = 600
                        , oauthAccessTokenExpiry = 3600
                        , oauthLoginSessionExpiry = 600
                        , oauthAuthCodePrefix = "code_"
                        , oauthRefreshTokenPrefix = "rt_"
                        , oauthClientIdPrefix = "client_"
                        , oauthSupportedScopes = [testScope]
                        , oauthSupportedResponseTypes = ResponseCode :| []
                        , oauthSupportedGrantTypes = OAuthAuthorizationCode :| []
                        , oauthSupportedAuthMethods = AuthNone :| []
                        , oauthSupportedCodeChallengeMethods = S256 :| []
                        }

            oauthRequireHTTPS env `shouldBe` True
            oauthBaseUrl env `shouldBe` testUri
            oauthAuthCodeExpiry env `shouldBe` 600
            oauthAccessTokenExpiry env `shouldBe` 3600
            oauthLoginSessionExpiry env `shouldBe` 600
            oauthAuthCodePrefix env `shouldBe` "code_"
            oauthRefreshTokenPrefix env `shouldBe` "rt_"
            oauthClientIdPrefix env `shouldBe` "client_"
            length (oauthSupportedScopes env) `shouldBe` 1

        it "allows empty scope list (no required scopes)" $ do
            let env =
                    OAuthEnv
                        { oauthRequireHTTPS = True
                        , oauthBaseUrl = testUri
                        , oauthAuthCodeExpiry = 600
                        , oauthAccessTokenExpiry = 3600
                        , oauthLoginSessionExpiry = 600
                        , oauthAuthCodePrefix = "code_"
                        , oauthRefreshTokenPrefix = "rt_"
                        , oauthClientIdPrefix = "client_"
                        , oauthSupportedScopes = []
                        , oauthSupportedResponseTypes = ResponseCode :| []
                        , oauthSupportedGrantTypes = OAuthAuthorizationCode :| []
                        , oauthSupportedAuthMethods = AuthNone :| []
                        , oauthSupportedCodeChallengeMethods = S256 :| []
                        }

            oauthSupportedScopes env `shouldBe` []

        it "supports multiple response types" $ do
            let env =
                    OAuthEnv
                        { oauthRequireHTTPS = False
                        , oauthBaseUrl = testUri
                        , oauthAuthCodeExpiry = 600
                        , oauthAccessTokenExpiry = 3600
                        , oauthLoginSessionExpiry = 600
                        , oauthAuthCodePrefix = "code_"
                        , oauthRefreshTokenPrefix = "rt_"
                        , oauthClientIdPrefix = "client_"
                        , oauthSupportedScopes = []
                        , oauthSupportedResponseTypes = ResponseCode :| [ResponseToken]
                        , oauthSupportedGrantTypes = OAuthAuthorizationCode :| []
                        , oauthSupportedAuthMethods = AuthNone :| []
                        , oauthSupportedCodeChallengeMethods = S256 :| []
                        }

            length (oauthSupportedResponseTypes env) `shouldBe` 2

        it "supports multiple grant types" $ do
            let env =
                    OAuthEnv
                        { oauthRequireHTTPS = True
                        , oauthBaseUrl = testUri
                        , oauthAuthCodeExpiry = 600
                        , oauthAccessTokenExpiry = 3600
                        , oauthLoginSessionExpiry = 600
                        , oauthAuthCodePrefix = "code_"
                        , oauthRefreshTokenPrefix = "rt_"
                        , oauthClientIdPrefix = "client_"
                        , oauthSupportedScopes = []
                        , oauthSupportedResponseTypes = ResponseCode :| []
                        , oauthSupportedGrantTypes = OAuthAuthorizationCode :| [OAuthClientCredentials]
                        , oauthSupportedAuthMethods = AuthNone :| []
                        , oauthSupportedCodeChallengeMethods = S256 :| []
                        }

            length (oauthSupportedGrantTypes env) `shouldBe` 2

        it "supports multiple auth methods" $ do
            let env =
                    OAuthEnv
                        { oauthRequireHTTPS = True
                        , oauthBaseUrl = testUri
                        , oauthAuthCodeExpiry = 600
                        , oauthAccessTokenExpiry = 3600
                        , oauthLoginSessionExpiry = 600
                        , oauthAuthCodePrefix = "code_"
                        , oauthRefreshTokenPrefix = "rt_"
                        , oauthClientIdPrefix = "client_"
                        , oauthSupportedScopes = []
                        , oauthSupportedResponseTypes = ResponseCode :| []
                        , oauthSupportedGrantTypes = OAuthAuthorizationCode :| []
                        , oauthSupportedAuthMethods = AuthNone :| [AuthClientSecretPost, AuthClientSecretBasic]
                        , oauthSupportedCodeChallengeMethods = S256 :| []
                        }

            length (oauthSupportedAuthMethods env) `shouldBe` 3

        it "supports multiple code challenge methods" $ do
            let env =
                    OAuthEnv
                        { oauthRequireHTTPS = True
                        , oauthBaseUrl = testUri
                        , oauthAuthCodeExpiry = 600
                        , oauthAccessTokenExpiry = 3600
                        , oauthLoginSessionExpiry = 600
                        , oauthAuthCodePrefix = "code_"
                        , oauthRefreshTokenPrefix = "rt_"
                        , oauthClientIdPrefix = "client_"
                        , oauthSupportedScopes = []
                        , oauthSupportedResponseTypes = ResponseCode :| []
                        , oauthSupportedGrantTypes = OAuthAuthorizationCode :| []
                        , oauthSupportedAuthMethods = AuthNone :| []
                        , oauthSupportedCodeChallengeMethods = S256 :| [Plain]
                        }

            length (oauthSupportedCodeChallengeMethods env) `shouldBe` 2
