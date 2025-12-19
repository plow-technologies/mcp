{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Servant.OAuth2.IDP.MetadataSpec
Description : Tests for OAuth metadata types per RFC 8414 and RFC 9728
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com
Stability   : experimental
Portability : GHC
-}
module Servant.OAuth2.IDP.MetadataSpec (spec) where

import Data.Aeson (decode, encode, object, (.=))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Servant.OAuth2.IDP.Metadata (
    mkOAuthMetadata,
    mkProtectedResourceMetadata,
    oauthAuthorizationEndpoint,
    oauthIssuer,
    oauthResponseTypesSupported,
    oauthTokenEndpoint,
    prAuthorizationServers,
    prResource,
    prScopesSupported,
 )
import Servant.OAuth2.IDP.Types (ClientAuthMethod (..), CodeChallengeMethod (..), GrantType (..), ResponseType (..), mkScope)
import Servant.OAuth2.IDP.Types.Internal (unsafeScope)
import Test.Hspec

spec :: Spec
spec = do
    describe "OAuthMetadata" $ do
        context "Smart constructor validation" $ do
            it "rejects non-HTTPS issuer URI" $ do
                let result =
                        mkOAuthMetadata
                            "http://auth.example.com" -- HTTP not HTTPS
                            "https://auth.example.com/authorize"
                            "https://auth.example.com/token"
                            Nothing
                            Nothing
                            Nothing
                            Nothing
                            [ResponseCode]
                            Nothing
                            Nothing
                            Nothing
                result `shouldBe` Nothing

            it "rejects relative issuer URI" $ do
                let result =
                        mkOAuthMetadata
                            "/auth" -- Relative URI
                            "https://auth.example.com/authorize"
                            "https://auth.example.com/token"
                            Nothing
                            Nothing
                            Nothing
                            Nothing
                            [ResponseCode]
                            Nothing
                            Nothing
                            Nothing
                result `shouldBe` Nothing

            it "rejects non-HTTPS authorization endpoint" $ do
                let result =
                        mkOAuthMetadata
                            "https://auth.example.com"
                            "http://auth.example.com/authorize" -- HTTP not HTTPS
                            "https://auth.example.com/token"
                            Nothing
                            Nothing
                            Nothing
                            Nothing
                            [ResponseCode]
                            Nothing
                            Nothing
                            Nothing
                result `shouldBe` Nothing

            it "rejects non-HTTPS token endpoint" $ do
                let result =
                        mkOAuthMetadata
                            "https://auth.example.com"
                            "https://auth.example.com/authorize"
                            "http://auth.example.com/token" -- HTTP not HTTPS
                            Nothing
                            Nothing
                            Nothing
                            Nothing
                            [ResponseCode]
                            Nothing
                            Nothing
                            Nothing
                result `shouldBe` Nothing

            it "rejects non-HTTPS registration endpoint when provided" $ do
                let result =
                        mkOAuthMetadata
                            "https://auth.example.com"
                            "https://auth.example.com/authorize"
                            "https://auth.example.com/token"
                            (Just "http://auth.example.com/register") -- HTTP not HTTPS
                            Nothing
                            Nothing
                            Nothing
                            [ResponseCode]
                            Nothing
                            Nothing
                            Nothing
                result `shouldBe` Nothing

            it "accepts valid HTTPS URIs" $ do
                let result =
                        mkOAuthMetadata
                            "https://auth.example.com"
                            "https://auth.example.com/authorize"
                            "https://auth.example.com/token"
                            (Just "https://auth.example.com/register")
                            (Just "https://auth.example.com/userinfo")
                            (Just "https://auth.example.com/.well-known/jwks.json")
                            Nothing
                            [ResponseCode]
                            Nothing
                            Nothing
                            Nothing
                case result of
                    Just _ -> pure ()
                    Nothing -> expectationFailure "Valid HTTPS URIs should be accepted"

        context "RFC 8414: Snake_case JSON serialization" $ do
            it "serializes required fields with snake_case keys" $ do
                case mkOAuthMetadata
                    "https://auth.example.com"
                    "https://auth.example.com/authorize"
                    "https://auth.example.com/token"
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    [ResponseCode]
                    Nothing
                    Nothing
                    Nothing of
                    Nothing -> expectationFailure "Smart constructor should succeed with valid HTTPS URIs"
                    Just metadata -> do
                        let encoded = encode metadata
                        let expected =
                                object
                                    [ "issuer" .= ("https://auth.example.com" :: Text)
                                    , "authorization_endpoint" .= ("https://auth.example.com/authorize" :: Text)
                                    , "token_endpoint" .= ("https://auth.example.com/token" :: Text)
                                    , "response_types_supported" .= [Aeson.String "code"]
                                    ]

                        decode encoded `shouldBe` Just expected

            it "serializes all optional fields with snake_case keys" $ do
                let openidScope = unsafeScope "openid"
                    profileScope = unsafeScope "profile"
                case mkOAuthMetadata
                    "https://auth.example.com"
                    "https://auth.example.com/authorize"
                    "https://auth.example.com/token"
                    (Just "https://auth.example.com/register")
                    (Just "https://auth.example.com/userinfo")
                    (Just "https://auth.example.com/.well-known/jwks.json")
                    (Just [openidScope, profileScope])
                    [ResponseCode, ResponseToken]
                    (Just [GrantAuthorizationCode, GrantRefreshToken])
                    (Just [AuthNone, AuthClientSecretPost])
                    (Just [S256, Plain]) of
                    Nothing -> expectationFailure "Smart constructor should succeed with valid HTTPS URIs"
                    Just metadata -> do
                        let encoded = encode metadata
                        let expected =
                                object
                                    [ "issuer" .= ("https://auth.example.com" :: Text)
                                    , "authorization_endpoint" .= ("https://auth.example.com/authorize" :: Text)
                                    , "token_endpoint" .= ("https://auth.example.com/token" :: Text)
                                    , "registration_endpoint" .= ("https://auth.example.com/register" :: Text)
                                    , "userinfo_endpoint" .= ("https://auth.example.com/userinfo" :: Text)
                                    , "jwks_uri" .= ("https://auth.example.com/.well-known/jwks.json" :: Text)
                                    , "scopes_supported" .= [Aeson.String "openid", Aeson.String "profile"]
                                    , "response_types_supported" .= [Aeson.String "code", Aeson.String "token"]
                                    , "grant_types_supported" .= [Aeson.String "authorization_code", Aeson.String "refresh_token"]
                                    , "token_endpoint_auth_methods_supported" .= [Aeson.String "none", Aeson.String "client_secret_post"]
                                    , "code_challenge_methods_supported" .= [Aeson.String "S256", Aeson.String "plain"]
                                    ]

                        decode encoded `shouldBe` Just expected

            it "round-trips through JSON with snake_case fields" $ do
                let openidScope = unsafeScope "openid"
                case mkOAuthMetadata
                    "https://auth.example.com"
                    "https://auth.example.com/authorize"
                    "https://auth.example.com/token"
                    (Just "https://auth.example.com/register")
                    Nothing
                    Nothing
                    (Just [openidScope])
                    [ResponseCode]
                    (Just [GrantAuthorizationCode])
                    Nothing
                    (Just [S256]) of
                    Nothing -> expectationFailure "Smart constructor should succeed with valid HTTPS URIs"
                    Just metadata -> do
                        let encoded = encode metadata
                        let decoded = decode encoded

                        decoded `shouldBe` Just metadata

            it "deserializes RFC 8414 compliant JSON with snake_case" $ do
                let json =
                        object
                            [ "issuer" .= ("https://auth.example.com" :: Text)
                            , "authorization_endpoint" .= ("https://auth.example.com/authorize" :: Text)
                            , "token_endpoint" .= ("https://auth.example.com/token" :: Text)
                            , "response_types_supported" .= [Aeson.String "code"]
                            ]

                let decoded = decode (encode json)

                case decoded of
                    Just metadata -> do
                        oauthIssuer metadata `shouldBe` "https://auth.example.com"
                        oauthAuthorizationEndpoint metadata `shouldBe` "https://auth.example.com/authorize"
                        oauthTokenEndpoint metadata `shouldBe` "https://auth.example.com/token"
                        oauthResponseTypesSupported metadata `shouldBe` [ResponseCode]
                    Nothing -> expectationFailure "Failed to decode RFC 8414 JSON"

    describe "ProtectedResourceMetadata" $ do
        context "Smart constructor validation" $ do
            it "rejects non-HTTPS resource URI" $ do
                let result =
                        mkProtectedResourceMetadata
                            "http://api.example.com" -- HTTP not HTTPS
                            ["https://auth.example.com"]
                            Nothing
                            Nothing
                            Nothing
                            Nothing
                result `shouldBe` Nothing

            it "rejects relative resource URI" $ do
                let result =
                        mkProtectedResourceMetadata
                            "/api" -- Relative URI
                            ["https://auth.example.com"]
                            Nothing
                            Nothing
                            Nothing
                            Nothing
                result `shouldBe` Nothing

            it "rejects non-HTTPS documentation URI when provided" $ do
                let result =
                        mkProtectedResourceMetadata
                            "https://api.example.com"
                            ["https://auth.example.com"]
                            Nothing
                            Nothing
                            Nothing
                            (Just "http://docs.example.com") -- HTTP not HTTPS
                result `shouldBe` Nothing

            it "accepts valid HTTPS URIs" $ do
                let result =
                        mkProtectedResourceMetadata
                            "https://api.example.com"
                            ["https://auth.example.com"]
                            Nothing
                            Nothing
                            Nothing
                            (Just "https://docs.example.com")
                case result of
                    Just _ -> pure ()
                    Nothing -> expectationFailure "Valid HTTPS URIs should be accepted"

        context "RFC 9728: Snake_case JSON serialization" $ do
            it "serializes required fields with snake_case keys" $ do
                case mkProtectedResourceMetadata
                    "https://api.example.com"
                    ["https://auth.example.com"]
                    Nothing
                    Nothing
                    Nothing
                    Nothing of
                    Nothing -> expectationFailure "Smart constructor should succeed with valid HTTPS URIs"
                    Just metadata -> do
                        let encoded = encode metadata
                        let expected =
                                object
                                    [ "resource" .= ("https://api.example.com" :: Text)
                                    , "authorization_servers" .= [Aeson.String "https://auth.example.com"]
                                    ]

                        decode encoded `shouldBe` Just expected

            it "serializes all optional fields with snake_case keys" $ do
                let openidScope = unsafeScope "openid"
                    profileScope = unsafeScope "profile"
                case mkProtectedResourceMetadata
                    "https://api.example.com"
                    ["https://auth.example.com", "https://auth2.example.com"]
                    (Just [openidScope, profileScope])
                    (Just ["header", "body"])
                    (Just "Example API")
                    (Just "https://docs.example.com/api") of
                    Nothing -> expectationFailure "Smart constructor should succeed with valid HTTPS URIs"
                    Just metadata -> do
                        let encoded = encode metadata
                        let expected =
                                object
                                    [ "resource" .= ("https://api.example.com" :: Text)
                                    , "authorization_servers" .= [Aeson.String "https://auth.example.com", Aeson.String "https://auth2.example.com"]
                                    , "scopes_supported" .= [Aeson.String "openid", Aeson.String "profile"]
                                    , "bearer_methods_supported" .= [Aeson.String "header", Aeson.String "body"]
                                    , "resource_name" .= ("Example API" :: Text)
                                    , "resource_documentation" .= ("https://docs.example.com/api" :: Text)
                                    ]

                        decode encoded `shouldBe` Just expected

            it "round-trips through JSON with snake_case fields" $ do
                let openidScope = unsafeScope "openid"
                case mkProtectedResourceMetadata
                    "https://api.example.com"
                    ["https://auth.example.com"]
                    (Just [openidScope])
                    (Just ["header"])
                    (Just "Example API")
                    Nothing of
                    Nothing -> expectationFailure "Smart constructor should succeed with valid HTTPS URIs"
                    Just metadata -> do
                        let encoded = encode metadata
                        let decoded = decode encoded

                        decoded `shouldBe` Just metadata

            it "deserializes RFC 9728 compliant JSON with snake_case" $ do
                let json =
                        object
                            [ "resource" .= ("https://api.example.com" :: Text)
                            , "authorization_servers" .= [Aeson.String "https://auth.example.com"]
                            , "scopes_supported" .= [Aeson.String "openid"]
                            ]

                let decoded = decode (encode json)

                case decoded of
                    Just metadata -> do
                        prResource metadata `shouldBe` "https://api.example.com"
                        prAuthorizationServers metadata `shouldBe` ["https://auth.example.com"]
                        case prScopesSupported metadata of
                            Just [scope] -> case mkScope "openid" of
                                Just expected -> scope `shouldBe` expected
                                Nothing -> expectationFailure "Test fixture: invalid scope"
                            _ -> expectationFailure "Expected exactly one scope"
                    Nothing -> expectationFailure "Failed to decode RFC 9728 JSON"
