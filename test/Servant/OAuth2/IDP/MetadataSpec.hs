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
import Servant.OAuth2.IDP.Metadata (OAuthMetadata (..), ProtectedResourceMetadata (..))
import Servant.OAuth2.IDP.Types (ClientAuthMethod (..), CodeChallengeMethod (..), GrantType (..), ResponseType (..), Scope (..))
import Test.Hspec

spec :: Spec
spec = do
    describe "OAuthMetadata" $ do
        context "RFC 8414: Snake_case JSON serialization" $ do
            it "serializes required fields with snake_case keys" $ do
                let metadata =
                        OAuthMetadata
                            { issuer = "https://auth.example.com"
                            , authorizationEndpoint = "https://auth.example.com/authorize"
                            , tokenEndpoint = "https://auth.example.com/token"
                            , registrationEndpoint = Nothing
                            , userInfoEndpoint = Nothing
                            , jwksUri = Nothing
                            , scopesSupported = Nothing
                            , responseTypesSupported = [ResponseCode]
                            , grantTypesSupported = Nothing
                            , tokenEndpointAuthMethodsSupported = Nothing
                            , codeChallengeMethodsSupported = Nothing
                            }

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
                let openidScope = Scope "openid"
                    profileScope = Scope "profile"
                    metadata =
                        OAuthMetadata
                            { issuer = "https://auth.example.com"
                            , authorizationEndpoint = "https://auth.example.com/authorize"
                            , tokenEndpoint = "https://auth.example.com/token"
                            , registrationEndpoint = Just "https://auth.example.com/register"
                            , userInfoEndpoint = Just "https://auth.example.com/userinfo"
                            , jwksUri = Just "https://auth.example.com/.well-known/jwks.json"
                            , scopesSupported = Just [openidScope, profileScope]
                            , responseTypesSupported = [ResponseCode, ResponseToken]
                            , grantTypesSupported = Just [GrantAuthorizationCode, GrantRefreshToken]
                            , tokenEndpointAuthMethodsSupported = Just [AuthNone, AuthClientSecretPost]
                            , codeChallengeMethodsSupported = Just [S256, Plain]
                            }

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
                let openidScope = Scope "openid"
                    metadata =
                        OAuthMetadata
                            { issuer = "https://auth.example.com"
                            , authorizationEndpoint = "https://auth.example.com/authorize"
                            , tokenEndpoint = "https://auth.example.com/token"
                            , registrationEndpoint = Just "https://auth.example.com/register"
                            , userInfoEndpoint = Nothing
                            , jwksUri = Nothing
                            , scopesSupported = Just [openidScope]
                            , responseTypesSupported = [ResponseCode]
                            , grantTypesSupported = Just [GrantAuthorizationCode]
                            , tokenEndpointAuthMethodsSupported = Nothing
                            , codeChallengeMethodsSupported = Just [S256]
                            }

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

                let decoded = decode (encode json) :: Maybe OAuthMetadata

                case decoded of
                    Just metadata -> do
                        issuer metadata `shouldBe` "https://auth.example.com"
                        authorizationEndpoint metadata `shouldBe` "https://auth.example.com/authorize"
                        tokenEndpoint metadata `shouldBe` "https://auth.example.com/token"
                        responseTypesSupported metadata `shouldBe` [ResponseCode]
                    Nothing -> expectationFailure "Failed to decode RFC 8414 JSON"

    describe "ProtectedResourceMetadata" $ do
        context "RFC 9728: Snake_case JSON serialization" $ do
            it "serializes required fields with snake_case keys" $ do
                let metadata =
                        ProtectedResourceMetadata
                            { prResource = "https://api.example.com"
                            , prAuthorizationServers = ["https://auth.example.com"]
                            , prScopesSupported = Nothing
                            , prBearerMethodsSupported = Nothing
                            , prResourceName = Nothing
                            , prResourceDocumentation = Nothing
                            }

                let encoded = encode metadata
                let expected =
                        object
                            [ "resource" .= ("https://api.example.com" :: Text)
                            , "authorization_servers" .= [Aeson.String "https://auth.example.com"]
                            ]

                decode encoded `shouldBe` Just expected

            it "serializes all optional fields with snake_case keys" $ do
                let openidScope = Scope "openid"
                    profileScope = Scope "profile"
                    metadata =
                        ProtectedResourceMetadata
                            { prResource = "https://api.example.com"
                            , prAuthorizationServers = ["https://auth.example.com", "https://auth2.example.com"]
                            , prScopesSupported = Just [openidScope, profileScope]
                            , prBearerMethodsSupported = Just ["header", "body"]
                            , prResourceName = Just "Example API"
                            , prResourceDocumentation = Just "https://docs.example.com/api"
                            }

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
                let openidScope = Scope "openid"
                    metadata =
                        ProtectedResourceMetadata
                            { prResource = "https://api.example.com"
                            , prAuthorizationServers = ["https://auth.example.com"]
                            , prScopesSupported = Just [openidScope]
                            , prBearerMethodsSupported = Just ["header"]
                            , prResourceName = Just "Example API"
                            , prResourceDocumentation = Nothing
                            }

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

                let decoded = decode (encode json) :: Maybe ProtectedResourceMetadata

                case decoded of
                    Just metadata -> do
                        prResource metadata `shouldBe` "https://api.example.com"
                        prAuthorizationServers metadata `shouldBe` ["https://auth.example.com"]
                        case prScopesSupported metadata of
                            Just [scope] -> scope `shouldBe` Scope "openid"
                            _ -> expectationFailure "Expected exactly one scope"
                    Nothing -> expectationFailure "Failed to decode RFC 9728 JSON"
