{-# LANGUAGE OverloadedStrings #-}

module Servant.OAuth2.IDP.APISpec (spec) where

import Data.Aeson (decode)
import Data.Maybe (isJust, isNothing)
import Servant.OAuth2.IDP.API (ClientRegistrationRequest (..))
import Test.Hspec

spec :: Spec
spec = do
    describe "FR-064: ClientRegistrationRequest with NonEmpty lists" $ do
        context "Valid JSON with non-empty lists" $ do
            it "parses valid registration request with single redirect_uri" $ do
                let json =
                        "{\
                        \  \"client_name\": \"Test Client\",\
                        \  \"redirect_uris\": [\"https://example.com/callback\"],\
                        \  \"grant_types\": [\"authorization_code\"],\
                        \  \"response_types\": [\"code\"],\
                        \  \"token_endpoint_auth_method\": \"none\"\
                        \}"
                    decoded = decode json :: Maybe ClientRegistrationRequest
                decoded `shouldSatisfy` isJust

            it "parses valid registration request with multiple redirect_uris" $ do
                let json =
                        "{\
                        \  \"client_name\": \"Test Client\",\
                        \  \"redirect_uris\": [\"https://example.com/callback1\", \"https://example.com/callback2\"],\
                        \  \"grant_types\": [\"authorization_code\", \"refresh_token\"],\
                        \  \"response_types\": [\"code\", \"token\"],\
                        \  \"token_endpoint_auth_method\": \"none\"\
                        \}"
                    decoded = decode json :: Maybe ClientRegistrationRequest
                decoded `shouldSatisfy` isJust

        context "Empty lists should fail to parse (NonEmpty enforcement)" $ do
            it "rejects empty redirect_uris array" $ do
                let json =
                        "{\
                        \  \"client_name\": \"Test Client\",\
                        \  \"redirect_uris\": [],\
                        \  \"grant_types\": [\"authorization_code\"],\
                        \  \"response_types\": [\"code\"],\
                        \  \"token_endpoint_auth_method\": \"none\"\
                        \}"
                    decoded = decode json :: Maybe ClientRegistrationRequest
                decoded `shouldSatisfy` isNothing

            it "rejects empty grant_types array" $ do
                let json =
                        "{\
                        \  \"client_name\": \"Test Client\",\
                        \  \"redirect_uris\": [\"https://example.com/callback\"],\
                        \  \"grant_types\": [],\
                        \  \"response_types\": [\"code\"],\
                        \  \"token_endpoint_auth_method\": \"none\"\
                        \}"
                    decoded = decode json :: Maybe ClientRegistrationRequest
                decoded `shouldSatisfy` isNothing

            it "rejects empty response_types array" $ do
                let json =
                        "{\
                        \  \"client_name\": \"Test Client\",\
                        \  \"redirect_uris\": [\"https://example.com/callback\"],\
                        \  \"grant_types\": [\"authorization_code\"],\
                        \  \"response_types\": [],\
                        \  \"token_endpoint_auth_method\": \"none\"\
                        \}"
                    decoded = decode json :: Maybe ClientRegistrationRequest
                decoded `shouldSatisfy` isNothing
