{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | test/Functional/OAuthFlowSpec.hs
module Functional.OAuthFlowSpec (spec) where

import Test.Hspec

import MCP.Server (MCPServer (..), MCPServerM)
import MCP.Server.OAuth.Test.Fixtures (referenceTestConfig)
import Servant.OAuth2.IDP.Test.Internal (oauthConformanceSpec)

-- | Minimal MCPServer instance for testing (uses default implementations)
instance MCPServer MCPServerM

spec :: Spec
spec = do
    config <- runIO referenceTestConfig
    oauthConformanceSpec config
