-- | test/Functional/OAuthFlowSpec.hs
module Functional.OAuthFlowSpec (spec) where

import Test.Hspec

import MCP.Server.OAuth.Test.Fixtures (referenceTestConfig)
import MCP.Server.OAuth.Test.Internal (oauthConformanceSpec)

spec :: Spec
spec = do
    config <- runIO referenceTestConfig
    oauthConformanceSpec config
