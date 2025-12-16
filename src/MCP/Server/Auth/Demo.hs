{- |
Module      : MCP.Server.Auth.Demo
Description : Compatibility re-export for Auth.Demo
Copyright   : (C) 2025 Matthias Pall Gissurarson
License     : MIT
Maintainer  : mpg@mpg.is
Stability   : experimental
Portability : GHC

This module re-exports "Servant.OAuth2.IDP.Auth.Demo" for backward compatibility.

New code should import from "Servant.OAuth2.IDP.Auth.Demo" directly.
-}
module MCP.Server.Auth.Demo (
    module Servant.OAuth2.IDP.Auth.Demo,
) where

import Servant.OAuth2.IDP.Auth.Demo
