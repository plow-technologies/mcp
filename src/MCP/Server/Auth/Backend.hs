{- |
Module      : MCP.Server.Auth.Backend
Description : Compatibility re-export for Auth.Backend
Copyright   : (C) 2025 Matthias Pall Gissurarson
License     : MIT
Maintainer  : mpg@mpg.is
Stability   : experimental
Portability : GHC

This module re-exports "Servant.OAuth2.IDP.Auth.Backend" for backward compatibility.

New code should import from "Servant.OAuth2.IDP.Auth.Backend" directly.
-}
module MCP.Server.Auth.Backend (
    module Servant.OAuth2.IDP.Auth.Backend,
) where

import Servant.OAuth2.IDP.Auth.Backend
