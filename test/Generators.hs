{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Module      : Generators
Description : QuickCheck Arbitrary instances for Auth Backend types
Copyright   : (C) 2025 Matthias Pall Gissurarson, PakSCADA LLC
License     : MIT
Maintainer  : mpg@mpg.is, alberto.valverde@pakenergy.com
Stability   : experimental
Portability : GHC

This module provides QuickCheck Arbitrary instances for Auth Backend types.

Most OAuth domain type instances have been moved to their defining modules
(Servant.OAuth2.IDP.Types, Servant.OAuth2.IDP.Auth.Backend, Servant.OAuth2.IDP.Auth.Demo)
to enable QuickCheck as a library dependency with proper dead code elimination.

This module now only contains instances that haven't been migrated yet.
-}
module Generators () where

-- Re-export instances from library modules for backward compatibility
-- Tests can now import Generators () or import the type modules directly
import Servant.OAuth2.IDP.Auth.Backend ()
import Servant.OAuth2.IDP.Auth.Demo ()
import Servant.OAuth2.IDP.Types ()
