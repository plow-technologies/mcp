{-# LANGUAGE LambdaCase #-}

{- |
Module      : MCP.Trace.Types
Description : Root trace type hierarchy for MCP library
Copyright   : (c) 2025
License     : MIT

Root trace type that composes all subsystem traces. Provides a unified
tracing interface with re-exports of all trace types for convenience.
-}
module MCP.Trace.Types
    ( -- * Root Type
      MCPTrace(..)
    , renderMCPTrace
      -- * Subsystem Types (re-exports)
    , ServerTrace(..)
    , renderServerTrace
    , ProtocolTrace(..)
    , renderProtocolTrace
    , StdIOTrace(..)
    , renderStdIOTrace
    , HTTPTrace(..)
    , renderHTTPTrace
    , OAuthTrace(..)
    , renderOAuthTrace
    ) where

import Data.Text (Text)
import MCP.Trace.HTTP (HTTPTrace(..), renderHTTPTrace)
import MCP.Trace.OAuth (OAuthTrace(..), renderOAuthTrace)
import MCP.Trace.Protocol (ProtocolTrace(..), renderProtocolTrace)
import MCP.Trace.Server (ServerTrace(..), renderServerTrace)
import MCP.Trace.StdIO (StdIOTrace(..), renderStdIOTrace)

-- | Root trace type for the MCP library.
--
-- Composes all subsystem traces via constructors.
-- Current implementation is a skeleton for Phase 2.
data MCPTrace
    = MCPServer ServerTrace      -- ^ Core server lifecycle events
    | MCPProtocol ProtocolTrace  -- ^ JSON-RPC message handling
    | MCPStdIO StdIOTrace        -- ^ StdIO transport events
    | MCPHttp HTTPTrace          -- ^ HTTP transport events (includes OAuth)
    deriving (Show, Eq)

-- | Render an MCPTrace to human-readable text.
--
-- Delegates to subsystem render functions.
renderMCPTrace :: MCPTrace -> Text
renderMCPTrace = \case
    MCPServer t   -> renderServerTrace t
    MCPProtocol t -> renderProtocolTrace t
    MCPStdIO t    -> renderStdIOTrace t
    MCPHttp t     -> renderHTTPTrace t
