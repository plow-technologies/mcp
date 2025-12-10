{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : MCP.Trace.Protocol
Description : JSON-RPC protocol handling tracing types
Copyright   : (c) 2025
License     : MIT

Protocol tracing types for structured logging of JSON-RPC message handling.
-}
module MCP.Trace.Protocol
    ( ProtocolTrace(..)
    , renderProtocolTrace
    ) where

import Data.Text (Text)

-- | JSON-RPC protocol handling events.
--
-- Current implementation is a skeleton placeholder.
-- Full implementation with leaf constructors will be added in Phase 3.
data ProtocolTrace
    = ProtocolPlaceholder
    -- ^ Placeholder constructor for Phase 2 skeleton.
    -- Will be replaced with leaf constructors (ProtocolRequestReceived, etc.) in Phase 3.
    deriving (Show, Eq)

-- | Render a ProtocolTrace to human-readable text.
--
-- Current implementation is a stub for Phase 2 skeleton.
renderProtocolTrace :: ProtocolTrace -> Text
renderProtocolTrace ProtocolPlaceholder = "[Protocol] (skeleton)"
