{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : MCP.Trace.StdIO
Description : StdIO transport tracing types
Copyright   : (c) 2025
License     : MIT

StdIO transport tracing types for structured logging of StdIO-specific events.
-}
module MCP.Trace.StdIO
    ( StdIOTrace(..)
    , renderStdIOTrace
    ) where

import Data.Text (Text)
import MCP.Trace.Protocol (ProtocolTrace, renderProtocolTrace)
import MCP.Trace.Server (ServerTrace, renderServerTrace)

-- | StdIO transport-specific events.
--
-- Current implementation is a skeleton with composite constructors.
-- Full implementation with leaf constructors will be added in Phase 3.
data StdIOTrace
    = StdIOPlaceholder
    -- ^ Placeholder constructor for Phase 2 skeleton.
    -- Will be replaced with leaf constructors (StdIOMessageReceived, etc.) in Phase 3.
    | StdIOProtocol ProtocolTrace
    -- ^ Nested protocol events in StdIO context.
    -- This composite constructor is part of the skeleton structure.
    | StdIOServer ServerTrace
    -- ^ Nested server lifecycle events in StdIO context.
    deriving (Show, Eq)

-- | Render a StdIOTrace to human-readable text.
--
-- Current implementation is a stub for Phase 2 skeleton.
-- Delegates to renderProtocolTrace for nested protocol events.
renderStdIOTrace :: StdIOTrace -> Text
renderStdIOTrace StdIOPlaceholder = "[StdIO] (skeleton)"
renderStdIOTrace (StdIOProtocol pt) = "[StdIO:Protocol] " <> renderProtocolTrace pt
renderStdIOTrace (StdIOServer st) = "[StdIO:Server] " <> renderServerTrace st
