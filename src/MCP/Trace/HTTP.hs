{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : MCP.Trace.HTTP
Description : HTTP transport tracing types
Copyright   : (c) 2025
License     : MIT

HTTP transport tracing types for structured logging of HTTP-specific events.
-}
module MCP.Trace.HTTP
    ( HTTPTrace(..)
    , renderHTTPTrace
    ) where

import Data.Text (Text)
import MCP.Trace.OAuth (OAuthTrace, renderOAuthTrace)
import MCP.Trace.Protocol (ProtocolTrace, renderProtocolTrace)
import MCP.Trace.Server (ServerTrace, renderServerTrace)

-- | HTTP transport-specific events.
--
-- Current implementation is a skeleton with composite constructors.
-- Full implementation with leaf constructors will be added in Phase 3.
data HTTPTrace
    = HTTPPlaceholder
    -- ^ Placeholder constructor for Phase 2 skeleton.
    -- Will be replaced with leaf constructors (HTTPServerStarting, etc.) in Phase 3.
    | HTTPProtocol ProtocolTrace
    -- ^ Nested protocol events in HTTP context.
    -- This composite constructor is part of the skeleton structure.
    | HTTPOAuth OAuthTrace
    -- ^ Nested OAuth events in HTTP context.
    -- This composite constructor is part of the skeleton structure.
    | HTTPServer ServerTrace
    -- ^ Nested server lifecycle events in HTTP context.
    deriving (Show, Eq)

-- | Render an HTTPTrace to human-readable text.
--
-- Current implementation is a stub for Phase 2 skeleton.
-- Delegates to sub-renders for nested events.
renderHTTPTrace :: HTTPTrace -> Text
renderHTTPTrace HTTPPlaceholder = "[HTTP] (skeleton)"
renderHTTPTrace (HTTPProtocol pt) = "[HTTP:Protocol] " <> renderProtocolTrace pt
renderHTTPTrace (HTTPOAuth ot) = "[HTTP:OAuth] " <> renderOAuthTrace ot
renderHTTPTrace (HTTPServer st) = "[HTTP:Server] " <> renderServerTrace st
