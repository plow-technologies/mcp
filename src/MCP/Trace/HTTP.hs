{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : MCP.Trace.HTTP
Description : HTTP transport tracing types
Copyright   : (c) 2025
License     : MIT

HTTP transport tracing types for structured logging of HTTP-specific events.
-}
module MCP.Trace.HTTP (
    HTTPTrace (..),
    renderHTTPTrace,
) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import MCP.Trace.OAuth (OAuthTrace, renderOAuthTrace)
import MCP.Trace.Protocol (ProtocolTrace, renderProtocolTrace)
import MCP.Trace.Server (ServerTrace, renderServerTrace)

{- | HTTP transport-specific events.

Current implementation is a skeleton with composite constructors.
Full implementation with leaf constructors will be added in Phase 3.
-}
data HTTPTrace
    = {- | Placeholder constructor for Phase 2 skeleton.
      Will be replaced with leaf constructors (HTTPServerStarting, etc.) in Phase 3.
      -}
      HTTPPlaceholder
    | -- | HTTP server starting up
      HTTPServerStarting
        { tracePort :: Int
        , traceBaseUrl :: Text
        }
    | -- | OAuth authentication enabled
      HTTPOAuthEnabled
        { traceAuthEndpoint :: Text
        , traceTokenEndpoint :: Text
        }
    | -- | OAuth providers configured
      HTTPOAuthProviders
        { traceProviderNames :: [Text]
        }
    | -- | PKCE required by MCP spec
      HTTPPKCEEnabled
    | -- | RFC8707 resource parameter debug log
      HTTPResourceParameterDebug
        { traceResourceParam :: Maybe Text
        , traceContext :: Text
        }
    | {- | Nested protocol events in HTTP context.
      This composite constructor is part of the skeleton structure.
      -}
      HTTPProtocol ProtocolTrace
    | {- | Nested OAuth events in HTTP context.
      This composite constructor is part of the skeleton structure.
      -}
      HTTPOAuth OAuthTrace
    | -- | Nested server lifecycle events in HTTP context.
      HTTPServer ServerTrace
    deriving (Show, Eq)

{- | Render an HTTPTrace to human-readable text.

Current implementation is a stub for Phase 2 skeleton.
Delegates to sub-renders for nested events.
-}
renderHTTPTrace :: HTTPTrace -> Text
renderHTTPTrace HTTPPlaceholder = "[HTTP] (skeleton)"
renderHTTPTrace (HTTPServerStarting port baseUrl) =
    "[HTTP] Starting MCP HTTP Server on port " <> T.pack (show port) <> " at " <> baseUrl
renderHTTPTrace (HTTPOAuthEnabled authEp tokenEp) =
    "[HTTP] OAuth authentication enabled - Auth: " <> authEp <> ", Token: " <> tokenEp
renderHTTPTrace (HTTPOAuthProviders providers) =
    "[HTTP] OAuth providers: " <> T.intercalate ", " providers
renderHTTPTrace HTTPPKCEEnabled =
    "[HTTP] PKCE enabled (required by MCP spec)"
renderHTTPTrace (HTTPResourceParameterDebug mResource ctx) =
    "[HTTP] Resource parameter (" <> ctx <> "): " <> fromMaybe "not provided" mResource
renderHTTPTrace (HTTPProtocol pt) = "[HTTP:Protocol] " <> renderProtocolTrace pt
renderHTTPTrace (HTTPOAuth ot) = "[HTTP:OAuth] " <> renderOAuthTrace ot
renderHTTPTrace (HTTPServer st) = "[HTTP:Server] " <> renderServerTrace st
