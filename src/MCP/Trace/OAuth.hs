{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : MCP.Trace.OAuth
Description : OAuth 2.0 flow tracing types
Copyright   : (c) 2025
License     : MIT

OAuth tracing types for structured logging of OAuth flows.

IMPORTANT: This module is intentionally MCP-independent to enable
future package separation. Do NOT import MCP-specific modules here.
-}
module MCP.Trace.OAuth (
    OAuthTrace (..),
    renderOAuthTrace,
) where

import Data.Text (Text)

{- | OAuth 2.0 flow events.

This type is semantically independent of MCP for future package separation.
Current implementation is a skeleton placeholder.
-}
data OAuthTrace
    = {- | Placeholder constructor for Phase 2 skeleton.
      Will be replaced with leaf constructors in Phase 4 (T024).
      -}
      OAuthPlaceholder Text
    deriving (Show, Eq)

{- | Render an OAuth trace event to human-readable text.

Current implementation is a stub for Phase 2 skeleton.
-}
renderOAuthTrace :: OAuthTrace -> Text
renderOAuthTrace (OAuthPlaceholder msg) = "[OAuth] " <> msg
