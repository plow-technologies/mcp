{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : MCP.Trace.Server
Description : Server lifecycle tracing types
Copyright   : (c) 2025
License     : MIT

Server tracing types for structured logging of server lifecycle and state changes.
-}
module MCP.Trace.Server
    ( ServerTrace(..)
    , renderServerTrace
    ) where

import Data.Text (Text)
import qualified Data.Text as T

-- | Server lifecycle and state change events.
--
-- Current implementation is a skeleton placeholder.
-- Full implementation with leaf constructors will be added in Phase 3.
{-# HLINT ignore "Use newtype instead of data" #-}
data ServerTrace
    = ServerPlaceholder
    -- ^ Placeholder constructor for Phase 2 skeleton.
    -- Will be replaced with leaf constructors (ServerInit, ServerShutdown, etc.) in Phase 3.
    deriving (Show, Eq)

-- | Render a ServerTrace to human-readable text.
--
-- Current implementation is a stub for Phase 2 skeleton.
renderServerTrace :: ServerTrace -> Text
renderServerTrace ServerPlaceholder = "[Server] (skeleton)"
