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
import qualified Data.Text as Text

-- | Server lifecycle and state change events.
data ServerTrace
    = ServerInit
        { serverName :: Text
        , serverVersion :: Text
        }
    -- ^ Server initialization event
    | ServerShutdown
    -- ^ Server shutdown event
    | ServerInitialized
        { clientInfo :: Maybe Text  -- ^ Client name if provided
        }
    -- ^ Server initialized with client connection
    | ServerCapabilityNegotiation
        { negotiatedCapabilities :: [Text]
        }
    -- ^ Capability negotiation completed
    | ServerStateChange
        { fromState :: Text
        , toState :: Text
        }
    -- ^ Server state transition
    deriving (Show, Eq)

-- | Render a ServerTrace to human-readable text.
renderServerTrace :: ServerTrace -> Text
renderServerTrace (ServerInit name version) =
    "[Server] Init: " <> name <> " v" <> version
renderServerTrace ServerShutdown =
    "[Server] Shutdown"
renderServerTrace (ServerInitialized maybeClient) =
    case maybeClient of
        Just client -> "[Server] Initialized with client: " <> client
        Nothing -> "[Server] Initialized (no client info)"
renderServerTrace (ServerCapabilityNegotiation caps) =
    "[Server] Capabilities negotiated: [" <> Text.intercalate ", " caps <> "]"
renderServerTrace (ServerStateChange from to) =
    "[Server] State change: " <> from <> " -> " <> to
