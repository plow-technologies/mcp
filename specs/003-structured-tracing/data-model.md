# Data Model: Structured Tracing Types

**Feature**: 003-structured-tracing
**Date**: 2025-12-10

## Trace Type Hierarchy

### Root Type: MCPTrace

**Module**: `MCP.Trace.Types`

```haskell
-- | Root trace type for the MCP library.
-- Composes all subsystem traces via constructors.
data MCPTrace
    = MCPServer ServerTrace      -- ^ Core server lifecycle events
    | MCPProtocol ProtocolTrace  -- ^ JSON-RPC message handling
    | MCPStdIO StdIOTrace        -- ^ StdIO transport events
    | MCPHttp HTTPTrace          -- ^ HTTP transport events (includes OAuth)
    deriving (Show, Eq)
```

**Render Function**:
```haskell
renderMCPTrace :: MCPTrace -> Text
```

---

### ServerTrace

**Module**: `MCP.Trace.Server`

```haskell
-- | Server lifecycle and state change events.
data ServerTrace
    = ServerInit
        { serverName :: Text
        , serverVersion :: Text
        }
    | ServerShutdown
    | ServerInitialized
        { clientInfo :: Maybe Text  -- Client name if provided
        }
    | ServerCapabilityNegotiation
        { negotiatedCapabilities :: [Text]
        }
    | ServerStateChange
        { fromState :: Text
        , toState :: Text
        }
    deriving (Show, Eq)
```

**Render Function**:
```haskell
renderServerTrace :: ServerTrace -> Text
```

---

### ProtocolTrace

**Module**: `MCP.Trace.Protocol`

```haskell
-- | JSON-RPC protocol handling events.
data ProtocolTrace
    = ProtocolRequestReceived
        { requestId :: Text
        , method :: Text
        }
    | ProtocolResponseSent
        { requestId :: Text
        , isError :: Bool
        }
    | ProtocolNotificationReceived
        { method :: Text
        }
    | ProtocolParseError
        { errorMessage :: Text
        , rawInput :: Maybe Text  -- Truncated for safety
        }
    | ProtocolMethodNotFound
        { requestId :: Text
        , method :: Text
        }
    | ProtocolInvalidParams
        { requestId :: Text
        , method :: Text
        , errorDetail :: Text
        }
    deriving (Show, Eq)
```

**Render Function**:
```haskell
renderProtocolTrace :: ProtocolTrace -> Text
```

---

### StdIOTrace

**Module**: `MCP.Trace.StdIO`

```haskell
-- | StdIO transport-specific events.
-- Embeds ServerTrace and ProtocolTrace for events occurring in StdIO context.
data StdIOTrace
    = StdIOServer ServerTrace      -- Embedded server lifecycle events
    | StdIOProtocol ProtocolTrace  -- Embedded protocol events
    | StdIOMessageReceived
        { messageSize :: Int  -- bytes
        }
    | StdIOMessageSent
        { messageSize :: Int
        }
    | StdIOReadError
        { errorMessage :: Text
        }
    | StdIOEOF
    deriving (Show, Eq)
```

**Render Function**:
```haskell
renderStdIOTrace :: StdIOTrace -> Text
-- Delegates to renderServerTrace for StdIOServer
-- Delegates to renderProtocolTrace for StdIOProtocol
```

**Composability**: Transport layer contramaps to provide child tracers:
```haskell
-- In StdIO transport:
let serverTracer = contramap StdIOServer stdioTracer   -- IOTracer ServerTrace
let protocolTracer = contramap StdIOProtocol stdioTracer -- IOTracer ProtocolTrace
-- Pass these to server core; server core never knows about StdIOTrace
```

---

### HTTPTrace

**Module**: `MCP.Trace.HTTP`

```haskell
-- | HTTP transport-specific events.
-- Embeds ServerTrace, ProtocolTrace, and OAuthTrace for events occurring in HTTP context.
data HTTPTrace
    = HTTPServer ServerTrace       -- Embedded server lifecycle events
    | HTTPProtocol ProtocolTrace   -- Embedded protocol events
    | HTTPOAuth OAuthTrace         -- Embedded OAuth events
    | HTTPServerStarting
        { port :: Int
        , baseUrl :: Text
        }
    | HTTPServerStarted
    | HTTPRequestReceived
        { path :: Text
        , method :: Text
        , hasAuth :: Bool
        }
    | HTTPAuthRequired
        { path :: Text
        }
    | HTTPAuthSuccess
        { userId :: Text
        }
    | HTTPAuthFailure
        { reason :: Text
        }
    deriving (Show, Eq)
```

**Render Function**:
```haskell
renderHTTPTrace :: HTTPTrace -> Text
-- Delegates to renderServerTrace for HTTPServer
-- Delegates to renderProtocolTrace for HTTPProtocol
-- Delegates to renderOAuthTrace for HTTPOAuth
```

**Composability**: Transport layer contramaps to provide child tracers:
```haskell
-- In HTTP transport:
let serverTracer = contramap HTTPServer httpTracer     -- IOTracer ServerTrace
let protocolTracer = contramap HTTPProtocol httpTracer -- IOTracer ProtocolTrace
let oauthTracer = contramap HTTPOAuth httpTracer       -- IOTracer OAuthTrace
-- Pass these to server core and auth module; they never know about HTTPTrace
```

---

### OAuthTrace (MCP-Independent)

**Module**: `MCP.Trace.OAuth`

**IMPORTANT**: This module MUST NOT import any MCP-specific types or modules.
This ensures future package separation is possible.

```haskell
-- | OAuth 2.0 flow events.
-- This type is semantically independent of MCP for future package separation.
data OAuthTrace
    = OAuthClientRegistration
        { clientId :: Text
        , clientName :: Text
        }
    | OAuthAuthorizationRequest
        { clientId :: Text
        , scopes :: [Text]
        , hasState :: Bool
        }
    | OAuthLoginPageServed
        { sessionId :: Text
        }
    | OAuthLoginAttempt
        { username :: Text
        , success :: Bool
        }
    | OAuthAuthorizationGranted
        { clientId :: Text
        , userId :: Text
        }
    | OAuthAuthorizationDenied
        { clientId :: Text
        , reason :: Text
        }
    | OAuthTokenExchange
        { grantType :: Text  -- "authorization_code" or "refresh_token"
        , success :: Bool
        }
    | OAuthTokenRefresh
        { success :: Bool
        }
    | OAuthSessionExpired
        { sessionId :: Text
        }
    | OAuthValidationError
        { errorType :: Text
        , errorDetail :: Text
        }
    deriving (Show, Eq)
```

**Render Function**:
```haskell
renderOAuthTrace :: OAuthTrace -> Text
```

---

## Type Relationships

```
┌─────────────────────────────────────────────────────────────────┐
│                         MCPTrace                                │
│  ┌──────────────────────────────┬──────────────────────────┐   │
│  │ MCPStdIO StdIOTrace          │ MCPHttp HTTPTrace        │   │
│  │  ├─ StdIOServer ServerTrace  │  ├─ HTTPServer ServerTrace│  │
│  │  ├─ StdIOProtocol Protocol   │  ├─ HTTPProtocol Protocol │  │
│  │  ├─ StdIOMessageReceived     │  ├─ HTTPOAuth OAuthTrace  │  │
│  │  ├─ StdIOMessageSent         │  ├─ HTTPServerStarting    │  │
│  │  ├─ StdIOReadError           │  ├─ HTTPServerStarted     │  │
│  │  └─ StdIOEOF                 │  ├─ HTTPRequestReceived   │  │
│  │                              │  ├─ HTTPAuthRequired      │  │
│  │                              │  ├─ HTTPAuthSuccess       │  │
│  │                              │  └─ HTTPAuthFailure       │  │
│  └──────────────────────────────┴──────────────────────────┘   │
└─────────────────────────────────────────────────────────────────┘

Composability: Traces nest according to the call stack.
- App main → MCPTrace (knows root type only)
- Transport → contramap MCPStdIO/MCPHttp → IOTracer StdIOTrace/HTTPTrace
- Server core → contramap StdIOServer/HTTPServer → IOTracer ServerTrace
- Protocol → contramap StdIOProtocol/HTTPProtocol → IOTracer ProtocolTrace

Each layer only knows its own trace type. Callers adapt DOWN via contramap.
```

**NOTE**: `MCPServer` and `MCPProtocol` constructors at the MCPTrace level may be removed in a future revision since all server/protocol events occur within a transport context. They currently exist for testing convenience but violate the "traces reflect call stack" principle.

## Validation Rules

1. **All trace types derive Show, Eq** - enables testing and debugging
2. **No MCP imports in OAuthTrace module** - enforces FR-010
3. **All fields use Text for strings** - no lazy Text, no String
4. **Sensitive data (passwords, tokens) MUST NOT appear in traces**
5. **Raw input in ParseError truncated to 100 chars max**

## State Transitions

N/A - Trace types are immutable data carriers, not stateful entities.

## Re-exports

**Module**: `MCP.Trace.Types`

```haskell
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
```

This allows consumers to import everything from one module while maintaining internal modularity.
