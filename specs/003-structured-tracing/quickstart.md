# Quickstart: MCP Library Tracing

**Feature**: 003-structured-tracing

## Enabling Tracing (4 lines)

```haskell
import MCP.Server.HTTP (runServerHTTP)
import MCP.Trace.Types (MCPTrace, HTTPTrace(..), renderMCPTrace)
import Plow.Logging (contramap)
import Plow.Logging.Async (withAsyncHandleTracer)
import System.IO (stdout)

main :: IO ()
main = withAsyncHandleTracer stdout 1000 $ \textTracer -> do
    let httpTracer = contramap (renderMCPTrace . MCPHttp) textTracer
    runServerHTTP config httpTracer
```

## Enabling Tracing for StdIO Server

```haskell
import MCP.Server.StdIO (runServer)
import MCP.Trace.Types (MCPTrace, StdIOTrace(..), renderMCPTrace)
import Plow.Logging (contramap)
import Plow.Logging.Async (withAsyncHandleTracer)
import System.IO (stderr)  -- Use stderr to avoid mixing with protocol on stdout

main :: IO ()
main = withAsyncHandleTracer stderr 1000 $ \textTracer -> do
    let stdioTracer = contramap (renderMCPTrace . MCPStdIO) textTracer
    runServer config stdioTracer
```

## Filtering Traces

The library exports predicate helpers in `MCP.Trace.Types` for common filtering scenarios:

| Predicate | Matches |
|-----------|---------|
| `isStdIOTrace` | All StdIO transport traces |
| `isHTTPTrace` | All HTTP transport traces |
| `isOAuthTrace` | OAuth subsystem traces (nested in HTTP) |
| `isErrorTrace` | Error traces from all subsystems |

### Filter to OAuth Events Only

```haskell
import MCP.Trace.Types (MCPTrace, renderMCPTrace, isOAuthTrace)
import Plow.Logging (filterTracer, contramap)
import Plow.Logging.Async (IOTracer)

-- Create filtered tracer using the exported predicate
oauthOnlyTracer :: IOTracer Text -> IOTracer MCPTrace
oauthOnlyTracer textTracer =
    filterTracer isOAuthTrace (contramap renderMCPTrace textTracer)
```

### Filter to Errors Only

```haskell
import MCP.Trace.Types (MCPTrace, renderMCPTrace, isErrorTrace)
import Plow.Logging (filterTracer, contramap)

-- Create error-only tracer
errorTracer :: IOTracer Text -> IOTracer MCPTrace
errorTracer textTracer =
    filterTracer isErrorTrace (contramap renderMCPTrace textTracer)
```

### Combined Filters

Combine predicates with standard boolean operators for fine-grained filtering:

```haskell
import MCP.Trace.Types (MCPTrace, isOAuthTrace, isErrorTrace, isHTTPTrace)
import Plow.Logging (filterTracer)

-- OAuth errors only (OAuth AND error)
oauthErrorsOnly :: MCPTrace -> Bool
oauthErrorsOnly t = isOAuthTrace t && isErrorTrace t

-- All HTTP traces OR any errors (useful for debugging HTTP with error context)
httpOrErrors :: MCPTrace -> Bool
httpOrErrors t = isHTTPTrace t || isErrorTrace t

-- Exclude OAuth traces (show everything except OAuth)
nonOAuthTracer :: IOTracer Text -> IOTracer MCPTrace
nonOAuthTracer textTracer =
    filterTracer (not . isOAuthTrace) (contramap renderMCPTrace textTracer)
```

### Custom Predicates

For filtering not covered by the built-in helpers, define custom predicates:

```haskell
import MCP.Trace.Types

-- Only protocol request/response traces (exclude errors)
isProtocolRequestResponse :: MCPTrace -> Bool
isProtocolRequestResponse (MCPProtocol (ProtocolRequest{})) = True
isProtocolRequestResponse (MCPProtocol (ProtocolResponse{})) = True
isProtocolRequestResponse _ = False
```

## Combining Multiple Outputs

```haskell
import Plow.Logging ((<>))

-- Log to both stdout and a file handle
combinedTracer :: IOTracer Text -> IOTracer Text -> IOTracer Text
combinedTracer t1 t2 = t1 <> t2
```

## Disabling Tracing

```haskell
import Plow.Logging (voidTracer)

-- Use voidTracer for no-op tracing (e.g., in tests)
runServerHTTP config voidTracer
```

## Expected Trace Output Format

```
[HTTP] Server starting on port 8080 (http://localhost:8080)
[HTTP] Server started
[HTTP] [OAuth] Client registered: client_abc123 (My App)
[HTTP] Request received: POST /mcp (authenticated)
[HTTP] [Protocol] Request 1: tools/list
[HTTP] [Protocol] Response 1: success
[Server] Initialized (client: Claude Desktop)
```

## Trace Type Quick Reference

| Prefix | Type | Events |
|--------|------|--------|
| `[Server]` | ServerTrace | init, shutdown, state changes |
| `[Protocol]` | ProtocolTrace | JSON-RPC requests, responses, errors |
| `[StdIO]` | StdIOTrace | message I/O, EOF, read errors |
| `[HTTP]` | HTTPTrace | server lifecycle, auth, requests |
| `[OAuth]` | OAuthTrace | registration, authorization, tokens |

## Testing with Traces

```haskell
import Control.Concurrent.STM (TVar, newTVarIO, readTVarIO, atomically, modifyTVar')
import Plow.Logging (Tracer(..))

-- Capture traces to a list for assertions
capturingTracer :: TVar [MCPTrace] -> IOTracer MCPTrace
capturingTracer var = IOTracer $ Tracer $ \trace ->
    atomically $ modifyTVar' var (trace :)

-- Usage in test
testTracesEmitted :: IO ()
testTracesEmitted = do
    traces <- newTVarIO []
    let tracer = capturingTracer traces
    -- run server operations...
    captured <- readTVarIO traces
    assertBool "Should have server init trace" $
        any isServerInit captured
```

---

## Progress Tracking with Beads

**CRITICAL**: Use `bd` (beads) for ALL progress tracking during implementation. Do NOT use markdown TODOs, task lists, or other tracking methods.

### Epic Structure

This feature uses hierarchical beads under epic `mcp-6k9`:

```
mcp-6k9           [epic]  Epic: Structured Tracing with plow-log
├── mcp-6k9.1     [epic]  Phase 1: Setup
│   ├── mcp-6k9.1.1       T001: Add plow-log dependencies
│   ├── mcp-6k9.1.2       T002: Create Trace directory
│   └── mcp-6k9.1.3       T003: Verify dependencies
├── mcp-6k9.2     [epic]  Phase 2: Foundational - Trace Type Skeleton
│   ├── mcp-6k9.2.1-6     T004-T009: Create trace type skeletons
│   └── mcp-6k9.2.7-11    T010-T014: Thread IOTracer
├── mcp-6k9.3     [epic]  Phase 3: User Story 1 (P1 MVP)
│   └── mcp-6k9.3.1-9     T015-T023: Server/Protocol traces
├── mcp-6k9.4     [epic]  Phase 4: User Story 2 (P2)
│   └── mcp-6k9.4.1-8     T024-T031: HTTP/OAuth traces
├── mcp-6k9.5     [epic]  Phase 5: User Story 3 (P3)
│   └── mcp-6k9.5.1-5     T032-T036: StdIO traces
├── mcp-6k9.6     [epic]  Phase 6: User Story 4 (P4)
│   └── mcp-6k9.6.1-4     T037-T040: Filtering
└── mcp-6k9.7     [epic]  Phase 7: Polish
    └── mcp-6k9.7.1-7     T041-T047: Tests & validation
```

### Quick Reference

```bash
# View ready work (unblocked tasks)
bd ready --json

# Start working on a task
bd update mcp-6k9.1.1 --status in_progress

# Complete a task
bd close mcp-6k9.1.1 --reason "Added plow-log deps to cabal"

# View epic status
bd epic status mcp-6k9

# List all tasks in this epic
bd list | grep "mcp-6k9"

# View task details
bd show mcp-6k9.2.1
```

### Workflow

1. **Check ready work**: `bd ready --json`
2. **Claim task**: `bd update <id> --status in_progress`
3. **Implement**: Follow task description
4. **Complete**: `bd close <id> --reason "Done"`
5. **Commit**: Always commit `.beads/issues.jsonl` with code changes

### Important Notes

- Tasks are organized hierarchically under parent phases
- Dependencies are tracked via `blocks:` relationships
- Phase 2 MUST complete before user story phases can start
- Each user story phase can be completed independently after Phase 2
