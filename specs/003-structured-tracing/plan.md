# Implementation Plan: Structured Tracing with plow-log

**Branch**: `003-structured-tracing` | **Date**: 2025-12-10 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/003-structured-tracing/spec.md`

## Summary

Add comprehensive richly-typed structured logging/tracing to the MCP library using plow-log and plow-log-async. The implementation follows a hierarchical trace type pattern where each architectural boundary (module/ReaderT environment/layer) has its own trace type, composed via `contramap` into a root trace type. Library consumers use `withAsyncHandleTracer` to obtain an `IOTracer Text`, then adapt it via `contramap renderMCPTrace` for library initialization.

**Key constraint from user**: Implement composite trace types and plumbing first (skeleton that compiles), then fill in leaf trace constructors and render functions incrementally.

## Technical Context

**Language/Version**: Haskell GHC2021 (GHC 9.4+)
**Primary Dependencies**:
- plow-log (^0.1.6) - contravariant logging with IOTracer, Tracer, traceWith, filterTracer
- plow-log-async (^0.1.4) - withAsyncHandleTracer for async IO backend
- Existing: aeson, text, servant-server, warp, mtl

**Storage**: N/A (traces emitted to user-provided IOTracer)
**Testing**: cabal test (property tests for render functions, integration tests for trace emission)
**Target Platform**: Linux server, any platform supporting GHC 9.4+
**Project Type**: Single Haskell library with executables
**Performance Goals**: Trace emission must not block request handling (handled by plow-log-async's queue)
**Constraints**: OAuth trace types must be MCP-independent for future package split
**Scale/Scope**: Library enhancement, ~6 trace type modules, ~500 LOC estimated

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

Reference: `.specify/memory/constitution.md`

| Principle | Status | Evidence/Notes |
|-----------|--------|----------------|
| I. Type-Driven Design | ✅ | Trace types designed as sum types before implementation; each subsystem has own type; constructors carry rich typed context (RequestId, method names) not strings |
| II. Deep Module Architecture | ✅ | Each trace module exports only its trace type + render function; internal constructors visible for pattern matching but smart constructors not needed (trace types are data carriers) |
| III. Denotational Semantics | ✅ | render*Trace functions are total, deterministic Text transformations; compositionality law: renderMCPTrace (MCPServer t) = renderServerTrace t |
| IV. Total Functions | ✅ | All render functions total (pattern match all constructors); traceWith from plow-log is total; no partial functions |
| V. Pure Core, Impure Shell | ✅ | Render functions are pure; traceWith calls occur only at IO boundaries (FR-011); trace type construction is pure |
| VI. Property-Based Testing | ✅ | Property tests for render round-trip not applicable (one-way); property tests for render totality (all constructors); golden tests for render output stability |

## Project Structure

### Documentation (this feature)

```text
specs/003-structured-tracing/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── contracts/           # N/A (library, no API contracts)
└── tasks.md             # Phase 2 output (/speckit.tasks command)
```

### Source Code (repository root)

```text
src/
├── MCP/
│   ├── Types.hs              # Existing - MCP protocol types
│   ├── Protocol.hs           # Existing - JSON-RPC types
│   ├── Server.hs             # Existing - MCPServer typeclass (add tracer param)
│   ├── Server/
│   │   ├── StdIO.hs          # Existing - modify to accept IOTracer StdIOTrace
│   │   ├── HTTP.hs           # Existing - modify to accept IOTracer HTTPTrace
│   │   └── Auth.hs           # Existing - OAuth logic (add tracer param)
│   └── Trace/                # NEW - all tracing types and renderers
│       ├── Types.hs          # Root MCPTrace type + re-exports
│       ├── Server.hs         # ServerTrace (init, shutdown, state)
│       ├── Protocol.hs       # ProtocolTrace (JSON-RPC handling)
│       ├── HTTP.hs           # HTTPTrace (HTTP-specific, imports OAuth)
│       ├── StdIO.hs          # StdIOTrace (StdIO-specific)
│       └── OAuth.hs          # OAuthTrace (MCP-independent for package split)

test/
├── Main.hs                   # Existing - test entry
└── Trace/                    # NEW - trace tests
    ├── RenderSpec.hs         # Property tests: render totality
    └── GoldenSpec.hs         # Golden tests: render output stability
```

**Structure Decision**: Single library structure preserved. New `MCP.Trace` module hierarchy added under `src/MCP/Trace/`. OAuth traces in separate module with no MCP imports to support future package extraction.

## Implementation Order (per user constraint)

The implementation MUST follow this order to ensure compilability at each step:

### Phase A: Skeleton (compiles with stub traces)
1. Create trace type modules with root types and composite constructors only
2. Create stub render functions that pattern match composites
3. Thread IOTracer through Server, StdIO, HTTP modules
4. Verify compilation

### Phase B: Leaf traces (incremental)
1. Add leaf constructors to each trace type
2. Implement render cases for each leaf
3. Add traceWith calls at emission sites
4. Verify compilation after each module

## Complexity Tracking

> No Constitution Check violations. Design aligns with all six principles.

| Aspect | Decision | Rationale |
|--------|----------|-----------|
| Separate OAuth traces | OAuthTrace in own module, no MCP imports | FR-010: future package split |
| Trace at IO boundary only | FR-011 compliance | MonadIO constraint on traceWith |
| Entry-point tracing | Not entry/exit bookends | HTTP results implicit via status; trace branch decisions instead |
| Transport embeds child traces | StdIOServer/HTTPServer composites | FR-006: contramap flows DOWN, children agnostic of parents |
| No redundant leaves | Remove StdIOServerInit etc. | Use `StdIOServer (ServerInit ...)` instead |
| Single ServerConfig tracer | `IOTracer ServerTrace` only | Transport contramaps; no Either/combined types needed |

## Tracer Threading Architecture

```
App main (IOTracer MCPTrace)
    │
    ├─► contramap MCPStdIO ──► StdIO Transport (IOTracer StdIOTrace)
    │                              │
    │                              ├─► contramap StdIOServer ──► ServerConfig (IOTracer ServerTrace)
    │                              └─► contramap StdIOProtocol ──► Protocol handling (IOTracer ProtocolTrace)
    │
    └─► contramap MCPHttp ──► HTTP Transport (IOTracer HTTPTrace)
                                   │
                                   ├─► contramap HTTPServer ──► ServerConfig (IOTracer ServerTrace)
                                   ├─► contramap HTTPProtocol ──► Protocol handling (IOTracer ProtocolTrace)
                                   └─► contramap HTTPOAuth ──► Auth module (IOTracer OAuthTrace)
```

**Key invariant**: Each component receives `IOTracer OwnTraceType`. Components NEVER know about parent trace types. Contramap happens at call boundaries, not emission sites.
