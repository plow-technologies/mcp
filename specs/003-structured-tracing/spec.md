# Feature Specification: Structured Tracing with plow-log

**Feature Branch**: `003-structured-tracing`
**Created**: 2025-12-10
**Status**: Draft
**Input**: User description: "Add comprehensive richly-typed structured logging/tracing using plow-log and plow-log-async libraries"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Library Consumer Enables Tracing (Priority: P1)

A developer using this MCP library wants to enable structured logging for their MCP server to understand request flow, debug issues, and monitor operations in production.

**Why this priority**: This is the primary use case - without the ability for library consumers to enable and consume traces, all other tracing functionality is useless.

**Independent Test**: Can be fully tested by initializing an MCP server with a tracer, making requests, and verifying trace output appears in the configured handler (e.g., console output).

**Acceptance Scenarios**:

1. **Given** a library consumer has an `IOTracer Text` from plow-log-async's `withAsyncHandleTracer`, **When** they apply `contramap renderMCPTrace` to adapt it for the library's root trace type, **Then** they receive an `IOTracer MCPTrace` suitable for initializing the library.

2. **Given** a configured MCP server with tracing enabled, **When** any MCP operation occurs (request handling, state changes, errors), **Then** relevant trace events are emitted through the provided tracer.

3. **Given** an MCP server running with tracing, **When** trace events are emitted, **Then** the rendered text output includes contextual information (request IDs, method names, client info, timing data as appropriate).

---

### User Story 2 - Trace HTTP Transport Operations (Priority: P2)

A developer running an MCP HTTP server needs visibility into HTTP-specific operations including OAuth flow, request routing, authentication events, and token management.

**Why this priority**: HTTP transport is more complex than StdIO with OAuth, authentication, and web-specific concerns that require dedicated tracing for debugging and monitoring.

**Independent Test**: Can be tested by running the HTTP server with tracing, performing OAuth flows and MCP requests, and verifying HTTP-specific traces appear.

**Acceptance Scenarios**:

1. **Given** an HTTP MCP server with tracing enabled, **When** an OAuth authorization flow occurs, **Then** trace events capture: client registration, authorization request, login attempt, token generation, and any errors.

2. **Given** an HTTP server handling MCP requests, **When** a JSON-RPC request is processed, **Then** traces include HTTP-specific context (endpoint, auth status) alongside the MCP operation details.

---

### User Story 3 - Trace StdIO Transport Operations (Priority: P3)

A developer running an MCP server via StdIO (e.g., for Claude Desktop integration) needs visibility into message handling, parsing, and protocol operations.

**Why this priority**: StdIO is simpler than HTTP but still requires tracing for debugging client integrations and protocol issues.

**Independent Test**: Can be tested by running the StdIO server with tracing, sending JSON-RPC messages via stdin, and verifying traces appear for message lifecycle.

**Acceptance Scenarios**:

1. **Given** a StdIO MCP server with tracing enabled, **When** a JSON-RPC message is received on stdin, **Then** traces capture: message receipt, parsing result, handler invocation, and response emission.

2. **Given** a StdIO server encountering malformed input, **When** a parse error occurs, **Then** a trace event captures the error with relevant context for debugging.

---

### User Story 4 - Selective Trace Filtering (Priority: P4)

A developer wants to control trace verbosity to focus on specific subsystems (e.g., only OAuth traces, or only tool execution traces) without modifying library code.

**Why this priority**: Production systems need trace filtering to manage volume and focus on relevant events. However, basic tracing functionality must work first.

**Independent Test**: Can be tested by providing a filtered tracer (using `filterTracer` with a predicate) and verifying only matching traces are emitted.

**Acceptance Scenarios**:

1. **Given** the hierarchical trace type structure, **When** a developer uses `filterTracer` with a predicate function, **Then** only trace events matching the predicate reach their handler.

2. **Given** multiple trace categories exist (HTTP, OAuth, Protocol, etc.), **When** a developer wants only OAuth traces, **Then** they can apply `filterTracer` with a predicate that matches OAuth trace constructors.

---

### Edge Cases

- What happens when the tracer's async handler is overwhelmed? (Assumption: plow-log-async handles backpressure; library emits traces regardless)
- How does tracing behave when server is shutting down? (Assumption: traces emitted until final cleanup; no special handling needed beyond what plow-log-async provides)
- What happens if trace rendering fails (e.g., Text encoding issues)? (Assumption: rendering functions are total and always produce valid Text)

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: The library MUST define a root trace sum type (`MCPTrace`) that contains constructors for each major subsystem's trace type.
- **FR-002**: Each subsystem (HTTP transport, StdIO transport, OAuth, Protocol handling, Server core) MUST have its own trace type containing richly-typed trace events.
- **FR-003**: Each trace type MUST have a corresponding `render*Trace :: *Trace -> Text` function that produces human-readable text output.
- **FR-004**: Render functions MUST delegate to sub-component render functions when rendering nested trace types.
- **FR-005**: Tracers MUST be threaded explicitly via function arguments or MonadReader environments, NEVER stored in mutable config structs alongside unrelated configuration. A ReaderT environment is acceptable when all consumers of that environment agree on the tracer's type (since ReaderT is a function argument in disguise). Server initialization functions (`runServer`, `runServerHTTP`) MUST accept an `IOTracer` parameter typed to their transport-level trace type (e.g., `IOTracer StdIOTrace`, `IOTracer HTTPTrace`). Library consumers adapt from their root tracer via `contramap`. This explicit threading enables clean composition: callers contramap for callees, avoiding two sources of truth.
- **FR-006**: Tracer type granularity MUST align with architectural boundaries (module/ReaderT environment/layer). Each component receives an `IOTracer` typed to its own trace type; callers adapt via `contramap` with the relevant trace constructor. **Composability rule**: Child components MUST be agnostic of parent trace types. Transport traces (StdIOTrace, HTTPTrace) MUST embed ServerTrace and ProtocolTrace via composite constructors (e.g., `StdIOServer ServerTrace`), enabling transports to contramap down to child tracers.
- **FR-007**: Trace events MUST capture relevant contextual information as typed values (e.g., `RequestId`, method names, error details) rather than pre-formatted strings.
- **FR-008**: The library MUST use `traceWith` from plow-log to emit trace events at appropriate points in the codebase.
- **FR-009**: The library MUST trace the following key operations at minimum:
  - Server initialization and shutdown
  - Request receipt (entry-point, not exit; HTTP results implicit via status code)
  - Handler invocation entry (per MCP method)
  - Branch decisions within multi-step IO actions (Either results, sum type outcomes)
  - Error conditions (parse errors, protocol errors, handler errors)
  - OAuth operations (for HTTP transport): registration, authorization, token exchange
  - State changes (initialization status, capability negotiation)
- **FR-010**: OAuth trace types MUST be semantically independent of MCP (no MCP-specific terminology or types in OAuth traces) to support future package separation. MCP trace types MAY reference OAuth traces since OAuth is part of the MCP auth specification.
- **FR-011**: Trace emissions MUST only occur in code with `MonadIO` constraint (at the IO boundary). Pure functions MUST NOT attempt to trace; instead, their results are traced by the calling IO code at the boundary. **CRITICAL**: `unsafePerformIO` MUST NEVER be used to call `traceWith` or any tracing function. This prohibition is absolute—no exceptions.

### Key Entities

- **MCPTrace**: Root trace sum type containing constructors for all subsystem traces
- **ServerTrace**: Core server lifecycle events (init, shutdown, state changes)
- **ProtocolTrace**: JSON-RPC message handling events
- **HTTPTrace**: HTTP transport-specific events (requests, responses, routing)
- **OAuthTrace**: OAuth flow events (registration, authorization, tokens) - semantically independent of MCP for future package separation
- **StdIOTrace**: StdIO transport-specific events (message I/O, parsing)

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Developers can enable tracing with fewer than 5 lines of setup code using plow-log-async.
- **SC-002**: All MCP request/response cycles produce at least one trace event visible to the configured handler.
- **SC-003**: OAuth authentication flows (HTTP transport) produce trace events for each major step (registration, authorization, token exchange).
- **SC-004**: Error trace events are self-contained for diagnosis, including request ID, method, error type, and causal chain as appropriate.
- **SC-005**: Trace rendering produces human-readable output that includes event type, relevant identifiers, and contextual data.
- **SC-006**: The trace type hierarchy allows filtering to any subsystem via `filterTracer` with appropriate predicates.

## Clarifications

### Session 2025-12-10

- Q: Is `contramap` used for filtering traces? → A: No. `contramap` composes tracers into a tree/DAG (adapts trace types). `filterTracer` is used for selective filtering.
- Q: Should OAuth trace types reference MCP concepts? → A: No. OAuth trace types MUST be semantically independent of MCP (future package split). MCP traces MAY reference OAuth (per auth spec).
- Q: Can tracing occur in pure code? → A: No. `traceWith` requires `MonadIO`. Traces MUST be emitted at the IO boundary, not inside pure functions. Pure results are traced at the boundary that can `liftIO`.
- Q: What granularity for tracer types? → A: Align with architectural boundaries (module/ReaderT environment/layer). Each component receives `IOTracer ComponentTrace`; callers adapt via `contramap`.
- Q: Trace granularity for handler invocations? → A: Entry-point tracing (not entry/exit bookends). Also trace branch decisions within multi-step IO actions (Either results, sum type outcomes). HTTP results implicit via status code.
- Q: What constitutes "sufficient context" for error traces? → A: Self-contained for diagnosis: request ID, method, error type, and causal chain (using best judgement).
- Q: Should transport traces embed server/protocol traces? → A: Yes. Transport traces (StdIOTrace, HTTPTrace) MUST contain composite constructors (StdIOServer, HTTPServer) to embed ServerTrace. This enables proper contramap threading where callers adapt tracers downward, and callees remain agnostic of parent trace types.
- Q: Should ServerConfig have separate server and protocol tracers? → A: No. ServerConfig has ONE tracer (`IOTracer ServerTrace`). Transport contramaps via StdIOServer/HTTPServer. Redundant leaves like StdIOServerInit are removed - use `StdIOServer (ServerInit ...)` instead.
- Q: Should tracers be stored in config structs or passed as arguments? → A: Thread explicitly via function arguments or MonadReader environments, NEVER in mutable config structs alongside unrelated config. ReaderT is acceptable when all environment consumers agree on tracer type (ReaderT is a function argument in disguise). Application creates tracer; library threads it explicitly. Clean composition requires callers to contramap for callees.
- Q: Can `unsafePerformIO` be used to enable tracing in pure code? → A: **Absolutely not.** `unsafePerformIO` MUST NEVER be used to call `traceWith` or any tracing function. Tracing is exclusively for code with `MonadIO` access. This prohibition is non-negotiable.

## Assumptions

- The plow-log and plow-log-async libraries are available and compatible with the project's GHC version.
- Library consumers are familiar with the contravariant functor pattern for adapting tracers.
- Trace events are emitted via `liftIO` at IO boundaries; async buffering is handled by plow-log-async.
- The `IOTracer` type from plow-log is the standard interface; no custom tracer types are needed.
- Trace rendering is deterministic and side-effect free.
