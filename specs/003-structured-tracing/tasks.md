# Tasks: Structured Tracing with plow-log

**Input**: Design documents from `/specs/003-structured-tracing/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, quickstart.md

**Progress Tracking**: Use `bd` (beads) for all progress tracking. See [Progress Tracking with Beads](#progress-tracking-with-beads) section.

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3, US4)
- Include exact file paths in descriptions

## Path Conventions

- **Source**: `src/MCP/` at repository root
- **Tests**: `test/` at repository root
- **New trace modules**: `src/MCP/Trace/`

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Project initialization and dependencies

- [X] T001 Add plow-log and plow-log-async dependencies to `mcp.cabal` build-depends
- [X] T002 [P] Create `src/MCP/Trace/` directory structure
- [X] T003 Verify dependencies resolve with `cabal build --dry-run`

---

## Phase 2: Foundational - Trace Type Skeleton (CRITICAL)

**Purpose**: Create trace type hierarchy with composite constructors ONLY (no leaf traces yet). This MUST compile before any user story work begins.

**⚠️ CRITICAL**: Per user constraint, skeleton types and plumbing MUST be implemented first.

### Phase 2A: Trace Types (Skeleton)

- [X] T004 [P] Create `src/MCP/Trace/OAuth.hs` with OAuthTrace type (composite placeholder only), renderOAuthTrace stub
- [ ] T005 [P] Create `src/MCP/Trace/Protocol.hs` with ProtocolTrace type (composite placeholder only), renderProtocolTrace stub
- [ ] T006 [P] Create `src/MCP/Trace/Server.hs` with ServerTrace type (composite placeholder only), renderServerTrace stub
- [ ] T007 Create `src/MCP/Trace/StdIO.hs` with StdIOTrace type (includes StdIOProtocol composite), renderStdIOTrace stub (depends on T005)
- [ ] T008 Create `src/MCP/Trace/HTTP.hs` with HTTPTrace type (includes HTTPProtocol, HTTPOAuth composites), renderHTTPTrace stub (depends on T004, T005)
- [ ] T009 Create `src/MCP/Trace/Types.hs` with MCPTrace root type, renderMCPTrace, re-exports (depends on T004-T008)

### Phase 2B: Thread Tracers Through Existing Modules

- [ ] T010 Add IOTracer parameter to `MCP.Server.StdIO.runServer` function signature in `src/MCP/Server/StdIO.hs`
- [ ] T011 Add IOTracer parameter to `MCP.Server.HTTP.runServerHTTP` function signature in `src/MCP/Server/HTTP.hs`
- [ ] T012 Add IOTracer parameter to OAuth handlers in `src/MCP/Server/Auth.hs`
- [ ] T013 Update `mcp.cabal` to expose new `MCP.Trace.*` modules
- [ ] T014 Verify skeleton compiles with `cabal build`

**Checkpoint**: Skeleton compiles. All trace types exist with composite constructors. IOTracer threaded through entry points.

---

## Phase 3: User Story 1 - Library Consumer Enables Tracing (Priority: P1) 🎯 MVP

**Goal**: Library consumers can enable tracing with <5 lines of setup code and see trace output.

**Independent Test**: Initialize MCP server with tracer via `withAsyncHandleTracer`, make a request, verify trace appears in console.

**Spec Reference**: [spec.md - User Story 1](./spec.md#user-story-1---library-consumer-enables-tracing-priority-p1)

### Implementation for User Story 1

- [ ] T015 [P] [US1] Add ServerTrace leaf constructors (ServerInit, ServerShutdown, ServerInitialized, etc.) in `src/MCP/Trace/Server.hs`
- [ ] T016 [P] [US1] Add ProtocolTrace leaf constructors (ProtocolRequestReceived, ProtocolResponseSent, etc.) in `src/MCP/Trace/Protocol.hs`
- [ ] T017 [US1] Implement renderServerTrace for all constructors in `src/MCP/Trace/Server.hs`
- [ ] T018 [US1] Implement renderProtocolTrace for all constructors in `src/MCP/Trace/Protocol.hs`
- [ ] T019 [US1] Update renderMCPTrace to delegate to subsystem renders in `src/MCP/Trace/Types.hs`
- [ ] T020 [US1] Add traceWith calls for server init/shutdown in `src/MCP/Server.hs`
- [ ] T021 [US1] Add traceWith calls for request/response handling in protocol handling code
- [ ] T022 [US1] Update example executables to demonstrate tracing setup in `examples/http-server.hs` and `app/Main.hs`
- [ ] T023 [US1] Verify SC-001: <5 lines setup works per `quickstart.md`

**Checkpoint**: User Story 1 complete. Basic tracing works for any MCP server.

---

## Phase 4: User Story 2 - Trace HTTP Transport Operations (Priority: P2)

**Goal**: HTTP-specific operations (OAuth, routing, auth) emit traces.

**Independent Test**: Run HTTP server with tracing, perform OAuth flow, verify HTTP and OAuth traces appear.

**Spec Reference**: [spec.md - User Story 2](./spec.md#user-story-2---trace-http-transport-operations-priority-p2)

### Implementation for User Story 2

- [X] T024 [P] [US2] Add OAuthTrace leaf constructors (OAuthClientRegistration, OAuthAuthorizationRequest, etc.) in `src/MCP/Trace/OAuth.hs`
- [X] T025 [P] [US2] Add HTTPTrace leaf constructors (HTTPServerStarting, HTTPRequestReceived, etc.) in `src/MCP/Trace/HTTP.hs`
- [X] T026 [US2] Implement renderOAuthTrace for all constructors in `src/MCP/Trace/OAuth.hs`
- [ ] T027 [US2] Implement renderHTTPTrace for all constructors (including delegation to OAuth) in `src/MCP/Trace/HTTP.hs`
- [ ] T028 [US2] Add traceWith calls in OAuth handlers in `src/MCP/Server/Auth.hs`
- [ ] T029 [US2] Add traceWith calls in HTTP server startup/request handling in `src/MCP/Server/HTTP.hs`
- [ ] T030 [US2] Verify OAuth traces are MCP-independent (FR-010) - no MCP imports in `src/MCP/Trace/OAuth.hs`
- [ ] T031 [US2] Verify SC-003: OAuth flows produce traces for each step

**Checkpoint**: User Story 2 complete. HTTP and OAuth tracing works.

---

## Phase 5: User Story 3 - Trace StdIO Transport Operations (Priority: P3)

**Goal**: StdIO transport operations (message I/O, parsing) emit traces.

**Independent Test**: Run StdIO server with tracing, send JSON-RPC via stdin, verify StdIO traces appear.

**Spec Reference**: [spec.md - User Story 3](./spec.md#user-story-3---trace-stdio-transport-operations-priority-p3)

### Implementation for User Story 3

- [ ] T032 [P] [US3] Add StdIOTrace leaf constructors (StdIOMessageReceived, StdIOReadError, etc.) in `src/MCP/Trace/StdIO.hs`
- [ ] T033 [US3] Implement renderStdIOTrace for all constructors in `src/MCP/Trace/StdIO.hs`
- [ ] T034 [US3] Add traceWith calls in StdIO message handling in `src/MCP/Server/StdIO.hs`
- [ ] T035 [US3] Add traceWith for parse errors and EOF in `src/MCP/Server/StdIO.hs`
- [ ] T036 [US3] Verify traces output to stderr (not stdout, to avoid protocol interference)

**Checkpoint**: User Story 3 complete. StdIO tracing works.

---

## Phase 6: User Story 4 - Selective Trace Filtering (Priority: P4)

**Goal**: Developers can filter traces by subsystem using `filterTracer`.

**Independent Test**: Create filtered tracer for OAuth-only, verify only OAuth traces pass through.

**Spec Reference**: [spec.md - User Story 4](./spec.md#user-story-4---selective-trace-filtering-priority-p4)

### Implementation for User Story 4

- [ ] T037 [P] [US4] Add filter predicate helpers (isOAuthTrace, isErrorTrace, etc.) in `src/MCP/Trace/Types.hs`
- [ ] T038 [US4] Document filtering patterns in `quickstart.md`
- [ ] T039 [US4] Add filtering example to example executables
- [ ] T040 [US4] Verify SC-006: filterTracer works with predicate matching subsystems

**Checkpoint**: User Story 4 complete. Trace filtering works.

---

## Phase 7: Polish & Cross-Cutting Concerns

**Purpose**: Tests, documentation, and final validation

- [ ] T041 [P] Create `test/Trace/RenderSpec.hs` with property tests for render totality
- [ ] T042 [P] Create `test/Trace/GoldenSpec.hs` with golden tests for render output stability
- [ ] T043 Update `test/Main.hs` to include new trace tests
- [ ] T044 Run full test suite with `cabal test`
- [ ] T045 [P] Update CHANGELOG.md with tracing feature
- [ ] T046 Verify all success criteria (SC-001 through SC-006)
- [ ] T047 Final `cabal build` and `hlint` check

---

## Dependencies & Execution Order

### Phase Dependencies

- **Phase 1 (Setup)**: No dependencies - can start immediately
- **Phase 2 (Foundational)**: Depends on Phase 1 - BLOCKS all user stories
- **Phase 3-6 (User Stories)**: All depend on Phase 2 completion
  - User stories can proceed sequentially in priority order (P1 → P2 → P3 → P4)
- **Phase 7 (Polish)**: Depends on desired user stories being complete

### User Story Dependencies

- **User Story 1 (P1)**: Can start after Phase 2 - No dependencies on other stories
- **User Story 2 (P2)**: Can start after Phase 2 - Builds on US1's Protocol traces
- **User Story 3 (P3)**: Can start after Phase 2 - Builds on US1's Protocol traces
- **User Story 4 (P4)**: Can start after Phase 2 - Requires trace types to exist for filtering

### Parallel Opportunities

- T002, T003 can run in parallel
- T004, T005, T006 can run in parallel (different files)
- T015, T016 can run in parallel (different files)
- T024, T025 can run in parallel (different files)
- T041, T042, T045 can run in parallel

---

## Parallel Example: Phase 2A (Skeleton Trace Types)

```bash
# Launch skeleton types in parallel (no dependencies between them):
bd update <T004-id> --status in_progress  # OAuth skeleton
bd update <T005-id> --status in_progress  # Protocol skeleton
bd update <T006-id> --status in_progress  # Server skeleton

# After all three complete, T007 and T008 can proceed
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup
2. Complete Phase 2: Foundational skeleton (CRITICAL)
3. Complete Phase 3: User Story 1
4. **STOP and VALIDATE**: Test tracing independently
5. Deploy/demo if ready

### Incremental Delivery

1. Setup + Foundational → Skeleton compiles
2. Add User Story 1 → Basic tracing works → Demo (MVP!)
3. Add User Story 2 → HTTP/OAuth tracing → Demo
4. Add User Story 3 → StdIO tracing → Demo
5. Add User Story 4 → Filtering → Demo
6. Polish → Tests, docs → Release

---

## Progress Tracking with Beads

**CRITICAL**: Use `bd` (beads) for ALL progress tracking. Do NOT use markdown TODOs or task lists.

### Epic Structure

This feature uses hierarchical beads under epic `mcp-6k9`:

```
mcp-6k9           [epic]  Epic: Structured Tracing with plow-log
├── mcp-6k9.1     [epic]  Phase 1: Setup
│   ├── mcp-6k9.1.1       T001: Add plow-log dependencies
│   ├── mcp-6k9.1.2       T002: Create Trace directory
│   └── mcp-6k9.1.3       T003: Verify dependencies
├── mcp-6k9.2     [epic]  Phase 2: Foundational - Trace Type Skeleton
│   ├── mcp-6k9.2.1       T004: Create OAuthTrace skeleton
│   ├── mcp-6k9.2.2       T005: Create ProtocolTrace skeleton
│   ├── mcp-6k9.2.3       T006: Create ServerTrace skeleton
│   ├── mcp-6k9.2.4       T007: Create StdIOTrace skeleton
│   ├── mcp-6k9.2.5       T008: Create HTTPTrace skeleton
│   ├── mcp-6k9.2.6       T009: Create MCPTrace root type
│   ├── mcp-6k9.2.7       T010: Thread IOTracer into StdIO
│   ├── mcp-6k9.2.8       T011: Thread IOTracer into HTTP
│   ├── mcp-6k9.2.9       T012: Thread IOTracer into Auth
│   ├── mcp-6k9.2.10      T013: Update cabal exposed-modules
│   └── mcp-6k9.2.11      T014: Verify skeleton compiles
├── mcp-6k9.3     [epic]  Phase 3: User Story 1 (P1 MVP)
│   └── mcp-6k9.3.1-9     T015-T023
├── mcp-6k9.4     [epic]  Phase 4: User Story 2 (P2)
│   └── mcp-6k9.4.1-8     T024-T031
├── mcp-6k9.5     [epic]  Phase 5: User Story 3 (P3)
│   └── mcp-6k9.5.1-5     T032-T036
├── mcp-6k9.6     [epic]  Phase 6: User Story 4 (P4)
│   └── mcp-6k9.6.1-4     T037-T040
└── mcp-6k9.7     [epic]  Phase 7: Polish
    └── mcp-6k9.7.1-7     T041-T047
```

### Quick Reference

```bash
# Check ready work
bd ready --json

# Start a task
bd update mcp-6k9.1.1 --status in_progress

# Complete a task
bd close mcp-6k9.1.1 --reason "Implemented"

# Create subtask discovered during work
bd create "Found: need helper function" -p 1 --parent mcp-6k9.2

# View task details
bd show mcp-6k9.2.1

# View epic status
bd epic status mcp-6k9

# List all tasks in this epic
bd list | grep "mcp-6k9"
```

### Workflow

1. **Before starting**: `bd ready --json` to see unblocked tasks
2. **Claim task**: `bd update <id> --status in_progress`
3. **Work on it**: Implement per task description
4. **Discover subtask?**: `bd create "Subtask" --parent <phase-id>`
5. **Complete**: `bd close <id> --reason "Done"`
6. **Commit**: Always commit `.beads/issues.jsonl` with code changes

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story for traceability
- Each user story should be independently completable and testable
- Commit after each task or logical group
- Stop at any checkpoint to validate story independently
- **Always use `bd` for progress tracking, not markdown checkboxes**
