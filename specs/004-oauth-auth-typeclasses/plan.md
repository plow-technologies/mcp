# Implementation Plan: OAuth State and Authentication Typeclasses

**Branch**: `004-oauth-auth-typeclasses` | **Date**: 2025-12-11 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/004-oauth-auth-typeclasses/spec.md`

**Note**: This template is filled in by the `/speckit.plan` command. See `.specify/templates/commands/plan.md` for the execution workflow.

## Summary

Design two typeclasses (`OAuthStateStore` for OAuth 2.1 state persistence, `AuthBackend` for user credential validation) using three-layer cake architecture with polymorphic `m` monad parameter. Each typeclass defines associated error and environment types. Provide in-memory (`TVar`) and hard-coded credential implementations that preserve current demo behavior. Use `generic-lens` (`AsType`/`HasType`) for composable error/environment handling, and `hoistServer` for Servant boundary translation.

## Technical Context

**Language/Version**: Haskell GHC2021 (GHC 9.4+ via base ^>=4.18.2.1)
**Primary Dependencies**: servant-server 0.19-0.20, servant-auth-server 0.4, jose 0.10-0.11, cryptonite 0.30, stm 2.5, mtl 2.3, aeson 2.1-2.2, generic-lens (to add), network-uri (to add)
**Storage**: In-memory (TVar-based) for default implementation; typeclass enables PostgreSQL/Redis backends
**Testing**: hspec + QuickCheck + hspec-quickcheck (property tests via `prop` combinator, polymorphic specs testing interface not implementation)
**Target Platform**: Linux server (GHC 9.4+)
**Project Type**: Single Haskell library with examples
**Performance Goals**: No measurable latency overhead (<5%) vs current TVar implementation
**Constraints**: Must maintain backward compatibility with existing OAuth flows
**Scale/Scope**: Single-instance semantics; distributed coordination delegated to backend implementations
**Type Safety**: Strong newtypes for all identifiers (AuthCodeId, ClientId, etc.), ADTs for enumerations (CodeChallengeMethod, GrantType), MonadTime for testable time-dependent operations

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

Reference: `.specify/memory/constitution.md`

| Principle | Status | Evidence/Notes |
|-----------|--------|----------------|
| I. Type-Driven Design | ✅ | Typeclasses with associated types (`OAuthStateError m`, `OAuthStateEnv m`, `AuthBackendError m`, `AuthBackendEnv m`) encode implementation-specific concerns at the type level. Smart constructors preserved for `HashedPassword`. Illegal states prevented by type constraints. |
| II. Deep Module Architecture | ✅ | Minimal public interface (typeclass methods only). Implementation details hidden in instance modules (`InMemory.hs`, `Demo.hs`). Complexity pulled into implementations, not pushed to callers. |
| III. Denotational Semantics | ✅ | FR-016 requires documented algebraic laws (round-trip, delete, idempotence, overwrite). Property-based tests will verify these laws. |
| IV. Total Functions | ✅ | All operations return results in monadic context (`m (Maybe a)`, `m (Either e a)`). No partial functions. Errors encoded via associated error types and `MonadError`. |
| V. Pure Core, Impure Shell | ✅ | Typeclasses abstract over effects (`MonadIO` in instance context, not class definition). Handler logic remains pure over the abstract interface; IO lives in implementations. |
| VI. Property-Based Testing | ✅ | FR-016 explicitly requires property tests for algebraic laws. Golden tests for API contracts preserved. |

**Gate Status**: PASS - All principles satisfied. Proceed to Phase 0.

## Project Structure

### Documentation (this feature)

```text
specs/004-oauth-auth-typeclasses/
├── plan.md              # This file (/speckit.plan command output)
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
├── contracts/           # Phase 1 output (typeclass interfaces)
└── tasks.md             # Phase 2 output (/speckit.tasks command)
```

### Source Code (repository root)

```text
src/
├── MCP/
│   ├── Types.hs              # Core MCP protocol types (unchanged)
│   ├── Protocol.hs           # JSON-RPC message types (unchanged)
│   ├── Server.hs             # MCPServer typeclass (unchanged)
│   ├── Server/
│   │   ├── HTTP.hs           # Modified: use typeclass constraints
│   │   ├── StdIO.hs          # Unchanged (no OAuth)
│   │   ├── Auth.hs           # Modified: re-exports from Auth/*
│   │   ├── Auth/
│   │   │   ├── Backend.hs    # NEW: AuthBackend typeclass + Username, PlaintextPassword newtypes
│   │   │   └── Demo.hs       # NEW: Demo credential implementation
│   │   ├── OAuth/
│   │   │   ├── Store.hs      # NEW: OAuthStateStore typeclass + MonadTime
│   │   │   ├── Types.hs      # NEW: Newtypes (AuthCodeId, ClientId, etc.), ADTs, domain entities
│   │   │   └── InMemory.hs   # NEW: TVar-based implementation
│   │   └── Time.hs           # NEW: MonadTime instances (IO, test monads)
│   └── Trace/                # Unchanged

test/
├── Main.hs                   # Modified: import and run law specs
├── Laws/
│   ├── OAuthStateStoreSpec.hs  # NEW: Polymorphic typeclass law tests (uses prop)
│   └── AuthBackendSpec.hs      # NEW: Polymorphic typeclass law tests (uses prop)
├── Generators.hs             # NEW: Arbitrary instances for all newtypes and domain types
├── TestMonad.hs              # NEW: Test monad with controlled MonadTime
└── Trace/                    # Unchanged
```

**Structure Decision**: Single project structure. New modules added under `src/MCP/Server/Auth/` and `src/MCP/Server/OAuth/` for typeclass definitions and implementations. Test modules added under `test/Laws/` for property-based law verification using polymorphic specs with `prop` combinator. `Arbitrary` instances in `test/Generators.hs`.

## Complexity Tracking

> No Constitution Check violations requiring justification.

| Violation | Why Needed | Simpler Alternative Rejected Because |
|-----------|------------|-------------------------------------|
| N/A | - | - |

## Post-Design Constitution Re-Check

*GATE: Verified after Phase 1 design completion.*

| Principle | Status | Post-Design Evidence |
|-----------|--------|----------------------|
| I. Type-Driven Design | ✅ | `contracts/OAuthStateStore.hs` and `contracts/AuthBackend.hs` define typeclasses with associated types. Types designed before implementation (spec → plan → contracts). |
| II. Deep Module Architecture | ✅ | Module structure in plan shows minimal exports (`OAuthStateStore(..)`, `AuthBackend(..)`). Implementation modules (`InMemory.hs`, `Demo.hs`) hide complexity. |
| III. Denotational Semantics | ✅ | Algebraic laws documented in `contracts/OAuthStateStore.hs` haddock (round-trip, delete, idempotence, overwrite). Test structure planned in `test/Laws/`. |
| IV. Total Functions | ✅ | All typeclass methods return `m ()` or `m (Maybe a)`. No partial functions. Error handling via `MonadError e m` with `AsType` prism constraints. |
| V. Pure Core, Impure Shell | ✅ | Typeclass definitions have no `MonadIO` constraint; implementations add it in instance context. Handler logic is pure over the abstract interface. |
| VI. Property-Based Testing | ✅ | `data-model.md` specifies laws. `quickstart.md` shows property test examples. Test modules planned: `OAuthStateStoreSpec.hs`, `AuthBackendSpec.hs`. |

**Gate Status**: PASS - All principles verified against design artifacts.

## Generated Artifacts

| Artifact | Path | Purpose |
|----------|------|---------|
| Plan | `specs/004-oauth-auth-typeclasses/plan.md` | This file |
| Research | `specs/004-oauth-auth-typeclasses/research.md` | Technical decisions and rationale |
| Data Model | `specs/004-oauth-auth-typeclasses/data-model.md` | Entity definitions and relationships |
| OAuthStateStore Contract | `specs/004-oauth-auth-typeclasses/contracts/OAuthStateStore.hs` | Typeclass interface specification |
| AuthBackend Contract | `specs/004-oauth-auth-typeclasses/contracts/AuthBackend.hs` | Typeclass interface specification |
| Quickstart | `specs/004-oauth-auth-typeclasses/quickstart.md` | Usage examples and integration guide |
