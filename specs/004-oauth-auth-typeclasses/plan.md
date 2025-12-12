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

---

## Phase 3: Type Unification (mcp-51r)

**Added**: 2025-12-11 | **Blocker**: mcp-51r | **Status**: Planning

### Problem Statement

During implementation, a blocker was discovered (mcp-51r): two incompatible OAuth state type systems exist:

1. **HTTP.hs (legacy)**: Uses `Text` for redirect URIs, grant types, client IDs, etc.
   - `clientRedirectUris :: [Text]`
   - `clientGrantTypes :: [Text]`
   - Simple `Text`-based identifiers throughout

2. **OAuth.Types (new)**: Uses type-safe newtypes with invariants
   - `clientRedirectUris :: NonEmpty RedirectUri`
   - `clientGrantTypes :: Set GrantType`
   - All identifiers wrapped in newtypes (`ClientId`, `AuthCodeId`, etc.)

Full handler migration to typeclass constraints requires type unification first.

### Clarified Decisions (from spec)

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Canonical types | OAuth.Types newtypes | Type safety, illegal states unrepresentable |
| Boundary conversion | Servant `FromHttpApiData`/`ToHttpApiData` instances | Natural boundary, enables property-based round-trip tests |
| Invalid data handling | Fail at boundary (400) | Parse-don't-validate; no domain errors for structural issues |
| Migration strategy | Incremental per-handler | Lower risk, continuous integration, each migration testable |
| PendingAuthorization fields | Same newtypes | Already validated at request parse time |

### New Requirements (FR-018 through FR-021)

- **FR-018**: All OAuth.Types newtypes MUST implement `FromHttpApiData` and `ToHttpApiData`
- **FR-019**: Round-trip property tests for all `FromHttpApiData`/`ToHttpApiData` instances
- **FR-020**: `FromJSON` instances reject invalid data at parse time (400, not domain error)
- **FR-021**: Incremental handler migration with nested beads under mcp-51r

### Implementation Approach

#### Step 1: Add Servant Instances to OAuth.Types

```haskell
-- In OAuth.Types or a new OAuth.Servant module

instance FromHttpApiData ClientId where
  parseUrlPiece t
    | Text.null t = Left "ClientId cannot be empty"
    | otherwise = Right (ClientId t)

instance ToHttpApiData ClientId where
  toUrlPiece = unClientId

-- Similar for: AuthCodeId, SessionId, UserId, RefreshTokenId, RedirectUri, Scope
```

#### Step 2: Update FromJSON Instances with Invariant Checking

```haskell
-- NonEmpty RedirectUri parsing
instance FromJSON ClientInfo where
  parseJSON = withObject "ClientInfo" $ \o -> do
    name <- o .: "client_name"
    uris <- o .: "redirect_uris"
    case nonEmpty uris of
      Nothing -> fail "redirect_uris must not be empty"
      Just ne -> ClientInfo name ne <$> ...
```

#### Step 3: Incremental Handler Migration

Each handler migration is a separate bead under mcp-51r:

| Handler | Current Types | Target Types | Complexity |
|---------|---------------|--------------|------------|
| `handleClientRegistration` | `[Text]` redirect URIs | `NonEmpty RedirectUri` | Medium |
| `handleAuthorize` | `Text` client_id | `ClientId` newtype | Low |
| `handleToken` | `Text` code, client_id | `AuthCodeId`, `ClientId` | Medium |
| `handleRefresh` | `Text` refresh_token | `RefreshTokenId` | Low |
| `handleLogin` | `Text` session_id | `SessionId` | Low |
| ... (15+ handlers total) | | | |

#### Step 4: Property Tests for Boundary Instances

```haskell
-- In test/Laws/BoundarySpec.hs
prop "ClientId round-trip" $ \(cid :: ClientId) ->
  parseUrlPiece (toUrlPiece cid) === Right cid

prop "RedirectUri round-trip" $ \(uri :: RedirectUri) ->
  parseUrlPiece (toUrlPiece uri) === Right uri

-- NonEmpty requires custom generator
prop "NonEmpty RedirectUri FromJSON round-trip" $ \(uris :: NonEmpty RedirectUri) ->
  decode (encode uris) === Just uris
```

### Updated Module Organization

```text
src/MCP/Server/OAuth/
├── Types.hs           # Newtypes, ADTs, domain entities
├── Types/
│   └── Servant.hs     # NEW: FromHttpApiData/ToHttpApiData instances (or in Types.hs)
├── Store.hs           # OAuthStateStore typeclass
└── InMemory.hs        # TVar-based implementation

test/
├── Laws/
│   ├── OAuthStateStoreSpec.hs
│   ├── AuthBackendSpec.hs
│   └── BoundarySpec.hs    # NEW: Round-trip tests for Servant instances
└── Generators.hs          # Extended: Arbitrary for all newtypes
```

### Dependencies to Add

| Package | Version | Purpose |
|---------|---------|---------|
| `servant` | (already present) | `FromHttpApiData`, `ToHttpApiData` |

No new dependencies required—Servant instances use existing `servant` package.

### Migration Tracking Structure

```
mcp-51r (epic): Unify OAuth state types between HTTP.hs and OAuth.Types
├── mcp-51r.1: Add FromHttpApiData/ToHttpApiData to OAuth.Types newtypes
├── mcp-51r.2: Add round-trip property tests for boundary instances
├── mcp-51r.3: Update FromJSON instances to enforce invariants
├── mcp-51r.4: Migrate handleClientRegistration to newtypes
├── mcp-51r.5: Migrate handleAuthorize to newtypes
├── mcp-51r.6: Migrate handleToken to newtypes
├── ... (one bead per handler)
└── mcp-51r.N: Remove legacy Text-based types from HTTP.hs
```

### Constitution Compliance (Phase 3)

| Principle | Status | Evidence |
|-----------|--------|----------|
| I. Type-Driven Design | ✅ | Newtypes with invariants (NonEmpty, Set) make illegal states unrepresentable |
| II. Deep Module Architecture | ✅ | Servant instances in Types module; handlers unchanged except type annotations |
| III. Denotational Semantics | ✅ | Round-trip laws for all boundary conversions |
| IV. Total Functions | ✅ | `parseUrlPiece` returns `Either Text a`; no partial functions |
| V. Pure Core, Impure Shell | ✅ | Boundary conversion is pure; IO only in handler execution |
| VI. Property-Based Testing | ✅ | FR-019 requires round-trip property tests |

**Gate Status**: PASS - Phase 3 planning complete.

---

## Phase 4: Type-Safe HTTP Response Headers (mcp-nyr.18)

**Added**: 2025-12-12 | **Blocker**: mcp-nyr.18 | **Status**: Planning

### Problem Statement

A critical bug was discovered via git bisect: commit 1d882149 accidentally swapped `addHeader` call order in `handleLogin`, causing successful OAuth logins to redirect to a malformed URL containing the cookie value instead of the callback URI.

**Root cause**: Both `Location` and `Set-Cookie` headers are typed as `Text`, providing no compile-time protection against position swaps. This is the same class of bug that newtypes are designed to prevent.

```haskell
-- Current type signature (vulnerable):
Headers '[Header "Location" Text, Header "Set-Cookie" Text] NoContent

-- Bug: compiler accepts both orderings since both are Text
return $ addHeader clearCookie $ addHeader redirectUrl NoContent  -- WRONG
return $ addHeader redirectUrl $ addHeader clearCookie NoContent  -- RIGHT
```

### Blessed Solution (from spec clarifications 2025-12-12)

Add semantic newtypes for HTTP response header values in `OAuth.Types`:

| Newtype | Purpose | Usage |
|---------|---------|-------|
| `RedirectTarget` | OAuth callback/redirect URL | `Header "Location" RedirectTarget` |
| `SessionCookie` | Session cookie value | `Header "Set-Cookie" SessionCookie` |

**Design principle**: Names describe the value's semantic purpose, not the header name. All OAuth domain types belong in `OAuth.Types` to keep OAuth combinators framework-agnostic.

### Type-Safe Signature

```haskell
-- Fixed type signature (compile-time protection):
Headers '[Header "Location" RedirectTarget, Header "Set-Cookie" SessionCookie] NoContent

-- Now compiler rejects wrong order:
return $ addHeader clearCookie $ addHeader redirectUrl NoContent  -- TYPE ERROR
return $ addHeader redirectUrl $ addHeader clearCookie NoContent  -- OK
```

### Implementation Approach

#### Step 1: Add Newtypes to OAuth.Types

```haskell
-- | Target URL for HTTP 3xx redirects in OAuth flows.
-- Semantic name describes purpose, not header name.
newtype RedirectTarget = RedirectTarget { unRedirectTarget :: Text }
  deriving (Show, Eq)
  deriving newtype (ToHttpApiData)

-- | Session cookie value for OAuth login flow.
-- Distinct type from RedirectTarget prevents position-swap bugs.
newtype SessionCookie = SessionCookie { unSessionCookie :: Text }
  deriving (Show, Eq)
  deriving newtype (ToHttpApiData)
```

#### Step 2: Update Handler Type Signatures

```haskell
-- handleLogin return type (HTTP.hs)
handleLogin :: ... -> Handler (Headers '[Header "Location" RedirectTarget, Header "Set-Cookie" SessionCookie] NoContent)

-- handleAuthorize return type (also uses Set-Cookie)
handleAuthorize :: ... -> Handler (Headers '[Header "Set-Cookie" SessionCookie] Text)
```

#### Step 3: Update addHeader Call Sites

```haskell
-- Approve branch (line ~1003)
let redirectUrl = RedirectTarget $ toUrlPiece (pendingRedirectUri pending) <> "?code=" <> code <> stateParam
    clearCookie = SessionCookie "mcp_session=; Max-Age=0; Path=/"
return $ addHeader redirectUrl $ addHeader clearCookie NoContent

-- Deny branch (line ~946) - same pattern
let redirectUrl = RedirectTarget $ toUrlPiece (pendingRedirectUri pending) <> "?" <> errorParams <> stateParam
    clearCookie = SessionCookie "mcp_session=; Max-Age=0; Path=/"
return $ addHeader redirectUrl $ addHeader clearCookie NoContent
```

#### Step 4: Add Regression Test

```haskell
-- Verify Location header contains expected redirect URL pattern
it "redirects to callback URI with authorization code on successful login" $ do
  resp <- postLogin validCredentials validSession
  let locationHeader = lookup "Location" (responseHeaders resp)
  locationHeader `shouldSatisfy` \case
    Just loc -> "?code=" `isInfixOf` loc && not ("mcp_session" `isInfixOf` loc)
    Nothing -> False
```

### Files to Modify

| File | Change |
|------|--------|
| `src/MCP/Server/OAuth/Types.hs` | Add `RedirectTarget`, `SessionCookie` newtypes with `ToHttpApiData` |
| `src/MCP/Server/HTTP.hs` | Update type signatures and `addHeader` call sites |
| `test/HTTP/OAuthSpec.hs` | Add regression test for Location header content |

### Constitution Compliance (Phase 4)

| Principle | Status | Evidence |
|-----------|--------|----------|
| I. Type-Driven Design | ✅ | Distinct newtypes make position-swap bugs a compile error |
| II. Deep Module Architecture | ✅ | Types in OAuth.Types; HTTP.hs only changes signatures |
| III. Denotational Semantics | ✅ | Newtypes have clear semantic meaning in their names |
| IV. Total Functions | ✅ | No change to function totality |
| V. Pure Core, Impure Shell | ✅ | Types are pure; framework-agnostic per design principle |
| VI. Property-Based Testing | ✅ | Regression test added; type safety is compile-time |

**Gate Status**: PASS - Phase 4 planning complete.
