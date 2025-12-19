# Implementation Plan: Remove MCP Imports from Servant.OAuth2.IDP

**Branch**: `005-servant-oauth-extraction` | **Date**: 2025-12-18 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/005-servant-oauth-extraction/spec.md`

## Summary

Prepare `Servant.OAuth2.IDP.*` modules for extraction to a separate package by removing all `MCP.*` dependencies. This is a pure refactoring: move types to new Servant modules, use explicit record parameters, clean break with no backwards-compatibility re-exports.

## Technical Context

**Language/Version**: Haskell GHC2021 (GHC 9.4+ via base ^>=4.18.2.1)
**Primary Dependencies**: servant-server 0.19-0.20, servant-auth-server 0.4, aeson 2.1-2.2, monad-time, network-uri, cryptonite 0.30, generic-lens
**Storage**: N/A (refactoring only, no data changes)
**Testing**: cabal test (HSpec test suite)
**Target Platform**: Linux server
**Project Type**: Single Haskell library
**Performance Goals**: N/A (no runtime changes)
**Constraints**: Must maintain API compatibility for OAuth functionality
**Scale/Scope**: ~15 files modified, 4 new files created

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

Reference: `.specify/memory/constitution.md`

| Principle | Status | Evidence/Notes |
|-----------|--------|----------------|
| I. Type-Driven Design | :white_check_mark: | `OAuthEnv` record designed with explicit typed fields; `OAuthTrace` ADT with typed constructors; moved types preserve existing validation (CodeChallenge, CodeVerifier newtypes) |
| II. Deep Module Architecture | :white_check_mark: | New modules expose minimal public interface; `OAuthEnv` hides configuration complexity behind single record; PKCE internals hidden behind `validateCodeVerifier` |
| III. Denotational Semantics | :white_check_mark: | PKCE validation has clear mathematical semantics (S256 hash comparison); no new abstractions requiring law documentation |
| IV. Total Functions | :white_check_mark: | `validateCodeVerifier :: CodeVerifier -> CodeChallenge -> Bool` - total function returning Bool; `generateCodeChallenge :: Text -> Text` - total |
| V. Pure Core, Impure Shell | :white_check_mark: | All new modules are pure (`Trace`, `Config`, `Metadata`, `PKCE`); impure operations remain at boundary in handlers |
| VI. Property-Based Testing | :white_check_mark: | Existing tests preserved; PKCE round-trip testable: `validateCodeVerifier (generateCodeVerifier) (generateCodeChallenge verifier) == True` |

## Project Structure

### Documentation (this feature)

```text
specs/005-servant-oauth-extraction/
├── plan.md              # This file
├── research.md          # Phase 0 output
├── data-model.md        # Phase 1 output
├── quickstart.md        # Phase 1 output
└── contracts/           # Phase 1 output (N/A for refactoring)
```

### Source Code (repository root)

```text
src/
├── Servant/OAuth2/IDP/
│   ├── API.hs              # MODIFY: Import Metadata from new location
│   ├── Auth/
│   │   ├── Backend.hs      # UNCHANGED
│   │   └── Demo.hs         # UNCHANGED
│   ├── Boundary.hs         # UNCHANGED
│   ├── Config.hs           # NEW: OAuthEnv record
│   ├── Handlers/
│   │   ├── Authorization.hs  # MODIFY: Use OAuthEnv, OAuthTrace
│   │   ├── Helpers.hs        # MODIFY: Use OAuthEnv, OAuthTrace
│   │   ├── HTML.hs           # UNCHANGED
│   │   ├── Login.hs          # MODIFY: Use OAuthEnv, OAuthTrace
│   │   ├── Metadata.hs       # MODIFY: Use OAuthEnv, import from Servant
│   │   ├── Registration.hs   # MODIFY: Use OAuthEnv, OAuthTrace
│   │   └── Token.hs          # MODIFY: Use OAuthEnv, OAuthTrace, PKCE
│   ├── Handlers.hs           # UNCHANGED (re-exports)
│   ├── LoginFlowError.hs     # UNCHANGED
│   ├── Metadata.hs           # NEW: OAuthMetadata, ProtectedResourceMetadata
│   ├── PKCE.hs               # NEW: validateCodeVerifier, generateCodeChallenge
│   ├── Server.hs             # MODIFY: Use OAuthEnv, OAuthTrace
│   ├── Store/
│   │   └── InMemory.hs       # MODIFY: MonadTime import
│   ├── Store.hs              # MODIFY: MonadTime import
│   ├── Test/
│   │   └── Internal.hs       # MODIFY: MonadTime import, fix doc comment typo
│   ├── Trace.hs              # NEW: OAuthTrace ADT
│   └── Types.hs              # UNCHANGED
└── MCP/
    ├── Server/
    │   ├── Auth.hs           # MODIFY: Remove moved types
    │   ├── HTTP/
    │   │   └── AppEnv.hs     # MODIFY: Add OAuthEnv, tracer adapter
    │   └── Time.hs           # UNCHANGED
    └── Trace/
        ├── HTTP.hs           # UNCHANGED (still uses MCP.Trace.OAuth)
        └── OAuth.hs          # UNCHANGED (stays as MCP-specific trace embedding)
```

**Structure Decision**: Single library structure. New modules added under `Servant.OAuth2.IDP.*` namespace to maintain package extraction path.

## Complexity Tracking

> No violations requiring justification. All changes follow Constitution principles.

| Violation | Why Needed | Simpler Alternative Rejected Because |
|-----------|------------|-------------------------------------|
| N/A | N/A | N/A |

---

## Phase 0: Research

### R-001: MonadTime Package Availability

**Question**: Is `Control.Monad.Time` from the `monad-time` package already a direct dependency?

**Finding**: Yes. The `MCP.Server.Time` module is a thin re-export:
```haskell
module MCP.Server.Time (MonadTime (..)) where
import Control.Monad.Time (MonadTime (..))
```

**Decision**: Direct import from `Control.Monad.Time` is safe - no new dependency needed.

### R-002: Test Module Documentation Typo

**Question**: What is `MCP.Server.OAuth.Test.Internal` that Test.Internal references?

**Finding**: The file `src/Servant/OAuth2/IDP/Test/Internal.hs` declares `module Servant.OAuth2.IDP.Test.Internal` (line 37), but the doc comment header says `Module : MCP.Server.OAuth.Test.Internal` (line 7). The references at lines 22, etc. are in documentation examples, not actual imports.

**Decision**: Fix the documentation typo. The module exists and is correctly named; only doc comments need updating.

### R-003: OAuthTrace Design

**Question**: Should `OAuthTrace` duplicate `MCP.Trace.OAuth.OAuthTrace` or import it?

**Finding**: `MCP.Trace.OAuth.OAuthTrace` already exists and is MCP-independent (comment in file: "This module is intentionally MCP-independent to enable future package separation").

**Decision**: Move the type from `MCP.Trace.OAuth` to `Servant.OAuth2.IDP.Trace`, then have MCP re-export or adapt. This achieves true separation. However, examining the architecture more closely:

- `MCP.Trace.HTTP` imports `MCP.Trace.OAuth` and embeds it via `HTTPOAuth OAuthTrace`
- Changing this creates a larger refactoring scope

**Revised Decision**: Create `Servant.OAuth2.IDP.Trace` with a new trace type. Handlers will emit via the Servant trace type. MCP adapts at boundary via `contramap`. This is cleaner separation.

### R-004: OAuthEnv vs HTTPServerConfig + OAuthConfig

**Question**: What fields from `HTTPServerConfig` and `OAuthConfig` are actually used in Servant handlers?

**Finding** (from grep of handler files):
- `httpBaseUrl` - for constructing redirect URLs
- `httpOAuthConfig` - for:
  - `authCodeExpirySeconds`
  - `accessTokenExpirySeconds`
  - `authCodePrefix`
  - `refreshTokenPrefix`
  - `clientIdPrefix`
  - `supportedScopes`
  - `supportedResponseTypes`
  - `supportedGrantTypes`
  - `supportedAuthMethods`
  - `supportedCodeChallengeMethods`
  - `loginSessionExpirySeconds`
  - `autoApproveAuth` (demo mode)
  - `demoUserIdTemplate` (demo mode)
  - `demoEmailDomain` (demo mode)

**Decision**: `OAuthEnv` should contain:
1. Core fields: baseUrl, expiry times, prefixes, supported params
2. NOT demo mode fields - those are MCP-specific

The handlers should be parameterized to work without demo mode fields. Demo mode logic stays in MCP.

---

## Phase 1: Design

### Data Model

See [data-model.md](./data-model.md).

### Key Types

#### OAuthEnv (new)

```haskell
data OAuthEnv = OAuthEnv
    { oauthBaseUrl :: Text
    , oauthAuthCodeExpiry :: NominalDiffTime
    , oauthAccessTokenExpiry :: NominalDiffTime
    , oauthLoginSessionExpiry :: NominalDiffTime
    , oauthAuthCodePrefix :: Text
    , oauthRefreshTokenPrefix :: Text
    , oauthClientIdPrefix :: Text
    , oauthSupportedScopes :: [Scope]
    , oauthSupportedResponseTypes :: [ResponseType]
    , oauthSupportedGrantTypes :: [GrantType]
    , oauthSupportedAuthMethods :: [ClientAuthMethod]
    , oauthSupportedCodeChallengeMethods :: [CodeChallengeMethod]
    }
    deriving (Generic)
```

#### OAuthTrace (new)

```haskell
data OAuthTrace
    = TraceClientRegistration ClientId ClientName
    | TraceAuthorizationRequest ClientId [Scope] Bool  -- hasState
    | TraceLoginPageServed SessionId
    | TraceLoginAttempt Text Bool                      -- username, success
    | TracePKCEValidation Bool                         -- isValid
    | TraceAuthorizationGranted ClientId UserId
    | TraceAuthorizationDenied ClientId Text           -- reason
    | TraceTokenExchange GrantType Bool                -- success
    | TraceTokenRefresh Bool                           -- success
    | TraceSessionExpired SessionId
    | TraceValidationError Text Text                   -- errorType, detail
    deriving (Show, Eq)
```

### Migration Path

1. **Phase A** (Additive): Create new Servant modules (`Trace`, `Config`, `Metadata`, `PKCE`)
2. **Phase B** (Update Servant): Change imports in Servant handlers to use new modules
3. **Phase C** (Update MCP): Add `OAuthEnv` to `AppEnv`, create adapter functions
4. **Phase D** (Clean Break): Remove moved types from `MCP.Server.Auth`

### Contracts

No external API contracts change. Internal module boundaries shift.

---

## Implementation Phases (for /speckit.tasks)

### Phase A: Create New Servant Modules

1. Create `src/Servant/OAuth2/IDP/Trace.hs` with `OAuthTrace` ADT
2. Create `src/Servant/OAuth2/IDP/Config.hs` with `OAuthEnv` record
3. Create `src/Servant/OAuth2/IDP/Metadata.hs` with moved types
4. Create `src/Servant/OAuth2/IDP/PKCE.hs` with moved functions
5. Update `mcp-haskell.cabal` with new modules

### Phase B: Update Servant Imports

1. Update `Store.hs` and `Store/InMemory.hs` - MonadTime import
2. Update `API.hs` - Metadata import
3. Update `Handlers/Metadata.hs` - use Servant Metadata
4. Update `Handlers/Token.hs` - use PKCE from Servant
5. Update `Handlers/Helpers.hs` - OAuthEnv and trace
6. Update `Handlers/Registration.hs` - OAuthEnv and trace
7. Update `Handlers/Authorization.hs` - OAuthEnv and trace
8. Update `Handlers/Login.hs` - OAuthEnv and trace
9. Update `Server.hs` - OAuthEnv and trace
10. Update `Test/Internal.hs` - fix doc comment typo, MonadTime import

### Phase C: Update MCP

1. Add `OAuthEnv` field to `AppEnv`
2. Create `mkOAuthEnv :: HTTPServerConfig -> OAuthEnv`
3. Create `mkOAuthTracer :: IOTracer HTTPTrace -> IOTracer OAuthTrace`
4. Update handler call sites to pass OAuthEnv

### Phase D: Clean Break

1. Remove `OAuthMetadata` from `MCP.Server.Auth` exports
2. Remove `ProtectedResourceMetadata` from `MCP.Server.Auth` exports
3. Remove `validateCodeVerifier` from `MCP.Server.Auth` exports
4. Remove `generateCodeChallenge` from `MCP.Server.Auth` exports
5. Verify: `rg "^import MCP\." src/Servant/` returns empty
6. Verify: `cabal build` succeeds
7. Verify: `cabal test` passes
