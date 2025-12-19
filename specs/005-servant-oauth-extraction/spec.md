# Feature Specification: Remove MCP Imports from Servant.OAuth2.IDP

**Branch**: `005-servant-oauth-extraction`
**Status**: In Planning
**Date**: 2025-12-18

## Summary

Prepare the `Servant.OAuth2.IDP.*` modules for extraction to a separate package by removing all `MCP.*` dependencies. This is a refactoring task that moves types and functions from MCP modules to new Servant modules, using explicit parameters and record types rather than new typeclasses.

## Goals

1. **Package Independence**: `Servant.OAuth2.IDP.*` modules should have zero imports from `MCP.*` namespace
2. **Clean Break**: No backwards compatibility re-exports - MCP modules that moved types simply remove them
3. **Minimal API Surface**: Use explicit record parameters rather than introducing new typeclasses

## Non-Goals

- Creating a separate Hackage package (that's a future step)
- Adding new functionality to OAuth
- Changing OAuth behavior

## Requirements

### Functional Requirements

#### FR-001: Move MonadTime Import
**Priority**: P0 (Critical)
**Description**: Replace all `import MCP.Server.Time (MonadTime)` with direct `import Control.Monad.Time (MonadTime)`
**Files Affected**:
- `src/Servant/OAuth2/IDP/Store.hs`
- `src/Servant/OAuth2/IDP/Store/InMemory.hs`
- `src/Servant/OAuth2/IDP/Handlers/Authorization.hs`
- `src/Servant/OAuth2/IDP/Handlers/Login.hs`
- `src/Servant/OAuth2/IDP/Test/Internal.hs`

#### FR-002: Create Servant.OAuth2.IDP.Metadata Module
**Priority**: P0 (Critical)
**Description**: Move `OAuthMetadata` and `ProtectedResourceMetadata` types from `MCP.Server.Auth` to new `Servant.OAuth2.IDP.Metadata` module
**Types to Move**:
- `OAuthMetadata` (RFC 8414 discovery response)
- `ProtectedResourceMetadata` (RFC 9728)
- All associated JSON instances

#### FR-003: Create Servant.OAuth2.IDP.PKCE Module
**Priority**: P0 (Critical)
**Description**: Move PKCE validation functions from `MCP.Server.Auth` to new `Servant.OAuth2.IDP.PKCE` module
**Functions to Move**:
- `validateCodeVerifier :: CodeVerifier -> CodeChallenge -> Bool`
- `generateCodeChallenge :: Text -> Text`

#### FR-004: Create Servant.OAuth2.IDP.Config Module
**Priority**: P1 (High)
**Description**: Define `OAuthEnv` record containing protocol-agnostic OAuth configuration
**Fields**:
- `baseUrl :: URI`
- `authCodeExpiry :: NominalDiffTime`
- `accessTokenExpiry :: NominalDiffTime`
- `loginSessionExpiry :: NominalDiffTime`
- Token prefixes (authCode, refreshToken, clientId)
- Supported scopes, response types, grant types, auth methods, code challenge methods

#### FR-005: Create Servant.OAuth2.IDP.Trace Module
**Priority**: P1 (High)
**Description**: Define `OAuthTrace` ADT for OAuth-specific trace events
**Constructors**:
- `TraceClientRegistration ClientId Text`
- `TraceAuthorizationRequest ClientId [Scope] Bool`
- `TraceLoginPageServed SessionId`
- `TraceLoginAttempt Text Bool`
- `TracePKCEValidation Bool`
- `TraceAuthorizationGranted ClientId Text`
- `TraceAuthorizationDenied ClientId Text`
- `TraceTokenExchange GrantType Bool`
- `TraceTokenRefresh Bool`
- `TraceSessionExpired SessionId`
- `TraceValidationError Text Text`

#### FR-006: Update Handler Signatures
**Priority**: P1 (High)
**Description**: Update all handler modules to use Servant types instead of MCP types
**Changes**:
- Replace `HasType HTTPServerConfig env` with `HasType OAuthEnv env`
- Replace `HasType (IOTracer HTTPTrace) env` with `HasType (IOTracer OAuthTrace) env`

#### FR-007: Update MCP.Server.HTTP.AppEnv
**Priority**: P1 (High)
**Description**: Embed `OAuthEnv` in `AppEnv` and create tracer adapter
**Changes**:
- Add `envOAuthEnv :: OAuthEnv` field
- Create `mkOAuthEnv :: HTTPServerConfig -> OAuthEnv` function
- Create `mkOAuthTracer :: IOTracer HTTPTrace -> IOTracer OAuthTrace` via contramap

#### FR-008: Remove Types from MCP.Server.Auth
**Priority**: P2 (Medium)
**Description**: Clean break - remove moved types from MCP.Server.Auth exports
**Types to Remove**:
- `OAuthMetadata`
- `ProtectedResourceMetadata`
- `validateCodeVerifier`
- `generateCodeChallenge`

## Acceptance Criteria

1. `rg "^import MCP\." src/Servant/` returns empty (no MCP imports in Servant modules)
2. `cabal build` succeeds with no errors
3. `cabal test` passes (all existing tests pass)
4. No functionality changes - this is a pure refactoring

## Dependencies

- `monad-time` package (already a dependency via MCP.Server.Time)
- `network-uri` package for URI type in OAuthEnv

## Files to Create

| File | Purpose |
|------|---------|
| `src/Servant/OAuth2/IDP/Trace.hs` | OAuthTrace ADT |
| `src/Servant/OAuth2/IDP/Config.hs` | OAuthEnv record |
| `src/Servant/OAuth2/IDP/Metadata.hs` | OAuthMetadata, ProtectedResourceMetadata |
| `src/Servant/OAuth2/IDP/PKCE.hs` | PKCE validation functions |

## Files to Modify

| File | Changes |
|------|---------|
| `src/Servant/OAuth2/IDP/Store.hs` | MonadTime import |
| `src/Servant/OAuth2/IDP/Store/InMemory.hs` | MonadTime import |
| `src/Servant/OAuth2/IDP/API.hs` | Metadata import |
| `src/Servant/OAuth2/IDP/Server.hs` | Config/Trace types |
| `src/Servant/OAuth2/IDP/Handlers/Helpers.hs` | Config record, trace events |
| `src/Servant/OAuth2/IDP/Handlers/Metadata.hs` | Config record, Metadata import |
| `src/Servant/OAuth2/IDP/Handlers/Registration.hs` | Config/Trace types |
| `src/Servant/OAuth2/IDP/Handlers/Authorization.hs` | Config/Trace types |
| `src/Servant/OAuth2/IDP/Handlers/Login.hs` | Config/Trace types |
| `src/Servant/OAuth2/IDP/Handlers/Token.hs` | Config/Trace types, PKCE import |
| `src/Servant/OAuth2/IDP/Test/Internal.hs` | MonadTime import, fix doc comment typo |
| `src/MCP/Server/Auth.hs` | Remove moved types (clean break) |
| `src/MCP/Server/HTTP/AppEnv.hs` | Add OAuthEnv, tracer adapter |
| `mcp-haskell.cabal` | Add new modules |

## Risks

1. **Documentation Typo**: The Test.Internal module has stale doc comments referencing `MCP.Server.OAuth.Test.Internal` instead of `Servant.OAuth2.IDP.Test.Internal` - minor fix needed
2. **API Breakage**: Clean break approach may break downstream code importing from MCP.Server.Auth - acceptable since this is pre-release
