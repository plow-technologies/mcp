# Feature Specification: OAuth State and Authentication Typeclasses

**Feature Branch**: `004-oauth-auth-typeclasses`
**Created**: 2025-12-11
**Status**: Draft
**Input**: User description: "Design two new typeclasses: 1) OAuth2+DCR state persistence interface for swappable production implementations, 2) User login/authentication interface. Both use polymorphic m Monad type parameter (three layer cake architecture). Provide in-memory storage and hard-coded credential implementations for examples."

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Production Deployment with Persistent Storage (Priority: P1)

A DevOps engineer deploys the MCP HTTP server in a production environment where OAuth state must persist across server restarts. They implement a PostgreSQL-backed storage layer that satisfies the OAuth state persistence interface.

**Why this priority**: Production deployments are the primary driver for making these interfaces swappable. Without persistent storage, server restarts invalidate all tokens and sessions.

**Independent Test**: Can be fully tested by implementing a mock persistent store and verifying that authorization codes, access tokens, refresh tokens, and client registrations survive simulated "restarts" (clearing in-memory state while persistent state remains).

**Acceptance Scenarios**:

1. **Given** a PostgreSQL-backed OAuth state implementation, **When** the server restarts after a user has obtained an access token, **Then** the access token remains valid.
2. **Given** a registered OAuth client stored in PostgreSQL, **When** the server restarts, **Then** the client registration is preserved and can be used for new authorization flows.
3. **Given** an in-progress authorization session, **When** implementing the state interface, **Then** pending authorizations can be stored and retrieved correctly.

---

### User Story 2 - Enterprise SSO Integration (Priority: P1)

An enterprise customer needs to integrate the MCP server with their existing identity provider (Active Directory, LDAP, Okta). They implement the authentication interface to validate credentials against their IdP instead of the hard-coded demo credentials.

**Why this priority**: Enterprise customers require integration with their existing authentication infrastructure. This is a blocking requirement for enterprise adoption.

**Independent Test**: Can be fully tested by implementing a mock LDAP connector and verifying that credential validation correctly delegates to the mock, returning success for valid credentials and failure for invalid ones.

**Acceptance Scenarios**:

1. **Given** an LDAP-backed authentication implementation, **When** a user logs in with valid LDAP credentials, **Then** authentication succeeds and an authorization code is issued.
2. **Given** an LDAP-backed authentication implementation, **When** a user logs in with invalid credentials, **Then** authentication fails with an appropriate error.
3. **Given** multiple authentication backends configured, **When** the typeclass is polymorphic over the monad, **Then** the implementation can perform IO operations (LDAP queries) as needed.

---

### User Story 3 - Development and Testing with In-Memory State (Priority: P2)

A developer runs the MCP server locally for development/testing. They use the default in-memory implementations for both OAuth state and authentication, getting the current behavior with no configuration changes.

**Why this priority**: Backward compatibility with existing usage is essential. The abstraction should not break existing development workflows.

**Independent Test**: Can be fully tested by running the existing demo server and verifying that all OAuth flows (client registration, authorization, token exchange, refresh) continue to work identically.

**Acceptance Scenarios**:

1. **Given** the default in-memory OAuth state implementation, **When** running the example server with `--oauth`, **Then** all OAuth endpoints function identically to current behavior.
2. **Given** the default hard-coded credential store implementation, **When** logging in with demo/demo123 or admin/admin456, **Then** authentication succeeds.
3. **Given** the default implementations, **When** no explicit configuration is provided, **Then** the server uses the in-memory/demo implementations automatically.

---

### User Story 4 - Custom Token Lifecycle Management (Priority: P3)

An operator needs custom token lifecycle management (e.g., revocation lists, token rotation policies, audit logging). By implementing the OAuth state interface, they can intercept all state mutations and add custom behavior.

**Why this priority**: Advanced use cases for security-conscious deployments. Less common but enabled by the abstraction.

**Independent Test**: Can be fully tested by implementing a wrapper that logs all state mutations and verifying that authorization code creation, token issuance, and token refresh all trigger the expected log entries.

**Acceptance Scenarios**:

1. **Given** a custom OAuth state implementation with audit logging, **When** a token is issued, **Then** the issuance event is logged with user, client, and timestamp.
2. **Given** a custom implementation with token revocation, **When** a token is revoked externally, **Then** subsequent validation fails.

---

### Edge Cases

- **Store unavailability**: Operations fail fast with a typed error; retry logic and circuit breakers are infrastructure concerns outside the typeclass contract.
- **Concurrent access**: Implementation-defined atomicity; each implementation ensures its own consistency guarantees (e.g., STM for in-memory, row-level locking for SQL).
- **Auth backend timeout**: Fail fast with typed error; consistent with store unavailability handling. Retry/fallback logic is outside typeclass scope.
- **Partial failures**: Compound operations (e.g., token exchange) are all-or-nothing; implementations must ensure no partial state is persisted on failure.
- **Multi-instance scaling**: Typeclass defines single-instance semantics; distributed coordination (if needed) is handled by the storage backend (e.g., PostgreSQL concurrency control), not the typeclass contract.
- **Observability**: Tracing is the responsibility of implementations; they are encouraged to include a specific tracer in their environment type (`OAuthStateEnv`, `AuthBackendEnv`), which users wire up at application composition time.

## Clarifications

### Session 2025-12-11

- Q: Which type system should be the canonical source of truth for OAuth state types? → A: OAuth.Types newtypes (ClientId, RedirectUri, etc.) are canonical; HTTP.hs converts Text ↔ newtype at boundary
- Q: Where should Text ↔ newtype boundary conversions be implemented? → A: Servant combinators (FromHttpApiData/ToHttpApiData instances); these ARE the boundary and enable property-based round-trip tests
- Q: How should invalid data (e.g., empty redirect URI list) be handled at the HTTP boundary? → A: Fail at boundary (400) via FromJSON/FromHttpApiData; make illegal states unrepresentable in types
- Q: What migration strategy should be used for the 15+ existing HTTP handlers? → A: Incremental per-handler; each handler migrated separately with its own nested bead under mcp-51r
- Q: Should PendingAuthorization use OAuth.Types newtypes or raw Text for its fields? → A: Same newtypes (ClientId, RedirectUri, etc.); already validated at request parse time
- Q: How should the system handle OAuth state store unavailability? → A: Fail fast with typed error (retries are infrastructure-level concerns)
- Q: How should concurrent access to the same authorization code be handled? → A: Implementation-defined atomicity (STM, row locking, etc.)
- Q: How should partial failures in compound operations be handled? → A: Fail entirely; all-or-nothing semantics, no partial state
- Q: What happens when an authentication backend times out? → A: Fail fast with typed error (consistent with store unavailability)
- Q: Should the typeclass design account for multi-instance/distributed deployment? → A: No; single-instance semantics, distributed coordination handled by storage backend
- Q: Who is responsible for tracing/observability? → A: Implementations; encouraged to include tracer in their environment type, users wire it up
- Q: Should the typeclasses document formal laws? → A: Yes; key round-trip laws (store/lookup, delete removes, idempotence) for property-based testing
- Q: Should lookup operations filter expired entries or return raw data? → A: Filter expired; enables backend TTL mechanisms (e.g., Redis TTL) and keeps expiry enforcement in store

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST provide an `OAuthStateStore` typeclass with operations for managing authorization codes, access tokens, refresh tokens, registered clients, and pending authorizations.
- **FR-002**: System MUST provide an `AuthBackend` typeclass with operations for validating user credentials.
- **FR-003**: Both typeclasses MUST be polymorphic over a constrained monad type parameter `m` (three-layer cake architecture), allowing implementations to require `MonadIO`, `MonadState`, etc. in their instance contexts.
- **FR-004**: System MUST provide an in-memory implementation of `OAuthStateStore` using `TVar` that maintains current behavior.
- **FR-005**: System MUST provide a hard-coded credential implementation of `AuthBackend` that maintains current demo behavior (demo/demo123, admin/admin456).
- **FR-006**: All existing OAuth endpoints MUST continue to function identically when using the default implementations.
- **FR-007**: The typeclass operations MUST return results in the monadic context, allowing implementations to perform effectful operations.
- **FR-008**: The `OAuthStateStore` typeclass MUST support atomic operations where the current implementation uses `atomically` with `TVar`.
- **FR-009**: The `AuthBackend` typeclass MUST allow implementations to access configuration (e.g., credential store, salt) needed for validation.
- **FR-010**: Each typeclass MUST define an associated error type that implementations can customize for their specific failure modes.
- **FR-011**: Each typeclass MUST define an associated environment type that implementations can customize for their specific configuration/state needs.
- **FR-012**: Handler functions MUST use `MonadError e m` with prism constraints (e.g., `AsType (OAuthStateError m) e`) to compose errors from multiple typeclasses into a unified error type.
- **FR-013**: Handler functions MUST use `MonadReader env m` with lens constraints (e.g., `HasType (OAuthStateEnv m) env`) to compose environments from multiple typeclasses into a unified environment type.
- **FR-014**: The typeclasses MUST be framework-agnostic; Servant integration MUST use `hoistServer` to translate the polymorphic error type to `ServerError` at the boundary.
- **FR-018**: All OAuth.Types newtypes (`ClientId`, `RedirectUri`, `AuthCodeId`, `GrantType`, etc.) MUST implement `FromHttpApiData` and `ToHttpApiData` instances for Servant boundary conversion. These instances serve as the canonical Text ↔ newtype conversion layer.
- **FR-019**: The `FromHttpApiData`/`ToHttpApiData` instances MUST have property-based round-trip tests verifying `parseUrlPiece . toUrlPiece ≡ Right`.
- **FR-020**: `FromJSON` instances for OAuth types with invariants (e.g., `NonEmpty RedirectUri`) MUST reject invalid data at parse time, returning appropriate JSON parse errors that Servant translates to 400 Bad Request. Domain error types MUST NOT include cases for structurally invalid input.
- **FR-021**: HTTP handler migration from Text-based to newtype-based OAuth state MUST be incremental (one handler at a time). Each handler migration is tracked as a nested bead under the parent unification task (mcp-51r). Old and new patterns may coexist during migration.
- **FR-015**: The design MUST allow the same handler logic to be used outside of Servant (e.g., in CLI tools, tests, or other frameworks) by providing different error/environment interpreters.
- **FR-016**: Each typeclass MUST document key algebraic laws enabling property-based testing:
  - Round-trip: `lookupX k` after `storeX k v` returns `Just v` (if not expired)
  - Delete: `lookupX k` after `deleteX k` returns `Nothing`
  - Idempotence: `storeX k v >> storeX k v` equivalent to `storeX k v`
  - Overwrite: `storeX k v2` after `storeX k v1` makes `lookupX k` return `Just v2`
- **FR-017**: Lookup operations for time-bounded entities (auth codes, pending authorizations) MUST filter expired entries, returning `Nothing` for expired items. This enables backend TTL mechanisms (e.g., Redis EXPIRE, PostgreSQL scheduled cleanup) and keeps expiry enforcement within the store abstraction.

### Key Entities

- **OAuthStateStore**: Typeclass abstracting persistence of OAuth protocol state (authorization codes, tokens, clients, sessions). Defines associated types `OAuthStateError` and `OAuthStateEnv` for implementation-specific failures and environment.
- **AuthBackend**: Typeclass abstracting user authentication/credential validation. Defines associated types `AuthBackendError` and `AuthBackendEnv` for implementation-specific failures and environment.
- **AuthorizationCode**: Data type from OAuth.Types representing an authorization code with PKCE, expiry, and user info. Uses type-safe newtypes (`AuthCodeId`, `CodeChallenge`, etc.).
- **ClientInfo**: Data type from OAuth.Types representing a registered OAuth client. Uses type-safe newtypes (`ClientId`, `NonEmpty RedirectUri`, `Set GrantType`).
- **PendingAuthorization**: Data type from OAuth.Types representing an in-progress login session. Uses type-safe newtypes (`ClientId`, `RedirectUri`, `CodeChallenge`, etc.) for all OAuth parameters (validated at request parse time).
- **AuthUser**: Existing data type representing an authenticated user.
- **Error Prisms**: Constraints using `AsType` from generic-lens to inject/project typeclass-specific errors into a unified error sum type.
- **Environment Lenses**: Constraints using `HasType` from generic-lens to access typeclass-specific environments from a unified environment product type.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: All existing OAuth integration tests pass without modification when using default implementations.
- **SC-002**: A developer can implement a new storage backend by providing instances for the typeclasses without modifying core server code.
- **SC-003**: The abstraction adds no measurable latency overhead (within 5%) for typical OAuth operations when using in-memory implementations.
- **SC-004**: Type errors at compile time prevent mixing incompatible implementations (e.g., using IO-based storage with a pure test monad).
- **SC-005**: The same handler functions can be used in non-Servant contexts (e.g., tests, CLI) by providing appropriate error interpreters.

## Assumptions

- The server will be parameterized over the typeclass constraints at the top level (e.g., `runServerHTTP` gains additional constraints).
- Each typeclass defines associated error and environment types; implementations define their specific failure modes and configuration needs.
- Handler functions use `MonadError e m` with `AsType` prism constraints from generic-lens to compose errors.
- Handler functions use `MonadReader env m` with `HasType` lens constraints from generic-lens to compose environments.
- Servant integration uses `hoistServer` to translate the polymorphic `m` to `Handler`, mapping errors to `ServerError`.
- The current `OAuthConfig` may be split or restructured to fit into the environment types.
- JWT signing and validation remain unchanged; only state persistence and credential validation are abstracted.
- The `generic-lens` package provides both `AsType` (for error prisms) and `HasType` (for environment lenses) constraints.
