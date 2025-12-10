# Research: Structured Tracing with plow-log

**Feature**: 003-structured-tracing
**Date**: 2025-12-10

## Dependencies

### plow-log (^0.1.6)

**Source**: [Hackage - plow-log](https://hackage.haskell.org/package/plow-log)

**Decision**: Use plow-log as the tracing abstraction layer.

**Rationale**:
- Contravariant logging library agnostic to backend
- Provides `IOTracer` which hides MonadIO constraint: `newtype IOTracer a = IOTracer (forall m. MonadIO m => Tracer m a)`
- `traceWith` for emission: `traceWith :: TraceWith x m => x a -> a -> m ()`
- `filterTracer` for selective filtering: `filterTracer :: Applicative m => (a -> Bool) -> Tracer m a -> Tracer m a`
- Contravariant instance provides `contramap` for adapting trace types
- Semigroup/Monoid instances allow combining multiple tracers

**Key Types**:
```haskell
newtype Tracer m a = Tracer (a -> m ())
newtype IOTracer a = IOTracer (forall m. MonadIO m => Tracer m a)
```

**Key Functions**:
- `traceWith` - emit a trace event
- `contramap` - adapt tracer to different input type
- `filterTracer` - filter traces by predicate
- `voidTracer` - no-op tracer (for testing/disabling)

### plow-log-async (^0.1.4)

**Source**: [Hackage - plow-log-async](https://hackage.haskell.org/package/plow-log-async)

**Decision**: Use plow-log-async for async IO backend.

**Rationale**:
- Provides non-blocking trace emission via queue
- `withAsyncHandleTracer` creates async tracer writing to Handle
- Queue prevents trace emission from blocking request handling

**Key Function**:
```haskell
withAsyncHandleTracer :: MonadUnliftIO m
                      => Handle
                      -> Int  -- queue size
                      -> (IOTracer Text -> m a)
                      -> m a
```

**Alternatives Considered**:
- co-log: More complex, comonadic approach; plow-log simpler for our use case
- contra-tracer: Lower-level, no IOTracer convenience type
- Direct Debug.Trace: Not production-suitable, no async support

## Trace Type Hierarchy Design

**Decision**: Hierarchical sum types with composite constructors wrapping subtrace types.

**Rationale**:
- Aligns with architectural boundaries (module/env/layer)
- Each component receives tracer typed to its trace type
- `contramap` adapts at call sites
- Enables filtering by subsystem via pattern matching predicates

**Structure**:
```
MCPTrace (root)
├── MCPServer ServerTrace
├── MCPProtocol ProtocolTrace
├── MCPStdIO StdIOTrace
└── MCPHttp HTTPTrace
    └── HTTPOAuth OAuthTrace  (composite within HTTPTrace)

OAuthTrace (independent, no MCP imports)
├── OAuthClientRegistration ...
├── OAuthAuthorization ...
├── OAuthTokenExchange ...
└── ...
```

**Key Insight**: OAuthTrace is NOT nested under MCPTrace directly. Instead, HTTPTrace has a constructor `HTTPOAuth OAuthTrace` that wraps it. This allows OAuthTrace module to have zero MCP dependencies while still being composable into the MCP trace hierarchy.

## Render Function Design

**Decision**: Pure `render*Trace :: *Trace -> Text` functions, delegating to subcomponent renders.

**Rationale**:
- Pure functions satisfy Constitution Principle V
- Compositionality: `renderMCPTrace (MCPHttp t) = renderHTTPTrace t`
- Total functions (pattern match all constructors) satisfy Principle IV
- Deterministic output enables golden tests

**Pattern**:
```haskell
renderMCPTrace :: MCPTrace -> Text
renderMCPTrace = \case
    MCPServer t   -> "[Server] " <> renderServerTrace t
    MCPProtocol t -> "[Protocol] " <> renderProtocolTrace t
    MCPStdIO t    -> "[StdIO] " <> renderStdIOTrace t
    MCPHttp t     -> "[HTTP] " <> renderHTTPTrace t
```

## Tracer Threading Design

**Decision**: Pass `IOTracer ComponentTrace` as parameter to component initialization functions.

**Rationale**:
- Explicit dependency (no global state)
- Each component receives tracer typed to its own trace type
- Callers adapt via `contramap Constructor`

**Example**:
```haskell
-- In MCP.Server.HTTP
runServerHTTP :: HTTPServerConfig -> IOTracer HTTPTrace -> IO ()
runServerHTTP config tracer = do
    let oauthTracer = contramap HTTPOAuth tracer
    -- pass oauthTracer to OAuth handlers
```

**Consumer Setup** (fewer than 5 lines per SC-001):
```haskell
withAsyncHandleTracer stdout 1000 $ \textTracer -> do
    let mcpTracer = contramap renderMCPTrace textTracer
    let httpTracer = contramap MCPHttp mcpTracer
    runServerHTTP config httpTracer
```

## Cabal Dependencies

**Decision**: Add to library build-depends:
```
plow-log >= 0.1.6 && < 0.2
plow-log-async >= 0.1.4 && < 0.2
unliftio-core >= 0.2 && < 0.3  -- for MonadUnliftIO (plow-log-async dep)
```

**Rationale**:
- plow-log-async requires MonadUnliftIO for withAsyncHandleTracer
- Version bounds follow PVP, allowing patch updates

## No Remaining NEEDS CLARIFICATION

All technical decisions resolved. Ready for Phase 1 data model design.
