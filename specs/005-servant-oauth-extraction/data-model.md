# Data Model: Remove MCP Imports from Servant.OAuth2.IDP

**Branch**: `005-servant-oauth-extraction`
**Date**: 2025-12-18

## Overview

This refactoring introduces 4 new types and moves 4 existing types to new modules. No data storage changes - this is a compile-time module boundary reorganization.

---

## New Types

### OAuthEnv

**Module**: `Servant.OAuth2.IDP.Config`

**Purpose**: Protocol-agnostic OAuth 2.1 server configuration. Contains all settings needed by OAuth handlers without MCP-specific concerns (demo mode, providers).

```haskell
data OAuthEnv = OAuthEnv
    { oauthBaseUrl :: Text
    -- ^ Base URL for OAuth endpoints (e.g., "https://api.example.com")
    -- Used to construct authorization_endpoint, token_endpoint in metadata

    , oauthAuthCodeExpiry :: NominalDiffTime
    -- ^ Authorization code lifetime (typically 10 minutes per OAuth spec)
    -- Codes older than this are rejected during token exchange

    , oauthAccessTokenExpiry :: NominalDiffTime
    -- ^ Access token lifetime (typically 1 hour)
    -- Used when generating JWT 'exp' claim

    , oauthLoginSessionExpiry :: NominalDiffTime
    -- ^ Pending authorization session lifetime
    -- Time user has to complete login after authorization request

    , oauthAuthCodePrefix :: Text
    -- ^ Prefix for generated authorization codes (e.g., "code_")
    -- Helps identify token types in logs/debugging

    , oauthRefreshTokenPrefix :: Text
    -- ^ Prefix for generated refresh tokens (e.g., "rt_")

    , oauthClientIdPrefix :: Text
    -- ^ Prefix for dynamically registered client IDs (e.g., "client_")

    , oauthSupportedScopes :: [Scope]
    -- ^ Scopes this server understands (returned in metadata)

    , oauthSupportedResponseTypes :: [ResponseType]
    -- ^ Response types supported (typically [ResponseTypeCode])

    , oauthSupportedGrantTypes :: [GrantType]
    -- ^ Grant types supported (typically [GrantTypeAuthorizationCode, GrantTypeRefreshToken])

    , oauthSupportedAuthMethods :: [ClientAuthMethod]
    -- ^ Token endpoint auth methods (typically [ClientSecretPost, ClientSecretBasic, None])

    , oauthSupportedCodeChallengeMethods :: [CodeChallengeMethod]
    -- ^ PKCE methods supported (typically [S256], Plain discouraged)
    }
    deriving (Generic)
```

**Validation Rules**:
- `oauthBaseUrl` must be non-empty
- `oauthAuthCodeExpiry` must be positive
- `oauthAccessTokenExpiry` must be positive
- `oauthLoginSessionExpiry` must be positive
- At least one entry in each supported* list

**Smart Constructor**:
```haskell
mkOAuthEnv :: Text -> OAuthEnv
-- Creates OAuthEnv with sensible defaults:
-- - authCodeExpiry: 600 seconds (10 minutes)
-- - accessTokenExpiry: 3600 seconds (1 hour)
-- - loginSessionExpiry: 600 seconds (10 minutes)
-- - prefixes: "code_", "rt_", "client_"
-- - supported*: standard OAuth 2.1 values
```

---

### OAuthTrace

**Module**: `Servant.OAuth2.IDP.Trace`

**Purpose**: Structured trace events for OAuth flows. Enables observability without coupling to MCP trace infrastructure.

```haskell
data OAuthTrace
    = TraceClientRegistration
        { trClientId :: ClientId
        , trClientName :: ClientName
        }
    -- ^ Client successfully registered via /register endpoint

    | TraceAuthorizationRequest
        { trClientId :: ClientId
        , trScopes :: [Scope]
        , trHasState :: Bool
        }
    -- ^ Authorization request received at /authorize

    | TraceLoginPageServed
        { trSessionId :: SessionId
        }
    -- ^ Login page HTML returned to user agent

    | TraceLoginAttempt
        { trUsername :: Text
        , trSuccess :: Bool
        }
    -- ^ User submitted login form

    | TracePKCEValidation
        { trIsValid :: Bool
        }
    -- ^ PKCE code_verifier validated against code_challenge

    | TraceAuthorizationGranted
        { trClientId :: ClientId
        , trUserId :: UserId
        }
    -- ^ User approved authorization, code generated

    | TraceAuthorizationDenied
        { trClientId :: ClientId
        , trReason :: Text
        }
    -- ^ User denied authorization or validation failed

    | TraceTokenExchange
        { trGrantType :: GrantType
        , trSuccess :: Bool
        }
    -- ^ Token endpoint request processed

    | TraceTokenRefresh
        { trSuccess :: Bool
        }
    -- ^ Refresh token exchange attempted

    | TraceSessionExpired
        { trSessionId :: SessionId
        }
    -- ^ Pending authorization session expired

    | TraceValidationError
        { trErrorType :: Text
        , trDetail :: Text
        }
    -- ^ Semantic validation failed (e.g., redirect_uri mismatch)

    deriving (Show, Eq)
```

**Rendering Function**:
```haskell
renderOAuthTrace :: OAuthTrace -> Text
-- Human-readable rendering for log output
```

---

## Moved Types

### OAuthMetadata

**From**: `MCP.Server.Auth`
**To**: `Servant.OAuth2.IDP.Metadata`

**Purpose**: RFC 8414 OAuth Authorization Server Metadata discovery response.

```haskell
data OAuthMetadata = OAuthMetadata
    { issuer :: Text
    , authorizationEndpoint :: Text
    , tokenEndpoint :: Text
    , registrationEndpoint :: Maybe Text
    , userInfoEndpoint :: Maybe Text
    , jwksUri :: Maybe Text
    , scopesSupported :: Maybe [Scope]
    , responseTypesSupported :: [ResponseType]
    , grantTypesSupported :: Maybe [GrantType]
    , tokenEndpointAuthMethodsSupported :: Maybe [ClientAuthMethod]
    , codeChallengeMethodsSupported :: Maybe [CodeChallengeMethod]
    }
    deriving (Show, Generic)
```

**JSON Instances**: Custom `FromJSON`/`ToJSON` with snake_case keys per RFC 8414.

---

### ProtectedResourceMetadata

**From**: `MCP.Server.Auth`
**To**: `Servant.OAuth2.IDP.Metadata`

**Purpose**: RFC 9728 OAuth Protected Resource Metadata.

```haskell
data ProtectedResourceMetadata = ProtectedResourceMetadata
    { resource :: Text
    -- ^ Protected resource identifier (MUST be absolute URI with https)

    , authorizationServers :: [Text]
    -- ^ List of authorization server issuer identifiers

    , scopesSupported :: Maybe [Scope]
    -- ^ Scope values the resource server understands

    , bearerMethodsSupported :: Maybe [Text]
    -- ^ Token presentation methods (default: ["header"])

    , resourceName :: Maybe Text
    -- ^ Human-readable name for display

    , resourceDocumentation :: Maybe Text
    -- ^ URL of developer documentation
    }
    deriving (Show, Generic)
```

**JSON Instances**: Custom `FromJSON`/`ToJSON` with snake_case keys per RFC 9728.

---

## Moved Functions

### validateCodeVerifier

**From**: `MCP.Server.Auth`
**To**: `Servant.OAuth2.IDP.PKCE`

```haskell
validateCodeVerifier :: CodeVerifier -> CodeChallenge -> Bool
validateCodeVerifier (CodeVerifier verifier) (CodeChallenge challenge) =
    generateCodeChallenge verifier == challenge
```

**Semantics**: Returns `True` iff `SHA256(base64url(verifier)) == challenge`.

**Property**: `forall v. validateCodeVerifier v (mkChallenge v) == True`
  where `mkChallenge = CodeChallenge . generateCodeChallenge . unCodeVerifier`

---

### generateCodeChallenge

**From**: `MCP.Server.Auth`
**To**: `Servant.OAuth2.IDP.PKCE`

```haskell
generateCodeChallenge :: Text -> Text
generateCodeChallenge verifier =
    let verifierBytes = TE.encodeUtf8 verifier
        challengeHash = hashWith SHA256 verifierBytes
        challengeBytes = convert challengeHash :: ByteString
     in TE.decodeUtf8 $ B64URL.encodeUnpadded challengeBytes
```

**Semantics**: S256 code challenge per RFC 7636 Section 4.2.

**Property**: Deterministic - same input always produces same output.

---

## Type Relationships

```
┌─────────────────────────────────────────────────────────────────┐
│                      Servant.OAuth2.IDP.*                       │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  ┌─────────────┐     ┌─────────────┐     ┌─────────────────┐   │
│  │  Config.hs  │     │  Trace.hs   │     │  Metadata.hs    │   │
│  │             │     │             │     │                 │   │
│  │  OAuthEnv   │     │ OAuthTrace  │     │ OAuthMetadata   │   │
│  │             │     │             │     │ ProtectedRes... │   │
│  └─────────────┘     └─────────────┘     └─────────────────┘   │
│                                                                 │
│  ┌─────────────┐                                               │
│  │   PKCE.hs   │        Uses:                                  │
│  │             │        - Servant.OAuth2.IDP.Types             │
│  │ validate... │          (CodeVerifier, CodeChallenge,        │
│  │ generate... │           Scope, ClientId, etc.)              │
│  └─────────────┘                                               │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
                              │
                              │ contramap (adapter)
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                          MCP.*                                  │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  ┌─────────────────────┐     ┌─────────────────────────────┐   │
│  │ MCP.Server.HTTP     │     │ MCP.Trace.HTTP              │   │
│  │ .AppEnv             │     │                             │   │
│  │                     │     │ HTTPOAuth (adapter to       │   │
│  │ AppEnv contains:    │     │  Servant.OAuth2.IDP.Trace)  │   │
│  │ - envOAuthEnv ::    │     │                             │   │
│  │     OAuthEnv        │     │                             │   │
│  └─────────────────────┘     └─────────────────────────────┘   │
│                                                                 │
│  ┌─────────────────────┐                                       │
│  │ MCP.Server.Auth     │                                       │
│  │                     │                                       │
│  │ OAuthConfig (demo   │                                       │
│  │   mode, providers)  │                                       │
│  │ generateCodeVerifier│                                       │
│  │   (IO action)       │                                       │
│  └─────────────────────┘                                       │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

---

## Migration Notes

### For Handlers

Before:
```haskell
import MCP.Server.Auth (OAuthConfig (..), validateCodeVerifier)
import MCP.Server.HTTP.AppEnv (HTTPServerConfig (..))

handleToken :: (HasType HTTPServerConfig env, ...) => ...
handleToken = do
    config <- view (typed @HTTPServerConfig)
    let oauthCfg = fromMaybe defaultConfig (httpOAuthConfig config)
    let valid = validateCodeVerifier verifier challenge
```

After:
```haskell
import Servant.OAuth2.IDP.Config (OAuthEnv (..))
import Servant.OAuth2.IDP.PKCE (validateCodeVerifier)

handleToken :: (HasType OAuthEnv env, ...) => ...
handleToken = do
    oauthEnv <- view (typed @OAuthEnv)
    let valid = validateCodeVerifier verifier challenge
```

### For MCP Integration

```haskell
-- In MCP.Server.HTTP.AppEnv

import Servant.OAuth2.IDP.Config (OAuthEnv (..))
import Servant.OAuth2.IDP.Trace (OAuthTrace)

-- Build OAuthEnv from HTTPServerConfig
mkOAuthEnv :: HTTPServerConfig -> OAuthEnv
mkOAuthEnv HTTPServerConfig{..} =
    let oauthCfg = fromMaybe defaultOAuthConfig httpOAuthConfig
    in OAuthEnv
        { oauthBaseUrl = httpBaseUrl
        , oauthAuthCodeExpiry = fromIntegral (authCodeExpirySeconds oauthCfg)
        , oauthAccessTokenExpiry = fromIntegral (accessTokenExpirySeconds oauthCfg)
        , oauthLoginSessionExpiry = fromIntegral (loginSessionExpirySeconds oauthCfg)
        , oauthAuthCodePrefix = authCodePrefix oauthCfg
        , oauthRefreshTokenPrefix = refreshTokenPrefix oauthCfg
        , oauthClientIdPrefix = clientIdPrefix oauthCfg
        , oauthSupportedScopes = supportedScopes oauthCfg
        , oauthSupportedResponseTypes = supportedResponseTypes oauthCfg
        , oauthSupportedGrantTypes = supportedGrantTypes oauthCfg
        , oauthSupportedAuthMethods = supportedAuthMethods oauthCfg
        , oauthSupportedCodeChallengeMethods = supportedCodeChallengeMethods oauthCfg
        }

-- Adapt trace types
mkOAuthTracer :: IOTracer HTTPTrace -> IOTracer OAuthTrace
mkOAuthTracer = contramap HTTPOAuth
```
