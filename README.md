# hs-inngest

A native Haskell SDK for [Inngest](https://www.inngest.com) — serve durable
functions over HTTP, author them in a monadic step DSL, and send events. No FFI;
the wire protocol is ported directly from `inngest-py`.

## Status

Complete v1 surface (HTTP serve; Connect/WebSocket transport is out of scope):

- **Serve** an app over HTTP (`GET` introspect / `POST` execute / `PUT` sync).
- **Durable step DSL**: `stepRun`, `sleep`, `sleepUntil`, `waitForEvent`,
  `invoke`, and `parallel` (server-driven planning).
- **Stateless replay** with SHA1 step-id hashing + per-run dedup, memoization,
  and interrupt-as-`ExceptT`.
- **Signing**: RFC 8785 JCS canonicalization, HMAC request-verify (canonical
  body) / response-sign (raw body), key hashing, key fallback.
- **Sync**: out-of-band `POST /fn/register` and in-band response, with
  null-stripping.
- **Errors**: retriable / non-retriable (`x-inngest-no-retry`) / retry-after.
- **Client**: event send + authed `use_api` run fetches.
- Dev-server and Cloud modes.

## Build & test

This package ships a Nix flake providing GHC 9.10.3, all dependencies, and
`cabal-install` offline:

```sh
nix develop .#dev --command cabal build
nix develop .#dev --command cabal test
```

## End-to-end example (against a real dev server)

`example/Main.hs` serves an app, registers it with a locally-running
[Inngest dev server](https://www.inngest.com/docs/local-development), sends an
event, and drives a `step → sleep → parallel → step` pipeline to completion.

Run the dev server and the example together with process-compose:

```sh
nix develop .#dev --command process-compose up
```

or headless (starts the dev server, runs the example, tears down):

```sh
nix develop .#dev --command bash scripts/e2e.sh
```

Expected: `E2E PASS: stepRun + sleep + parallel + waitForEvent + invoke + onFailure`.
This exercises sync, event send, the 206 replay loop, parallel
planning/targeting, sleep, waitForEvent, invoke, and onFailure against the real
server. (The Inngest dev server is SSPL-licensed, so the flake sets `allowUnfree`.)

## CI

`.github/workflows/ci.yml` installs Nix and runs three separate steps on every
push/PR — **Build** (`cabal build all`), **Unit tests**
(`cabal test --test-show-details=direct`, so the hspec tree is visible), and
**End-to-end** against a real Inngest dev server (`scripts/e2e.sh`).

## Logging

The SDK bakes in no logger; a function's base monad `m` (any `MonadUnliftIO`)
carries whatever logging the user wants. The example and test suite run in
`KatipContextT IO`, emitting structured JSON logs from step bodies — and because
memoized steps don't re-run, those logs are naturally de-duplicated across the
replay loop (log inside steps, not around them).

## Quick sketch

```haskell
import Inngest

myFn :: Function IO
myFn = createFunction (defaultFnOpts "welcome")
                      [TriggerEvent "user/created" Nothing] $ \_ctx _event -> do
  user <- stepRun "load-user" loadUser
  sleep "cool-off" (hours 1)
  stepRun "email" (sendWelcome user)

app :: Application
app = toApplication id (devConfig "my-app") "http://localhost:8288/api/inngest" [myFn]
```

Mount `toApplication` (WAI) directly, or embed via `inngestServer` in a Servant
tree (the endpoint is exposed as a `Raw` `InngestAPI`).

## Layout

| Module | Responsibility |
|---|---|
| `Inngest.Types` | events, opcodes, `Duration`, triggers, request decode |
| `Inngest.Config` | dev/cloud modes, keys, origins |
| `Inngest.Signing` | JCS canonicalize, HMAC verify/sign, key hashing |
| `Inngest.Step` | `InngestT`, step DSL, hashing/memo/interrupt, `parallel`, `invoke` |
| `Inngest.Function` | `createFunction`, `FnOpts`, `FunctionConfig`, `onFailure` |
| `Inngest.Execution` | replay driver → `(status, body, retry headers)` |
| `Inngest.Errors` | retriable / non-retriable / retry-after taxonomy |
| `Inngest.Sync` | register request + in-band response |
| `Inngest.Client` | event send + `use_api` fetches |
| `Inngest.Serve.Servant` | `handleInngest`, `toApplication`, `InngestAPI` |
