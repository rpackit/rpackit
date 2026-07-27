# Changelog

## rpackit (development version)

- `rlang` is now an explicit runtime dependency because rpackit’s
  [`cli::cli_abort()`](https://cli.r-lib.org/reference/cli_abort.html)
  error paths require it. Minimal installations no longer lose the
  original diagnostic while trying to construct a structured error.
- The default portable R registry now uses the concise `rpackit/runtime`
  repository, and Windows release artifacts are published from
  `rpackit/runtime-win`. Existing `portable-r-*` artifact and schema
  names remain unchanged for compatibility.
- Dependency plans now fail visibly when `renv.lock` omits a required
  package, when a locked package version violates `DESCRIPTION`, or when
  `Remotes` lacks exact lockfile provenance. Remote specifications are
  not retained in the returned plan, and credential-bearing lockfile URL
  components are redacted. Desktop preparation enforces these errors
  before copying a runtime and verifies every required package
  constraint again after restore or installation; manifests record the
  constraints and whether they were verified. Bundle validation binds
  those records to the copied app, and runtime verification checks their
  installed versions again.
- The pkgdown site now declares its unreleased status and uses explicit
  project links, so local and CI builds do not depend on CRAN or
  Bioconductor package discovery.
- [`check_app()`](https://rpackit.github.io/rpackit/reference/check_app.md)
  now detects direct [`system()`](https://rdrr.io/r/base/system.html),
  [`system2()`](https://rdrr.io/r/base/system2.html), and `shell()`
  calls from parsed R syntax and reports their file and source line.
  Comments, strings, object methods, and same-named functions in
  non-base namespaces no longer create false target blockers. A new
  getting-started vignette connects inspection, dependency planning,
  portable-resource preparation, validation, and managed launch/cleanup.
- Desktop launches now enforce a fresh 256-bit session credential across
  dynamic HTTP, static resources, and WebSocket sessions using Shiny’s
  shared secret request header.
- Session credentials move through a current-account-private, one-time
  file that the launcher deletes before any app or port validation.
  Windows DACLs are restricted and verified for the current account plus
  SYSTEM; POSIX permissions are verified as directory mode 0700 and file
  mode 0600. The credential is no longer placed by rpackit in child
  arguments, environment variables, URLs, manifests, generated event
  fields, status objects, or print output. Launcher errors and returned
  logs/events are redacted; arbitrary app output remains inside the
  private raw log and is outside that guarantee.
- Added a post-bind `listening` lifecycle event and authenticated
  readiness probe, removing the previous pre-bind token-disclosure race.
- Added
  [`desktop_app_launch_config()`](https://rpackit.github.io/rpackit/reference/desktop_app_launch_config.md)
  as the explicit, redaction-safe handoff contract for trusted
  development and third-party native consumers. Cleanup prevents future
  handoffs and clears the managed handle, but cannot revoke
  caller-retained copies. A generated Tauri app does not call or
  serialize this R-level handoff; its baseline is transport contract
  version 2’s authenticated loopback reverse proxy, not a direct request
  interceptor or bare proxy.
- Documented the pre-release `rpackit-tauri` Windows acceptance spike: a
  one-time native bootstrap secret creates a host-only HttpOnly
  proxy-session cookie through an HTTP response, and the proxy injects
  the separate Shiny secret only after authenticating later HTTP and
  WebSocket requests. This is development evidence, not a generated app
  or release-ready transport; the full fixed-runtime, crash-persistence,
  browser-escape, resource-abuse, malformed-upstream, and
  listener-overlap matrix remains open.
- New bundles use launcher protocol 2 and report
  `network_token_enforced = TRUE`. Legacy protocol-1 bundles remain
  inspectable only when manifest and launcher content agree, and are
  refused at launch with a rebuild instruction.
- Directory-app `DESCRIPTION` behavior remains intact, unauthorized
  WebSocket tests require positive close evidence, and incomplete
  private-file cleanup retains a process handle for an explicit retry.
  Direct sidecar launches also work correctly when the optional
  `--control` argument is omitted.

## rpackit 0.1.2

- Added
  [`resolve_portable_runtime()`](https://rpackit.github.io/rpackit/reference/resolve_portable_runtime.md)
  for verified schema-v1 registry selection, HTTPS/local artifact
  sources, SHA-256 verification, traversal-aware ZIP extraction, atomic
  caching, and offline cache reuse.
- `prepare_desktop(runtime_dir = NULL)` now resolves a verified runtime
  for the current platform automatically while preserving explicit
  runtime paths.
- Desktop preparation now rejects incompatible `renv.lock` R versions
  and DESCRIPTION R constraints before copying a runtime or installing
  packages.
- Desktop manifests and returned bundle objects now retain selected
  runtime version and checksum-bound registry/artifact provenance.
  Validation remains compatible with earlier schema-v1 bundles that
  predate these additive fields.

## rpackit 0.1.1

- Added
  [`plan_dependencies()`](https://rpackit.github.io/rpackit/reference/plan_dependencies.md)
  for non-executing dependency discovery from parsed R calls,
  `DESCRIPTION`, and `renv.lock`.
- Added explicit dependency precedence, provenance, diagnostics, and
  clear read/parse failures.
- Updated
  [`check_app()`](https://rpackit.github.io/rpackit/reference/check_app.md)
  to use parsed source dependencies while preserving its existing
  `packages` result.
- Added
  [`prepare_desktop()`](https://rpackit.github.io/rpackit/reference/prepare_desktop.md)
  and
  [`validate_desktop_bundle()`](https://rpackit.github.io/rpackit/reference/validate_desktop_bundle.md)
  for atomic, portable-R-backed desktop resource bundles with an
  explicit launcher and versioned manifest contract.
- Added
  [`start_desktop_app()`](https://rpackit.github.io/rpackit/reference/start_desktop_app.md),
  [`desktop_app_status()`](https://rpackit.github.io/rpackit/reference/desktop_app_status.md),
  and
  [`stop_desktop_app()`](https://rpackit.github.io/rpackit/reference/stop_desktop_app.md)
  for real Shiny child-process readiness, status, graceful shutdown,
  forced tracked-process termination, and structured startup failures.
- Desktop status distinguishes the processx wrapper PID from the
  launcher-reported runtime PID, as required by portable R on Windows,
  and uses a create-time-aware runtime handle when confirming cleanup.
- Extended the launcher with versioned NDJSON lifecycle events and an
  optional private control-file argument suitable for a future Tauri
  sidecar, while continuing to record that network token enforcement is
  not implemented.
- [`doctor()`](https://rpackit.github.io/rpackit/reference/doctor.md)
  now requires the Tauri CLI, a JavaScript package manager, the native
  platform toolchain, and WebView2 on Windows before reporting that a
  Tauri desktop build is supported.
