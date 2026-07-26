# Changelog

## rpackit (development version)

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
  as the explicit, redaction-safe handoff contract for a native
  exact-origin header injector or loopback proxy. Cleanup prevents
  future handoffs and clears the managed handle, but cannot revoke
  caller-retained copies.
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
