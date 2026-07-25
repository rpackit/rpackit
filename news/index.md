# Changelog

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
