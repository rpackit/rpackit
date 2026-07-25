# rpackit 0.1.1

- Added `plan_dependencies()` for non-executing dependency discovery from
  parsed R calls, `DESCRIPTION`, and `renv.lock`.
- Added explicit dependency precedence, provenance, diagnostics, and clear
  read/parse failures.
- Updated `check_app()` to use parsed source dependencies while preserving its
  existing `packages` result.
- Added `prepare_desktop()` and `validate_desktop_bundle()` for atomic,
  portable-R-backed desktop resource bundles with an explicit launcher and
  versioned manifest contract.
- `doctor()` now requires the Tauri CLI, a JavaScript package manager, the
  native platform toolchain, and WebView2 on Windows before reporting that a
  Tauri desktop build is supported.
