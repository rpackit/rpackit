# Prepare portable desktop resources for an R application

`prepare_desktop()` creates the versioned resource contract consumed by
the future rpackit Tauri shell:

## Usage

``` r
prepare_desktop(
  app_dir,
  runtime_dir = NULL,
  output_dir = NULL,
  app_name = NULL,
  install_packages = TRUE,
  repos = getOption("repos"),
  verify_runtime = TRUE,
  quiet = FALSE,
  r_version = NULL,
  registry = getOption("rpackit.runtime_registry", .rpackit_runtime_registry),
  cache_dir = NULL,
  offline = FALSE
)
```

## Arguments

- app_dir:

  Path to a supported Shiny application.

- runtime_dir:

  Path to an extracted portable R home, or `NULL` to resolve a verified
  runtime for the current platform and architecture.

- output_dir:

  New output directory. Defaults to `app_dir/dist/desktop-resources`.

- app_name:

  Human-readable application name.

- install_packages:

  Install required packages into the copied runtime.

- repos:

  Repository URLs used when installing packages.

- verify_runtime:

  Execute the supplied `Rscript` and read its exact R version before
  copying it. An explicit runtime is still probed when `renv.lock` or
  DESCRIPTION constrains R, even when this is `FALSE`, because
  compatibility must be established before copying or installation.

- quiet:

  Suppress the completion summary.

- r_version:

  Exact portable R version used for automatic resolution. Defaults to
  the version recorded in `renv.lock`, when present, or the newest
  verified version.

- registry:

  HTTPS URL or local path to a portable-R schema-v1 registry.

- cache_dir:

  Portable runtime cache directory.

- offline:

  Reuse an existing same-registry runtime cache entry without reading
  any registry or artifact.

## Value

An `rpackit_desktop_bundle` object. Its `runtime` field records the
explicit runtime path or the verified registry selection and provenance.

## Details

    output/
      resources/
        R/
        app/
        launcher.R
        rpackit.json

The application is inspected without execution. The supplied portable R
runtime is copied into the bundle, so the generated resources do not
depend on a system R installation at run time. By default, required
packages are installed into the copied runtime. A `renv.lock` uses
`renv::restore()`; otherwise the parsed dependency plan is installed
from `repos`. When `runtime_dir = NULL`, a verified runtime is resolved
from the portable-R registry and reused from a SHA-256-keyed user cache
when available. The lockfile R version and DESCRIPTION R constraint are
checked against the selected runtime before copying it or installing
packages.

The generated launcher accepts `--app`, `--port`, and a one-time
current-account-private `--token-file`, plus an optional private
`--control` path used for graceful shutdown. It consumes and deletes the
credential before validating the app or port, binds Shiny only to
`127.0.0.1`, and enforces that credential through Shiny's
`Shiny-Shared-Secret` checks for HTTP, static resources, and WebSockets.
Dynamic HTTP and WebSocket comparisons are constant-time. The manifest
records this authenticated transport explicitly. This function still
prepares resources; it does not claim to produce a Tauri executable or a
browser-compatible header injector.

Build inputs are never modified. Output is assembled in a sibling
staging directory and renamed into place only after validation. Existing
output is never overwritten.
