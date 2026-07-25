# Prepare portable desktop resources for an R application

`prepare_desktop()` creates the versioned resource contract consumed by
the future rpackit Tauri shell:

## Usage

``` r
prepare_desktop(
  app_dir,
  runtime_dir,
  output_dir = NULL,
  app_name = NULL,
  install_packages = TRUE,
  repos = getOption("repos"),
  verify_runtime = TRUE,
  quiet = FALSE
)
```

## Arguments

- app_dir:

  Path to a supported Shiny application.

- runtime_dir:

  Path to an extracted portable R home.

- output_dir:

  New output directory. Defaults to `app_dir/dist/desktop-resources`.

- app_name:

  Human-readable application name.

- install_packages:

  Install required packages into the copied runtime.

- repos:

  Repository URLs used when installing packages.

- verify_runtime:

  Execute the supplied `Rscript --version` before copying it.

- quiet:

  Suppress the completion summary.

## Value

An `rpackit_desktop_bundle` object.

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
from `repos`.

The generated launcher accepts `--app`, `--port`, and `--token`, plus an
optional private `--control` path used for graceful shutdown. It binds
Shiny only to `127.0.0.1`, exports the token to the application as
`RPACKIT_SESSION_TOKEN`, and emits versioned lifecycle events.
Network-level token enforcement belongs to the desktop shell/proxy and
is deliberately recorded as not yet implemented in `rpackit.json`; this
function does not claim to produce a Tauri executable.

Build inputs are never modified. Output is assembled in a sibling
staging directory and renamed into place only after validation. Existing
output is never overwritten.
