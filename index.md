# rpackit

**Pack and ship R apps.**

`rpackit` is the R package and desktop-tooling layer of the rpackit
project. It starts with Shiny applications and helps choose the correct
output target: portable desktop, browser-only static web, or a dynamic
server bundle.

## Implemented

- [`doctor()`](https://rpackit.github.io/rpackit/reference/doctor.md)
  reports the current platform and required build tools;
- [`check_app()`](https://rpackit.github.io/rpackit/reference/check_app.md)
  recognizes common Shiny project layouts;
- [`plan_dependencies()`](https://rpackit.github.io/rpackit/reference/plan_dependencies.md)
  combines parsed R calls, `DESCRIPTION`, and `renv.lock` without
  executing application code;
- [`prepare_desktop()`](https://rpackit.github.io/rpackit/reference/prepare_desktop.md)
  builds atomic portable-R resource bundles;
- [`start_desktop_app()`](https://rpackit.github.io/rpackit/reference/start_desktop_app.md),
  [`desktop_app_status()`](https://rpackit.github.io/rpackit/reference/desktop_app_status.md),
  and
  [`stop_desktop_app()`](https://rpackit.github.io/rpackit/reference/stop_desktop_app.md)
  manage a real loopback-only Shiny subprocess lifecycle;
- package, `renv`, system-call, Python, native-package, and large-data
  checks;
- an evidence-backed target suitability matrix;
- no application code execution during inspection.

## Installation

``` r

# install.packages("pak")
pak::pak("rpackit/rpackit")
```

## Example

``` r

library(rpackit)

doctor()
check_app("path/to/shiny-app")
```

## Dependency planning

``` r

plan <- plan_dependencies("path/to/shiny-app")
plan$dependencies
plan$references
```

The planner uses complementary sources with explicit precedence:

1.  `renv.lock` supplies exact versions and package sources;
2.  `DESCRIPTION` supplies direct roles and version constraints;
3.  parsed [`library()`](https://rdrr.io/r/base/library.html),
    [`require()`](https://rdrr.io/r/base/library.html),
    [`requireNamespace()`](https://rdrr.io/r/base/ns-load.html), `::`,
    and `:::` calls discover undeclared usage.

Every observation remains available in `plan$references`, including its
file and source line where available. Comments and string contents are
ignored. Unreadable or syntactically invalid inputs fail with the
affected path instead of silently returning an incomplete plan.
`Suggests` and `Enhances` are optional:

``` r

plan_dependencies("path/to/shiny-app", include_suggests = TRUE)
```

## Portable desktop resources

The first desktop build layer prepares a complete resource directory
from an extracted portable R runtime:

``` r

bundle <- prepare_desktop(
  "path/to/shiny-app",
  runtime_dir = "path/to/portable-r"
)
validate_desktop_bundle(bundle$path, verify_runtime = TRUE)

process <- start_desktop_app(bundle$path)
desktop_app_status(process)
# Hand process$launch_url directly to an embedded webview; do not log it.
stop_desktop_app(process)
```

This copies the app and runtime, restores or installs required packages,
writes the loopback-only `launcher.R`, and records an explicit
`rpackit.json` manifest. Existing output is never overwritten. The
lifecycle manager starts the bundled `Rscript`, waits for both a
versioned launcher event and a real HTTP response, and requests graceful
shutdown through a private control file. If graceful shutdown times out,
it asks `processx` to terminate the tracked process and its known tree,
with a tracked-process kill as fallback. Status reports both the
processx wrapper `pid` and the launcher-reported `runtime_pid`, which
can differ for portable R on Windows. Readiness captures a
create-time-aware handle for the observed runtime PID, and cleanup is
confirmed only after both captured processes stop. Those observations do
not independently prove other process-tree membership or descendant
termination.

The `start`/`status`/`stop` sequence above is the currently implemented
and end-to-end-tested lifecycle. It is not yet a Tauri shell or
executable. The session token is currently a correlation/bootstrap value
exported as `RPACKIT_SESSION_TOKEN`; it does not authenticate HTTP or
WebSocket traffic. Only the process handle retains the token-bearing
`launch_url`; status and print methods expose the token-free loopback
URL. The manifest and returned status therefore explicitly report
`network_token_enforced = FALSE`. Network enforcement, Tauri project
generation, and native executable packaging remain desktop milestones.
Static-web and server builders also remain milestone work.

For a complete Windows walkthrough using the published portable R
prototype, including SHA-256 verification and lifecycle cleanup, see the
[`hello-shiny`
quickstart](https://github.com/rpackit/rpackit-examples/tree/main/hello-shiny).

## License

MIT © Yaoxiang Li
