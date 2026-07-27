# Inspect and prepare a Shiny app

`rpackit` separates packaging into a few explicit checks. This makes it
possible to understand an application before copying a runtime or
installing packages:

1.  [`doctor()`](https://rpackit.github.io/rpackit/reference/doctor.md)
    checks the local build environment.
2.  [`check_app()`](https://rpackit.github.io/rpackit/reference/check_app.md)
    identifies the application layout and suitable targets.
3.  [`plan_dependencies()`](https://rpackit.github.io/rpackit/reference/plan_dependencies.md)
    explains which R packages a bundle needs and why.
4.  [`prepare_desktop()`](https://rpackit.github.io/rpackit/reference/prepare_desktop.md)
    creates portable desktop resources atomically.
5.  [`validate_desktop_bundle()`](https://rpackit.github.io/rpackit/reference/validate_desktop_bundle.md)
    verifies those resources without running the application.
6.  [`generate_tauri_app()`](https://rpackit.github.io/rpackit/reference/generate_tauri_app.md)
    renders application-specific native source from a checksum-pinned
    maintained template.
7.  [`start_desktop_app()`](https://rpackit.github.io/rpackit/reference/start_desktop_app.md)
    and
    [`stop_desktop_app()`](https://rpackit.github.io/rpackit/reference/stop_desktop_app.md)
    provide a managed, authenticated development lifecycle.

## Create a small application

The example below creates an ordinary single-file Shiny application in
the R session’s temporary directory.

``` r

library(rpackit)

app <- tempfile("hello-rpackit-")
dir.create(app)
writeLines(
  c(
    "library(shiny)",
    "shinyApp(",
    "  fluidPage(h1('Hello from rpackit')),",
    "  function(input, output, session) {}",
    ")"
  ),
  file.path(app, "app.R")
)
```

## Inspect before building

``` r

inspection <- check_app(app, quiet = TRUE)
inspection$app_type
#> [1] "shiny-single-file"
inspection$targets
#>             target status
#> 1 portable desktop    yes
#> 2       static web  maybe
#> 3   dynamic server    yes
#>                                                             reason
#> 1                                           supported Shiny layout
#> 2 requires package-level shinylive/webR compatibility verification
#> 3                                           supported Shiny layout
inspection$findings$system_calls
#> [1] call file line
#> <0 rows> (or 0-length row.names)
```

The target matrix is evidence, not a promise that every dependency works
in every target. In particular, a `maybe` result for static web means
that the application still needs a real shinylive/webR export test.

External commands need special attention in a portable build. When
source code directly calls
[`system()`](https://rdrr.io/r/base/system.html),
[`system2()`](https://rdrr.io/r/base/system2.html), or `shell()`,
`inspection$findings$system_calls` identifies the call, file, and line.
Comments and quoted string contents are ignored because rpackit inspects
parsed R syntax rather than searching raw text. Calls inside quoted
language expressions are reported conservatively because static
inspection cannot know whether the application evaluates them later.

## Review the dependency plan

``` r

plan <- plan_dependencies(app)
plan$dependencies
#>   package version constraint roles direct required locked lock_source
#> 1   shiny    <NA>       <NA>  <NA>   TRUE     TRUE  FALSE        <NA>
#>   repository remote             provenance constraint_satisfied
#> 1       <NA>   <NA> source:library@app.R:1                   NA
plan$diagnostics
#> [1] severity code     file     line     message 
#> <0 rows> (or 0-length row.names)
```

The planner combines parsed source calls, `DESCRIPTION`, and `renv.lock`
without evaluating application code. Start with `diagnostics`: a dynamic
package name such as
[`library(pkg, character.only = TRUE)`](https://rdrr.io/r/base/library.html)
cannot be resolved statically and should be made explicit before
packaging.

When present, `renv.lock` supplies exact package versions and sources.
`DESCRIPTION` supplies direct dependency roles and constraints. Parsed
source calls catch packages that were used but not declared. The
complete evidence is available in `plan$references`.

Resolve every row where `plan$diagnostics$severity == "error"` before
asking rpackit to install dependencies. The planner reports required
packages missing from a lockfile and locked versions that violate
`DESCRIPTION`. If `DESCRIPTION` contains `Remotes`, create and review
`renv.lock` first; rpackit does not silently install a same-named
repository package instead. Remote specifications are not copied into
the returned plan, which avoids echoing URL credentials accidentally
stored in project metadata. Credential-bearing URL components in
lockfile provenance are redacted as well.

## Check the build machine

``` r

environment <- doctor(quiet = TRUE)
environment$platform
#> [1] "linux"
environment$capabilities
#>                                    task supported
#> 1                        app inspection      TRUE
#> 2 portable Windows runtime verification     FALSE
#> 3                   Tauri desktop build     FALSE
#> 4                   Docker server build      TRUE
```

Missing tools do not prevent read-only inspection. They only affect the
build targets that require them. For example, a Windows Tauri build
needs Node, Cargo, the Tauri CLI, a JavaScript package manager, the
native toolchain, and WebView2.

## Prepare portable desktop resources

On a platform covered by the verified portable-R registry, rpackit can
resolve and cache the runtime automatically:

``` r

bundle <- prepare_desktop(
  app,
  runtime_dir = NULL
)

validation <- validate_desktop_bundle(
  bundle$path,
  verify_runtime = TRUE
)
validation
```

Set `offline = TRUE` in
[`prepare_desktop()`](https://rpackit.github.io/rpackit/reference/prepare_desktop.md)
when a matching runtime is already cached and the build must not read
the registry or artifact. Alternatively, pass an extracted portable R
home as `runtime_dir`.

Preparation never overwrites an existing output directory. It assembles
a sibling staging directory, validates it, and only then publishes the
completed resource tree. If preparation fails, fix the reported input or
dependency problem and choose a new or removed output directory before
retrying. Locked dependency constraints fail before runtime copying, and
the installed package versions are checked again before the staging
directory can be published. Later validation reparses the copied app and
rejects manifest package or constraint drift. Set
`verify_runtime = TRUE` when you also want validation to execute bundled
R and recheck installed package versions.

## Generate native source

Generation requires a dependency-complete Windows bundle. Run it in a
build workspace or Windows GitHub Actions runner so the copied portable
R tree does not become a second long-lived runtime inside a synced
source checkout:

``` r

project <- generate_tauri_app(
  bundle$path,
  identifier = "com.example.hello-rpackit",
  version = "0.1.0"
)

validate_tauri_project(
  project$path,
  verify_runtime = TRUE
)
```

The official template archive is small, checksum-pinned, downloaded only
to a temporary file, and removed after use. The generated project
excludes the transport acceptance spike and testkit. Its
`rpackit-native.json` binds the application identity, icon and
resource-manifest digests, template integrity, contract versions,
reviewed tool/runtime minima, and explicit launch mode.

This step generates source only. It leaves Tauri installer bundling
disabled and records that clean-machine verification has not happened.
Heavy Rust compilation belongs in the maintained Windows workflow; do
not commit Cargo `target/`, executables, installers, or copied portable
runtime archives.

## Run and stop the prepared app

The R-level lifecycle is intended for trusted development and native
integration testing:

``` r

process <- start_desktop_app(bundle$path)
on.exit(stop_desktop_app(process, quiet = TRUE), add = TRUE)

desktop_app_status(process)
launch <- desktop_app_launch_config(process)

# Hand `launch` directly to a trusted native consumer.
# Never print, log, serialize, or persist launch$headers.

stop_desktop_app(process)
```

The endpoint requires a request header that a stock browser cannot add
to top-level navigation and WebSocket requests. Do not paste the
token-free URL from
[`desktop_app_status()`](https://rpackit.github.io/rpackit/reference/desktop_app_status.md)
into a normal browser and do not put the secret in the URL. Always stop
the process, including on errors; the
[`on.exit()`](https://rdrr.io/r/base/on.exit.html) pattern above
supplies that cleanup path.

[`prepare_desktop()`](https://rpackit.github.io/rpackit/reference/prepare_desktop.md)
creates the portable resource contract and
[`generate_tauri_app()`](https://rpackit.github.io/rpackit/reference/generate_tauri_app.md)
renders maintained native source around it. Neither function claims a
supported Tauri installer. The [rpackit
roadmap](https://github.com/rpackit/roadmap) records the native
transport and release gates separately from this R workflow.
