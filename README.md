# rpackit

**Pack and ship R apps.**

`rpackit` is the R package and CLI layer of the rpackit project. It starts with
Shiny applications and helps choose the correct output target: portable
desktop, browser-only static web, or a dynamic server bundle.

## Implemented

- `doctor()` reports the current platform and required build tools;
- `check_app()` recognizes common Shiny project layouts;
- `plan_dependencies()` combines parsed R calls, `DESCRIPTION`, and
  `renv.lock` without executing application code;
- package, `renv`, system-call, Python, native-package, and large-data checks;
- an evidence-backed target suitability matrix;
- no application code execution during inspection.

## Installation

```r
# install.packages("pak")
pak::pak("rpackit/rpackit")
```

## Example

```r
library(rpackit)

doctor()
check_app("path/to/shiny-app")
```

## Dependency planning

```r
plan <- plan_dependencies("path/to/shiny-app")
plan$dependencies
plan$references
```

The planner uses complementary sources with explicit precedence:

1. `renv.lock` supplies exact versions and package sources;
2. `DESCRIPTION` supplies direct roles and version constraints;
3. parsed `library()`, `require()`, `requireNamespace()`, `::`, and `:::`
   calls discover undeclared usage.

Every observation remains available in `plan$references`, including its file
and source line where available. Comments and string contents are ignored.
Unreadable or syntactically invalid inputs fail with the affected path instead
of silently returning an incomplete plan. `Suggests` and `Enhances` are
optional:

```r
plan_dependencies("path/to/shiny-app", include_suggests = TRUE)
```

## Portable desktop resources

The first desktop build layer prepares a complete resource directory from an
extracted portable R runtime:

```r
bundle <- prepare_desktop(
  "path/to/shiny-app",
  runtime_dir = "path/to/portable-r"
)
validate_desktop_bundle(bundle$path, verify_runtime = TRUE)
```

This copies the app and runtime, restores or installs required packages, writes
the loopback-only `launcher.R`, and records an explicit `rpackit.json`
manifest. Existing output is never overwritten.

This is a real, testable input to the desktop shell, but it is not yet a Tauri
executable. Network-level token enforcement, Tauri project generation, and
process lifecycle management remain the next desktop milestone. Static-web and
server builders also remain milestone work.

## License

MIT © Yaoxiang Li
