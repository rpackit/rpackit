# Inspect a Shiny application and recommend packaging targets

Recognizes single-file `app.R` and split `ui.R`/`server.R` layouts.
Source inspection identifies package calls and common blockers for
browser-only static builds. Direct calls to
[`system()`](https://rdrr.io/r/base/system.html),
[`system2()`](https://rdrr.io/r/base/system2.html), and `shell()` are
detected from parsed R syntax rather than raw text, so comments, string
contents, object methods, and same-named functions in non-base
namespaces do not create false blockers. Calls inside quoted language
expressions are conservatively reported because their later evaluation
cannot be determined statically. The call, file, and source line are
returned in `findings$system_calls`. No application code is executed.

## Usage

``` r
check_app(app_dir, quiet = FALSE)
```

## Arguments

- app_dir:

  Path to the application directory.

- quiet:

  Suppress the human-readable report.

## Value

An `rpackit_app_check` object with the detected layout, dependency plan,
target matrix, recommendations, and structured findings.

## Examples

``` r
app <- tempfile("shiny-app-")
dir.create(app)
writeLines(
  "shiny::shinyApp(shiny::fluidPage('hello'), function(input, output) {})",
  file.path(app, "app.R")
)
check_app(app)
#> 
#> ── rpackit app check ───────────────────────────────────────────────────────────
#> Path: /tmp/RtmpBhJaZT/shiny-app-195d4794e517
#> Detected app type: shiny-single-file
#> 
#> ── Packages ──
#> 
#> • shiny
#> 
#> ── Target matrix ──
#> 
#> portable desktop: YES - supported Shiny layout
#> static web: MAYBE - requires package-level shinylive/webR compatibility
#> verification
#> dynamic server: YES - supported Shiny layout
#> 
#> ── Recommendation ──
#> 
#> • Portable desktop is the strongest offline target.
#> • Run a shinylive export test before promising static-web support.
#> • Dynamic server packaging is supported.
```
