# Inspect a Shiny application and recommend packaging targets

Recognizes single-file `app.R` and split `ui.R`/`server.R` layouts.
Source inspection identifies package calls and common blockers for
browser-only static builds. No application code is executed.

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

An `rpackit_app_check` object.

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
#> Path: /tmp/RtmplgHx5h/shiny-app-195a6f9d150
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
