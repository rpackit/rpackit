# Plan R application dependencies without executing application code

`plan_dependencies()` combines three sources of dependency information:

## Usage

``` r
plan_dependencies(app_dir, include_suggests = FALSE)
```

## Arguments

- app_dir:

  Path to the R application directory.

- include_suggests:

  Include `Suggests` and `Enhances` entries from `DESCRIPTION`.

## Value

An `rpackit_dependency_plan` object with `dependencies`, `references`,
`diagnostics`, source-file paths, and R version requirements.

## Details

1.  `renv.lock` has highest precedence for exact package versions,
    sources, and repositories.

2.  `DESCRIPTION` supplies direct dependency roles and version
    constraints.

3.  Parsed R calls discover packages that are used but not declared.

The sources are complementary rather than mutually exclusive. The
`references` table retains every observation, while the `dependencies`
table contains one resolved row per package. R source is parsed with
[`parse()`](https://rdrr.io/r/base/parse.html); comments and string
contents are never treated as package calls, and application code is
never evaluated.

In `dependencies`, `version`, `lock_source`, `repository`, and `remote`
come from `renv.lock`; `constraint` and `roles` come from `DESCRIPTION`;
`direct` marks packages seen in source or DESCRIPTION; and `locked`
marks packages present in the lockfile. `provenance` is a compact
summary. The normalized `references` table is the authoritative record
of each origin, file, source line, role or call type, and version
requirement. Non-fatal findings such as a dynamic
[`library()`](https://rdrr.io/r/base/library.html) package name appear
in `diagnostics`.

Required DESCRIPTION fields (`Depends`, `Imports`, and `LinkingTo`) are
included by default. Set `include_suggests = TRUE` to also include
`Suggests` and `Enhances`. A lockfile package that is not directly
referenced is retained as a locked transitive dependency.

## Examples

``` r
app <- tempfile("rpackit-dependencies-")
dir.create(app)
writeLines(
  c("library(shiny)", "jsonlite::toJSON(list(ready = TRUE))"),
  file.path(app, "app.R")
)
plan_dependencies(app)
#> 
#> ── rpackit dependency plan ─────────────────────────────────────────────────────
#> Path: /tmp/RtmpySlRmH/rpackit-dependencies-19bb14b1a0a5
#> 2 packages; 2 direct; 0 locked
#>   package version constraint roles direct required locked lock_source
#>  jsonlite    <NA>       <NA>  <NA>   TRUE     TRUE  FALSE        <NA>
#>     shiny    <NA>       <NA>  <NA>   TRUE     TRUE  FALSE        <NA>
#>  repository remote             provenance
#>        <NA>   <NA>      source:::@app.R:2
#>        <NA>   <NA> source:library@app.R:1
```
