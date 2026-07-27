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
`constraint_satisfied` reports whether a locked version satisfies every
declared constraint; `direct` marks packages seen in source or
DESCRIPTION; and `locked` marks packages present in the lockfile.
`provenance` is a compact summary. The normalized `references` table is
the authoritative record of each origin, file, source line, role or call
type, and version requirement. Findings such as a dynamic
[`library()`](https://rdrr.io/r/base/library.html) package name appear
in `diagnostics`. Error diagnostics identify unsafe installation plans,
including required packages missing from a lockfile, locked versions
that violate DESCRIPTION, and DESCRIPTION `Remotes` without an exact
`renv.lock`. `has_description_remotes` and `description_remotes_count`
expose only the presence and count of remote specifications, not their
possibly credential-bearing text. Credential-bearing URL components in
lockfile remote provenance are redacted before they enter the returned
tables.

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
#> Path: /tmp/RtmpMTGnDH/rpackit-dependencies-1a0b38a672e0
#> 2 packages; 2 direct; 0 locked
#>   package version constraint roles direct required locked lock_source
#>  jsonlite    <NA>       <NA>  <NA>   TRUE     TRUE  FALSE        <NA>
#>     shiny    <NA>       <NA>  <NA>   TRUE     TRUE  FALSE        <NA>
#>  repository remote             provenance constraint_satisfied
#>        <NA>   <NA>      source:::@app.R:2                   NA
#>        <NA>   <NA> source:library@app.R:1                   NA
```
