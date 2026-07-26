# Validate a prepared desktop resource bundle

Checks the resource topology, manifest version, application layout,
portable runtime paths, loopback-only launcher contract, and exact
agreement between the copied application's dependency plan and the
manifest package and constraint records. Application code is parsed but
never executed. With `verify_runtime = TRUE`, installed package presence
and every recorded DESCRIPTION version constraint are rechecked inside
the bundled runtime.

## Usage

``` r
validate_desktop_bundle(bundle_dir, verify_runtime = FALSE, quiet = FALSE)
```

## Arguments

- bundle_dir:

  Prepared bundle directory containing `resources/`.

- verify_runtime:

  Execute the bundled `Rscript`, read
  [`getRversion()`](https://rdrr.io/r/base/numeric_version.html), and
  require it to match the version recorded in the manifest when present.

- quiet:

  Suppress the validation summary.

## Value

An `rpackit_desktop_validation` object.
