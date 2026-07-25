# Validate a prepared desktop resource bundle

Checks the resource topology, manifest version, application layout,
portable runtime paths, and loopback-only launcher contract. Application
code is not executed.

## Usage

``` r
validate_desktop_bundle(bundle_dir, verify_runtime = FALSE, quiet = FALSE)
```

## Arguments

- bundle_dir:

  Prepared bundle directory containing `resources/`.

- verify_runtime:

  Execute the bundled `Rscript --version`.

- quiet:

  Suppress the validation summary.

## Value

An `rpackit_desktop_validation` object.
