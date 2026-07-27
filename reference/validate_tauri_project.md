# Validate a generated Tauri source project

`validate_tauri_project()` checks application identity, versioned native
metadata, template integrity records, the reduced Rust workspace, icon
and resource-manifest digests, and the embedded desktop resource bundle.
It does not compile Rust or create an installer.

## Usage

``` r
validate_tauri_project(project_dir, verify_runtime = FALSE, quiet = FALSE)
```

## Arguments

- project_dir:

  Generated project directory.

- verify_runtime:

  Execute the embedded `Rscript` and verify installed packages as part
  of
  [`validate_desktop_bundle()`](https://rpackit.github.io/rpackit/reference/validate_desktop_bundle.md).

- quiet:

  Suppress the validation summary.

## Value

An `rpackit_tauri_validation` object.
