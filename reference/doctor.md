# Inspect the local rpackit build environment

Reports the current platform and availability of tools used by R
package, desktop, and server build targets. The function is read-only
and does not install missing software.

## Usage

``` r
doctor(quiet = FALSE)
```

## Arguments

- quiet:

  Suppress the human-readable summary.

## Value

An `rpackit_doctor` object.

## Examples

``` r
if (FALSE) { # interactive()
doctor()
}
```
