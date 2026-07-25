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
doctor()
#> 
#> ── rpackit doctor ──────────────────────────────────────────────────────────────
#> Platform: linux x86_64
#> R: 4.6.1
#> 
#> ── Tools ──
#> 
#> ✓ R - R version 4.6.1 (2026-06-24) -- "Happy Hop"
#> ✓ Rscript - Rscript (R) version 4.6.1 (2026-06-24)
#> ✓ Git - git version 2.54.0
#> ✓ Node - v22.23.1
#> ✓ Cargo - cargo 1.97.1 (c980f4866 2026-06-30)
#> ✗ TauriCLI
#> ✓ PackageManager - npm 10.9.8
#> ✗ NativeDesktop
#> ✓ Docker - Docker version 28.0.4, build b8034c0
#> ✓ GitHubCLI - gh version 2.96.0 (2026-07-02)
#> 
#> ── Supported tasks ──
#> 
#> ✓ app inspection
#> - portable Windows runtime verification
#> - Tauri desktop build
#> ✓ Docker server build
```
