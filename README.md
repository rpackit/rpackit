# rpackit

**Pack and ship R apps.**

`rpackit` is the R package and CLI layer of the rpackit project. It starts with
Shiny applications and helps choose the correct output target: portable
desktop, browser-only static web, or a dynamic server bundle.

## Implemented in 0.1.0

- `doctor()` reports the current platform and required build tools;
- `check_app()` recognizes common Shiny project layouts;
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

Desktop, static-web, and server builders remain milestone work. The package
does not yet claim to generate a Tauri executable.

## License

MIT © Yaoxiang Li
