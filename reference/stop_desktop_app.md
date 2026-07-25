# Stop a managed desktop application process

Requests a graceful Shiny shutdown through the launcher's private
control file. If the tracked process remains alive after `timeout`,
`processx` is asked to terminate it and its known process tree, with a
tracked-process kill as fallback. Cleanup is confirmed only after both
the tracked process and the create-time-aware handle captured for the
observed launcher runtime have stopped. Other descendant membership and
termination are not independently verified. Repeated calls are safe.

## Usage

``` r
stop_desktop_app(process, timeout = 5, quiet = FALSE)
```

## Arguments

- process:

  An `rpackit_desktop_process` returned by
  [`start_desktop_app()`](https://rpackit.github.io/rpackit/reference/start_desktop_app.md).

- timeout:

  Seconds to wait for graceful shutdown before requesting termination of
  the tracked process.

- quiet:

  Suppress the stopped summary.

## Value

An `rpackit_desktop_status` object, invisibly.
