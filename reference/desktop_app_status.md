# Inspect a managed desktop application process

`pid` is the process tracked by `processx`; `runtime_pid` is the
positive PID reported by the launcher and can differ for a Windows
`Rscript.exe` wrapper. `wrapper_alive` and `runtime_alive` report both
captured processes; `alive` remains conservatively true if either is
alive or runtime liveness is unknown. These values do not prove other
process-tree membership.

## Usage

``` r
desktop_app_status(process, tail = 20L)
```

## Arguments

- process:

  An `rpackit_desktop_process` returned by
  [`start_desktop_app()`](https://rpackit.github.io/rpackit/reference/start_desktop_app.md).

- tail:

  Maximum number of recent standard-output and standard-error lines to
  return.

## Value

An `rpackit_desktop_status` object. Its `url` is the token-free loopback
endpoint; the object never contains the session token or authenticated
launch headers.
