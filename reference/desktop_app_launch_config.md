# Return the authenticated native-shell launch contract

Returns the secret-bearing request configuration for a running desktop
app. A native shell or local proxy must add `headers` to the initial
navigation, every subrequest, and every WebSocket upgrade for exactly
`origin`. It must not expose the header to browser JavaScript or forward
it across a redirect.

## Usage

``` r
desktop_app_launch_config(process)
```

## Arguments

- process:

  An `rpackit_desktop_process` returned by
  [`start_desktop_app()`](https://rpackit.github.io/rpackit/reference/start_desktop_app.md).

## Value

An `rpackit_desktop_launch_config` with `url`, exact `origin`, secret
`headers`, covered `request_types`, and `follow_redirects = FALSE`.

## Details

This object contains the live session credential. Do not print its
internal fields, log it, serialize it, or persist it. Its print method
deliberately shows only non-secret metadata. New configurations become
unavailable after
[`stop_desktop_app()`](https://rpackit.github.io/rpackit/reference/stop_desktop_app.md)
confirms cleanup, but any configuration already returned is an ordinary
R object whose credential cannot be revoked. Consumers must drop every
retained copy after the native handoff ends.
