# rpackit

**Pack and ship R apps.**

`rpackit` is the R package and desktop-tooling layer of the rpackit project.
It starts with Shiny applications and helps choose the correct output target:
portable desktop, browser-only static web, or a dynamic server bundle.

## Implemented

- `doctor()` reports the current platform and required build tools;
- `check_app()` recognizes common Shiny project layouts;
- `plan_dependencies()` combines parsed R calls, `DESCRIPTION`, and
  `renv.lock` without executing application code, and verifies lockfile
  completeness, sources, and declared package-version constraints;
- `resolve_portable_runtime()` selects verified registry entries, checks
  SHA-256, and maintains an atomic local runtime cache;
- `prepare_desktop()` builds atomic portable-R resource bundles;
- `start_desktop_app()`, `desktop_app_launch_config()`,
  `desktop_app_status()`, and `stop_desktop_app()` manage an authenticated,
  loopback-only Shiny subprocess lifecycle;
- package, `renv`, system-call, Python, native-package, and large-data checks;
- an evidence-backed target suitability matrix;
- no application code execution during inspection.

## Installation

```r
# install.packages("pak")
pak::pak("rpackit/rpackit")
```

## Inspect an app first

```r
library(rpackit)

doctor()
inspection <- check_app("path/to/shiny-app")
inspection$targets
inspection$findings$system_calls
```

`check_app()` does not run the application. It reports the detected layout,
the packages used by source code, and a target matrix explaining whether the
app is a good candidate for portable desktop, static web, or a dynamic server.
Direct `system()`, `system2()`, and `shell()` calls include file-and-line
evidence because their external programs must be handled explicitly. Detection
uses parsed R syntax, so comments, quoted string contents, object methods such
as `tool$system()`, and same-named functions from non-base namespaces do not
incorrectly block a target.

Run `vignette("getting-started", package = "rpackit")` for an end-to-end
inspection, dependency-planning, and desktop-resource workflow.

## Dependency planning

```r
plan <- plan_dependencies("path/to/shiny-app")
plan$dependencies
plan$references
```

The planner uses complementary sources with explicit precedence:

1. `renv.lock` supplies exact versions and package sources;
2. `DESCRIPTION` supplies direct roles and version constraints;
3. parsed `library()`, `require()`, `requireNamespace()`, `::`, and `:::`
   calls discover undeclared usage.

Every observation remains available in `plan$references`, including its file
and source line where available. Comments and string contents are ignored.
Unreadable or syntactically invalid inputs fail with the affected path instead
of silently returning an incomplete plan. `Suggests` and `Enhances` are
optional:

```r
plan_dependencies("path/to/shiny-app", include_suggests = TRUE)
```

Start with `plan$diagnostics`. Error diagnostics identify required packages
missing from `renv.lock`, locked versions that violate `DESCRIPTION`, and a
`Remotes` field without exact lockfile provenance. Remote specifications are
counted but not copied into the returned plan, so credentials accidentally
embedded in DESCRIPTION do not appear in normal plan output. Credential-bearing
URL components in lockfile provenance are redacted as well. `prepare_desktop()`
refuses an installable bundle until these errors are resolved; use
`install_packages = FALSE` only when deliberately preparing an uninstalled
resource contract for inspection.

## Portable desktop resources

On a platform with a verified registry entry, the first desktop build layer
can resolve, verify, cache, and bundle portable R in one call:

```r
bundle <- prepare_desktop(
  "path/to/shiny-app",
  runtime_dir = NULL
)
validate_desktop_bundle(bundle$path, verify_runtime = TRUE)

process <- start_desktop_app(bundle$path)
desktop_app_status(process)
launch <- desktop_app_launch_config(process)
# `launch` is a trusted development/third-party native handoff.
# Never serialize or log launch$headers.
stop_desktop_app(process)
```

The current public registry contains a verified Windows x86_64 runtime. On an
unsupported platform, the resolver lists the verified platform/version choices
instead of silently using system R. An already extracted runtime remains fully
supported:

```r
runtime <- resolve_portable_runtime()
runtime[c("r_version", "platform", "arch", "sha256", "cache_hit")]

bundle <- prepare_desktop(
  "path/to/shiny-app",
  runtime_dir = runtime$path
)
```

Remote registry and artifact sources must use stable HTTPS URLs without
credentials, query strings, or fragments. Local files are also accepted as an
rpackit transport extension for air-gapped mirrors and tests; UNC shares are
rejected. The resolver accepts only `verified` entries, checks SHA-256 before
extraction, rejects unsafe paths and ZIP entry types other than files or
directories, and publishes runtimes atomically to rpackit's user cache. A
later call reuses the registry-bound, content-addressed version/SHA cache
entry. Use `offline = TRUE` to require a cache hit from the same registry
source without reading any registry or artifact.

SHA-256 proves that the downloaded archive matches the registry record; it is
not code signing. Cache hits are structurally revalidated for links and path
escapes, but the cache still contains executable code and must not be writable
by untrusted users. Automatic tar extraction currently fails closed because
tar link targets cannot yet be proven safe before extraction.

This copies the app and runtime, restores or installs required packages, writes
the loopback-only `launcher.R`, and records an explicit `rpackit.json`
manifest, including runtime version and registry artifact provenance when
automatic resolution was used. A `renv.lock` R version and DESCRIPTION
`Depends: R` constraint are checked before runtime copying or package
installation. Required package constraints are also checked against locked
versions before copying and against the installed library before atomic
publication. `DESCRIPTION Remotes` is never silently treated as CRAN: create
and review `renv.lock` first. Bundle validation reparses the copied app and
requires its packages and constraints to match the manifest;
`verify_runtime = TRUE` checks those versions again inside portable R.
Existing output is never overwritten.

The lifecycle manager starts the bundled `Rscript`, waits for a post-bind
`listening` event, verifies a real authenticated HTTP response, and requests
graceful shutdown through a private control file. If graceful shutdown times
out, it asks `processx` to terminate the tracked process and its known tree,
with a tracked-process kill as fallback.
Status reports both the processx wrapper `pid` and the launcher-reported
`runtime_pid`, which can differ for portable R on Windows. Readiness captures a
create-time-aware handle for the observed runtime PID, and cleanup is confirmed
only after both captured processes stop. Those observations do not independently
prove other process-tree membership or descendant termination.

The launcher uses a fresh 256-bit operating-system credential for each
session. rpackit writes it to a current-account-private, one-time file; only
that file's path appears in the child command line, and the launcher consumes
and deletes the file before app or port validation. On Windows, the directory
and file DACLs are restricted and verified for the current account plus
SYSTEM. On POSIX, directory mode 0700 and file mode 0600 are verified. The
credential is not exported to app environment variables and is never placed
by rpackit in the URL, manifest, generated lifecycle-event fields, returned
status, or print output. Launcher error messages and returned logs/events are
redacted. Raw log files are private lifecycle artifacts, not a secrecy
boundary: trusted app/package code runs in the credential-bearing process and
can deliberately print the option.

Shiny enforces the credential through the `Shiny-Shared-Secret` request header
for dynamic HTTP, static resources, and WebSocket session acceptance. Missing
and wrong HTTP headers receive 403. A WebSocket protocol upgrade may return
101, after which Shiny immediately closes an unauthenticated socket before the
application server starts; the attack-surface tests assert positive close
evidence. New manifests and status objects report
`network_token_enforced = TRUE`. Legacy unauthenticated bundles remain
inspectable with `validate_desktop_bundle()` but `start_desktop_app()` refuses
to launch them.

This is a secure backend launch contract, not yet a stock-browser workflow.
Browsers cannot add a custom header to top-level navigation or ordinary
`WebSocket()` calls.
[Transport contract version 2](https://github.com/rpackit/roadmap/blob/main/TAURI_SECURE_TRANSPORT.md)
defines an authenticated native loopback reverse proxy as the generated-app
baseline, not a direct request interceptor or bare loopback proxy. Native code
sends a third, one-time secret only on the fixed bootstrap request; the HTTP
response creates a host-only, HttpOnly proxy-session cookie. That cookie
authenticates each later HTTP and WebSocket request before the proxy dials the
fixed Shiny upstream, strips browser credentials and spoofed forwarding
fields, and injects exactly one `Shiny-Shared-Secret` upstream. No credential
is placed in the browser-facing URL or JavaScript. A generated app owns
launcher protocol 2 directly and does not call or serialize the R-level
`desktop_app_launch_config()` handoff.

The pre-release
[`rpackit-tauri` Windows spike](https://github.com/rpackit/rpackit-tauri)
exercises this contract with a real WebView2 development runtime. It is not a
generated application, supported installer, or release-ready transport. The
current Windows development gate proves exact loopback routing wins across
IPv4 wildcard, IPv6 v6-only wildcard, and IPv6 dual-stack wildcard contenders.
The dual-stack contender is exercised against both exact families; wildcard
bind success is not mistaken for interception. The reviewed fixed-runtime,
crash-persistence, browser-escape, resource-abuse, and malformed-upstream
matrices remain open. The threat model excludes malicious same-user processes,
administrator or debugger access, and untrusted app/package code running
inside the credential-bearing R process. Tauri project generation, native
executable packaging, static-web builders, and server builders remain later
milestones.

If process termination succeeds but private lifecycle files cannot be removed,
the error retains the managed process handle so `stop_desktop_app()` can retry
cleanup. Directory applications continue to honor Shiny's `DisplayMode` and
`IncludeWWW` `DESCRIPTION` fields. Confirmed cleanup clears the credential
from the managed process handle and prevents new launch configurations; it
cannot revoke an already returned R object, so native consumers must discard
all copies after use.

For a complete Windows walkthrough using the published portable R prototype,
including SHA-256 verification and lifecycle cleanup, see the
[`hello-shiny` quickstart](https://github.com/rpackit/rpackit-examples/tree/main/hello-shiny).

## License

MIT © Yaoxiang Li
