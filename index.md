# rpackit

**Pack and ship R apps.**

`rpackit` is the R package and desktop-tooling layer of the rpackit
project. It starts with Shiny applications and helps choose the correct
output target: portable desktop, browser-only static web, or a dynamic
server bundle.

## Implemented

- [`doctor()`](https://rpackit.github.io/rpackit/reference/doctor.md)
  reports the current platform and required build tools;
- [`check_app()`](https://rpackit.github.io/rpackit/reference/check_app.md)
  recognizes common Shiny project layouts;
- [`plan_dependencies()`](https://rpackit.github.io/rpackit/reference/plan_dependencies.md)
  combines parsed R calls, `DESCRIPTION`, and `renv.lock` without
  executing application code, and verifies lockfile completeness,
  sources, and declared package-version constraints;
- [`resolve_portable_runtime()`](https://rpackit.github.io/rpackit/reference/resolve_portable_runtime.md)
  selects verified registry entries, checks SHA-256, and maintains an
  atomic local runtime cache;
- [`prepare_desktop()`](https://rpackit.github.io/rpackit/reference/prepare_desktop.md)
  builds atomic portable-R resource bundles;
- [`generate_tauri_app()`](https://rpackit.github.io/rpackit/reference/generate_tauri_app.md)
  and
  [`validate_tauri_project()`](https://rpackit.github.io/rpackit/reference/validate_tauri_project.md)
  render and inspect application-specific Windows Tauri source from a
  checksum-pinned template;
- [`start_desktop_app()`](https://rpackit.github.io/rpackit/reference/start_desktop_app.md),
  [`desktop_app_launch_config()`](https://rpackit.github.io/rpackit/reference/desktop_app_launch_config.md),
  [`desktop_app_status()`](https://rpackit.github.io/rpackit/reference/desktop_app_status.md),
  and
  [`stop_desktop_app()`](https://rpackit.github.io/rpackit/reference/stop_desktop_app.md)
  manage an authenticated, loopback-only Shiny subprocess lifecycle;
- package, `renv`, system-call, Python, native-package, and large-data
  checks;
- an evidence-backed target suitability matrix;
- no application code execution during inspection.

## Installation

``` r

# install.packages("pak")
pak::pak("rpackit/rpackit")
```

## Inspect an app first

``` r

library(rpackit)

doctor()
inspection <- check_app("path/to/shiny-app")
inspection$targets
inspection$findings$system_calls
```

[`check_app()`](https://rpackit.github.io/rpackit/reference/check_app.md)
does not run the application. It reports the detected layout, the
packages used by source code, and a target matrix explaining whether the
app is a good candidate for portable desktop, static web, or a dynamic
server. Direct [`system()`](https://rdrr.io/r/base/system.html),
[`system2()`](https://rdrr.io/r/base/system2.html), and `shell()` calls
include file-and-line evidence because their external programs must be
handled explicitly. Detection uses parsed R syntax, so comments, quoted
string contents, object methods such as `tool$system()`, and same-named
functions from non-base namespaces do not incorrectly block a target.

Run
[`vignette("getting-started", package = "rpackit")`](https://rpackit.github.io/rpackit/articles/getting-started.md)
for an end-to-end inspection, dependency-planning, and desktop-resource
workflow.

## Dependency planning

``` r

plan <- plan_dependencies("path/to/shiny-app")
plan$dependencies
plan$references
```

The planner uses complementary sources with explicit precedence:

1.  `renv.lock` supplies exact versions and package sources;
2.  `DESCRIPTION` supplies direct roles and version constraints;
3.  parsed [`library()`](https://rdrr.io/r/base/library.html),
    [`require()`](https://rdrr.io/r/base/library.html),
    [`requireNamespace()`](https://rdrr.io/r/base/ns-load.html), `::`,
    and `:::` calls discover undeclared usage.

Every observation remains available in `plan$references`, including its
file and source line where available. Comments and string contents are
ignored. Unreadable or syntactically invalid inputs fail with the
affected path instead of silently returning an incomplete plan.
`Suggests` and `Enhances` are optional:

``` r

plan_dependencies("path/to/shiny-app", include_suggests = TRUE)
```

Start with `plan$diagnostics`. Error diagnostics identify required
packages missing from `renv.lock`, locked versions that violate
`DESCRIPTION`, and a `Remotes` field without exact lockfile provenance.
Remote specifications are counted but not copied into the returned plan,
so credentials accidentally embedded in DESCRIPTION do not appear in
normal plan output. Credential-bearing URL components in lockfile
provenance are redacted as well.
[`prepare_desktop()`](https://rpackit.github.io/rpackit/reference/prepare_desktop.md)
refuses an installable bundle until these errors are resolved; use
`install_packages = FALSE` only when deliberately preparing an
uninstalled resource contract for inspection.

## Portable desktop resources

On a platform with a verified registry entry, the first desktop build
layer can resolve, verify, cache, and bundle portable R in one call:

``` r

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

The current public registry contains a verified Windows x86_64 runtime.
On an unsupported platform, the resolver lists the verified
platform/version choices instead of silently using system R. An already
extracted runtime remains fully supported:

``` r

runtime <- resolve_portable_runtime()
runtime[c("r_version", "platform", "arch", "sha256", "cache_hit")]

bundle <- prepare_desktop(
  "path/to/shiny-app",
  runtime_dir = runtime$path
)
```

Remote registry and artifact sources must use stable HTTPS URLs without
credentials, query strings, or fragments. Local files are also accepted
as an rpackit transport extension for air-gapped mirrors and tests; UNC
shares are rejected. The resolver accepts only `verified` entries,
checks SHA-256 before extraction, rejects unsafe paths and ZIP entry
types other than files or directories, and publishes runtimes atomically
to rpackit’s user cache. A later call reuses the registry-bound,
content-addressed version/SHA cache entry. Use `offline = TRUE` to
require a cache hit from the same registry source without reading any
registry or artifact.

SHA-256 proves that the downloaded archive matches the registry record;
it is not code signing. Cache hits are structurally revalidated for
links and path escapes, but the cache still contains executable code and
must not be writable by untrusted users. Automatic tar extraction
currently fails closed because tar link targets cannot yet be proven
safe before extraction.

This copies the app and runtime, restores or installs required packages,
writes the loopback-only `launcher.R`, and records an explicit
`rpackit.json` manifest, including runtime version and registry artifact
provenance when automatic resolution was used. A `renv.lock` R version
and DESCRIPTION `Depends: R` constraint are checked before runtime
copying or package installation. Required package constraints are also
checked against locked versions before copying and against the installed
library before atomic publication. `DESCRIPTION Remotes` is never
silently treated as CRAN: create and review `renv.lock` first. Bundle
validation reparses the copied app and requires its packages and
constraints to match the manifest; `verify_runtime = TRUE` checks those
versions again inside portable R. Existing output is never overwritten.

## Native Tauri source

Once a Windows bundle is dependency-complete and constraint-verified,
render the maintained native project around it:

``` r

project <- generate_tauri_app(
  bundle$path,
  identifier = "com.example.my-app",
  version = "0.1.0"
)

validate_tauri_project(project$path, verify_runtime = TRUE)
```

The application name defaults to the bundle name, and an `.ico` can be
passed with `icon =`. Choose a stable reverse-domain identifier before
distributing an application; changing it later changes the
Windows/WebView application identity.

Generation downloads the official `rpackit-tauri` template ZIP to a
temporary file, verifies its pinned SHA-256, copies only the maintained
shell and six required runtime crates, and deletes the downloaded
template and extraction directory. It does not keep another template
cache. The output records application metadata, template integrity,
transport contract 2, resource schema 1, launcher protocol 2, exact
Rust/Tauri/wry/WebView2 minima, icon and resource-manifest digests, and
launch configuration in `src-tauri/rpackit-native.json`.

The prepared portable R tree is intentionally copied into
`src-tauri/resources`, so generate real projects in a build workspace or
GitHub Actions runner rather than keeping duplicate runtime trees in a
synced source checkout. Cargo compilation and runtime downloads remain
remote in the maintained acceptance workflow.

This milestone generates and validates native source; it does not yet
produce a supported installer. `bundle.active` remains false, metadata
records `installer = "not-built"` and `clean_machine_verified = false`,
and validation refuses to reinterpret either state as a release. The
next gate packages the generated hello-shiny project and verifies it on
a clean Windows machine without system R.

The lifecycle manager starts the bundled `Rscript`, waits for a
post-bind `listening` event, verifies a real authenticated HTTP
response, and requests graceful shutdown through a private control file.
If graceful shutdown times out, it asks `processx` to terminate the
tracked process and its known tree, with a tracked-process kill as
fallback. Status reports both the processx wrapper `pid` and the
launcher-reported `runtime_pid`, which can differ for portable R on
Windows. Readiness captures a create-time-aware handle for the observed
runtime PID, and cleanup is confirmed only after both captured processes
stop. Those observations do not independently prove other process-tree
membership or descendant termination.

The launcher uses a fresh 256-bit operating-system credential for each
session. rpackit writes it to a current-account-private, one-time file;
only that file’s path appears in the child command line, and the
launcher consumes and deletes the file before app or port validation. On
Windows, the directory and file DACLs are restricted and verified for
the current account plus SYSTEM. On POSIX, directory mode 0700 and file
mode 0600 are verified. The credential is not exported to app
environment variables and is never placed by rpackit in the URL,
manifest, generated lifecycle-event fields, returned status, or print
output. Launcher error messages and returned logs/events are redacted.
Raw log files are private lifecycle artifacts, not a secrecy boundary:
trusted app/package code runs in the credential-bearing process and can
deliberately print the option.

Shiny enforces the credential through the `Shiny-Shared-Secret` request
header for dynamic HTTP, static resources, and WebSocket session
acceptance. Missing and wrong HTTP headers receive 403. A WebSocket
protocol upgrade may return 101, after which Shiny immediately closes an
unauthenticated socket before the application server starts; the
attack-surface tests assert positive close evidence. New manifests and
status objects report `network_token_enforced = TRUE`. Legacy
unauthenticated bundles remain inspectable with
[`validate_desktop_bundle()`](https://rpackit.github.io/rpackit/reference/validate_desktop_bundle.md)
but
[`start_desktop_app()`](https://rpackit.github.io/rpackit/reference/start_desktop_app.md)
refuses to launch them.

This is a secure backend launch contract, not yet a stock-browser
workflow. Browsers cannot add a custom header to top-level navigation or
ordinary `WebSocket()` calls. [Transport contract version
2](https://github.com/rpackit/roadmap/blob/main/TAURI_SECURE_TRANSPORT.md)
defines an authenticated native loopback reverse proxy as the
generated-app baseline, not a direct request interceptor or bare
loopback proxy. Native code sends a third, one-time secret only on the
fixed bootstrap request; the HTTP response creates a host-only, HttpOnly
proxy-session cookie. That cookie authenticates each later HTTP and
WebSocket request before the proxy dials the fixed Shiny upstream,
strips browser credentials and spoofed forwarding fields, and injects
exactly one `Shiny-Shared-Secret` upstream. No credential is placed in
the browser-facing URL or JavaScript. A generated app owns launcher
protocol 2 directly and does not call or serialize the R-level
[`desktop_app_launch_config()`](https://rpackit.github.io/rpackit/reference/desktop_app_launch_config.md)
handoff.

The maintained [`rpackit-tauri` Windows
owner](https://github.com/rpackit/rpackit-tauri) passes the complete
development and reviewed fixed-WebView2 transport matrix, the real
portable-R/hello-shiny launcher lifecycle, and deterministic
WebView/window/profile cleanup. The source generator pins
[`windows-template-v1.0.0`](https://github.com/rpackit/rpackit-tauri/releases/tag/windows-template-v1.0.0)
and rejects unknown contract or template versions. This evidence does
not turn the generated source into a supported installer; native
executable packaging, clean-machine verification, static-web builders,
and server builders remain later milestones. The threat model excludes
malicious same-user processes, administrator or debugger access, and
untrusted app/package code running inside the credential-bearing R
process.

If process termination succeeds but private lifecycle files cannot be
removed, the error retains the managed process handle so
[`stop_desktop_app()`](https://rpackit.github.io/rpackit/reference/stop_desktop_app.md)
can retry cleanup. Directory applications continue to honor Shiny’s
`DisplayMode` and `IncludeWWW` `DESCRIPTION` fields. Confirmed cleanup
clears the credential from the managed process handle and prevents new
launch configurations; it cannot revoke an already returned R object, so
native consumers must discard all copies after use.

For a complete Windows walkthrough using the published portable R
prototype, including SHA-256 verification and lifecycle cleanup, see the
[`hello-shiny`
quickstart](https://github.com/rpackit/rpackit-examples/tree/main/hello-shiny).

## License

MIT © Yaoxiang Li
