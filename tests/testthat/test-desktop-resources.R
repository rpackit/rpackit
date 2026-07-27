make_fake_runtime <- function() {
  path <- tempfile("rpackit-runtime-")
  dir.create(file.path(path, "bin"), recursive = TRUE)
  dir.create(file.path(path, "library"))
  rscript <- if (.Platform$OS.type == "windows") {
    file.path(path, "bin", "Rscript.exe")
  } else {
    file.path(path, "bin", "Rscript")
  }
  file.create(rscript)
  path
}

make_desktop_app <- function() {
  path <- tempfile("rpackit-desktop-app-")
  dir.create(path)
  writeLines(
    "shiny::shinyApp(shiny::fluidPage('hello'), function(input, output) {})",
    file.path(path, "app.R")
  )
  dir.create(file.path(path, "www"))
  writeLines("body {}", file.path(path, "www", "style.css"))
  dir.create(file.path(path, ".git"))
  writeLines("ignored", file.path(path, ".git", "config"))
  dir.create(file.path(path, "dist"))
  writeLines("ignored", file.path(path, "dist", "old.txt"))
  path
}

test_that("desktop resources are prepared atomically and validate", {
  app <- make_desktop_app()
  output <- tempfile("rpackit-desktop-output-")
  result <- prepare_desktop(
    app,
    make_fake_runtime(),
    output_dir = output,
    app_name = "Hello",
    install_packages = FALSE,
    verify_runtime = FALSE,
    quiet = TRUE
  )

  expect_s3_class(result, "rpackit_desktop_bundle")
  expect_identical(
    names(result)[seq_len(7L)],
    c(
      "path", "resources", "app_name", "app_type", "packages",
      "dependencies_installed", "validation"
    )
  )
  expect_true(file.exists(file.path(output, "resources", "app", "app.R")))
  expect_true(
    file.exists(file.path(output, "resources", "app", "www", "style.css"))
  )
  expect_false(dir.exists(file.path(output, "resources", "app", ".git")))
  expect_false(dir.exists(file.path(output, "resources", "app", "dist")))
  expect_true(
    file.exists(file.path(output, "resources", "launcher.R"))
  )
  validation <- validate_desktop_bundle(output, quiet = TRUE)
  expect_s3_class(validation, "rpackit_desktop_validation")
  expect_identical(
    names(validation)[seq_len(6L)],
    c(
      "valid", "path", "app_type", "runtime_platform",
      "dependencies_installed", "network_token_enforced"
    )
  )
  expect_true(validation$valid)
  expect_false(validation$dependencies_installed)
  expect_true(validation$network_token_enforced)
})

test_that("desktop manifest records runtime, app, and dependencies honestly", {
  app <- make_desktop_app()
  output <- tempfile("rpackit-desktop-manifest-")
  prepare_desktop(
    app,
    make_fake_runtime(),
    output_dir = output,
    install_packages = FALSE,
    verify_runtime = FALSE,
    quiet = TRUE
  )
  manifest <- jsonlite::fromJSON(
    file.path(output, "resources", "rpackit.json"),
    simplifyVector = FALSE
  )

  expect_identical(manifest$schema_version, "1")
  expect_identical(manifest$app$type, "shiny-single-file")
  expect_identical(manifest$launcher$host, "127.0.0.1")
  expect_identical(manifest$launcher$protocol_version, "2")
  expect_identical(manifest$launcher$token, "private-file")
  expect_identical(manifest$launcher$control, "optional-argument")
  expect_identical(manifest$launcher$event_stream$format, "ndjson")
  expect_identical(
    manifest$launcher$event_stream$prefix,
    "RPACKIT_EVENT "
  )
  expect_identical(
    manifest$launcher$readiness$strategy,
    "authenticated-http-poll"
  )
  expect_identical(
    manifest$launcher$readiness$starting_event,
    "listening"
  )
  expect_true(manifest$launcher$network_token_enforced)
  expect_identical(
    manifest$launcher$authentication$scheme,
    "shiny-shared-secret"
  )
  expect_identical(
    manifest$launcher$authentication$header,
    "Shiny-Shared-Secret"
  )
  expect_identical(
    unlist(manifest$launcher$authentication$scope, use.names = FALSE),
    c("http", "websocket")
  )
  expect_identical(
    manifest$launcher$authentication$token_transport,
    "private-file"
  )
  expect_false(manifest$launcher$authentication$token_in_url)
  expect_false(manifest$dependencies$installed)
  expect_false(manifest$dependencies$constraints_verified)
  expect_identical(length(manifest$dependencies$constraints), 0L)
  expect_true(all(
    c("jsonlite", "later", "shiny") %in%
      unlist(manifest$dependencies$packages)
  ))
  expect_match(manifest$runtime$rscript, "^R/")
})

test_that("dependency-plan errors fail before runtime copying", {
  app <- make_desktop_app()
  secret <- "do-not-print-this-token"
  writeLines(
    c(
      "Package: remotefixture",
      "Version: 0.0.1",
      "Imports: shiny (>= 1.9.0)",
      paste0(
        "Remotes: shiny=git::https://user:",
        secret,
        "@example.test/shiny.git"
      )
    ),
    file.path(app, "DESCRIPTION")
  )
  output <- tempfile("rpackit-dependency-preflight-")
  condition <- expect_error(
    prepare_desktop(
      app,
      make_fake_runtime(),
      output_dir = output,
      install_packages = TRUE,
      verify_runtime = FALSE,
      quiet = TRUE
    ),
    regexp = "Dependency plan cannot be installed safely",
    class = "rpackit_dependency_plan_error"
  )
  expect_false(dir.exists(output))
  expect_false(grepl(secret, conditionMessage(condition), fixed = TRUE))

  uninstalled_output <- tempfile("rpackit-uninstalled-remote-")
  result <- prepare_desktop(
    app,
    make_fake_runtime(),
    output_dir = uninstalled_output,
    install_packages = FALSE,
    verify_runtime = FALSE,
    quiet = TRUE
  )
  expect_false(result$dependencies_installed)
  manifest <- jsonlite::fromJSON(
    file.path(uninstalled_output, "resources", "rpackit.json"),
    simplifyVector = FALSE
  )
  expect_false(manifest$dependencies$constraints_verified)
  expect_identical(
    manifest$dependencies$constraints[[1L]]$package,
    "shiny"
  )
  expect_identical(
    manifest$dependencies$constraints[[1L]]$requirement,
    ">= 1.9.0"
  )
  manifest$dependencies$constraints_verified <- TRUE
  jsonlite::write_json(
    manifest,
    file.path(uninstalled_output, "resources", "rpackit.json"),
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )
  expect_error(
    validate_desktop_bundle(uninstalled_output, quiet = TRUE),
    "invalid dependency-constraint evidence"
  )
})

test_that("dependency installation script verifies every DESCRIPTION version", {
  app <- make_desktop_app()
  writeLines(
    c(
      "Package: constraintfixture",
      "Version: 0.0.1",
      "Imports: shiny (>= 1.9.0), jsonlite (!= 1.0.0)"
    ),
    file.path(app, "DESCRIPTION")
  )
  plan <- plan_dependencies(app)
  resources <- tempfile("rpackit-install-script-")
  dir.create(resources)
  rpackit:::.desktop_copy_tree(
    make_fake_runtime(),
    file.path(resources, "R")
  )
  dir.create(file.path(resources, "app"))
  captured <- new.env(parent = emptyenv())
  local_mocked_bindings(
    .desktop_run_rscript = function(rscript, arguments, context) {
      script <- gsub("^[\"']|[\"']$", "", arguments[[2L]])
      captured$lines <- readLines(script, warn = FALSE)
      invisible(character())
    },
    .package = "rpackit"
  )

  result <- rpackit:::.desktop_install_dependencies(
    resources,
    plan,
    c(CRAN = "https://cloud.r-project.org")
  )
  script <- paste(captured$lines, collapse = "\n")

  expect_identical(
    result$constraints$package,
    c("shiny", "jsonlite")
  )
  expect_identical(
    result$constraints$operator,
    c(">=", "!=")
  )
  expect_match(script, "utils::compareVersion", fixed = TRUE)
  expect_match(script, "base::package_version", fixed = TRUE)
  expect_false(grepl("utils::package_version", script, fixed = TRUE))
  expect_match(
    script,
    "Bundled dependency version requirements are not satisfied",
    fixed = TRUE
  )
})

test_that("bundle validation binds dependency evidence to the copied app", {
  app <- make_desktop_app()
  writeLines(
    c(
      "Package: manifestfixture",
      "Version: 0.0.1",
      "Imports: shiny (>= 1.9.0)"
    ),
    file.path(app, "DESCRIPTION")
  )
  output <- tempfile("rpackit-manifest-dependencies-")
  prepare_desktop(
    app,
    make_fake_runtime(),
    output_dir = output,
    install_packages = FALSE,
    verify_runtime = FALSE,
    quiet = TRUE
  )
  manifest_path <- file.path(output, "resources", "rpackit.json")
  manifest <- jsonlite::fromJSON(manifest_path, simplifyVector = FALSE)
  original <- manifest

  manifest$dependencies$constraints[[1L]]$requirement <- ">= 99.0.0"
  jsonlite::write_json(
    manifest,
    manifest_path,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )
  expect_error(
    validate_desktop_bundle(output, quiet = TRUE),
    "constraints do not match"
  )

  original$dependencies$packages <- original$dependencies$packages[
    unlist(original$dependencies$packages, use.names = FALSE) != "jsonlite"
  ]
  jsonlite::write_json(
    original,
    manifest_path,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )
  expect_error(
    validate_desktop_bundle(output, quiet = TRUE),
    "dependencies do not match"
  )
})

test_that("legacy bundles require matching protocol-1 launcher content", {
  app <- make_desktop_app()
  output <- tempfile("rpackit-legacy-auth-bundle-")
  prepare_desktop(
    app,
    make_fake_runtime(),
    output_dir = output,
    install_packages = FALSE,
    verify_runtime = FALSE,
    quiet = TRUE
  )
  manifest_path <- file.path(output, "resources", "rpackit.json")
  manifest <- jsonlite::fromJSON(manifest_path, simplifyVector = FALSE)
  manifest$launcher$protocol_version <- "1"
  manifest$launcher$token <- "required-argument"
  manifest$launcher$network_token_enforced <- FALSE
  manifest$launcher$authentication <- NULL
  manifest$launcher$readiness <- list(
    strategy = "http-poll",
    starting_event = "starting"
  )
  jsonlite::write_json(
    manifest,
    manifest_path,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )

  expect_error(
    validate_desktop_bundle(output, quiet = TRUE),
    "manifest and launcher contracts do not match"
  )

  file.copy(
    testthat::test_path("fixtures", "launcher-protocol-1.R"),
    file.path(output, "resources", "launcher.R"),
    overwrite = TRUE
  )
  validation <- validate_desktop_bundle(output, quiet = TRUE)

  expect_false(validation$network_token_enforced)
  expect_error(
    rpackit:::.desktop_launch_spec(output),
    "predates enforced network session tokens",
    class = "rpackit_legacy_desktop_bundle_error"
  )
})

test_that("bundle validation accepts legacy schema-v1 runtime metadata", {
  app <- make_desktop_app()
  output <- tempfile("rpackit-legacy-manifest-")
  prepare_desktop(
    app,
    make_fake_runtime(),
    output_dir = output,
    install_packages = FALSE,
    verify_runtime = FALSE,
    quiet = TRUE
  )
  manifest_path <- file.path(output, "resources", "rpackit.json")
  manifest <- jsonlite::fromJSON(manifest_path, simplifyVector = FALSE)
  manifest$runtime$source <- NULL
  manifest$runtime$r_version <- NULL
  manifest$dependencies$constraints <- NULL
  manifest$dependencies$constraints_verified <- NULL
  jsonlite::write_json(
    manifest,
    manifest_path,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )

  validation <- validate_desktop_bundle(output, quiet = TRUE)
  expect_true(validation$valid)
  expect_identical(validation$runtime_source, "explicit")
  expect_null(validation$runtime_version)
})

test_that("registry manifests require an exact runtime version", {
  app <- make_desktop_app()
  output <- tempfile("rpackit-registry-version-manifest-")
  prepare_desktop(
    app,
    make_fake_runtime(),
    output_dir = output,
    install_packages = FALSE,
    verify_runtime = FALSE,
    quiet = TRUE
  )
  manifest_path <- file.path(output, "resources", "rpackit.json")
  manifest <- jsonlite::fromJSON(manifest_path, simplifyVector = FALSE)
  manifest$runtime$source <- "registry"
  manifest$runtime$r_version <- NULL
  manifest$runtime$provenance <- list(
    registry = "https://example.test/versions.json",
    metadata_source = "https://example.test/runtime.json",
    artifact_url = "https://example.test/runtime.zip",
    sha256 = paste(rep("a", 64L), collapse = ""),
    archive_format = "zip",
    cache_hit = FALSE
  )
  jsonlite::write_json(
    manifest,
    manifest_path,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )

  expect_error(
    validate_desktop_bundle(output, quiet = TRUE),
    "must record runtime.r_version"
  )
})

test_that("registry manifest provenance rejects unsafe sources without leaks", {
  app <- make_desktop_app()
  output <- tempfile("rpackit-unsafe-provenance-manifest-")
  prepare_desktop(
    app,
    make_fake_runtime(),
    output_dir = output,
    install_packages = FALSE,
    verify_runtime = FALSE,
    quiet = TRUE
  )
  manifest_path <- file.path(output, "resources", "rpackit.json")
  manifest <- jsonlite::fromJSON(manifest_path, simplifyVector = FALSE)
  manifest$runtime$source <- "registry"
  manifest$runtime$r_version <- "4.6.1"
  valid_provenance <- list(
    registry = "https://example.test/versions.json",
    metadata_source = "https://example.test/runtime.json",
    artifact_url = "https://example.test/runtime.zip",
    sha256 = paste(rep("a", 64L), collapse = ""),
    archive_format = "zip",
    cache_hit = FALSE
  )
  unsafe <- list(
    registry = "http://user:secret@example.test/versions.json?token=private",
    metadata_source = "\\\\server\\share\\runtime.json",
    artifact_url = "https://user:secret@example.test/runtime.zip?token=private"
  )

  for (field in names(unsafe)) {
    manifest$runtime$provenance <- valid_provenance
    manifest$runtime$provenance[[field]] <- unsafe[[field]]
    jsonlite::write_json(
      manifest,
      manifest_path,
      auto_unbox = TRUE,
      pretty = TRUE,
      null = "null"
    )
    condition <- tryCatch(
      validate_desktop_bundle(output, quiet = TRUE),
      error = identity
    )
    expect_s3_class(condition, "error")
    expect_match(
      conditionMessage(condition),
      "invalid registry runtime provenance"
    )
    expect_false(grepl(
      "user|secret|token|private",
      conditionMessage(condition),
      ignore.case = TRUE
    ))
  }
})

test_that("runtime verification matches the manifest version", {
  app <- make_desktop_app()
  output <- tempfile("rpackit-runtime-version-validation-")
  prepare_desktop(
    app,
    make_fake_runtime(),
    output_dir = output,
    install_packages = FALSE,
    verify_runtime = FALSE,
    quiet = TRUE
  )
  manifest_path <- file.path(output, "resources", "rpackit.json")
  manifest <- jsonlite::fromJSON(manifest_path, simplifyVector = FALSE)
  manifest$runtime$r_version <- "4.6.1"
  jsonlite::write_json(
    manifest,
    manifest_path,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )
  local_mocked_bindings(
    .desktop_runtime_version = function(runtime) "4.5.0",
    .package = "rpackit"
  )

  expect_error(
    validate_desktop_bundle(
      output,
      verify_runtime = TRUE,
      quiet = TRUE
    ),
    "reports version 4.5.0.*records 4.6.1",
    class = "rpackit_runtime_version_mismatch_error"
  )
})

test_that("launcher requires all arguments and binds only to loopback", {
  lines <- rpackit:::.desktop_launcher_lines()
  launcher <- paste(lines, collapse = "\n")

  expect_match(launcher, "--app")
  expect_match(launcher, "--port")
  expect_match(launcher, "--token-file")
  expect_match(launcher, "--control")
  expect_match(launcher, "readLines(token_file, n = 2L", fixed = TRUE)
  expect_match(launcher, "RPACKIT_EVENT")
  expect_match(launcher, "shiny.sharedSecret = token", fixed = TRUE)
  expect_match(launcher, "redact_credential", fixed = TRUE)
  expect_match(launcher, "token_enforced = TRUE", fixed = TRUE)
  expect_match(launcher, "'listening'", fixed = TRUE)
  expect_match(launcher, "host = '127.0.0.1'", fixed = TRUE)
  expect_false(grepl("0.0.0.0", launcher, fixed = TRUE))
  expect_false(grepl("RPACKIT_SESSION_TOKEN", launcher, fixed = TRUE))
  expect_false(grepl("?rpackit_token=", launcher, fixed = TRUE))
})

test_that("launcher consumes the credential before other validation failures", {
  launcher <- tempfile("rpackit-launcher-", fileext = ".R")
  writeLines(rpackit:::.desktop_launcher_lines(), launcher, useBytes = TRUE)
  rscript <- file.path(
    R.home("bin"),
    if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"
  )

  for (contents in list(
    "valid-session-token-0123456789",
    c("first-session-token-0123456789", "unexpected-second-line")
  )) {
    credential <- tempfile("rpackit-credential-")
    writeLines(contents, credential, useBytes = TRUE)
    result <- processx::run(
      rscript,
      c(
        "--vanilla",
        launcher,
        "--app", tempfile("missing-rpackit-app-"),
        "--port", "not-a-port",
        "--token-file", credential
      ),
      error_on_status = FALSE,
      echo = FALSE,
      windows_hide_window = TRUE
    )

    expect_identical(result$status, 1L)
    expect_false(file.exists(credential))
  }
})

test_that("launcher redacts credentials from structured runtime errors", {
  skip_if_not_installed("shiny")
  launcher <- tempfile("rpackit-launcher-", fileext = ".R")
  writeLines(rpackit:::.desktop_launcher_lines(), launcher, useBytes = TRUE)
  app <- tempfile("rpackit-redaction-app-")
  dir.create(app)
  writeLines(
    c(
      "shiny::shinyApp(",
      "  ui = shiny::fluidPage('redaction'),",
      "  server = function(input, output, session) {},",
      "  onStart = function() {",
      "    stop('credential: ', getOption('shiny.sharedSecret'))",
      "  }",
      ")"
    ),
    file.path(app, "app.R")
  )
  credential <- tempfile("rpackit-credential-")
  token <- "launcher-redaction-token-0123456789"
  writeLines(token, credential, useBytes = TRUE)
  rscript <- file.path(
    R.home("bin"),
    if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"
  )

  result <- processx::run(
    rscript,
    c(
      "--vanilla",
      launcher,
      "--app", app,
      "--port", "54321",
      "--token-file", credential
    ),
    error_on_status = FALSE,
    echo = FALSE,
    windows_hide_window = TRUE
  )
  output <- paste(result$stdout, result$stderr, sep = "\n")

  expect_identical(result$status, 1L)
  expect_false(file.exists(credential))
  expect_false(grepl(token, output, fixed = TRUE))
  expect_match(output, "<redacted>", fixed = TRUE)
})

test_that("runtime probes isolate and restore caller R environment", {
  variables <- c(
    "R_LIBS",
    "R_LIBS_USER",
    "R_PROFILE_USER",
    "R_ENVIRON_USER"
  )
  old <- Sys.getenv(variables, unset = NA_character_)
  on.exit({
    existing <- !is.na(old)
    if (any(existing)) {
      do.call(
        Sys.setenv,
        stats::setNames(as.list(old[existing]), variables[existing])
      )
    }
    if (any(!existing)) {
      Sys.unsetenv(variables[!existing])
    }
  }, add = TRUE)
  do.call(
    Sys.setenv,
    stats::setNames(
      as.list(paste0("caller-value-", seq_along(variables))),
      variables
    )
  )
  before <- Sys.getenv(variables)
  rscript <- file.path(
    R.home("bin"),
    if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"
  )

  output <- rpackit:::.desktop_run_rscript(
    rscript,
    "--version",
    "Test runtime probe"
  )

  expect_match(paste(output, collapse = "\n"), "Rscript")
  expect_identical(Sys.getenv(variables), before)
})

test_that("existing outputs and invalid inputs fail safely", {
  app <- make_desktop_app()
  runtime <- make_fake_runtime()
  output <- tempfile("rpackit-existing-output-")
  dir.create(output)
  marker <- file.path(output, "keep.txt")
  writeLines("keep", marker)

  expect_error(
    prepare_desktop(
      app,
      runtime,
      output_dir = output,
      install_packages = FALSE,
      verify_runtime = FALSE,
      quiet = TRUE
    ),
    "Refusing to replace"
  )
  expect_identical(readLines(marker), "keep")
  expect_error(
    prepare_desktop(
      app,
      tempfile("missing-runtime-"),
      output_dir = tempfile("unused-output-"),
      install_packages = FALSE,
      verify_runtime = FALSE,
      quiet = TRUE
    ),
    "existing R runtime"
  )

  unknown <- tempfile("rpackit-unknown-app-")
  dir.create(unknown)
  writeLines("print('not Shiny')", file.path(unknown, "analysis.R"))
  expect_error(
    prepare_desktop(
      unknown,
      runtime,
      output_dir = tempfile("unused-output-"),
      install_packages = FALSE,
      verify_runtime = FALSE,
      quiet = TRUE
    ),
    "not ready"
  )
})

test_that("bundle validation rejects unsafe launcher and manifest paths", {
  app <- make_desktop_app()
  output <- tempfile("rpackit-tamper-output-")
  prepare_desktop(
    app,
    make_fake_runtime(),
    output_dir = output,
    install_packages = FALSE,
    verify_runtime = FALSE,
    quiet = TRUE
  )
  launcher <- file.path(output, "resources", "launcher.R")
  writeLines(
    c(readLines(launcher), "host = '0.0.0.0'"),
    launcher
  )
  expect_error(
    validate_desktop_bundle(output, quiet = TRUE),
    "loopback-only"
  )

  safe_output <- tempfile("rpackit-path-output-")
  prepare_desktop(
    app,
    make_fake_runtime(),
    output_dir = safe_output,
    install_packages = FALSE,
    verify_runtime = FALSE,
    quiet = TRUE
  )
  manifest_path <- file.path(safe_output, "resources", "rpackit.json")
  manifest <- jsonlite::fromJSON(manifest_path, simplifyVector = FALSE)
  manifest$runtime$rscript <- "../outside.exe"
  jsonlite::write_json(manifest, manifest_path, auto_unbox = TRUE)
  expect_error(
    validate_desktop_bundle(safe_output, quiet = TRUE),
    "safe relative"
  )
})

test_that("bundle validation rejects dishonest manifest state", {
  app <- make_desktop_app()
  output <- tempfile("rpackit-contract-output-")
  prepare_desktop(
    app,
    make_fake_runtime(),
    output_dir = output,
    install_packages = FALSE,
    verify_runtime = FALSE,
    quiet = TRUE
  )
  manifest_path <- file.path(output, "resources", "rpackit.json")
  manifest <- jsonlite::fromJSON(manifest_path, simplifyVector = FALSE)
  manifest$app$type <- "shiny-split"
  jsonlite::write_json(manifest, manifest_path, auto_unbox = TRUE)

  expect_error(
    validate_desktop_bundle(output, quiet = TRUE),
    "app type does not match"
  )

  token_output <- tempfile("rpackit-token-contract-output-")
  prepare_desktop(
    app,
    make_fake_runtime(),
    output_dir = token_output,
    install_packages = FALSE,
    verify_runtime = FALSE,
    quiet = TRUE
  )
  token_manifest_path <- file.path(
    token_output,
    "resources",
    "rpackit.json"
  )
  token_manifest <- jsonlite::fromJSON(
    token_manifest_path,
    simplifyVector = FALSE
  )
  token_manifest$launcher$authentication$header <- "Authorization"
  jsonlite::write_json(
    token_manifest,
    token_manifest_path,
    auto_unbox = TRUE
  )

  expect_error(
    validate_desktop_bundle(token_output, quiet = TRUE),
    "supported lifecycle"
  )
})
