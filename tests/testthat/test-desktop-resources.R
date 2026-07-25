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
  expect_true(validation$valid)
  expect_false(validation$dependencies_installed)
  expect_false(validation$network_token_enforced)
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
  expect_false(manifest$launcher$network_token_enforced)
  expect_false(manifest$dependencies$installed)
  expect_true("shiny" %in% unlist(manifest$dependencies$packages))
  expect_match(manifest$runtime$rscript, "^R/")
})

test_that("launcher requires all arguments and binds only to loopback", {
  lines <- rpackit:::.desktop_launcher_lines()
  launcher <- paste(lines, collapse = "\n")

  expect_match(launcher, "--app")
  expect_match(launcher, "--port")
  expect_match(launcher, "--token")
  expect_match(launcher, "RPACKIT_SESSION_TOKEN")
  expect_match(launcher, "host = '127.0.0.1'", fixed = TRUE)
  expect_false(grepl("0.0.0.0", launcher, fixed = TRUE))
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
})
