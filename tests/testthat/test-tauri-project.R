make_tauri_windows_bundle <- function() {
  runtime <- tempfile("rpackit-windows-runtime-")
  dir.create(file.path(runtime, "bin"), recursive = TRUE)
  dir.create(file.path(runtime, "library"))
  file.create(file.path(runtime, "bin", "Rscript.exe"))

  app <- tempfile("rpackit-tauri-app-")
  dir.create(app)
  writeLines(
    "shiny::shinyApp(shiny::fluidPage('hello'), function(input, output) {})",
    file.path(app, "app.R")
  )

  output <- tempfile("rpackit-tauri-bundle-")
  prepare_desktop(
    app,
    runtime,
    output_dir = output,
    app_name = "Hello Desktop",
    install_packages = FALSE,
    verify_runtime = FALSE,
    quiet = TRUE
  )
  manifest_path <- file.path(output, "resources", "rpackit.json")
  manifest <- jsonlite::fromJSON(
    manifest_path,
    simplifyVector = FALSE
  )
  manifest$dependencies$installed <- TRUE
  manifest$dependencies$strategy <- "install-packages"
  manifest$dependencies$constraints_verified <- TRUE
  rpackit:::.desktop_write_json(manifest, manifest_path)
  output
}

tauri_template_fixture <- function() {
  source <- testthat::test_path("fixtures", "tauri-template")
  copy <- tempfile("rpackit-tauri-template-fixture-")
  rpackit:::.desktop_copy_tree(source, copy)
  writeLines("/target/", file.path(copy, ".gitignore"), useBytes = TRUE)
  copy
}

test_that("application-specific Tauri projects generate and validate", {
  bundle <- make_tauri_windows_bundle()
  output <- tempfile("rpackit-generated-tauri-")
  project <- generate_tauri_app(
    bundle,
    output_dir = output,
    product_name = "Hello Native",
    identifier = "org.example.hello-native",
    version = "1.2.3",
    template_source = tauri_template_fixture(),
    quiet = TRUE
  )

  expect_s3_class(project, "rpackit_tauri_project")
  expect_s3_class(project$validation, "rpackit_tauri_validation")
  expect_identical(project$product_name, "Hello Native")
  expect_identical(project$identifier, "org.example.hello-native")
  expect_identical(project$version, "1.2.3")
  expect_true(file.exists(file.path(output, "Cargo.lock")))
  expect_true(file.exists(file.path(output, "src-tauri", "resources", "app", "app.R")))
  expect_true(file.exists(file.path(output, "src-tauri", "icons", "icon.ico")))
  expect_false(dir.exists(file.path(output, "apps", "windows-spike")))
  expect_false(dir.exists(file.path(output, "crates", "transport-testkit")))
  expect_setequal(
    basename(list.dirs(file.path(output, "crates"), recursive = FALSE)),
    rpackit:::.tauri_expected_crates
  )

  config <- jsonlite::fromJSON(
    file.path(output, "src-tauri", "tauri.conf.json"),
    simplifyVector = FALSE
  )
  expect_identical(config$productName, "Hello Native")
  expect_identical(config$identifier, "org.example.hello-native")
  expect_identical(config$version, "1.2.3")
  expect_false(config$bundle$active)
  expect_identical(
    config$bundle$resources[["resources/"]],
    "resources/"
  )

  metadata <- jsonlite::fromJSON(
    file.path(output, "src-tauri", "rpackit-native.json"),
    simplifyVector = FALSE
  )
  expect_identical(metadata$contracts$transport, "2")
  expect_identical(metadata$contracts$resource_bundle, "1")
  expect_identical(metadata$contracts$launcher, "2")
  expect_identical(metadata$template$version, "1.0.0")
  expect_false(metadata$template$official)
  expect_identical(metadata$template$integrity$type, "tree-sha256")
  expect_match(metadata$template$integrity$sha256, "^[a-f0-9]{64}$")
  expect_identical(metadata$launch$mode, "explicit-resource-bundle")
  expect_identical(metadata$packaging$installer, "not-built")
  expect_false(metadata$requirements$clean_machine_verified)

  validation <- validate_tauri_project(output, quiet = TRUE)
  expect_s3_class(validation, "rpackit_tauri_validation")
  expect_true(validation$valid)
})

test_that("defaults are usable and output is atomic and non-overwriting", {
  bundle <- make_tauri_windows_bundle()
  output <- tempfile("rpackit-generated-default-")
  project <- generate_tauri_app(
    bundle,
    output_dir = output,
    template_source = tauri_template_fixture(),
    quiet = TRUE
  )
  expect_identical(project$product_name, "Hello Desktop")
  expect_identical(project$identifier, "dev.rpackit.hello-desktop")

  expect_error(
    generate_tauri_app(
      bundle,
      output_dir = output,
      template_source = tauri_template_fixture(),
      quiet = TRUE
    ),
    "Refusing to replace"
  )
  expect_false(any(grepl(
    "^\\.rpackit-tauri-stage-",
    list.files(dirname(output), all.files = TRUE)
  )))
})

test_that("generation rejects invalid identities and incomplete bundles", {
  bundle <- make_tauri_windows_bundle()
  expect_error(
    generate_tauri_app(
      bundle,
      output_dir = tempfile("rpackit-invalid-identity-"),
      identifier = "Not a reverse domain",
      template_source = tauri_template_fixture(),
      quiet = TRUE
    ),
    "reverse-domain"
  )

  manifest_path <- file.path(bundle, "resources", "rpackit.json")
  manifest <- jsonlite::fromJSON(
    manifest_path,
    simplifyVector = FALSE
  )
  manifest$dependencies$installed <- FALSE
  manifest$dependencies$constraints_verified <- FALSE
  rpackit:::.desktop_write_json(manifest, manifest_path)
  expect_error(
    generate_tauri_app(
      bundle,
      output_dir = tempfile("rpackit-incomplete-bundle-"),
      template_source = tauri_template_fixture(),
      quiet = TRUE
    ),
    class = "rpackit_tauri_bundle_error"
  )
})

test_that("unknown template contracts fail closed", {
  source <- tauri_template_fixture()
  copy <- tempfile("rpackit-tauri-template-copy-")
  rpackit:::.desktop_copy_tree(source, copy)
  descriptor_path <- file.path(
    copy,
    "templates",
    "windows-v1",
    "template.json"
  )
  descriptor <- jsonlite::fromJSON(
    descriptor_path,
    simplifyVector = FALSE
  )
  descriptor$contracts$transport <- "3"
  rpackit:::.desktop_write_json(descriptor, descriptor_path)

  expect_error(
    generate_tauri_app(
      make_tauri_windows_bundle(),
      output_dir = tempfile("rpackit-unknown-contract-"),
      template_source = copy,
      quiet = TRUE
    ),
    class = "rpackit_tauri_template_contract_error"
  )
})

test_that("custom template ZIPs require and verify SHA-256", {
  source <- tauri_template_fixture()
  archive <- tempfile("rpackit-tauri-template-", fileext = ".zip")
  zip::zipr(
    archive,
    files = basename(source),
    root = dirname(source),
    include_directories = TRUE
  )
  sha256 <- digest::digest(file = archive, algo = "sha256")
  project <- generate_tauri_app(
    make_tauri_windows_bundle(),
    output_dir = tempfile("rpackit-archive-project-"),
    template_source = archive,
    template_sha256 = sha256,
    quiet = TRUE
  )
  expect_identical(
    project$validation$template_integrity,
    sha256
  )

  expect_error(
    generate_tauri_app(
      make_tauri_windows_bundle(),
      output_dir = tempfile("rpackit-bad-archive-"),
      template_source = archive,
      template_sha256 = paste(rep("f", 64L), collapse = ""),
      quiet = TRUE
    ),
    class = "rpackit_tauri_template_checksum_error"
  )
})

test_that("project validation binds identity and resource digests", {
  output <- tempfile("rpackit-tampered-project-")
  generate_tauri_app(
    make_tauri_windows_bundle(),
    output_dir = output,
    identifier = "org.example.bound",
    template_source = tauri_template_fixture(),
    quiet = TRUE
  )
  writeLines(
    'const APPLICATION_ID: &str = "org.example.other";',
    file.path(output, "src-tauri", "src", "windows_app.rs")
  )
  expect_error(
    validate_tauri_project(output, quiet = TRUE),
    "identity disagrees"
  )
})

test_that("malformed native metadata fails with a project error", {
  output <- tempfile("rpackit-malformed-project-")
  generate_tauri_app(
    make_tauri_windows_bundle(),
    output_dir = output,
    template_source = tauri_template_fixture(),
    quiet = TRUE
  )
  writeLines(
    "[]",
    file.path(output, "src-tauri", "rpackit-native.json")
  )
  expect_error(
    validate_tauri_project(output, quiet = TRUE),
    class = "rpackit_tauri_project_error"
  )
})
