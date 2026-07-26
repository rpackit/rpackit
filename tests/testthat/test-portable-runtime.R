test_that("default registry uses the runtime repository", {
  expect_identical(
    rpackit:::.rpackit_runtime_registry,
    paste0(
      "https://raw.githubusercontent.com/rpackit/runtime/",
      "main/versions.json"
    )
  )
})

make_portable_registry <- function(
  r_version = "4.6.1",
  status = "verified",
  sha256 = NULL,
  platform = rpackit:::.rpackit_platform()$platform,
  arch = rpackit:::.rpackit_platform()$architecture
) {
  root <- tempfile("rpackit-runtime-registry-")
  metadata_dir <- file.path(root, "metadata")
  payload <- file.path(root, "payload")
  dir.create(metadata_dir, recursive = TRUE)
  dir.create(payload)

  runtime_name <- paste(
    "portable-r",
    platform,
    arch,
    r_version,
    sep = "-"
  )
  runtime <- file.path(payload, runtime_name)
  dir.create(file.path(runtime, "bin"), recursive = TRUE)
  dir.create(file.path(runtime, "library"))
  rscript_relative <- if (identical(platform, "windows")) {
    "bin/Rscript.exe"
  } else {
    "bin/Rscript"
  }
  file.create(file.path(runtime, rscript_relative))

  archive <- file.path(metadata_dir, paste0(runtime_name, ".zip"))
  zip::zipr(
    archive,
    files = runtime_name,
    root = payload,
    include_directories = TRUE
  )
  observed_sha256 <- digest::digest(
    file = archive,
    algo = "sha256"
  )
  if (is.null(sha256)) {
    sha256 <- observed_sha256
  }

  metadata_name <- paste0(
    platform,
    "-",
    arch,
    "-",
    r_version,
    ".json"
  )
  metadata_path <- file.path(metadata_dir, metadata_name)
  jsonlite::write_json(
    list(
      schema_version = "1",
      r_version = r_version,
      platform = platform,
      arch = arch,
      artifact_url = basename(archive),
      sha256 = sha256,
      archive_format = "zip",
      r_home = runtime_name,
      rscript = paste(runtime_name, rscript_relative, sep = "/"),
      library = paste(runtime_name, "library", sep = "/")
    ),
    metadata_path,
    auto_unbox = TRUE,
    pretty = TRUE
  )
  registry <- file.path(root, "versions.json")
  jsonlite::write_json(
    list(
      schema_version = "1",
      runtimes = list(list(
        r_version = r_version,
        platform = platform,
        arch = arch,
        status = status,
        metadata = paste("metadata", metadata_name, sep = "/")
      ))
    ),
    registry,
    auto_unbox = TRUE,
    pretty = TRUE
  )
  list(
    root = root,
    registry = registry,
    archive = archive,
    runtime_name = runtime_name,
    r_version = r_version,
    platform = platform,
    arch = arch,
    sha256 = observed_sha256
  )
}

make_portable_test_app <- function() {
  path <- tempfile("rpackit-portable-app-")
  dir.create(path)
  writeLines(
    "shiny::shinyApp(shiny::fluidPage('hello'), function(input, output) {})",
    file.path(path, "app.R")
  )
  path
}

make_portable_fake_runtime <- function() {
  path <- tempfile("rpackit-portable-explicit-runtime-")
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

test_that("verified local registry resolves atomically and reuses cache", {
  fixture <- make_portable_registry()
  cache <- tempfile("rpackit-runtime-cache-")

  first <- resolve_portable_runtime(
    r_version = fixture$r_version,
    platform = fixture$platform,
    arch = fixture$arch,
    registry = fixture$registry,
    cache_dir = cache,
    quiet = TRUE
  )

  expect_s3_class(first, "rpackit_portable_runtime")
  expect_false(first$cache_hit)
  expect_identical(first$status, "verified")
  expect_identical(first$sha256, fixture$sha256)
  expect_true(dir.exists(first$path))
  expect_true(file.exists(file.path(
    first$cache_path,
    "rpackit-runtime.json"
  )))
  unlink(fixture$archive, force = TRUE)

  second <- resolve_portable_runtime(
    r_version = fixture$r_version,
    platform = fixture$platform,
    arch = fixture$arch,
    registry = fixture$registry,
    cache_dir = cache,
    quiet = TRUE
  )

  expect_true(second$cache_hit)
  expect_identical(second$path, first$path)
  expect_identical(second$cache_path, first$cache_path)
})

test_that("offline mode reuses cache without contacting a registry", {
  fixture <- make_portable_registry()
  cache <- tempfile("rpackit-runtime-offline-cache-")
  online <- resolve_portable_runtime(
    r_version = fixture$r_version,
    platform = fixture$platform,
    arch = fixture$arch,
    registry = fixture$registry,
    cache_dir = cache,
    quiet = TRUE
  )
  unlink(fixture$root, recursive = TRUE, force = TRUE)

  offline <- resolve_portable_runtime(
    r_version = fixture$r_version,
    platform = fixture$platform,
    arch = fixture$arch,
    registry = fixture$registry,
    cache_dir = cache,
    offline = TRUE,
    quiet = TRUE
  )

  expect_true(offline$cache_hit)
  expect_identical(offline$path, online$path)
})

test_that("local registry labels remain stable after removal", {
  root <- tempfile("rpackit-registry-label-")
  dir.create(root)
  registry <- file.path(root, "versions.json")
  file.create(registry)
  before <- rpackit:::.portable_source_label(registry, "registry")

  unlink(root, recursive = TRUE, force = TRUE)
  after <- rpackit:::.portable_source_label(registry, "registry")

  expect_identical(after, before)
})

test_that("local source labels use platform-correct root semantics", {
  if (.Platform$OS.type == "windows") {
    current_drive <- toupper(substr(
      gsub("\\\\", "/", getwd()),
      1L,
      2L
    ))
    expect_identical(
      rpackit:::.portable_source_label(
        "\\registry\\versions.json",
        "registry"
      ),
      paste0(current_drive, "/registry/versions.json")
    )
    expect_identical(
      rpackit:::.portable_source_label(
        "/registry/versions.json",
        "registry"
      ),
      paste0(current_drive, "/registry/versions.json")
    )
  } else {
    expect_error(
      rpackit:::.portable_source_label(
        "C:/registry/versions.json",
        "registry"
      ),
      "must use HTTPS or a local filesystem path",
      class = "rpackit_runtime_registry_error"
    )
  }
})

test_that("offline cache entries are bound to their registry source", {
  fixture <- make_portable_registry()
  cache <- tempfile("rpackit-runtime-registry-bound-cache-")
  resolve_portable_runtime(
    r_version = fixture$r_version,
    platform = fixture$platform,
    arch = fixture$arch,
    registry = fixture$registry,
    cache_dir = cache,
    quiet = TRUE
  )

  expect_error(
    resolve_portable_runtime(
      r_version = fixture$r_version,
      platform = fixture$platform,
      arch = fixture$arch,
      registry = file.path(fixture$root, "different-versions.json"),
      cache_dir = cache,
      offline = TRUE,
      quiet = TRUE
    ),
    "No checksum-verified cached.*different-versions",
    class = "rpackit_runtime_offline_error"
  )
})

test_that("same-SHA registries get distinct offline cache identities", {
  fixture_a <- make_portable_registry()
  root_b <- tempfile("rpackit-runtime-registry-b-")
  metadata_b <- file.path(root_b, "metadata")
  dir.create(metadata_b, recursive = TRUE)
  expect_true(file.copy(
    fixture_a$registry,
    file.path(root_b, "versions.json")
  ))
  source_files <- list.files(
    file.path(fixture_a$root, "metadata"),
    full.names = TRUE
  )
  expect_true(all(file.copy(source_files, metadata_b)))
  registry_b <- file.path(root_b, "versions.json")
  cache <- tempfile("rpackit-multi-registry-cache-")

  online_a <- resolve_portable_runtime(
    r_version = fixture_a$r_version,
    platform = fixture_a$platform,
    arch = fixture_a$arch,
    registry = fixture_a$registry,
    cache_dir = cache,
    quiet = TRUE
  )
  online_b <- resolve_portable_runtime(
    r_version = fixture_a$r_version,
    platform = fixture_a$platform,
    arch = fixture_a$arch,
    registry = registry_b,
    cache_dir = cache,
    quiet = TRUE
  )

  expect_identical(online_b$sha256, online_a$sha256)
  expect_false(online_b$cache_hit)
  expect_false(identical(online_b$cache_path, online_a$cache_path))
  unlink(root_b, recursive = TRUE, force = TRUE)

  offline_b <- resolve_portable_runtime(
    r_version = fixture_a$r_version,
    platform = fixture_a$platform,
    arch = fixture_a$arch,
    registry = registry_b,
    cache_dir = cache,
    offline = TRUE,
    quiet = TRUE
  )
  expect_true(offline_b$cache_hit)
  expect_identical(offline_b$path, online_b$path)
})

test_that("a same-registry publish race reuses the verified winner", {
  fixture <- make_portable_registry()
  cache <- tempfile("rpackit-runtime-race-cache-")
  local_mocked_bindings(
    .portable_publish_stage = function(stage, target) {
      if (!base::file.rename(stage, target)) {
        stop("Could not simulate the publish winner.")
      }
      FALSE
    },
    .package = "rpackit"
  )

  raced <- resolve_portable_runtime(
    r_version = fixture$r_version,
    platform = fixture$platform,
    arch = fixture$arch,
    registry = fixture$registry,
    cache_dir = cache,
    quiet = TRUE
  )
  expect_true(raced$cache_hit)

  offline <- resolve_portable_runtime(
    r_version = fixture$r_version,
    platform = fixture$platform,
    arch = fixture$arch,
    registry = fixture$registry,
    cache_dir = cache,
    offline = TRUE,
    quiet = TRUE
  )
  expect_true(offline$cache_hit)
  expect_identical(offline$path, raced$path)
})

test_that("credential-bearing provenance URLs are rejected", {
  expect_error(
    rpackit:::.portable_validate_https_url(
      "https://user:secret@example.test/runtime.zip",
      "artifact URL"
    ),
    "must not contain user credentials",
    class = "rpackit_runtime_registry_error"
  )
  expect_error(
    rpackit:::.portable_validate_https_url(
      "https://example.test/runtime.zip?signature=secret",
      "artifact URL"
    ),
    "must not contain.*query string",
    class = "rpackit_runtime_registry_error"
  )
})

test_that("unsupported URLs are rejected without exposing their secrets", {
  unsafe_sources <- c(
    "http://user:secret@example.test/runtime.zip?token=private",
    "data:text/plain,private-secret",
    "mailto:user-secret@example.test"
  )

  for (source in unsafe_sources) {
    condition <- tryCatch(
      rpackit:::.portable_download(
        source,
        tempfile("rpackit-secret-download-"),
        quiet = TRUE,
        context = "portable R artifact"
      ),
      error = identity
    )
    expect_s3_class(condition, "rpackit_runtime_download_error")
    expect_false(grepl(
      "user|secret|token|private",
      conditionMessage(condition),
      ignore.case = TRUE
    ))
  }
})

test_that("local artifact paths resolve safely relative to metadata", {
  metadata_source <- file.path(tempdir(), "mirror", "runtime.json")
  expected <- normalizePath(
    file.path(tempdir(), "mirror"),
    winslash = "/",
    mustWork = FALSE
  )
  expected <- file.path(expected, "subdir", "runtime.zip")

  expect_identical(
    rpackit:::.portable_artifact_source(
      "subdir\\runtime.zip",
      metadata_source
    ),
    expected
  )
  expect_error(
    rpackit:::.portable_artifact_source(
      "..\\outside.zip",
      metadata_source
    ),
    "traversal-free",
    class = "rpackit_runtime_registry_error"
  )
  expect_error(
    rpackit:::.portable_artifact_source(
      "\\\\server\\share\\runtime.zip",
      metadata_source
    ),
    "UNC or network share",
    class = "rpackit_runtime_registry_error"
  )
  expect_error(
    rpackit:::.portable_artifact_source(
      "runtime.zip",
      "//server/share/metadata.json"
    ),
    "UNC or network share",
    class = "rpackit_runtime_registry_error"
  )
  expect_error(
    rpackit:::.portable_source_label(
      "\\\\server\\share\\versions.json",
      "registry"
    ),
    "UNC or network share",
    class = "rpackit_runtime_registry_error"
  )
})

test_that("online cache hits report the currently selected provenance", {
  fixture <- make_portable_registry()
  cache <- tempfile("rpackit-runtime-provenance-cache-")
  first <- resolve_portable_runtime(
    r_version = fixture$r_version,
    platform = fixture$platform,
    arch = fixture$arch,
    registry = fixture$registry,
    cache_dir = cache,
    quiet = TRUE
  )
  metadata_path <- list.files(
    file.path(fixture$root, "metadata"),
    pattern = "\\.json$",
    full.names = TRUE
  )[[1L]]
  metadata <- jsonlite::fromJSON(
    metadata_path,
    simplifyVector = FALSE
  )
  current_artifact <- normalizePath(
    fixture$archive,
    winslash = "/",
    mustWork = TRUE
  )
  metadata$artifact_url <- current_artifact
  jsonlite::write_json(
    metadata,
    metadata_path,
    auto_unbox = TRUE,
    pretty = TRUE
  )

  second <- resolve_portable_runtime(
    r_version = fixture$r_version,
    platform = fixture$platform,
    arch = fixture$arch,
    registry = fixture$registry,
    cache_dir = cache,
    quiet = TRUE
  )

  expect_false(identical(first$artifact_url, current_artifact))
  expect_true(second$cache_hit)
  expect_identical(second$artifact_url, current_artifact)
  expect_identical(second$path, first$path)
})

test_that("resolver accepts only verified entries with actionable errors", {
  prototype <- make_portable_registry(status = "prototype")
  expect_error(
    resolve_portable_runtime(
      platform = prototype$platform,
      arch = prototype$arch,
      registry = prototype$registry,
      cache_dir = tempfile("rpackit-prototype-cache-"),
      quiet = TRUE
    ),
    regexp = "No verified.*available",
    class = "rpackit_runtime_unavailable_error"
  )

  verified <- make_portable_registry()
  expect_error(
    resolve_portable_runtime(
      r_version = "4.5.0",
      platform = verified$platform,
      arch = verified$arch,
      registry = verified$registry,
      cache_dir = tempfile("rpackit-version-cache-"),
      quiet = TRUE
    ),
    regexp = "Available versions.*4.6.1",
    class = "rpackit_runtime_unavailable_error"
  )
})

test_that("checksum failure never publishes a cache entry", {
  fixture <- make_portable_registry(sha256 = paste(rep("0", 64L), collapse = ""))
  cache <- tempfile("rpackit-bad-sha-cache-")

  expect_error(
    resolve_portable_runtime(
      platform = fixture$platform,
      arch = fixture$arch,
      registry = fixture$registry,
      cache_dir = cache,
      quiet = TRUE
    ),
    regexp = "SHA-256 verification failed",
    class = "rpackit_runtime_checksum_error"
  )
  expect_length(
    list.files(
      cache,
      pattern = "^rpackit-runtime\\.json$",
      recursive = TRUE
    ),
    0L
  )
})

test_that("archive path validation rejects traversal and foreign roots", {
  expect_error(
    rpackit:::.portable_validate_archive_entries(
      c("runtime/bin/Rscript", "runtime/../outside"),
      "runtime"
    ),
    "escapes",
    class = "rpackit_runtime_archive_error"
  )
  expect_error(
    rpackit:::.portable_validate_archive_entries(
      "different-root/bin/Rscript",
      "runtime"
    ),
    "disagrees",
    class = "rpackit_runtime_archive_error"
  )
  expect_error(
    rpackit:::.portable_validate_archive_entries(
      "C:/outside/Rscript.exe",
      "runtime"
    ),
    "unsafe",
    class = "rpackit_runtime_archive_error"
  )
})

test_that("ZIP type validation rejects links before extraction", {
  expect_invisible(
    rpackit:::.portable_validate_zip_entry_types(
      c("runtime/", "runtime/bin/Rscript"),
      c("directory", "file")
    )
  )
  expect_error(
    rpackit:::.portable_validate_zip_entry_types(
      c("runtime/link", "runtime/link/payload"),
      c("symlink", "file")
    ),
    regexp = "runtime/link.*symlink",
    class = "rpackit_runtime_archive_error"
  )
})

test_that("automatic desktop preparation uses a verified resolved runtime", {
  fixture <- make_portable_registry()
  cache <- tempfile("rpackit-prepare-cache-")
  app <- make_portable_test_app()
  output <- tempfile("rpackit-auto-runtime-output-")

  bundle <- prepare_desktop(
    app,
    runtime_dir = NULL,
    output_dir = output,
    install_packages = FALSE,
    verify_runtime = FALSE,
    quiet = TRUE,
    registry = fixture$registry,
    cache_dir = cache
  )

  expect_s3_class(bundle$runtime, "rpackit_portable_runtime")
  expect_identical(bundle$runtime$r_version, fixture$r_version)
  manifest <- jsonlite::fromJSON(
    file.path(bundle$resources, "rpackit.json"),
    simplifyVector = FALSE
  )
  expect_identical(manifest$runtime$source, "registry")
  expect_identical(manifest$runtime$r_version, fixture$r_version)
  expect_identical(
    manifest$runtime$provenance$sha256,
    fixture$sha256
  )
  expect_identical(
    manifest$runtime$provenance$artifact_url,
    basename(fixture$archive)
  )
  expect_false(manifest$runtime$provenance$cache_hit)
  expect_identical(bundle$validation$path, bundle$path)
  expect_true(file.exists(file.path(
    bundle$resources,
    "R",
    if (fixture$platform == "windows") {
      "bin/Rscript.exe"
    } else {
      "bin/Rscript"
    }
  )))
})

test_that("runtime compatibility fails before bundle output is created", {
  app <- make_portable_test_app()
  writeLines(
    c(
      "Package: runtimefixture",
      "Version: 0.0.1",
      "Title: Runtime Fixture",
      "Description: Checks the selected R version.",
      "License: MIT",
      "Depends: R (>= 4.6.0)",
      "Imports: shiny"
    ),
    file.path(app, "DESCRIPTION")
  )
  runtime <- make_portable_fake_runtime()
  output <- tempfile("rpackit-version-mismatch-output-")
  local_mocked_bindings(
    .desktop_runtime_version = function(runtime) "4.5.0",
    .package = "rpackit"
  )

  expect_error(
    prepare_desktop(
      app,
      runtime_dir = runtime,
      output_dir = output,
      install_packages = FALSE,
      verify_runtime = FALSE,
      quiet = TRUE
    ),
    regexp = "does not satisfy DESCRIPTION",
    class = "rpackit_runtime_version_mismatch_error"
  )
  expect_false(file.exists(output) || dir.exists(output))
})

test_that("automatic extraction fails closed for tar metadata", {
  root <- tempfile("rpackit-tar-cache-")
  dir.create(root)
  metadata <- list(
    schema_version = "1",
    r_version = "4.6.1",
    platform = "linux",
    arch = "x86_64",
    status = "verified",
    artifact_url = "https://example.test/runtime.tar.gz",
    artifact_source = "https://example.test/runtime.tar.gz",
    sha256 = paste(rep("a", 64L), collapse = ""),
    archive_format = "tar.gz",
    r_home = "portable-r-linux-x86_64-4.6.1",
    rscript = "portable-r-linux-x86_64-4.6.1/bin/Rscript",
    library = "portable-r-linux-x86_64-4.6.1/library",
    registry = "https://example.test/versions.json",
    metadata_source = "https://example.test/runtime.json"
  )

  expect_error(
    rpackit:::.portable_populate_cache(metadata, root, quiet = TRUE),
    regexp = "ZIP runtimes only",
    class = "rpackit_runtime_archive_error"
  )
  expect_length(list.files(root, all.files = TRUE, no.. = TRUE), 0L)
})

test_that("metadata must use the desktop runtime layout", {
  fixture <- make_portable_registry()
  metadata_path <- list.files(
    file.path(fixture$root, "metadata"),
    pattern = "\\.json$",
    full.names = TRUE
  )[[1L]]
  metadata <- jsonlite::fromJSON(
    metadata_path,
    simplifyVector = FALSE
  )
  metadata$library <- paste0(metadata$r_home, "/lib")
  jsonlite::write_json(
    metadata,
    metadata_path,
    auto_unbox = TRUE,
    pretty = TRUE
  )

  expect_error(
    resolve_portable_runtime(
      platform = fixture$platform,
      arch = fixture$arch,
      registry = fixture$registry,
      cache_dir = tempfile("rpackit-layout-cache-"),
      quiet = TRUE
    ),
    "require one top-level r_home",
    class = "rpackit_runtime_registry_error"
  )
})

test_that("runtime validators require Rscript to be a regular file", {
  platform <- rpackit:::.rpackit_platform()$platform
  r_home <- "portable-r-invalid-rscript"
  rscript <- paste0(
    r_home,
    if (identical(platform, "windows")) {
      "/bin/Rscript.exe"
    } else {
      "/bin/Rscript"
    }
  )
  stage <- tempfile("rpackit-rscript-directory-stage-")
  dir.create(file.path(stage, rscript), recursive = TRUE)
  dir.create(file.path(stage, r_home, "library"))
  metadata <- list(
    r_home = r_home,
    rscript = rscript,
    library = paste0(r_home, "/library")
  )

  expect_error(
    rpackit:::.portable_validate_extracted_tree(stage, metadata),
    "does not contain the runtime paths",
    class = "rpackit_runtime_archive_error"
  )

  explicit <- tempfile("rpackit-rscript-directory-explicit-")
  candidate <- if (.Platform$OS.type == "windows") {
    "bin/Rscript.exe"
  } else {
    "bin/Rscript"
  }
  dir.create(file.path(explicit, candidate), recursive = TRUE)
  dir.create(file.path(explicit, "library"))
  expect_error(
    rpackit:::.desktop_runtime(explicit),
    "must contain exactly one"
  )
})

test_that("renv lock version takes precedence for automatic resolution", {
  fixture <- make_portable_registry(r_version = "4.6.1")
  app <- make_portable_test_app()
  writeLines(
    c(
      "{",
      '  "R": {"Version": "4.6.1"},',
      '  "Packages": {',
      '    "shiny": {"Version": "1.11.1", "Source": "Repository"}',
      "  }",
      "}"
    ),
    file.path(app, "renv.lock")
  )
  output <- tempfile("rpackit-locked-runtime-output-")

  bundle <- prepare_desktop(
    app,
    runtime_dir = NULL,
    output_dir = output,
    install_packages = FALSE,
    verify_runtime = FALSE,
    quiet = TRUE,
    registry = fixture$registry,
    cache_dir = tempfile("rpackit-locked-runtime-cache-")
  )

  expect_identical(bundle$runtime$r_version, "4.6.1")
})

test_that("requested runtime mismatch fails before registry or output access", {
  app <- make_portable_test_app()
  writeLines(
    c(
      "{",
      '  "R": {"Version": "4.6.1"},',
      '  "Packages": {',
      '    "shiny": {"Version": "1.11.1", "Source": "Repository"}',
      "  }",
      "}"
    ),
    file.path(app, "renv.lock")
  )
  output <- tempfile("rpackit-early-version-output-")
  cache <- tempfile("rpackit-early-version-cache-")

  expect_error(
    prepare_desktop(
      app,
      runtime_dir = NULL,
      output_dir = output,
      install_packages = FALSE,
      verify_runtime = FALSE,
      quiet = TRUE,
      r_version = "4.5.0",
      registry = "https://invalid.example.test/versions.json",
      cache_dir = cache
    ),
    regexp = "does not match the renv.lock",
    class = "rpackit_runtime_version_mismatch_error"
  )
  expect_false(file.exists(output) || dir.exists(output))
  expect_false(file.exists(cache) || dir.exists(cache))
})

test_that("existing output is refused before automatic resolution", {
  app <- make_portable_test_app()
  output <- tempfile("rpackit-existing-output-")
  cache <- tempfile("rpackit-unused-cache-")
  dir.create(output)

  expect_error(
    prepare_desktop(
      app,
      runtime_dir = NULL,
      output_dir = output,
      install_packages = FALSE,
      verify_runtime = FALSE,
      quiet = TRUE,
      registry = "https://invalid.example.test/versions.json",
      cache_dir = cache
    ),
    "Refusing to replace existing"
  )
  expect_false(file.exists(cache) || dir.exists(cache))
})
