.tauri_official_template <- list(
  name = "rpackit-windows",
  version = "1.1.0",
  source = paste0(
    "https://github.com/rpackit/rpackit-tauri/releases/download/",
    "windows-template-v1.1.0/",
    "rpackit-windows-template-v1.1.0.zip"
  ),
  sha256 = "2f4e03c71a2a5c3dac01309259279827988ca028b0776138b3c0d3d18c7a6246"
)

.tauri_expected_crates <- c(
  "launcher-protocol",
  "resource-bundle",
  "transport",
  "windows-launcher",
  "windows-lifecycle",
  "windows-webview"
)

.tauri_expected_root_files <- c(
  ".gitignore",
  "Cargo.lock",
  "LICENSE",
  "rust-toolchain.toml",
  "rustfmt.toml"
)

.tauri_expected_requirements <- list(
  architecture = "x86_64",
  rust = "1.97.1",
  tauri = "2.11.5",
  tauri_cli = "2.11.4",
  wry = "0.55.1",
  webview2 = "149.0.4022.98",
  windows_minimum = "10.0.17134"
)

.tauri_scalar_flag <- function(value, argument) {
  if (!is.logical(value) || length(value) != 1L || is.na(value)) {
    cli::cli_abort("{.arg {argument}} must be TRUE or FALSE.")
  }
  value
}

.tauri_sha256 <- function(value, argument) {
  if (!is.character(value) || length(value) != 1L || is.na(value) ||
      !grepl("^[a-f0-9]{64}$", value)) {
    cli::cli_abort(
      "{.arg {argument}} must be one lowercase SHA-256 digest.",
      class = "rpackit_tauri_template_checksum_error"
    )
  }
  value
}

.tauri_read_json <- function(path, context) {
  tryCatch(
    jsonlite::fromJSON(path, simplifyVector = FALSE),
    error = function(error) {
      cli::cli_abort(
        "Cannot parse {context}: {conditionMessage(error)}.",
        class = "rpackit_tauri_project_error",
        parent = error
      )
    }
  )
}

.tauri_vector <- function(value) {
  unname(unlist(value, use.names = FALSE))
}

.tauri_validate_descriptor <- function(descriptor) {
  expected_contracts <- list(
    transport = "2",
    resource_bundle = "1",
    launcher = "2"
  )
  if (!is.list(descriptor)) {
    cli::cli_abort(
      "The Tauri source template descriptor must be a JSON object.",
      class = "rpackit_tauri_template_contract_error"
    )
  }
  project <- descriptor$project
  source <- descriptor$source
  valid <- identical(descriptor$schema_version, "1") &&
    identical(descriptor$template, .tauri_official_template$name) &&
    identical(descriptor$template_version, .tauri_official_template$version) &&
    identical(descriptor$contracts, expected_contracts) &&
    identical(descriptor$requirements, .tauri_expected_requirements) &&
    is.list(project) &&
    identical(
      project$root_manifest,
      "templates/windows-v1/Cargo.toml"
    ) &&
    identical(project$application_source, "apps/windows-shell") &&
    identical(project$application_target, "src-tauri") &&
    identical(project$default_icon, "apps/windows-spike/icons/icon.ico") &&
    identical(.tauri_vector(project$crates), .tauri_expected_crates) &&
    identical(
      .tauri_vector(project$root_files),
      .tauri_expected_root_files
    ) &&
    is.list(source) &&
    identical(
      source$repository,
      "https://github.com/rpackit/rpackit-tauri"
    )
  if (!isTRUE(valid)) {
    cli::cli_abort(
      sprintf(
        paste0(
          "The Tauri source template does not match supported template %s, ",
          "transport 2, resource schema 1, and launcher protocol 2."
        ),
        .tauri_official_template$version
      ),
      class = "rpackit_tauri_template_contract_error"
    )
  }
  invisible(descriptor)
}

.tauri_safe_archive_entries <- function(archive) {
  listing <- tryCatch(
    zip::zip_list(archive),
    error = function(error) {
      cli::cli_abort(
        "Could not inspect the Tauri source template ZIP.",
        class = "rpackit_tauri_template_archive_error",
        parent = error
      )
    }
  )
  .portable_validate_zip_entry_types(listing$filename, listing$type)
  entries <- listing$filename
  sizes <- listing$uncompressed_size
  if (!is.character(entries) || !length(entries) || length(entries) > 2048L ||
      anyNA(entries) || !is.numeric(sizes) || length(sizes) != length(entries) ||
      anyNA(sizes) || any(sizes < 0) || sum(sizes) > 32 * 1024^2) {
    cli::cli_abort(
      "The Tauri source template ZIP exceeds its bounded archive contract.",
      class = "rpackit_tauri_template_archive_error"
    )
  }
  for (entry in entries) {
    path <- sub("/+$", "", entry)
    pieces <- strsplit(path, "/", fixed = TRUE)[[1L]]
    if (!nzchar(path) ||
        grepl("\\\\", path) ||
        startsWith(path, "/") ||
        startsWith(path, "//") ||
        grepl(":", path, fixed = TRUE) ||
        any(!nzchar(pieces)) ||
        any(pieces %in% c(".", "..")) ||
        length(pieces) > 20L) {
      cli::cli_abort(
        "The Tauri source template ZIP contains an unsafe path.",
        class = "rpackit_tauri_template_archive_error"
      )
    }
  }
  marker <- "templates/windows-v1/template.json"
  matches <- entries[
    endsWith(sub("/+$", "", entries), marker)
  ]
  if (length(matches) != 1L) {
    cli::cli_abort(
      "The Tauri source template ZIP must contain exactly one descriptor.",
      class = "rpackit_tauri_template_archive_error"
    )
  }
  descriptor_entry <- sub("/+$", "", matches[[1L]])
  root <- substr(
    descriptor_entry,
    1L,
    nchar(descriptor_entry) - nchar(marker)
  )
  entry_paths <- sub("/+$", "", entries)
  root_path <- sub("/$", "", root)
  contained <- if (identical(root, paste0(root_path, "/"))) {
    entry_paths == root_path | startsWith(entry_paths, root)
  } else {
    rep(FALSE, length(entry_paths))
  }
  if (!nzchar(root) || any(!contained)) {
    cli::cli_abort(
      "The Tauri source template ZIP must have one contained source root.",
      class = "rpackit_tauri_template_archive_error"
    )
  }
  list(entries = entries, root = root_path)
}

.tauri_validate_extracted_tree <- function(root) {
  entries <- list.files(
    root,
    recursive = TRUE,
    full.names = TRUE,
    all.files = TRUE,
    no.. = TRUE,
    include.dirs = TRUE
  )
  normalized_root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  for (entry in entries) {
    if (nzchar(Sys.readlink(entry))) {
      cli::cli_abort(
        "The Tauri source template may not contain symbolic links.",
        class = "rpackit_tauri_template_archive_error"
      )
    }
    normalized <- normalizePath(entry, winslash = "/", mustWork = TRUE)
    if (!.rpackit_path_within(normalized, normalized_root)) {
      cli::cli_abort(
        "The Tauri source template escaped its private extraction root.",
        class = "rpackit_tauri_template_archive_error"
      )
    }
  }
  invisible(root)
}

.tauri_extract_archive <- function(archive) {
  listing <- .tauri_safe_archive_entries(archive)
  stage <- tempfile("rpackit-tauri-template-")
  if (!dir.create(stage, recursive = FALSE, showWarnings = FALSE)) {
    cli::cli_abort(
      "Cannot create a private Tauri template staging directory.",
      class = "rpackit_tauri_template_archive_error"
    )
  }
  completed <- FALSE
  on.exit({
    if (!completed && dir.exists(stage)) {
      unlink(stage, recursive = TRUE, force = TRUE)
    }
  }, add = TRUE)
  tryCatch(
    zip::unzip(
      archive,
      overwrite = FALSE,
      junkpaths = FALSE,
      exdir = stage
    ),
    error = function(error) {
      cli::cli_abort(
        "Could not extract the verified Tauri source template ZIP.",
        class = "rpackit_tauri_template_archive_error",
        parent = error
      )
    }
  )
  root <- file.path(stage, listing$root)
  if (!dir.exists(root)) {
    cli::cli_abort(
      "The Tauri source template ZIP did not produce its declared root.",
      class = "rpackit_tauri_template_archive_error"
    )
  }
  .tauri_validate_extracted_tree(root)
  completed <- TRUE
  list(root = root, stage = stage)
}

.tauri_template_files <- function(root, descriptor) {
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  project <- descriptor$project
  files <- file.path(root, .tauri_vector(project$root_files))
  files <- c(
    files,
    file.path(root, project$root_manifest),
    file.path(root, "templates/windows-v1/template.json"),
    file.path(root, project$default_icon)
  )
  directories <- c(
    file.path(root, project$application_source),
    file.path(root, "crates", .tauri_vector(project$crates))
  )
  missing <- c(
    files[!file.exists(files) | dir.exists(files)],
    directories[!dir.exists(directories)]
  )
  if (length(missing)) {
    cli::cli_abort(
      "The Tauri source template is incomplete.",
      class = "rpackit_tauri_template_contract_error"
    )
  }
  selected <- c(
    files,
    unlist(lapply(directories, function(directory) {
      list.files(
        directory,
        recursive = TRUE,
        full.names = TRUE,
        all.files = TRUE,
        no.. = TRUE,
        include.dirs = FALSE
      )
    }), use.names = FALSE)
  )
  for (path in c(directories, selected)) {
    if (nzchar(Sys.readlink(path))) {
      cli::cli_abort(
        "The Tauri source template may not use symbolic links.",
        class = "rpackit_tauri_template_contract_error"
      )
    }
    normalized <- normalizePath(path, winslash = "/", mustWork = TRUE)
    if (!.rpackit_path_within(normalized, root)) {
      cli::cli_abort(
        "The Tauri source template contains an escaping source path.",
        class = "rpackit_tauri_template_contract_error"
      )
    }
  }
  selected
}

.tauri_template_tree_sha256 <- function(root, descriptor) {
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  files <- .tauri_template_files(root, descriptor)
  relative <- substring(
    gsub("\\\\", "/", normalizePath(files, winslash = "/", mustWork = TRUE)),
    nchar(root) + 2L
  )
  order <- order(relative)
  relative <- relative[order]
  files <- files[order]
  hashes <- vapply(files, function(path) {
    digest::digest(file = path, algo = "sha256")
  }, character(1))
  digest::digest(
    paste(relative, hashes, sep = "\r", collapse = "\n"),
    algo = "sha256",
    serialize = FALSE
  )
}

.tauri_materialize_template <- function(template_source, template_sha256,
                                        quiet) {
  official <- is.null(template_source)
  if (official) {
    template_source <- .tauri_official_template$source
    template_sha256 <- .tauri_official_template$sha256
  }
  if (!is.character(template_source) || length(template_source) != 1L ||
      is.na(template_source) || !nzchar(template_source)) {
    cli::cli_abort(
      "{.arg template_source} must be NULL or one non-empty source."
    )
  }
  if (dir.exists(template_source)) {
    if (!is.null(template_sha256)) {
      template_sha256 <- .tauri_sha256(
        template_sha256,
        "template_sha256"
      )
    }
    root <- normalizePath(template_source, winslash = "/", mustWork = TRUE)
    descriptor <- .tauri_read_json(
      file.path(root, "templates/windows-v1/template.json"),
      "Tauri template descriptor"
    )
    .tauri_validate_descriptor(descriptor)
    observed <- .tauri_template_tree_sha256(root, descriptor)
    if (!is.null(template_sha256) &&
        !identical(observed, template_sha256)) {
      cli::cli_abort(
        "The local Tauri template tree SHA-256 does not match.",
        class = "rpackit_tauri_template_checksum_error"
      )
    }
    return(list(
      root = root,
      descriptor = descriptor,
      integrity = observed,
      integrity_type = "tree-sha256",
      official = FALSE,
      temporary = NULL
    ))
  }
  if (is.null(template_sha256)) {
    cli::cli_abort(
      "{.arg template_sha256} is required for a custom template ZIP.",
      class = "rpackit_tauri_template_checksum_error"
    )
  }
  template_sha256 <- .tauri_sha256(template_sha256, "template_sha256")
  archive <- tempfile("rpackit-tauri-template-", fileext = ".zip")
  .portable_download(
    template_source,
    archive,
    quiet,
    "Tauri source template"
  )
  observed <- digest::digest(file = archive, algo = "sha256")
  if (!identical(observed, template_sha256)) {
    unlink(archive, force = TRUE)
    cli::cli_abort(
      c(
        "The Tauri source template SHA-256 verification failed.",
        "x" = "Expected: {template_sha256}",
        "x" = "Observed: {observed}"
      ),
      class = "rpackit_tauri_template_checksum_error"
    )
  }
  extracted <- .tauri_extract_archive(archive)
  unlink(archive, force = TRUE)
  descriptor <- .tauri_read_json(
    file.path(
      extracted$root,
      "templates/windows-v1/template.json"
    ),
    "Tauri template descriptor"
  )
  .tauri_validate_descriptor(descriptor)
  .tauri_template_files(extracted$root, descriptor)
  list(
    root = extracted$root,
    descriptor = descriptor,
    integrity = observed,
    integrity_type = "archive-sha256",
    official = official,
    temporary = extracted$stage
  )
}

.tauri_slug <- function(value) {
  ascii <- iconv(value, from = "", to = "ASCII//TRANSLIT", sub = "")
  if (is.na(ascii)) {
    ascii <- ""
  }
  slug <- tolower(gsub("[^A-Za-z0-9]+", "-", ascii))
  slug <- sub("^-+", "", sub("-+$", "", slug))
  if (!nzchar(slug)) {
    slug <- "app"
  }
  slug <- substr(slug, 1L, 48L)
  slug <- sub("-+$", "", slug)
  if (nzchar(slug)) slug else "app"
}

.tauri_product_name <- function(value) {
  if (!is.character(value) || length(value) != 1L || is.na(value) ||
      !nzchar(trimws(value)) || nchar(value, type = "chars") > 128L ||
      grepl("[\r\n/\\\\[:cntrl:]]", value)) {
    cli::cli_abort(
      "{.arg product_name} must be one non-empty, path-free line."
    )
  }
  value
}

.tauri_identifier <- function(value) {
  if (!is.character(value) || length(value) != 1L || is.na(value) ||
      nchar(value, type = "bytes") > 255L ||
      !grepl(
        paste0(
          "^[a-z][a-z0-9]*",
          "(?:[.-][a-z0-9](?:[a-z0-9-]*[a-z0-9])?)+$"
        ),
        value,
        perl = TRUE
      )) {
    cli::cli_abort(
      paste0(
        "{.arg identifier} must be a lowercase reverse-domain identifier, ",
        "for example {.val com.example.my-app}."
      )
    )
  }
  value
}

.tauri_version <- function(value) {
  pattern <- paste0(
    "^(0|[1-9][0-9]*)\\.(0|[1-9][0-9]*)\\.(0|[1-9][0-9]*)",
    "(?:-[0-9A-Za-z-]+(?:\\.[0-9A-Za-z-]+)*)?",
    "(?:\\+[0-9A-Za-z-]+(?:\\.[0-9A-Za-z-]+)*)?$"
  )
  if (!is.character(value) || length(value) != 1L || is.na(value) ||
      !grepl(pattern, value, perl = TRUE)) {
    cli::cli_abort(
      "{.arg version} must be one semantic version such as {.val 0.1.0}."
    )
  }
  value
}

.tauri_icon <- function(value) {
  if (is.null(value)) {
    return(NULL)
  }
  if (!is.character(value) || length(value) != 1L || is.na(value) ||
      !file.exists(value) || dir.exists(value) ||
      nzchar(Sys.readlink(value)) ||
      !grepl("\\.ico$", value, ignore.case = TRUE)) {
    cli::cli_abort(
      "{.arg icon} must be an existing Windows .ico file."
    )
  }
  size <- file.info(value)$size
  if (is.na(size) || size < 1 || size > 10 * 1024^2) {
    cli::cli_abort(
      "{.arg icon} must be non-empty and no larger than 10 MiB."
    )
  }
  normalizePath(value, winslash = "/", mustWork = TRUE)
}

.tauri_output_path <- function(output_dir, bundle_dir, slug) {
  if (is.null(output_dir)) {
    output_dir <- file.path(
      dirname(bundle_dir),
      paste0(slug, "-tauri")
    )
  }
  if (!is.character(output_dir) || length(output_dir) != 1L ||
      is.na(output_dir) || !nzchar(output_dir)) {
    cli::cli_abort(
      "{.arg output_dir} must be NULL or one non-empty path."
    )
  }
  parent <- dirname(output_dir)
  if (!dir.exists(parent) &&
      !dir.create(parent, recursive = TRUE, showWarnings = FALSE)) {
    cli::cli_abort(
      "Cannot create output parent directory {.path {parent}}."
    )
  }
  parent <- normalizePath(parent, winslash = "/", mustWork = TRUE)
  output <- gsub("\\\\", "/", file.path(parent, basename(output_dir)))
  if (file.exists(output) || dir.exists(output)) {
    cli::cli_abort(
      "Refusing to replace existing {.arg output_dir} {.path {output}}."
    )
  }
  if (.rpackit_path_within(output, bundle_dir)) {
    cli::cli_abort(
      "{.arg output_dir} may not be inside {.arg bundle_dir}."
    )
  }
  output
}

.tauri_copy_file <- function(source, destination) {
  parent <- dirname(destination)
  if (!dir.exists(parent) &&
      !dir.create(parent, recursive = TRUE, showWarnings = FALSE)) {
    cli::cli_abort(
      "Cannot create generated-project directory {.path {parent}}."
    )
  }
  if (!file.copy(
    source,
    destination,
    overwrite = FALSE,
    copy.mode = TRUE,
    copy.date = TRUE
  )) {
    cli::cli_abort(
      "Cannot copy generated-project file {.path {basename(source)}}."
    )
  }
  invisible(destination)
}

.tauri_replace_once <- function(path, before, after, context) {
  size <- file.info(path)$size
  if (is.na(size) || size > 1024^2) {
    cli::cli_abort(
      "Cannot safely patch {context}.",
      class = "rpackit_tauri_template_contract_error"
    )
  }
  text <- readChar(path, nchars = size, useBytes = TRUE)
  positions <- gregexpr(before, text, fixed = TRUE)[[1L]]
  count <- if (identical(positions[[1L]], -1L)) 0L else length(positions)
  if (count != 1L) {
    cli::cli_abort(
      "The Tauri template has an unexpected {context} patch surface.",
      class = "rpackit_tauri_template_contract_error"
    )
  }
  text <- sub(before, after, text, fixed = TRUE)
  writeBin(charToRaw(text), path)
  invisible(path)
}

.tauri_write_readme <- function(path) {
  lines <- c(
    "# Generated rpackit Windows project",
    "",
    "This source project contains one validated Windows desktop resource",
    "bundle and the maintained native rpackit shell. Application identity,",
    "template integrity, contracts, requirements, and launch mode are recorded",
    "in `src-tauri/rpackit-native.json`.",
    "",
    "## Build on Windows",
    "",
    "Run heavy compilation on a Windows runner or build machine:",
    "",
    "```powershell",
    "Set-Location src-tauri",
    "cargo tauri build",
    "```",
    "",
    "This creates a current-user NSIS setup executable under",
    "`target/release/bundle/nsis`. Generation configures packaging but does",
    "not itself claim that an installer was built, signed, or clean-machine",
    "verified.",
    "",
    "An installed application needs no path arguments: it resolves the bundled",
    "resources and private per-user session/profile parents automatically.",
    "",
    "## Development launch",
    "",
    "For development, the shell also accepts explicit absolute",
    "resource/session/profile paths. Create the two parent directories below",
    "a current-account-owned local directory, then run from the project root:",
    "",
    "```powershell",
    "$work = Join-Path $env:LOCALAPPDATA 'rpackit-generated-development'",
    "$sessions = New-Item -ItemType Directory -Force `",
    "  -Path (Join-Path $work 'sessions')",
    "$profiles = New-Item -ItemType Directory -Force `",
    "  -Path (Join-Path $work 'profiles')",
    "cargo run --locked -p rpackit-windows-shell -- `",
    "  --bundle (Resolve-Path 'src-tauri') `",
    "  --session-parent $sessions.FullName `",
    "  --profile-parent $profiles.FullName",
    "```",
    "",
    "No portable runtime, Cargo target directory, executable, or installer",
    "belongs in source control."
  )
  writeLines(lines, path, useBytes = TRUE)
  invisible(path)
}

.tauri_native_metadata <- function(product_name, identifier, version,
                                   descriptor, integrity, integrity_type,
                                   official, resources, icon) {
  list(
    schema_version = "1",
    project_type = "rpackit-tauri-windows",
    application = list(
      product_name = product_name,
      identifier = identifier,
      version = version
    ),
    contracts = descriptor$contracts,
    template = list(
      name = descriptor$template,
      version = descriptor$template_version,
      official = official,
      integrity = list(
        type = integrity_type,
        sha256 = integrity
      )
    ),
    requirements = c(
      descriptor$requirements,
      list(clean_machine_verified = FALSE)
    ),
    assets = list(
      icon = list(
        path = "icons/icon.ico",
        sha256 = digest::digest(file = icon, algo = "sha256")
      )
    ),
    resources = list(
      path = "resources",
      manifest = "resources/rpackit.json",
      manifest_sha256 = digest::digest(
        file = file.path(resources, "rpackit.json"),
        algo = "sha256"
      )
    ),
    launch = list(
      mode = "packaged-or-explicit-resource-bundle",
      development_bundle = ".",
      packaged_bundle = "$RESOURCE",
      arguments = as.list(c(
        "--bundle",
        "--session-parent",
        "--profile-parent"
      ))
    ),
    packaging = list(
      installer = "nsis-configured",
      tauri_bundle_active = TRUE,
      install_mode = "currentUser"
    ),
    generated_by = list(
      package = "rpackit",
      version = utils::packageDescription("rpackit")[["Version"]]
    )
  )
}

.tauri_copy_template <- function(stage, materialized, bundle_dir,
                                 product_name, identifier, version, icon) {
  root <- materialized$root
  descriptor <- materialized$descriptor
  project <- descriptor$project
  for (file in .tauri_vector(project$root_files)) {
    .tauri_copy_file(file.path(root, file), file.path(stage, file))
  }
  .tauri_copy_file(
    file.path(root, project$root_manifest),
    file.path(stage, "Cargo.toml")
  )
  .desktop_copy_tree(
    file.path(root, project$application_source),
    file.path(stage, project$application_target)
  )
  dir.create(file.path(stage, "crates"))
  for (crate in .tauri_vector(project$crates)) {
    .desktop_copy_tree(
      file.path(root, "crates", crate),
      file.path(stage, "crates", crate)
    )
  }
  app <- file.path(stage, project$application_target)
  cargo <- file.path(app, "Cargo.toml")
  cargo_text <- readLines(cargo, warn = FALSE, encoding = "UTF-8")
  old_paths <- grepl("../../crates/", cargo_text, fixed = TRUE)
  if (sum(old_paths) != 3L) {
    cli::cli_abort(
      "The Tauri template has an unexpected crate path surface.",
      class = "rpackit_tauri_template_contract_error"
    )
  }
  cargo_text[old_paths] <- gsub(
    "../../crates/",
    "../crates/",
    cargo_text[old_paths],
    fixed = TRUE
  )
  writeLines(cargo_text, cargo, useBytes = TRUE)
  .tauri_replace_once(
    file.path(app, "src", "windows_app.rs"),
    'const APPLICATION_ID: &str = "dev.rpackit.shell";',
    sprintf('const APPLICATION_ID: &str = "%s";', identifier),
    "application identifier"
  )
  config_path <- file.path(app, "tauri.conf.json")
  config <- .tauri_read_json(config_path, "Tauri configuration")
  config$productName <- product_name
  config$version <- version
  config$identifier <- identifier
  config$bundle$active <- TRUE
  config$bundle$targets <- list("nsis")
  config$bundle$icon <- list("icons/icon.ico")
  config$bundle$resources <- list("resources/" = "resources/")
  config$bundle$windows$minimumWebview2Version <-
    descriptor$requirements$webview2
  config$bundle$windows$nsis <- list(installMode = "currentUser")
  .desktop_write_json(config, config_path)
  icons <- file.path(app, "icons")
  dir.create(icons)
  icon_source <- if (is.null(icon)) {
    file.path(root, project$default_icon)
  } else {
    icon
  }
  generated_icon <- file.path(icons, "icon.ico")
  .tauri_copy_file(icon_source, generated_icon)
  resources <- file.path(app, "resources")
  .desktop_copy_tree(
    file.path(bundle_dir, "resources"),
    resources
  )
  metadata <- .tauri_native_metadata(
    product_name = product_name,
    identifier = identifier,
    version = version,
    descriptor = descriptor,
    integrity = materialized$integrity,
    integrity_type = materialized$integrity_type,
    official = materialized$official,
    resources = resources,
    icon = generated_icon
  )
  .desktop_write_json(
    metadata,
    file.path(app, "rpackit-native.json")
  )
  .tauri_write_readme(file.path(stage, "README.md"))
  invisible(stage)
}

.tauri_project_required <- function(path) {
  file.path(path, c(
    "Cargo.toml",
    "Cargo.lock",
    "rust-toolchain.toml",
    "src-tauri/Cargo.toml",
    "src-tauri/tauri.conf.json",
    "src-tauri/rpackit-native.json",
    "src-tauri/icons/icon.ico",
    "src-tauri/src/main.rs",
    "src-tauri/src/windows_app.rs",
    "src-tauri/resources/rpackit.json",
    "src-tauri/resources/launcher.R"
  ))
}

#' Validate a generated Tauri source project
#'
#' `validate_tauri_project()` checks application identity, versioned native
#' metadata, template integrity records, the reduced Rust workspace, icon and
#' resource-manifest digests, and the embedded desktop resource bundle. It
#' does not compile Rust or create an installer.
#'
#' @param project_dir Generated project directory.
#' @param verify_runtime Execute the embedded `Rscript` and verify installed
#'   packages as part of [validate_desktop_bundle()].
#' @param quiet Suppress the validation summary.
#' @return An `rpackit_tauri_validation` object.
#' @export
validate_tauri_project <- function(project_dir, verify_runtime = FALSE,
                                   quiet = FALSE) {
  if (!is.character(project_dir) || length(project_dir) != 1L ||
      is.na(project_dir) || !dir.exists(project_dir)) {
    cli::cli_abort(
      "{.arg project_dir} must be an existing generated-project directory."
    )
  }
  verify_runtime <- .tauri_scalar_flag(verify_runtime, "verify_runtime")
  quiet <- .tauri_scalar_flag(quiet, "quiet")
  path <- normalizePath(project_dir, winslash = "/", mustWork = TRUE)
  required <- .tauri_project_required(path)
  if (any(!file.exists(required)) || any(dir.exists(required))) {
    cli::cli_abort(
      "The generated Tauri project is incomplete.",
      class = "rpackit_tauri_project_error"
    )
  }
  metadata <- .tauri_read_json(
    file.path(path, "src-tauri", "rpackit-native.json"),
    "rpackit native metadata"
  )
  config <- .tauri_read_json(
    file.path(path, "src-tauri", "tauri.conf.json"),
    "Tauri configuration"
  )
  valid_structure <- is.list(metadata) &&
    is.list(metadata$application) &&
    is.list(metadata$template) &&
    is.list(metadata$template$integrity) &&
    is.list(metadata$requirements) &&
    is.list(metadata$assets) &&
    is.list(metadata$assets$icon) &&
    is.list(metadata$resources) &&
    is.list(metadata$launch) &&
    is.list(metadata$packaging) &&
    is.list(config) &&
    is.list(config$bundle) &&
    is.list(config$bundle$resources) &&
    is.list(config$bundle$windows) &&
    is.list(config$bundle$windows$nsis)
  if (!isTRUE(valid_structure)) {
    cli::cli_abort(
      "The generated project contains malformed native metadata.",
      class = "rpackit_tauri_project_error"
    )
  }
  application <- metadata$application
  product_name <- .tauri_product_name(application$product_name)
  identifier <- .tauri_identifier(application$identifier)
  version <- .tauri_version(application$version)
  expected_contracts <- list(
    transport = "2",
    resource_bundle = "1",
    launcher = "2"
  )
  requirements <- metadata$requirements
  expected_requirements <- c(
    .tauri_expected_requirements,
    list(clean_machine_verified = FALSE)
  )
  integrity <- metadata$template$integrity
  if (!identical(metadata$schema_version, "1") ||
      !identical(metadata$project_type, "rpackit-tauri-windows") ||
      !identical(metadata$contracts, expected_contracts) ||
      !identical(metadata$template$name, .tauri_official_template$name) ||
      !identical(metadata$template$version, .tauri_official_template$version) ||
      !is.logical(metadata$template$official) ||
      length(metadata$template$official) != 1L ||
      is.na(metadata$template$official) ||
      !is.character(integrity$type) ||
      length(integrity$type) != 1L ||
      is.na(integrity$type) ||
      !integrity$type %in% c("archive-sha256", "tree-sha256") ||
      !is.character(integrity$sha256) ||
      length(integrity$sha256) != 1L ||
      is.na(integrity$sha256) ||
      !grepl("^[a-f0-9]{64}$", integrity$sha256) ||
      !identical(requirements, expected_requirements)) {
    cli::cli_abort(
      "The generated project uses unsupported or invalid native metadata.",
      class = "rpackit_tauri_project_error"
    )
  }
  if (!identical(config$productName, product_name) ||
      !identical(config$identifier, identifier) ||
      !identical(config$version, version) ||
      !identical(config$bundle$active, TRUE) ||
      !identical(.tauri_vector(config$bundle$targets), "nsis") ||
      !identical(.tauri_vector(config$bundle$icon), "icons/icon.ico") ||
      !identical(
        config$bundle$resources[["resources/"]],
        "resources/"
      ) ||
      !identical(
        config$bundle$windows$minimumWebview2Version,
        .tauri_expected_requirements$webview2
      ) ||
      !identical(
        config$bundle$windows$nsis$installMode,
        "currentUser"
      )) {
    cli::cli_abort(
      "Tauri configuration disagrees with native application metadata.",
      class = "rpackit_tauri_project_error"
    )
  }
  rust <- readLines(
    file.path(path, "src-tauri", "src", "windows_app.rs"),
    warn = FALSE,
    encoding = "UTF-8"
  )
  identity_line <- sprintf(
    'const APPLICATION_ID: &str = "%s";',
    identifier
  )
  if (sum(grepl(identity_line, rust, fixed = TRUE)) != 1L) {
    cli::cli_abort(
      "The native shell identity disagrees with Tauri configuration.",
      class = "rpackit_tauri_project_error"
    )
  }
  app_cargo <- readLines(
    file.path(path, "src-tauri", "Cargo.toml"),
    warn = FALSE,
    encoding = "UTF-8"
  )
  if (any(grepl("../../crates/", app_cargo, fixed = TRUE)) ||
      sum(grepl("../crates/", app_cargo, fixed = TRUE)) != 3L) {
    cli::cli_abort(
      "The generated native crate paths are invalid.",
      class = "rpackit_tauri_project_error"
    )
  }
  expected_crates <- file.path(path, "crates", .tauri_expected_crates)
  if (any(!dir.exists(expected_crates)) ||
      dir.exists(file.path(path, "crates", "transport-testkit")) ||
      dir.exists(file.path(path, "apps", "windows-spike"))) {
    cli::cli_abort(
      "The generated Rust workspace does not match the reduced template.",
      class = "rpackit_tauri_project_error"
    )
  }
  icon <- file.path(path, "src-tauri", "icons", "icon.ico")
  manifest <- file.path(
    path,
    "src-tauri",
    "resources",
    "rpackit.json"
  )
  if (!identical(
        digest::digest(file = icon, algo = "sha256"),
        metadata$assets$icon$sha256
      ) ||
      !identical(metadata$assets$icon$path, "icons/icon.ico") ||
      !identical(
        digest::digest(file = manifest, algo = "sha256"),
        metadata$resources$manifest_sha256
      ) ||
      !identical(metadata$resources$path, "resources") ||
      !identical(metadata$resources$manifest, "resources/rpackit.json") ||
      !identical(
        metadata$launch$mode,
        "packaged-or-explicit-resource-bundle"
      ) ||
      !identical(metadata$launch$development_bundle, ".") ||
      !identical(metadata$launch$packaged_bundle, "$RESOURCE") ||
      !identical(
        .tauri_vector(metadata$launch$arguments),
        c("--bundle", "--session-parent", "--profile-parent")
      ) ||
      !identical(metadata$packaging$installer, "nsis-configured") ||
      !identical(metadata$packaging$tauri_bundle_active, TRUE) ||
      !identical(metadata$packaging$install_mode, "currentUser")) {
    cli::cli_abort(
      "Generated assets or launch metadata failed validation.",
      class = "rpackit_tauri_project_error"
    )
  }
  bundle <- validate_desktop_bundle(
    file.path(path, "src-tauri"),
    verify_runtime = verify_runtime,
    quiet = TRUE
  )
  if (!identical(bundle$runtime_platform, "windows") ||
      !isTRUE(bundle$dependencies_installed) ||
      !isTRUE(bundle$dependency_constraints_verified) ||
      !isTRUE(bundle$network_token_enforced)) {
    cli::cli_abort(
      paste0(
        "A generated Tauri project requires a dependency-complete, ",
        "authenticated Windows resource bundle."
      ),
      class = "rpackit_tauri_bundle_error"
    )
  }
  result <- structure(
    list(
      valid = TRUE,
      path = path,
      product_name = product_name,
      identifier = identifier,
      version = version,
      template_version = metadata$template$version,
      template_integrity = integrity$sha256,
      contracts = metadata$contracts,
      bundle = bundle
    ),
    class = "rpackit_tauri_validation"
  )
  if (!quiet) {
    print(result)
  }
  invisible(result)
}

#' Generate an application-specific Tauri source project
#'
#' `generate_tauri_app()` validates one dependency-complete Windows desktop
#' resource bundle, verifies a versioned rpackit Tauri source template, and
#' atomically renders a reduced application project around those resources.
#' It stamps the product name, reverse-domain identifier, semantic version,
#' optional Windows icon, transport/resource/launcher contracts, toolchain
#' minima, template integrity, packaged/development launch configuration, and
#' current-user NSIS packaging.
#'
#' The official source ZIP is downloaded to a temporary file, checked against
#' its pinned SHA-256, and removed after generation. It is not retained in a
#' package cache. A trusted local template directory can be supplied for
#' offline development; its selected source tree digest is recorded instead.
#'
#' This function generates packaging-ready source. It does not compile Rust,
#' build or sign an installer, or claim clean-machine verification.
#'
#' @param bundle_dir Prepared Windows bundle from [prepare_desktop()].
#' @param output_dir New generated-project directory. By default, a sibling
#'   directory named from the application is used.
#' @param product_name Display name. Defaults to the bundle application name.
#' @param identifier Lowercase reverse-domain application identifier. Defaults
#'   to `dev.rpackit.<application-slug>`.
#' @param version Semantic application version.
#' @param icon Optional existing Windows `.ico` file.
#' @param template_source `NULL` for the pinned official source ZIP, or a
#'   trusted local template directory/custom HTTPS or local ZIP.
#' @param template_sha256 Required lowercase SHA-256 for a custom ZIP.
#'   Optional for a local directory; when supplied, it must match its selected
#'   template tree.
#' @param verify_runtime Execute the bundled `Rscript` and recheck packages
#'   before generation.
#' @param quiet Suppress the generation summary.
#' @return An `rpackit_tauri_project` object.
#' @export
generate_tauri_app <- function(
  bundle_dir,
  output_dir = NULL,
  product_name = NULL,
  identifier = NULL,
  version = "0.1.0",
  icon = NULL,
  template_source = NULL,
  template_sha256 = NULL,
  verify_runtime = FALSE,
  quiet = FALSE
) {
  verify_runtime <- .tauri_scalar_flag(verify_runtime, "verify_runtime")
  quiet <- .tauri_scalar_flag(quiet, "quiet")
  bundle <- validate_desktop_bundle(
    bundle_dir,
    verify_runtime = verify_runtime,
    quiet = TRUE
  )
  if (!identical(bundle$runtime_platform, "windows") ||
      !isTRUE(bundle$dependencies_installed) ||
      !isTRUE(bundle$dependency_constraints_verified) ||
      !isTRUE(bundle$network_token_enforced)) {
    cli::cli_abort(
      paste0(
        "Tauri generation requires a dependency-complete, constraint-",
        "verified, authenticated Windows desktop bundle."
      ),
      class = "rpackit_tauri_bundle_error"
    )
  }
  bundle_path <- bundle$path
  bundle_manifest <- .tauri_read_json(
    file.path(bundle_path, "resources", "rpackit.json"),
    "desktop resource manifest"
  )
  if (is.null(product_name)) {
    product_name <- bundle_manifest$app$name
  }
  product_name <- .tauri_product_name(product_name)
  slug <- .tauri_slug(product_name)
  if (is.null(identifier)) {
    identifier <- paste("dev", "rpackit", slug, sep = ".")
  }
  identifier <- .tauri_identifier(identifier)
  version <- .tauri_version(version)
  icon <- .tauri_icon(icon)
  output <- .tauri_output_path(output_dir, bundle_path, slug)
  materialized <- .tauri_materialize_template(
    template_source,
    template_sha256,
    quiet
  )
  if (!is.null(materialized$temporary)) {
    on.exit(
      unlink(materialized$temporary, recursive = TRUE, force = TRUE),
      add = TRUE
    )
  }
  stage <- tempfile(".rpackit-tauri-stage-", tmpdir = dirname(output))
  if (!dir.create(stage, recursive = FALSE, showWarnings = FALSE)) {
    cli::cli_abort(
      "Cannot create generated-project staging directory."
    )
  }
  completed <- FALSE
  on.exit({
    if (!completed && dir.exists(stage)) {
      unlink(stage, recursive = TRUE, force = TRUE)
    }
  }, add = TRUE)
  .tauri_copy_template(
    stage = stage,
    materialized = materialized,
    bundle_dir = bundle_path,
    product_name = product_name,
    identifier = identifier,
    version = version,
    icon = icon
  )
  validation <- validate_tauri_project(
    stage,
    verify_runtime = verify_runtime,
    quiet = TRUE
  )
  if (!file.rename(stage, output)) {
    cli::cli_abort(
      "Cannot atomically publish generated project to {.path {output}}."
    )
  }
  completed <- TRUE
  path <- normalizePath(output, winslash = "/", mustWork = TRUE)
  validation$path <- path
  result <- structure(
    list(
      path = path,
      src_tauri = file.path(path, "src-tauri"),
      product_name = product_name,
      identifier = identifier,
      version = version,
      template_version = materialized$descriptor$template_version,
      template_integrity = materialized$integrity,
      contracts = materialized$descriptor$contracts,
      validation = validation
    ),
    class = "rpackit_tauri_project"
  )
  if (!quiet) {
    print(result)
  }
  invisible(result)
}

#' @export
print.rpackit_tauri_project <- function(x, ...) {
  cli::cli_h1("rpackit generated Tauri project")
  cli::cli_li("Application: {x$product_name} ({x$identifier})")
  cli::cli_li("Version: {x$version}")
  cli::cli_li("Template: {x$template_version}")
  cli::cli_li("Path: {.path {x$path}}")
  cli::cli_alert_info(
    "Packaging-ready source generated; run cargo tauri build on Windows."
  )
  invisible(x)
}

#' @export
print.rpackit_tauri_validation <- function(x, ...) {
  cli::cli_h1("rpackit Tauri project validation")
  cli::cli_li("Application: {x$product_name} ({x$identifier})")
  cli::cli_li("Version: {x$version}")
  cli::cli_li("Template: {x$template_version}")
  cli::cli_li("Contracts: transport 2, resources 1, launcher 2")
  cli::cli_li("Valid: yes")
  invisible(x)
}
