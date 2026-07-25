.rpackit_path_within <- function(path, parent) {
  path <- tolower(gsub("\\\\", "/", path))
  parent <- sub("/+$", "", tolower(gsub("\\\\", "/", parent)))
  identical(path, parent) || startsWith(path, paste0(parent, "/"))
}

.desktop_output_path <- function(output_dir, app_path) {
  if (is.null(output_dir)) {
    output_dir <- file.path(app_path, "dist", "desktop-resources")
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
    cli::cli_abort("Cannot create output parent directory {.path {parent}}.")
  }
  parent <- normalizePath(parent, winslash = "/", mustWork = TRUE)
  output <- file.path(parent, basename(output_dir))
  output <- gsub("\\\\", "/", output)
  if (file.exists(output) || dir.exists(output)) {
    cli::cli_abort(
      "Refusing to replace existing {.arg output_dir} {.path {output}}."
    )
  }
  if (.rpackit_path_within(output, app_path)) {
    allowed_root <- gsub(
      "\\\\",
      "/",
      file.path(app_path, "dist")
    )
    if (!.rpackit_path_within(output, allowed_root)) {
      cli::cli_abort(
        "An output inside {.arg app_dir} must be under its {.path dist} ",
        "directory."
      )
    }
  }
  output
}

.desktop_runtime <- function(runtime_dir) {
  if (!is.character(runtime_dir) || length(runtime_dir) != 1L ||
      is.na(runtime_dir) || !dir.exists(runtime_dir)) {
    cli::cli_abort("{.arg runtime_dir} must be an existing R runtime directory.")
  }
  path <- normalizePath(runtime_dir, winslash = "/", mustWork = TRUE)
  candidates <- c("bin/Rscript.exe", "bin/Rscript")
  found <- candidates[file.exists(file.path(path, candidates))]
  if (length(found) != 1L) {
    cli::cli_abort(
      "{.arg runtime_dir} must contain exactly one of ",
      "{.path bin/Rscript.exe} or {.path bin/Rscript}."
    )
  }
  library <- file.path(path, "library")
  if (!dir.exists(library)) {
    cli::cli_abort(
      "{.arg runtime_dir} must contain a runtime-local {.path library}."
    )
  }
  list(
    path = path,
    rscript = found[[1L]],
    library = "library",
    platform = if (endsWith(found[[1L]], ".exe")) "windows" else "unix"
  )
}

.desktop_run_rscript <- function(rscript, arguments, context) {
  isolation <- gsub(
    "\\\\",
    "/",
    file.path(tempdir(), "rpackit-isolated-user-state")
  )
  environment_names <- c(
    "R_LIBS",
    "R_LIBS_USER",
    "R_PROFILE_USER",
    "R_ENVIRON_USER"
  )
  old_environment <- Sys.getenv(environment_names, unset = NA_character_)
  isolation_values <- c(
    isolation,
    isolation,
    paste0(isolation, "/Rprofile"),
    paste0(isolation, "/Renviron")
  )
  do.call(
    Sys.setenv,
    stats::setNames(as.list(isolation_values), environment_names)
  )
  on.exit({
    existing <- !is.na(old_environment)
    if (any(existing)) {
      do.call(
        Sys.setenv,
        stats::setNames(
          as.list(old_environment[existing]),
          environment_names[existing]
        )
      )
    }
    if (any(!existing)) {
      Sys.unsetenv(environment_names[!existing])
    }
  }, add = TRUE)
  output <- tryCatch(
    suppressWarnings(system2(
      rscript,
      arguments,
      stdout = TRUE,
      stderr = TRUE
    )),
    error = function(error) {
      cli::cli_abort(
        "{context} failed to start: {conditionMessage(error)}."
      )
    }
  )
  status <- attr(output, "status")
  if (!is.null(status) && as.integer(status) != 0L) {
    tail_output <- utils::tail(output, 20L)
    detail <- if (length(tail_output)) {
      paste(tail_output, collapse = "\n")
    } else {
      "No process output was captured."
    }
    cli::cli_abort(
      c(
        "{context} failed with exit status {as.integer(status)}.",
        "i" = "{detail}"
      )
    )
  }
  unname(output)
}

.desktop_verify_runtime <- function(runtime) {
  rscript <- file.path(runtime$path, runtime$rscript)
  .desktop_run_rscript(
    rscript,
    "--version",
    "Portable R runtime verification"
  )
  invisible(runtime)
}

.desktop_app_excluded <- function(relative) {
  relative <- gsub("\\\\", "/", relative)
  first <- strsplit(relative, "/", fixed = TRUE)[[1L]][[1L]]
  if (first %in% c(".git", ".Rproj.user", "build", "dist")) {
    return(TRUE)
  }
  relative == "renv/library" ||
    startsWith(relative, "renv/library/") ||
    relative == "renv/staging" ||
    startsWith(relative, "renv/staging/") ||
    relative == "packrat/lib" ||
    startsWith(relative, "packrat/lib/")
}

.desktop_copy_tree <- function(source, destination, exclude = NULL) {
  source <- normalizePath(source, winslash = "/", mustWork = TRUE)
  if (!dir.create(destination, recursive = TRUE, showWarnings = FALSE) &&
      !dir.exists(destination)) {
    cli::cli_abort("Cannot create bundle directory {.path {destination}}.")
  }
  visited <- new.env(parent = emptyenv())
  assign(tolower(source), TRUE, envir = visited)

  copy_directory <- NULL
  copy_directory <- function(current, target, prefix = "") {
    entries <- list.files(
      current,
      all.files = TRUE,
      no.. = TRUE,
      full.names = TRUE,
      recursive = FALSE
    )
    for (entry in entries) {
      name <- basename(entry)
      relative <- if (nzchar(prefix)) {
        paste(prefix, name, sep = "/")
      } else {
        name
      }
      if (!is.null(exclude) && isTRUE(exclude(relative))) {
        next
      }
      link <- Sys.readlink(entry)
      if (nzchar(link)) {
        cli::cli_abort(
          "Symbolic links are not supported in bundle input: ",
          "{.path {relative}}."
        )
      }
      canonical <- normalizePath(entry, winslash = "/", mustWork = TRUE)
      if (!.rpackit_path_within(canonical, source)) {
        cli::cli_abort(
          "Bundle input escapes its source directory: {.path {relative}}."
        )
      }
      destination_path <- file.path(target, name)
      if (dir.exists(entry)) {
        key <- tolower(canonical)
        if (exists(key, envir = visited, inherits = FALSE)) {
          cli::cli_abort(
            "Bundle input contains a directory cycle at {.path {relative}}."
          )
        }
        assign(key, TRUE, envir = visited)
        if (!dir.create(
          destination_path,
          recursive = FALSE,
          showWarnings = FALSE
        ) && !dir.exists(destination_path)) {
          cli::cli_abort(
            "Cannot create bundle directory {.path {relative}}."
          )
        }
        copy_directory(entry, destination_path, relative)
      } else if (!file.copy(
        entry,
        destination_path,
        overwrite = FALSE,
        copy.mode = TRUE,
        copy.date = TRUE
      )) {
        cli::cli_abort("Cannot copy bundle file {.path {relative}}.")
      }
    }
    invisible(NULL)
  }

  copy_directory(source, destination)
  invisible(destination)
}

.desktop_launcher_lines <- function() {
  c(
    "fail <- function(message) stop(message, call. = FALSE)",
    "arguments <- commandArgs(trailingOnly = TRUE)",
    "if (!length(arguments) || length(arguments) %% 2L != 0L) {",
    "  fail('Usage: launcher.R --app <path> --port <port> --token <token>')",
    "}",
    "keys <- arguments[seq.int(1L, length(arguments), by = 2L)]",
    "values <- arguments[seq.int(2L, length(arguments), by = 2L)]",
    "required <- c('--app', '--port', '--token')",
    "if (anyDuplicated(keys) || !setequal(keys, required)) {",
    "  fail('Exactly one --app, --port, and --token argument is required.')",
    "}",
    "options <- stats::setNames(values, keys)",
    "app <- options[['--app']]",
    "if (!dir.exists(app)) fail('The application directory does not exist.')",
    "app <- normalizePath(app, winslash = '/', mustWork = TRUE)",
    "layout_ok <- file.exists(file.path(app, 'app.R')) ||",
    "  all(file.exists(file.path(app, c('ui.R', 'server.R'))))",
    "if (!layout_ok) fail('The application is not a supported Shiny layout.')",
    "port <- suppressWarnings(as.integer(options[['--port']]))",
    "if (is.na(port) || port < 1L || port > 65535L) {",
    "  fail('--port must be a whole number between 1 and 65535.')",
    "}",
    "token <- options[['--token']]",
    "if (is.na(token) || nchar(token, type = 'bytes') < 16L) {",
    "  fail('--token must contain at least 16 bytes.')",
    "}",
    "runtime_library <- file.path(R.home(), 'library')",
    "if (!dir.exists(runtime_library)) {",
    "  fail('The bundled R library directory is missing.')",
    "}",
    ".libPaths(unique(c(runtime_library, .libPaths())))",
    "if (!requireNamespace('shiny', quietly = TRUE)) {",
    "  fail(\"The bundled runtime does not contain the 'shiny' package.\")",
    "}",
    "Sys.setenv(RPACKIT_SESSION_TOKEN = token)",
    "url <- sprintf(",
    "  'http://127.0.0.1:%d/?rpackit_token=%s',",
    "  port,",
    "  utils::URLencode(token, reserved = TRUE)",
    ")",
    "cat(sprintf('RPACKIT_STARTING %s\\n', url))",
    "flush.console()",
    "shiny::runApp(",
    "  app,",
    "  host = '127.0.0.1',",
    "  port = port,",
    "  launch.browser = FALSE,",
    "  quiet = TRUE",
    ")"
  )
}

.desktop_write_json <- function(value, path) {
  jsonlite::write_json(
    value,
    path,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )
  invisible(path)
}

.desktop_repositories <- function(repos) {
  if (!is.character(repos) || !length(repos) || anyNA(repos) ||
      any(!nzchar(repos))) {
    cli::cli_abort(
      "{.arg repos} must contain one or more non-empty repository URLs."
    )
  }
  repos[repos == "@CRAN@"] <- "https://cloud.r-project.org"
  repos
}

.desktop_r_literal <- function(value) {
  paste(utils::capture.output(dput(value)), collapse = "\n")
}

.desktop_install_dependencies <- function(resources, plan, repos) {
  runtime <- .desktop_runtime(file.path(resources, "R"))
  rscript <- file.path(runtime$path, runtime$rscript)
  app <- normalizePath(
    file.path(resources, "app"),
    winslash = "/",
    mustWork = TRUE
  )
  library <- normalizePath(
    file.path(resources, "R", runtime$library),
    winslash = "/",
    mustWork = TRUE
  )
  packages <- sort(unique(c(
    "shiny",
    plan$dependencies$package[plan$dependencies$required]
  )))
  script <- tempfile("rpackit-install-", tmpdir = dirname(resources))
  on.exit(unlink(script, force = TRUE), add = TRUE)
  locked <- !is.na(plan$lockfile)
  lines <- c(
    sprintf("library_path <- %s", .desktop_r_literal(library)),
    sprintf("app_path <- %s", .desktop_r_literal(app)),
    sprintf("repos <- %s", .desktop_r_literal(repos)),
    sprintf("packages <- %s", .desktop_r_literal(packages)),
    ".libPaths(unique(c(library_path, .libPaths())))",
    "options(repos = repos)"
  )
  if (locked) {
    lines <- c(
      lines,
      "if (!requireNamespace('renv', quietly = TRUE)) {",
      "  install.packages('renv', lib = library_path, dependencies = NA)",
      "}",
      "renv::restore(",
      "  project = app_path,",
      "  lockfile = file.path(app_path, 'renv.lock'),",
      "  library = library_path,",
      "  prompt = FALSE",
      ")"
    )
  } else {
    lines <- c(
      lines,
      "installed <- rownames(installed.packages())",
      "missing <- setdiff(packages, installed)",
      "if (length(missing)) {",
      "  install.packages(missing, lib = library_path, dependencies = NA)",
      "}"
    )
  }
  lines <- c(
    lines,
    "missing <- setdiff(packages, rownames(installed.packages()))",
    "if (length(missing)) {",
    "  stop(",
    "    'Bundled dependency installation is incomplete: ',",
    "    paste(missing, collapse = ', '),",
    "    call. = FALSE",
    "  )",
    "}"
  )
  writeLines(lines, script, useBytes = TRUE)
  .desktop_run_rscript(
    rscript,
    c("--vanilla", shQuote(script)),
    "Bundled dependency installation"
  )
  list(
    packages = packages,
    strategy = if (locked) "renv-restore" else "install-packages"
  )
}

.desktop_manifest <- function(app_name, check, runtime, dependency_plan,
                              installed, install_result) {
  packages <- sort(unique(c(
    "shiny",
    dependency_plan$dependencies$package[
      dependency_plan$dependencies$required
    ]
  )))
  list(
    schema_version = "1",
    bundle_type = "rpackit-desktop-resources",
    app = list(
      name = app_name,
      type = check$app_type,
      path = "app"
    ),
    runtime = list(
      path = "R",
      rscript = paste0("R/", gsub("\\\\", "/", runtime$rscript)),
      library = paste0("R/", gsub("\\\\", "/", runtime$library)),
      platform = runtime$platform
    ),
    launcher = list(
      script = "launcher.R",
      host = "127.0.0.1",
      port = "required-argument",
      token = "required-argument",
      network_token_enforced = FALSE,
      readiness = "RPACKIT_STARTING then HTTP polling"
    ),
    dependencies = list(
      installed = installed,
      strategy = if (installed) install_result$strategy else "not-installed",
      packages = as.list(packages),
      locked_r_version = dependency_plan$locked_r_version,
      r_constraint = dependency_plan$r_constraint
    ),
    created_by = list(
      package = "rpackit",
      version = utils::packageDescription("rpackit")[["Version"]]
    )
  )
}

#' Prepare portable desktop resources for an R application
#'
#' `prepare_desktop()` creates the versioned resource contract consumed by the
#' future rpackit Tauri shell:
#'
#' ```
#' output/
#'   resources/
#'     R/
#'     app/
#'     launcher.R
#'     rpackit.json
#' ```
#'
#' The application is inspected without execution. The supplied portable R
#' runtime is copied into the bundle, so the generated resources do not depend
#' on a system R installation at run time. By default, required packages are
#' installed into the copied runtime. A `renv.lock` uses `renv::restore()`;
#' otherwise the parsed dependency plan is installed from `repos`.
#'
#' The generated launcher accepts `--app`, `--port`, and `--token`, binds Shiny
#' only to `127.0.0.1`, and exports the token to the application as
#' `RPACKIT_SESSION_TOKEN`. Network-level token enforcement belongs to the
#' desktop shell/proxy and is deliberately recorded as not yet implemented in
#' `rpackit.json`; this function does not claim to produce a Tauri executable.
#'
#' Build inputs are never modified. Output is assembled in a sibling staging
#' directory and renamed into place only after validation. Existing output is
#' never overwritten.
#'
#' @param app_dir Path to a supported Shiny application.
#' @param runtime_dir Path to an extracted portable R home.
#' @param output_dir New output directory. Defaults to
#'   `app_dir/dist/desktop-resources`.
#' @param app_name Human-readable application name.
#' @param install_packages Install required packages into the copied runtime.
#' @param repos Repository URLs used when installing packages.
#' @param verify_runtime Execute the supplied `Rscript --version` before
#'   copying it.
#' @param quiet Suppress the completion summary.
#' @return An `rpackit_desktop_bundle` object.
#' @export
prepare_desktop <- function(
  app_dir,
  runtime_dir,
  output_dir = NULL,
  app_name = NULL,
  install_packages = TRUE,
  repos = getOption("repos"),
  verify_runtime = TRUE,
  quiet = FALSE
) {
  check <- check_app(app_dir, quiet = TRUE)
  desktop_status <- check$targets$status[
    check$targets$target == "portable desktop"
  ]
  if (!identical(desktop_status, "yes")) {
    reason <- check$targets$reason[
      check$targets$target == "portable desktop"
    ]
    cli::cli_abort(
      "Application is not ready for portable desktop resources: {reason}."
    )
  }
  app_path <- check$path
  if (is.null(app_name)) {
    app_name <- basename(app_path)
  }
  if (!is.character(app_name) || length(app_name) != 1L ||
      is.na(app_name) || !nzchar(trimws(app_name)) ||
      grepl("[\r\n]", app_name)) {
    cli::cli_abort("{.arg app_name} must be one non-empty line of text.")
  }
  for (value in list(
    install_packages = install_packages,
    verify_runtime = verify_runtime,
    quiet = quiet
  )) {
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      cli::cli_abort(
        "{.arg install_packages}, {.arg verify_runtime}, and {.arg quiet} ",
        "must each be TRUE or FALSE."
      )
    }
  }
  runtime <- .desktop_runtime(runtime_dir)
  if (isTRUE(verify_runtime)) {
    .desktop_verify_runtime(runtime)
  }
  repos <- .desktop_repositories(repos)
  output <- .desktop_output_path(output_dir, app_path)
  stage <- tempfile(".rpackit-stage-", tmpdir = dirname(output))
  if (!dir.create(stage, recursive = FALSE, showWarnings = FALSE)) {
    cli::cli_abort("Cannot create staging directory for {.path {output}}.")
  }
  completed <- FALSE
  on.exit({
    if (!completed && dir.exists(stage)) {
      unlink(stage, recursive = TRUE, force = TRUE)
    }
  }, add = TRUE)

  resources <- file.path(stage, "resources")
  dir.create(resources)
  .desktop_copy_tree(
    runtime$path,
    file.path(resources, "R")
  )
  .desktop_copy_tree(
    app_path,
    file.path(resources, "app"),
    exclude = .desktop_app_excluded
  )
  writeLines(
    .desktop_launcher_lines(),
    file.path(resources, "launcher.R"),
    useBytes = TRUE
  )
  install_result <- NULL
  if (isTRUE(install_packages)) {
    install_result <- .desktop_install_dependencies(
      resources,
      check$dependency_plan,
      repos
    )
  }
  copied_runtime <- .desktop_runtime(file.path(resources, "R"))
  manifest <- .desktop_manifest(
    app_name = app_name,
    check = check,
    runtime = copied_runtime,
    dependency_plan = check$dependency_plan,
    installed = isTRUE(install_packages),
    install_result = install_result
  )
  .desktop_write_json(
    manifest,
    file.path(resources, "rpackit.json")
  )
  validation <- validate_desktop_bundle(stage, quiet = TRUE)
  if (!file.rename(stage, output)) {
    cli::cli_abort("Cannot move completed bundle to {.path {output}}.")
  }
  completed <- TRUE
  result <- structure(
    list(
      path = normalizePath(output, winslash = "/", mustWork = TRUE),
      resources = file.path(
        normalizePath(output, winslash = "/", mustWork = TRUE),
        "resources"
      ),
      app_name = app_name,
      app_type = check$app_type,
      packages = manifest$dependencies$packages,
      dependencies_installed = isTRUE(install_packages),
      validation = validation
    ),
    class = "rpackit_desktop_bundle"
  )
  if (!isTRUE(quiet)) {
    print(result)
  }
  invisible(result)
}

.desktop_safe_manifest_path <- function(resources, value, field) {
  if (!is.character(value) || length(value) != 1L || is.na(value) ||
      !nzchar(value) || grepl("\\\\", value) ||
      startsWith(value, "/") || grepl("(^|/)\\.\\.(/|$)", value)) {
    cli::cli_abort(
      "Manifest field {.field {field}} must be a safe relative POSIX path."
    )
  }
  path <- normalizePath(
    file.path(resources, value),
    winslash = "/",
    mustWork = TRUE
  )
  if (!.rpackit_path_within(path, resources)) {
    cli::cli_abort("Manifest field {.field {field}} escapes resources.")
  }
  path
}

.desktop_verify_packages <- function(rscript, library, packages) {
  packages <- unlist(packages, use.names = FALSE)
  if (!is.character(packages) || !length(packages) || anyNA(packages) ||
      any(!grepl("^[A-Za-z][A-Za-z0-9.]*$", packages))) {
    cli::cli_abort(
      "Desktop manifest contains an invalid dependency package list."
    )
  }
  script <- tempfile("rpackit-verify-packages-")
  on.exit(unlink(script, force = TRUE), add = TRUE)
  writeLines(
    c(
      sprintf("library_path <- %s", .desktop_r_literal(library)),
      sprintf("packages <- %s", .desktop_r_literal(packages)),
      ".libPaths(unique(c(library_path, .libPaths())))",
      "missing <- setdiff(packages, rownames(installed.packages()))",
      "if (length(missing)) {",
      "  stop(",
      "    'Bundled dependencies are missing: ',",
      "    paste(missing, collapse = ', '),",
      "    call. = FALSE",
      "  )",
      "}"
    ),
    script,
    useBytes = TRUE
  )
  .desktop_run_rscript(
    rscript,
    c("--vanilla", shQuote(script)),
    "Bundled dependency verification"
  )
  invisible(packages)
}

#' Validate a prepared desktop resource bundle
#'
#' Checks the resource topology, manifest version, application layout, portable
#' runtime paths, and loopback-only launcher contract. Application code is not
#' executed.
#'
#' @param bundle_dir Prepared bundle directory containing `resources/`.
#' @param verify_runtime Execute the bundled `Rscript --version`.
#' @param quiet Suppress the validation summary.
#' @return An `rpackit_desktop_validation` object.
#' @export
validate_desktop_bundle <- function(bundle_dir, verify_runtime = FALSE,
                                    quiet = FALSE) {
  if (!is.character(bundle_dir) || length(bundle_dir) != 1L ||
      is.na(bundle_dir) || !dir.exists(bundle_dir)) {
    cli::cli_abort("{.arg bundle_dir} must be an existing directory.")
  }
  for (value in list(verify_runtime = verify_runtime, quiet = quiet)) {
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      cli::cli_abort(
        "{.arg verify_runtime} and {.arg quiet} must be TRUE or FALSE."
      )
    }
  }
  path <- normalizePath(bundle_dir, winslash = "/", mustWork = TRUE)
  resources <- file.path(path, "resources")
  required <- file.path(resources, c("R", "app", "launcher.R", "rpackit.json"))
  missing <- required[!file.exists(required) & !dir.exists(required)]
  if (length(missing)) {
    cli::cli_abort(
      "Desktop bundle is missing: {paste(basename(missing), collapse = ', ')}."
    )
  }
  manifest <- tryCatch(
    jsonlite::fromJSON(
      file.path(resources, "rpackit.json"),
      simplifyVector = FALSE
    ),
    error = function(error) {
      cli::cli_abort(
        "Cannot parse {.path rpackit.json}: {conditionMessage(error)}."
      )
    }
  )
  if (!identical(manifest$schema_version, "1") ||
      !identical(manifest$bundle_type, "rpackit-desktop-resources")) {
    cli::cli_abort("Desktop manifest uses an unsupported contract.")
  }
  if (!identical(manifest$launcher$host, "127.0.0.1")) {
    cli::cli_abort("Desktop launcher host must be 127.0.0.1.")
  }
  if (!is.logical(manifest$launcher$network_token_enforced) ||
      length(manifest$launcher$network_token_enforced) != 1L ||
      is.na(manifest$launcher$network_token_enforced)) {
    cli::cli_abort(
      "Desktop manifest must explicitly record network token enforcement."
    )
  }
  if (!is.logical(manifest$dependencies$installed) ||
      length(manifest$dependencies$installed) != 1L ||
      is.na(manifest$dependencies$installed)) {
    cli::cli_abort(
      "Desktop manifest must explicitly record dependency installation."
    )
  }
  launcher <- readLines(
    file.path(resources, "launcher.R"),
    warn = FALSE,
    encoding = "UTF-8"
  )
  launcher_text <- paste(launcher, collapse = "\n")
  if (grepl("0\\.0\\.0\\.0", launcher_text) ||
      !grepl("host\\s*=\\s*['\"]127\\.0\\.0\\.1['\"]", launcher_text)) {
    cli::cli_abort("Desktop launcher violates the loopback-only contract.")
  }
  app <- .desktop_safe_manifest_path(
    resources,
    manifest$app$path,
    "app.path"
  )
  detected_app_type <- if (file.exists(file.path(app, "app.R"))) {
    "shiny-single-file"
  } else if (all(file.exists(file.path(app, c("ui.R", "server.R"))))) {
    "shiny-split"
  } else {
    "unknown"
  }
  if (identical(detected_app_type, "unknown")) {
    cli::cli_abort("Desktop bundle does not contain a supported Shiny app.")
  }
  if (!identical(manifest$app$type, detected_app_type)) {
    cli::cli_abort("Desktop manifest app type does not match bundled files.")
  }
  rscript <- .desktop_safe_manifest_path(
    resources,
    manifest$runtime$rscript,
    "runtime.rscript"
  )
  library <- .desktop_safe_manifest_path(
    resources,
    manifest$runtime$library,
    "runtime.library"
  )
  if (!file.exists(rscript) || !dir.exists(library)) {
    cli::cli_abort("Desktop bundle contains an incomplete portable R runtime.")
  }
  runtime_path <- .desktop_safe_manifest_path(
    resources,
    manifest$runtime$path,
    "runtime.path"
  )
  runtime <- .desktop_runtime(runtime_path)
  if (!identical(manifest$runtime$platform, runtime$platform)) {
    cli::cli_abort(
      "Desktop manifest runtime platform does not match bundled files."
    )
  }
  if (isTRUE(verify_runtime)) {
    .desktop_verify_runtime(runtime)
    if (isTRUE(manifest$dependencies$installed)) {
      .desktop_verify_packages(
        rscript,
        library,
        manifest$dependencies$packages
      )
    }
  }
  result <- structure(
    list(
      valid = TRUE,
      path = path,
      app_type = manifest$app$type,
      runtime_platform = manifest$runtime$platform,
      dependencies_installed = isTRUE(manifest$dependencies$installed),
      network_token_enforced = isTRUE(
        manifest$launcher$network_token_enforced
      )
    ),
    class = "rpackit_desktop_validation"
  )
  if (!isTRUE(quiet)) {
    print(result)
  }
  invisible(result)
}

#' @export
print.rpackit_desktop_bundle <- function(x, ...) {
  cli::cli_h1("rpackit desktop resources")
  cli::cli_text("Path: {x$path}")
  cli::cli_text("App: {x$app_name} ({x$app_type})")
  cli::cli_text(
    "Dependencies installed: ",
    "{if (x$dependencies_installed) 'yes' else 'no'}"
  )
  cli::cli_text(
    "Tauri executable: not built; pass these resources to the desktop shell."
  )
  invisible(x)
}

#' @export
print.rpackit_desktop_validation <- function(x, ...) {
  cli::cli_h1("rpackit desktop bundle validation")
  cli::cli_text("Path: {x$path}")
  cli::cli_text("Contract: valid")
  cli::cli_text("Loopback host: 127.0.0.1")
  cli::cli_text(
    "Network token enforcement: ",
    "{if (x$network_token_enforced) 'yes' else 'not yet implemented'}"
  )
  invisible(x)
}
