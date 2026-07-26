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
  candidate_paths <- file.path(path, candidates)
  found <- candidates[
    file.exists(candidate_paths) & !dir.exists(candidate_paths)
  ]
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

.desktop_runtime_version <- function(runtime) {
  rscript <- file.path(runtime$path, runtime$rscript)
  script <- tempfile("rpackit-runtime-version-", fileext = ".R")
  on.exit(unlink(script, force = TRUE), add = TRUE)
  writeLines(
    "cat(as.character(getRversion()), '\\n', sep = '')",
    script,
    useBytes = TRUE
  )
  output <- .desktop_run_rscript(
    rscript,
    c("--vanilla", shQuote(script)),
    "Portable R runtime verification"
  )
  output <- trimws(output)
  versions <- output[grepl(
    "^[0-9]+\\.[0-9]+\\.[0-9]+$",
    output
  )]
  if (length(versions) != 1L) {
    cli::cli_abort(
      "Portable R runtime did not report one exact R version.",
      class = "rpackit_runtime_version_error"
    )
  }
  versions[[1L]]
}

.desktop_verify_runtime <- function(runtime) {
  .desktop_runtime_version(runtime)
  invisible(runtime)
}

.desktop_version_comparison <- function(version, operator, required) {
  version <- numeric_version(version)
  required <- numeric_version(required)
  switch(operator,
    ">=" = version >= required,
    ">" = version > required,
    "<=" = version <= required,
    "<" = version < required,
    "=" = version == required,
    "==" = version == required,
    FALSE
  )
}

.desktop_validate_runtime_plan <- function(runtime_version, plan) {
  runtime_version <- .portable_version(runtime_version, "runtime version")
  locked <- plan$locked_r_version
  if (!is.null(locked) &&
    is.character(locked) &&
    length(locked) == 1L &&
    !is.na(locked)) {
    locked <- .portable_version(locked, "renv.lock R version")
    if (!identical(runtime_version, locked)) {
      cli::cli_abort(
        c(
          "Portable R {runtime_version} does not match the renv.lock R ",
          "version {locked}.",
          "i" = "Resolve R {locked} or update the lockfile intentionally."
        ),
        class = "rpackit_runtime_version_mismatch_error"
      )
    }
  }
  constraint <- plan$r_constraint
  if (!is.null(constraint) &&
    is.character(constraint) &&
    length(constraint) == 1L &&
    !is.na(constraint)) {
    match <- regexec(
      "^\\s*(>=|<=|==|=|>|<)\\s*([0-9]+(?:\\.[0-9]+){1,2})\\s*$",
      constraint,
      perl = TRUE
    )
    pieces <- regmatches(constraint, match)[[1L]]
    if (length(pieces) != 3L) {
      cli::cli_abort(
        "Cannot interpret DESCRIPTION R constraint {.val {constraint}}.",
        class = "rpackit_runtime_version_constraint_error"
      )
    }
    if (!.desktop_version_comparison(
      runtime_version,
      pieces[[2L]],
      pieces[[3L]]
    )) {
      cli::cli_abort(
        c(
          "Portable R {runtime_version} does not satisfy DESCRIPTION ",
          "requirement R ({constraint}).",
          "i" = "Choose a compatible portable R version."
        ),
        class = "rpackit_runtime_version_mismatch_error"
      )
    }
  }
  invisible(runtime_version)
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
    "if (!requireNamespace('jsonlite', quietly = TRUE)) {",
    "  cat(paste0(",
    "    'RPACKIT_EVENT ',",
    "    '{\"protocol_version\":\"1\",\"event\":\"error\",',",
    "    '\"phase\":\"bootstrap\",',",
    "    '\"message\":\"The bundled runtime does not contain jsonlite.\"}',",
    "    '\\n'",
    "  ))",
    "  quit(save = 'no', status = 1L, runLast = FALSE)",
    "}",
    "event_prefix <- 'RPACKIT_EVENT '",
    "emit_event <- function(event, fields = list()) {",
    "  payload <- c(",
    "    list(",
    "      protocol_version = '1',",
    "      event = event,",
    "      timestamp = format(Sys.time(), tz = 'UTC', usetz = TRUE)",
    "    ),",
    "    fields",
    "  )",
    "  encoded <- jsonlite::toJSON(",
    "    payload,",
    "    auto_unbox = TRUE,",
    "    null = 'null'",
    "  )",
    "  cat(event_prefix, encoded, '\\n', sep = '')",
    "  flush.console()",
    "}",
    "launcher_error <- function(message, phase) {",
    "  stop(",
    "    structure(",
    "      list(message = message, call = NULL, phase = phase),",
    "      class = c('rpackit_launcher_error', 'error', 'condition')",
    "    )",
    "  )",
    "}",
    "main <- function() {",
    "  arguments <- commandArgs(trailingOnly = TRUE)",
    "  if (!length(arguments) || length(arguments) %% 2L != 0L) {",
    "    launcher_error(",
    "      paste0(",
    "        'Usage: launcher.R --app <path> --port <port> ',",
    "        '--token <token> [--control <path>]'",
    "      ),",
    "      'arguments'",
    "    )",
    "  }",
    "  keys <- arguments[seq.int(1L, length(arguments), by = 2L)]",
    "  values <- arguments[seq.int(2L, length(arguments), by = 2L)]",
    "  required <- c('--app', '--port', '--token')",
    "  allowed <- c(required, '--control')",
    "  if (anyDuplicated(keys) || !all(required %in% keys) ||",
    "      any(!keys %in% allowed)) {",
    "    launcher_error(",
    "      paste0(",
    "        'Exactly one --app, --port, and --token argument is required; ',",
    "        '--control may appear once.'",
    "      ),",
    "      'arguments'",
    "    )",
    "  }",
    "  options <- stats::setNames(values, keys)",
    "  app <- options[['--app']]",
    "  if (!dir.exists(app)) {",
    "    launcher_error('The application directory does not exist.', 'app')",
    "  }",
    "  app <- normalizePath(app, winslash = '/', mustWork = TRUE)",
    "  layout_ok <- file.exists(file.path(app, 'app.R')) ||",
    "    all(file.exists(file.path(app, c('ui.R', 'server.R'))))",
    "  if (!layout_ok) {",
    "    launcher_error(",
    "      'The application is not a supported Shiny layout.',",
    "      'app'",
    "    )",
    "  }",
    "  port <- suppressWarnings(as.integer(options[['--port']]))",
    "  if (is.na(port) || port < 1L || port > 65535L) {",
    "    launcher_error(",
    "      '--port must be a whole number between 1 and 65535.',",
    "      'arguments'",
    "    )",
    "  }",
    "  token <- options[['--token']]",
    "  if (is.na(token) || nchar(token, type = 'bytes') < 16L ||",
    "      nchar(token, type = 'bytes') > 256L ||",
    "      !grepl('^[A-Za-z0-9._~-]+$', token)) {",
    "    launcher_error(",
    "      '--token must contain 16 to 256 URL-safe ASCII characters.',",
    "      'arguments'",
    "    )",
    "  }",
    "  control <- options[['--control']]",
    "  if (!is.null(control)) {",
    "    if (is.na(control) || !nzchar(control) || grepl('[\\r\\n]', control)) {",
    "      launcher_error('--control must be a usable path.', 'arguments')",
    "    }",
    "    control_parent <- dirname(control)",
    "    if (!dir.exists(control_parent)) {",
    "      launcher_error(",
    "        'The --control parent directory does not exist.',",
    "        'arguments'",
    "      )",
    "    }",
    "    control <- file.path(",
    "      normalizePath(control_parent, winslash = '/', mustWork = TRUE),",
    "      basename(control)",
    "    )",
    "    if (file.exists(control) || dir.exists(control)) {",
    "      launcher_error(",
    "        'The --control path must not exist at startup.',",
    "        'arguments'",
    "      )",
    "    }",
    "  }",
    "  runtime_library <- file.path(R.home(), 'library')",
    "  if (!dir.exists(runtime_library)) {",
    "    launcher_error(",
    "      'The bundled R library directory is missing.',",
    "      'runtime'",
    "    )",
    "  }",
    "  .libPaths(unique(c(runtime_library, .libPaths())))",
    "  if (!requireNamespace('shiny', quietly = TRUE)) {",
    "    launcher_error(",
    "      \"The bundled runtime does not contain the 'shiny' package.\",",
    "      'runtime'",
    "    )",
    "  }",
    "  if (!is.null(control) &&",
    "      !requireNamespace('later', quietly = TRUE)) {",
    "    launcher_error(",
    "      \"The bundled runtime does not contain the 'later' package.\",",
    "      'runtime'",
    "    )",
    "  }",
    "  Sys.setenv(RPACKIT_SESSION_TOKEN = token)",
    "  if (!is.null(control)) {",
    "    watch_control <- NULL",
    "    watch_control <- function() {",
    "      if (file.exists(control)) {",
    "        emit_event('stopping', list(reason = 'control-file'))",
    "        shiny::stopApp()",
    "      } else {",
    "        later::later(watch_control, delay = 0.1)",
    "      }",
    "      invisible(NULL)",
    "    }",
    "    later::later(watch_control, delay = 0.1)",
    "  }",
    "  emit_event(",
    "    'starting',",
    "    list(",
    "      pid = Sys.getpid(),",
    "      host = '127.0.0.1',",
    "      port = port,",
    "      token_enforced = FALSE,",
    "      graceful_stop = !is.null(control)",
    "    )",
    "  )",
    "  shiny::runApp(",
    "    app,",
    "    host = '127.0.0.1',",
    "    port = port,",
    "    launch.browser = FALSE,",
    "    quiet = TRUE",
    "  )",
    "  emit_event('stopped', list(pid = Sys.getpid()))",
    "  quit(save = 'no', status = 0L, runLast = FALSE)",
    "}",
    "tryCatch(",
    "  main(),",
    "  error = function(error) {",
    "    phase <- if (inherits(error, 'rpackit_launcher_error')) {",
    "      error$phase",
    "    } else {",
    "      'runtime'",
    "    }",
    "    emit_event(",
    "      'error',",
    "      list(",
    "        phase = phase,",
    "        message = conditionMessage(error),",
    "        pid = Sys.getpid()",
    "      )",
    "    )",
    "    message('rpackit launcher error: ', conditionMessage(error))",
    "    quit(save = 'no', status = 1L, runLast = FALSE)",
    "  }",
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

.desktop_launcher_packages <- c("jsonlite", "later", "shiny")

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
    .desktop_launcher_packages,
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
                              installed, install_result, runtime_version,
                              runtime_provenance = NULL) {
  packages <- sort(unique(c(
    .desktop_launcher_packages,
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
    runtime = c(list(
      path = "R",
      rscript = paste0("R/", gsub("\\\\", "/", runtime$rscript)),
      library = paste0("R/", gsub("\\\\", "/", runtime$library)),
      platform = runtime$platform,
      r_version = if (is.na(runtime_version)) NULL else runtime_version,
      source = if (is.null(runtime_provenance)) "explicit" else "registry"
    ), if (is.null(runtime_provenance)) {
      list()
    } else {
      list(provenance = list(
        registry = runtime_provenance$registry,
        metadata_source = runtime_provenance$metadata_source,
        artifact_url = runtime_provenance$artifact_url,
        sha256 = runtime_provenance$sha256,
        archive_format = runtime_provenance$archive_format,
        cache_hit = runtime_provenance$cache_hit
      ))
    }),
    launcher = list(
      script = "launcher.R",
      host = "127.0.0.1",
      port = "required-argument",
      token = "required-argument",
      control = "optional-argument",
      protocol_version = "1",
      event_stream = list(
        format = "ndjson",
        destination = "stdout",
        prefix = "RPACKIT_EVENT "
      ),
      network_token_enforced = FALSE,
      readiness = list(
        strategy = "http-poll",
        starting_event = "starting"
      )
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
#' When `runtime_dir = NULL`, a verified runtime is resolved from the
#' portable-R registry and reused from a SHA-256-keyed user cache when
#' available. The lockfile R version and DESCRIPTION R constraint are checked
#' against the selected runtime before copying it or installing packages.
#'
#' The generated launcher accepts `--app`, `--port`, and `--token`, plus an
#' optional private `--control` path used for graceful shutdown. It binds Shiny
#' only to `127.0.0.1`, exports the token to the application as
#' `RPACKIT_SESSION_TOKEN`, and emits versioned lifecycle events. Network-level
#' token enforcement belongs to the desktop shell/proxy and is deliberately
#' recorded as not yet implemented in `rpackit.json`; this function does not
#' claim to produce a Tauri executable.
#'
#' Build inputs are never modified. Output is assembled in a sibling staging
#' directory and renamed into place only after validation. Existing output is
#' never overwritten.
#'
#' @param app_dir Path to a supported Shiny application.
#' @param runtime_dir Path to an extracted portable R home, or `NULL` to
#'   resolve a verified runtime for the current platform and architecture.
#' @param output_dir New output directory. Defaults to
#'   `app_dir/dist/desktop-resources`.
#' @param app_name Human-readable application name.
#' @param install_packages Install required packages into the copied runtime.
#' @param repos Repository URLs used when installing packages.
#' @param verify_runtime Execute the supplied `Rscript` and read its exact R
#'   version before copying it. An explicit runtime is still probed when
#'   `renv.lock` or DESCRIPTION constrains R, even when this is `FALSE`, because
#'   compatibility must be established before copying or installation.
#' @param quiet Suppress the completion summary.
#' @param r_version Exact portable R version used for automatic resolution.
#'   Defaults to the version recorded in `renv.lock`, when present, or the
#'   newest verified version.
#' @param registry HTTPS URL or local path to a portable-R schema-v1 registry.
#' @param cache_dir Portable runtime cache directory.
#' @param offline Reuse an existing same-registry runtime cache entry without
#'   reading any registry or artifact.
#' @return An `rpackit_desktop_bundle` object. Its `runtime` field records the
#'   explicit runtime path or the verified registry selection and provenance.
#' @export
prepare_desktop <- function(
  app_dir,
  runtime_dir = NULL,
  output_dir = NULL,
  app_name = NULL,
  install_packages = TRUE,
  repos = getOption("repos"),
  verify_runtime = TRUE,
  quiet = FALSE,
  r_version = NULL,
  registry = getOption(
    "rpackit.runtime_registry",
    .rpackit_runtime_registry
  ),
  cache_dir = NULL,
  offline = FALSE
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
    quiet = quiet,
    offline = offline
  )) {
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      cli::cli_abort(
        "{.arg install_packages}, {.arg verify_runtime}, {.arg quiet}, and ",
        "{.arg offline} must each be TRUE or FALSE."
      )
    }
  }
  repos <- .desktop_repositories(repos)
  output <- .desktop_output_path(output_dir, app_path)
  resolved_runtime <- NULL
  if (is.null(runtime_dir)) {
    requested_version <- r_version
    if (is.null(requested_version) &&
      !is.null(check$dependency_plan$locked_r_version) &&
      is.character(check$dependency_plan$locked_r_version) &&
      length(check$dependency_plan$locked_r_version) == 1L &&
      !is.na(check$dependency_plan$locked_r_version)) {
      requested_version <- check$dependency_plan$locked_r_version
    }
    if (!is.null(requested_version)) {
      .desktop_validate_runtime_plan(
        requested_version,
        check$dependency_plan
      )
    }
    resolved_runtime <- resolve_portable_runtime(
      r_version = requested_version,
      registry = registry,
      cache_dir = cache_dir,
      offline = offline,
      quiet = quiet
    )
    runtime_dir <- resolved_runtime$path
  } else if (!is.null(r_version)) {
    cli::cli_abort(
      "{.arg r_version} can only be used when {.arg runtime_dir} is NULL."
    )
  }
  runtime <- .desktop_runtime(runtime_dir)
  has_version_requirement <- (
    !is.null(check$dependency_plan$locked_r_version) &&
      !is.na(check$dependency_plan$locked_r_version)
  ) || (
    !is.null(check$dependency_plan$r_constraint) &&
      !is.na(check$dependency_plan$r_constraint)
  )
  observed_runtime_version <- if (isTRUE(verify_runtime) ||
    (is.null(resolved_runtime) &&
      has_version_requirement)) {
    .desktop_runtime_version(runtime)
  } else if (!is.null(resolved_runtime)) {
    resolved_runtime$r_version
  } else {
    NA_character_
  }
  if (!is.null(resolved_runtime) &&
    !is.na(observed_runtime_version) &&
    !identical(
      observed_runtime_version,
      resolved_runtime$r_version
    )) {
    cli::cli_abort(
      c(
        "Verified runtime metadata records R {resolved_runtime$r_version}, ",
        "but the extracted runtime reports R {observed_runtime_version}.",
        "i" = "The cached runtime must not be used."
      ),
      class = "rpackit_runtime_version_mismatch_error"
    )
  }
  if (has_version_requirement) {
    .desktop_validate_runtime_plan(
      observed_runtime_version,
      check$dependency_plan
    )
  }
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
    install_result = install_result,
    runtime_version = observed_runtime_version,
    runtime_provenance = resolved_runtime
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
  normalized_output <- normalizePath(
    output,
    winslash = "/",
    mustWork = TRUE
  )
  validation$path <- normalized_output
  result <- structure(
    list(
      path = normalized_output,
      resources = file.path(
        normalized_output,
        "resources"
      ),
      app_name = app_name,
      app_type = check$app_type,
      packages = manifest$dependencies$packages,
      dependencies_installed = isTRUE(install_packages),
      validation = validation,
      runtime = if (is.null(resolved_runtime)) {
        list(
          source = "explicit",
          path = runtime$path,
          r_version = observed_runtime_version,
          platform = runtime$platform
        )
      } else {
        resolved_runtime
      }
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

.desktop_validate_lifecycle_contract <- function(manifest) {
  if (!identical(manifest$launcher$protocol_version, "1") ||
      !identical(manifest$launcher$control, "optional-argument") ||
      !identical(manifest$launcher$event_stream$format, "ndjson") ||
      !identical(
        manifest$launcher$event_stream$destination,
        "stdout"
      ) ||
      !identical(
        manifest$launcher$event_stream$prefix,
        "RPACKIT_EVENT "
      ) ||
      !identical(
        manifest$launcher$readiness$strategy,
        "http-poll"
      ) ||
      !identical(
        manifest$launcher$readiness$starting_event,
        "starting"
      )) {
    cli::cli_abort(
      "Desktop manifest does not provide lifecycle protocol version 1."
    )
  }
  invisible(manifest)
}

#' Validate a prepared desktop resource bundle
#'
#' Checks the resource topology, manifest version, application layout, portable
#' runtime paths, and loopback-only launcher contract. Application code is not
#' executed.
#'
#' @param bundle_dir Prepared bundle directory containing `resources/`.
#' @param verify_runtime Execute the bundled `Rscript`, read `getRversion()`,
#'   and require it to match the version recorded in the manifest when present.
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
  .desktop_validate_lifecycle_contract(manifest)
  if (!is.logical(manifest$launcher$network_token_enforced) ||
      length(manifest$launcher$network_token_enforced) != 1L ||
      is.na(manifest$launcher$network_token_enforced)) {
    cli::cli_abort(
      "Desktop manifest must explicitly record network token enforcement."
    )
  }
  if (isTRUE(manifest$launcher$network_token_enforced)) {
    cli::cli_abort(
      "This resource-stage launcher does not enforce network tokens; ",
      "{.field launcher.network_token_enforced} must be false."
    )
  }
  if (!is.logical(manifest$dependencies$installed) ||
      length(manifest$dependencies$installed) != 1L ||
      is.na(manifest$dependencies$installed)) {
    cli::cli_abort(
      "Desktop manifest must explicitly record dependency installation."
    )
  }
  launcher_path <- .desktop_safe_manifest_path(
    resources,
    manifest$launcher$script,
    "launcher.script"
  )
  launcher <- readLines(
    launcher_path,
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
  if (!file.exists(rscript) || dir.exists(rscript) || !dir.exists(library)) {
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
  runtime_source <- manifest$runtime$source
  if (is.null(runtime_source)) {
    runtime_source <- "explicit"
  }
  if (!identical(runtime_source, "explicit") &&
    !identical(runtime_source, "registry")) {
    cli::cli_abort(
      "Desktop manifest must identify an explicit or registry runtime source."
    )
  }
  if (!is.null(manifest$runtime$r_version)) {
    .portable_version(
      manifest$runtime$r_version,
      "runtime.r_version"
    )
  }
  if (identical(runtime_source, "registry")) {
    if (is.null(manifest$runtime$r_version)) {
      cli::cli_abort(
        "A registry runtime manifest must record runtime.r_version."
      )
    }
    provenance <- manifest$runtime$provenance
    required_provenance <- c(
      "registry", "metadata_source", "artifact_url", "sha256",
      "archive_format", "cache_hit"
    )
    safe_provenance <- if (is.list(provenance)) {
      tryCatch(
        {
          .portable_validate_source_reference(
            provenance$registry,
            "runtime.provenance.registry"
          )
          .portable_validate_source_reference(
            provenance$metadata_source,
            "runtime.provenance.metadata_source"
          )
          .portable_validate_source_reference(
            provenance$artifact_url,
            "runtime.provenance.artifact_url"
          )
          !.portable_is_https(provenance$registry) ||
            (
              .portable_is_https(provenance$metadata_source) &&
                .portable_is_https(provenance$artifact_url)
            )
        },
        error = function(error) FALSE
      )
    } else {
      FALSE
    }
    if (!is.list(provenance) ||
      any(vapply(required_provenance[-length(required_provenance)], function(field) {
        !is.character(provenance[[field]]) ||
          length(provenance[[field]]) != 1L ||
          is.na(provenance[[field]]) ||
          !nzchar(provenance[[field]])
      }, logical(1))) ||
      !is.logical(provenance$cache_hit) ||
      length(provenance$cache_hit) != 1L ||
      is.na(provenance$cache_hit) ||
      !grepl("^[a-f0-9]{64}$", provenance$sha256) ||
      !identical(provenance$archive_format, "zip") ||
      !isTRUE(safe_provenance)) {
      cli::cli_abort(
        "Desktop manifest contains invalid registry runtime provenance."
      )
    }
  }
  if (isTRUE(verify_runtime)) {
    observed_runtime_version <- .desktop_runtime_version(runtime)
    if (!is.null(manifest$runtime$r_version) &&
      !identical(
        observed_runtime_version,
        manifest$runtime$r_version
      )) {
      cli::cli_abort(
        c(
          "Bundled R reports version {observed_runtime_version}, but the ",
          "desktop manifest records {manifest$runtime$r_version}."
        ),
        class = "rpackit_runtime_version_mismatch_error"
      )
    }
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
      ),
      runtime_version = manifest$runtime$r_version,
      runtime_source = runtime_source,
      runtime_provenance = manifest$runtime$provenance
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
  if (inherits(x$runtime, "rpackit_portable_runtime")) {
    cli::cli_text(
      "Runtime: R {x$runtime$r_version} ",
      "({x$runtime$platform}/{x$runtime$arch}, verified registry artifact)"
    )
  } else {
    cli::cli_text("Runtime: explicit path")
  }
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
