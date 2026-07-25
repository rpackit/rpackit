.desktop_scalar_timeout <- function(value, argument) {
  if (!is.numeric(value) || length(value) != 1L || is.na(value) ||
      !is.finite(value) || value < 0) {
    cli::cli_abort(
      "{.arg {argument}} must be one finite, non-negative number."
    )
  }
  as.numeric(value)
}

.desktop_scalar_flag <- function(value, argument) {
  if (!is.logical(value) || length(value) != 1L || is.na(value)) {
    cli::cli_abort("{.arg {argument}} must be TRUE or FALSE.")
  }
  value
}

.desktop_nonce_state <- new.env(parent = emptyenv())
.desktop_nonce_state$counter <- 0

.desktop_next_nonce <- function() {
  .desktop_nonce_state$counter <- .desktop_nonce_state$counter + 1
  .desktop_nonce_state$counter
}

.desktop_preserve_rng <- function(code) {
  seed_existed <- exists(
    ".Random.seed",
    envir = .GlobalEnv,
    inherits = FALSE
  )
  if (seed_existed) {
    seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  }
  on.exit({
    if (seed_existed) {
      assign(".Random.seed", seed, envir = .GlobalEnv)
    } else if (exists(
      ".Random.seed",
      envir = .GlobalEnv,
      inherits = FALSE
    )) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  force(code)
}

.desktop_private_seed <- function(nonce) {
  value <- (
    floor(as.numeric(Sys.time()) * 1e6) +
      as.numeric(Sys.getpid()) * 104729 +
      nonce * 13007
  ) %% 2147483646
  as.integer(value) + 1L
}

.desktop_private_sample <- function(values, size, replace, nonce) {
  .desktop_preserve_rng({
    set.seed(.desktop_private_seed(nonce))
    sample(values, size = size, replace = replace)
  })
}

.desktop_launch_port <- function(port) {
  if (is.null(port)) {
    nonce <- .desktop_next_nonce()
    candidates <- .desktop_private_sample(
      49152:65535,
      size = 100L,
      replace = FALSE,
      nonce = nonce
    )
    for (candidate in candidates) {
      if (!.desktop_port_responding(candidate)) {
        return(as.integer(candidate))
      }
    }
    cli::cli_abort(
      "Could not find an unused loopback port for the desktop app."
    )
  }
  integer_port <- suppressWarnings(as.integer(port))
  if (!is.numeric(port) || length(port) != 1L || is.na(port) ||
      !is.finite(port) || is.na(integer_port) || port != integer_port ||
      port < 1L || port > 65535L) {
    cli::cli_abort(
      "{.arg port} must be NULL or one whole number from 1 to 65535."
    )
  }
  port <- integer_port
  if (.desktop_port_responding(port)) {
    cli::cli_abort("Loopback port {port} is already accepting connections.")
  }
  port
}

.desktop_port_responding <- function(port, timeout = 0.1) {
  connection <- suppressWarnings(tryCatch(
    socketConnection(
      host = "127.0.0.1",
      port = port,
      server = FALSE,
      blocking = TRUE,
      open = "r+b",
      timeout = max(1L, as.integer(ceiling(timeout)))
    ),
    error = function(error) NULL
  ))
  if (is.null(connection)) {
    return(FALSE)
  }
  close(connection)
  TRUE
}

.desktop_session_token <- function(token) {
  if (is.null(token)) {
    nonce <- .desktop_next_nonce()
    bytes <- .desktop_private_sample(
      0:255,
      size = 16L,
      replace = TRUE,
      nonce = nonce
    )
    timestamp <- gsub(
      "[^0-9]",
      "",
      format(Sys.time(), "%Y%m%d%H%M%OS6", tz = "UTC")
    )
    nonce_text <- format(
      nonce,
      scientific = FALSE,
      trim = TRUE
    )
    return(paste0(
      "rp-",
      Sys.getpid(),
      "-",
      timestamp,
      "-",
      nonce_text,
      "-",
      paste(sprintf("%02x", bytes), collapse = "")
    ))
  }
  if (!is.character(token) || length(token) != 1L || is.na(token) ||
      nchar(token, type = "bytes") < 16L ||
      nchar(token, type = "bytes") > 256L ||
      !grepl("^[A-Za-z0-9._~-]+$", token)) {
    cli::cli_abort(
      "{.arg token} must be NULL or 16 to 256 URL-safe ASCII characters."
    )
  }
  token
}

.desktop_launch_spec <- function(bundle_dir) {
  validation <- validate_desktop_bundle(bundle_dir, quiet = TRUE)
  resources <- file.path(validation$path, "resources")
  manifest <- jsonlite::fromJSON(
    file.path(resources, "rpackit.json"),
    simplifyVector = FALSE
  )
  .desktop_validate_lifecycle_contract(manifest)
  list(
    bundle = validation$path,
    resources = resources,
    rscript = .desktop_safe_manifest_path(
      resources,
      manifest$runtime$rscript,
      "runtime.rscript"
    ),
    library = .desktop_safe_manifest_path(
      resources,
      manifest$runtime$library,
      "runtime.library"
    ),
    app = .desktop_safe_manifest_path(
      resources,
      manifest$app$path,
      "app.path"
    ),
    launcher = .desktop_safe_manifest_path(
      resources,
      manifest$launcher$script,
      "launcher.script"
    ),
    manifest = manifest
  )
}

.desktop_log_lines <- function(path, lines = 200L) {
  if (!file.exists(path)) {
    return(character())
  }
  output <- tryCatch(
    readLines(path, warn = FALSE, encoding = "UTF-8"),
    error = function(error) character()
  )
  utils::tail(output, lines)
}

.desktop_redact_text <- function(value, token) {
  if (!is.character(value) || !is.character(token) ||
      length(token) != 1L || is.na(token) || !nzchar(token)) {
    return(value)
  }
  patterns <- unique(c(
    token,
    utils::URLencode(token, reserved = TRUE)
  ))
  for (pattern in patterns[nzchar(patterns)]) {
    value <- gsub(
      pattern,
      "<redacted-session-token>",
      value,
      fixed = TRUE
    )
  }
  value
}

.desktop_redact_value <- function(value, token) {
  if (is.character(value)) {
    return(.desktop_redact_text(value, token))
  }
  if (is.list(value)) {
    redacted <- lapply(value, .desktop_redact_value, token = token)
    value_names <- names(value)
    if (!is.null(value_names)) {
      names(redacted) <- .desktop_redact_text(value_names, token)
    }
    return(redacted)
  }
  value
}

.desktop_log_events <- function(path, lines = 500L) {
  if (!file.exists(path)) {
    return(list())
  }
  output <- tryCatch(
    readLines(path, warn = FALSE, encoding = "UTF-8"),
    error = function(error) character()
  )
  prefix <- "RPACKIT_EVENT "
  encoded <- substring(
    output[startsWith(output, prefix)],
    nchar(prefix) + 1L
  )
  events <- lapply(encoded, function(value) {
    tryCatch(
      jsonlite::fromJSON(value, simplifyVector = TRUE),
      error = function(error) NULL
    )
  })
  events <- Filter(
    function(event) {
      is.list(event) &&
        identical(event$protocol_version, "1") &&
        is.character(event$event) &&
        length(event$event) == 1L
    },
    events
  )
  if (is.null(lines)) {
    return(events)
  }
  utils::tail(events, lines)
}

.desktop_latest_event <- function(events, event) {
  matches <- vapply(
    events,
    function(value) identical(value$event, event),
    logical(1)
  )
  if (!any(matches)) {
    return(NULL)
  }
  events[[which(matches)[[sum(matches)]]]]
}

.desktop_event_pid <- function(pid) {
  if (!is.numeric(pid) || length(pid) != 1L || is.na(pid) ||
      !is.finite(pid) || pid < 1 || pid > .Machine$integer.max ||
      pid != as.integer(pid)) {
    return(NA_integer_)
  }
  as.integer(pid)
}

.desktop_matching_starting_event <- function(events, handle) {
  candidates <- Filter(
    function(event) identical(event$event, "starting"),
    events
  )
  for (event in candidates) {
    runtime_pid <- .desktop_event_pid(event$pid)
    event_port <- .desktop_event_pid(event$port)
    if (!is.na(runtime_pid) &&
        !is.na(event_port) &&
        event_port <= 65535L &&
        identical(event_port, handle$port) &&
        identical(event$host, "127.0.0.1") &&
        identical(event$token_enforced, FALSE)) {
      return(list(
        event = event,
        runtime_pid = runtime_pid,
        port = event_port
      ))
    }
  }
  NULL
}

.desktop_process_exit_status <- function(process) {
  if (is.null(process) || isTRUE(process$is_alive())) {
    return(NA_integer_)
  }
  status <- process$get_exit_status()
  if (is.null(status)) NA_integer_ else as.integer(status)
}

.desktop_runtime_process <- function(pid) {
  process <- tryCatch(
    ps::ps_handle(pid),
    error = function(error) NULL
  )
  if (is.null(process)) {
    return(NULL)
  }
  running <- tryCatch(
    ps::ps_is_running(process),
    error = function(error) FALSE
  )
  if (isTRUE(running)) process else NULL
}

.desktop_runtime_alive <- function(handle) {
  if (is.null(handle$runtime_process)) {
    return(NA)
  }
  tryCatch(
    isTRUE(ps::ps_is_running(handle$runtime_process)),
    error = function(error) NA
  )
}

.desktop_managed_alive <- function(handle) {
  wrapper_alive <- !is.null(handle$process) &&
    isTRUE(handle$process$is_alive())
  runtime_alive <- .desktop_runtime_alive(handle)
  runtime_expected <- !is.null(handle$runtime_pid) &&
    length(handle$runtime_pid) == 1L &&
    !is.na(handle$runtime_pid)
  runtime_unknown <- runtime_expected && is.na(runtime_alive)
  wrapper_alive || isTRUE(runtime_alive) || runtime_unknown
}

.desktop_runtime_cleanup_confirmed <- function(handle) {
  if (is.null(handle$runtime_process)) {
    return(NA)
  }
  identical(.desktop_runtime_alive(handle), FALSE)
}

.desktop_cleanup_confirmed <- function(handle) {
  wrapper_stopped <- is.null(handle$process) ||
    !isTRUE(handle$process$is_alive())
  runtime_expected <- !is.null(handle$runtime_pid) &&
    length(handle$runtime_pid) == 1L &&
    !is.na(handle$runtime_pid)
  if (!runtime_expected) {
    return(wrapper_stopped)
  }
  if (is.null(handle$runtime_process)) {
    return(FALSE)
  }
  wrapper_stopped && isTRUE(.desktop_runtime_cleanup_confirmed(handle))
}

.desktop_cache_process_output <- function(handle) {
  handle$stdout_cache <- .desktop_redact_text(
    .desktop_log_lines(handle$stdout_path),
    handle$token
  )
  handle$stderr_cache <- .desktop_redact_text(
    .desktop_log_lines(handle$stderr_path),
    handle$token
  )
  handle$events_cache <- .desktop_redact_value(
    .desktop_log_events(handle$stdout_path),
    handle$token
  )
  invisible(handle)
}

.desktop_remove_session <- function(handle) {
  if (isTRUE(handle$cleaned) ||
      !.desktop_cleanup_confirmed(handle)) {
    return(invisible(handle))
  }
  .desktop_cache_process_output(handle)
  if (dir.exists(handle$session_dir)) {
    unlink(handle$session_dir, recursive = TRUE, force = TRUE)
  }
  handle$cleaned <- !dir.exists(handle$session_dir)
  invisible(handle)
}

.desktop_terminate_process <- function(handle) {
  if (!is.null(handle$process) && isTRUE(handle$process$is_alive())) {
    try(handle$process$kill_tree(grace = 0.1), silent = TRUE)
    try(handle$process$wait(1000L), silent = TRUE)
  }
  if (!is.null(handle$process) && isTRUE(handle$process$is_alive())) {
    try(handle$process$kill(grace = 0), silent = TRUE)
    try(handle$process$wait(1000L), silent = TRUE)
  }
  deadline <- unclass(Sys.time()) + 1
  while (!.desktop_cleanup_confirmed(handle) &&
         unclass(Sys.time()) < deadline) {
    Sys.sleep(0.02)
  }
  invisible(.desktop_cleanup_confirmed(handle))
}

.desktop_process_finalizer <- function(handle) {
  if (!isTRUE(handle$cleaned)) {
    .desktop_terminate_process(handle)
    .desktop_remove_session(handle)
  }
  invisible(NULL)
}

.desktop_start_abort <- function(message, phase, handle = NULL,
                                 parent = NULL, token = NULL) {
  pid <- NA_integer_
  exit_status <- NA_integer_
  stdout <- character()
  stderr <- character()
  events <- list()
  bundle <- NULL
  cleanup_confirmed <- NA
  process_handle <- NULL
  if (!is.null(handle)) {
    token <- handle$token
    pid <- handle$pid
    bundle <- handle$bundle
    process_stopped <- .desktop_terminate_process(handle)
    exit_status <- .desktop_process_exit_status(handle$process)
    .desktop_cache_process_output(handle)
    stdout <- handle$stdout_cache
    stderr <- handle$stderr_cache
    events <- handle$events_cache
    handle$state <- "failed"
    handle$exit_status <- exit_status
    handle$stopped_at <- Sys.time()
    if (isTRUE(process_stopped)) {
      .desktop_remove_session(handle)
    } else {
      handle$token <- NULL
      handle$launch_url <- NULL
      process_handle <- handle
    }
    cleanup_confirmed <- isTRUE(process_stopped) &&
      !dir.exists(handle$session_dir)
  }
  if (!is.character(phase) || length(phase) != 1L || is.na(phase)) {
    phase <- "unknown"
  }
  phase <- .desktop_redact_text(phase, token)
  message <- .desktop_redact_text(message, token)
  if (!is.null(parent) && !is.null(token)) {
    parent <- simpleError(
      .desktop_redact_text(conditionMessage(parent), token),
      call = NULL
    )
  }
  cleanup_note <- if (is.null(handle)) {
    "No managed child process was started."
  } else if (isTRUE(cleanup_confirmed)) {
    paste0(
      "The process tracked by processx and any captured launcher runtime ",
      "were confirmed stopped, and private lifecycle files were removed. ",
      "Other descendant membership and termination are not independently ",
      "verified."
    )
  } else {
    paste0(
      "Tracked-process cleanup could not be confirmed; the condition's ",
      "`process_handle` field can be passed to `stop_desktop_app()`."
    )
  }
  cli::cli_abort(
    c(
      "Desktop app failed during {.emph {phase}}: {message}",
      "i" = "{cleanup_note}"
    ),
    class = c(
      "rpackit_desktop_start_error",
      "rpackit_desktop_error"
    ),
    phase = phase,
    bundle = bundle,
    pid = pid,
    exit_status = exit_status,
    stdout = stdout,
    stderr = stderr,
    events = events,
    cleanup_confirmed = cleanup_confirmed,
    cleanup_scope = "tracked-process-observed-runtime-and-session",
    runtime_cleanup_confirmed = if (is.null(handle)) {
      NA
    } else {
      .desktop_runtime_cleanup_confirmed(handle)
    },
    descendant_cleanup_confirmed = NA,
    process_handle = process_handle,
    parent = parent
  )
}

.desktop_http_ready <- function(port, token, timeout = 0.2) {
  connection <- suppressWarnings(tryCatch(
    socketConnection(
      host = "127.0.0.1",
      port = port,
      server = FALSE,
      blocking = TRUE,
      open = "r+b",
      timeout = max(1L, as.integer(ceiling(timeout)))
    ),
    error = function(error) NULL
  ))
  if (is.null(connection)) {
    return(FALSE)
  }
  on.exit(close(connection), add = TRUE)
  request <- paste0(
    "GET /?rpackit_token=",
    utils::URLencode(token, reserved = TRUE),
    " HTTP/1.1\r\n",
    "Host: 127.0.0.1:",
    port,
    "\r\nConnection: close\r\n\r\n"
  )
  response <- tryCatch({
    writeBin(charToRaw(request), connection)
    flush(connection)
    readLines(connection, n = 1L, warn = FALSE)
  }, error = function(error) character())
  length(response) >= 1L &&
    grepl("^HTTP/[0-9.]+ [23][0-9]{2}\\b", response[[1L]])
}

.desktop_wait_for_ready <- function(handle, timeout) {
  deadline <- unclass(Sys.time()) + timeout
  repeat {
    events <- .desktop_log_events(handle$stdout_path, lines = NULL)
    starting <- .desktop_matching_starting_event(events, handle)
    runtime_capture_failed <- FALSE
    if (!is.null(starting)) {
      handle$runtime_pid <- starting$runtime_pid
      if (is.null(handle$runtime_process)) {
        handle$runtime_process <- .desktop_runtime_process(
          starting$runtime_pid
        )
      }
      runtime_capture_failed <- is.null(handle$runtime_process)
    }
    error_event <- .desktop_latest_event(events, "error")
    if (!is.null(error_event)) {
      event_phase <- if (is.character(error_event$phase) &&
                         length(error_event$phase) == 1L) {
        error_event$phase
      } else {
        "launcher"
      }
      event_message <- if (is.character(error_event$message) &&
                           length(error_event$message) == 1L) {
        error_event$message
      } else {
        "The launcher reported an unspecified error."
      }
      .desktop_start_abort(
        event_message,
        event_phase,
        handle = handle
      )
    }
    if (runtime_capture_failed) {
      .desktop_start_abort(
        paste0(
          "Could not capture a create-time-aware handle for launcher PID ",
          starting$runtime_pid,
          "."
        ),
        "readiness",
        handle = handle
      )
    }
    if (!isTRUE(handle$process$is_alive())) {
      handle$process$wait(200L)
      events <- .desktop_log_events(handle$stdout_path, lines = NULL)
      error_event <- .desktop_latest_event(events, "error")
      if (!is.null(error_event) &&
          is.character(error_event$message) &&
          length(error_event$message) == 1L) {
        phase <- if (is.character(error_event$phase) &&
                     length(error_event$phase) == 1L) {
          error_event$phase
        } else {
          "launcher"
        }
        .desktop_start_abort(
          error_event$message,
          phase,
          handle = handle
        )
      }
      .desktop_start_abort(
        "The launcher exited before its HTTP endpoint became ready.",
        "readiness",
        handle = handle
      )
    }
    if (!is.null(starting) &&
        .desktop_http_ready(handle$port, handle$token)) {
      handle$state <- "ready"
      handle$ready_at <- Sys.time()
      handle$events_cache <- events
      return(invisible(handle))
    }
    if (unclass(Sys.time()) >= deadline) {
      .desktop_start_abort(
        paste0(
          "Timed out after ",
          format(timeout, trim = TRUE),
          " seconds while waiting for a matching launcher event and HTTP."
        ),
        "readiness",
        handle = handle
      )
    }
    Sys.sleep(0.05)
  }
}

.desktop_new_process <- function(spec, port, token) {
  session <- tempfile("rpackit-desktop-session-")
  if (!dir.create(
    session,
    recursive = FALSE,
    showWarnings = FALSE,
    mode = "0700"
  )) {
    .desktop_start_abort(
      "Could not create the private lifecycle directory.",
      "bootstrap"
    )
  }
  if (.Platform$OS.type != "windows") {
    permission_set <- suppressWarnings(Sys.chmod(
      session,
      mode = "0700",
      use_umask = FALSE
    ))
    permission_mode <- suppressWarnings(as.integer(
      file.info(session)$mode
    ))
    private_mode <- length(permission_mode) == 1L &&
      !is.na(permission_mode) &&
      bitwAnd(permission_mode, as.integer(as.octmode("0077"))) == 0L
    if (!isTRUE(permission_set) || !private_mode) {
      unlink(session, recursive = TRUE, force = TRUE)
      .desktop_start_abort(
        "Could not restrict the lifecycle directory to its owner.",
        "bootstrap"
      )
    }
  }
  stdout <- file.path(session, "stdout.log")
  stderr <- file.path(session, "stderr.log")
  control <- file.path(session, "stop")
  child_environment <- Sys.getenv()
  child_environment <- child_environment[
    !toupper(names(child_environment)) %in% c(
      "R_ARCH",
      "R_DOC_DIR",
      "R_ENVIRON",
      "R_ENVIRON_USER",
      "R_HOME",
      "R_INCLUDE_DIR",
      "R_LIBS",
      "R_LIBS_SITE",
      "R_LIBS_USER",
      "R_PROFILE",
      "R_PROFILE_USER",
      "R_SHARE_DIR",
      "RPACKIT_LAUNCH_PROTOCOL"
    )
  ]
  runtime_home <- dirname(dirname(spec$rscript))
  child_environment[c(
    "R_HOME",
    "R_LIBS",
    "R_LIBS_SITE",
    "R_LIBS_USER",
    "RPACKIT_LAUNCH_PROTOCOL"
  )] <- c(
    runtime_home,
    spec$library,
    spec$library,
    spec$library,
    "1"
  )
  path_name <- names(child_environment)[
    tolower(names(child_environment)) == "path"
  ][1L]
  if (!is.na(path_name)) {
    child_environment[[path_name]] <- paste(
      dirname(spec$rscript),
      child_environment[[path_name]],
      sep = .Platform$path.sep
    )
  }
  process <- tryCatch(
    processx::process$new(
      command = spec$rscript,
      args = c(
        "--vanilla",
        spec$launcher,
        "--app", spec$app,
        "--port", as.character(port),
        "--token", token,
        "--control", control
      ),
      stdout = stdout,
      stderr = stderr,
      env = child_environment,
      cleanup = TRUE,
      cleanup_tree = TRUE,
      wd = spec$resources,
      windows_hide_window = TRUE
    ),
    error = function(error) {
      unlink(session, recursive = TRUE, force = TRUE)
      .desktop_start_abort(
        .desktop_redact_text(conditionMessage(error), token),
        "spawn",
        token = token
      )
    }
  )
  handle <- new.env(parent = emptyenv())
  handle$process <- process
  handle$pid <- as.integer(process$get_pid())
  handle$runtime_pid <- NA_integer_
  handle$runtime_process <- NULL
  handle$bundle <- spec$bundle
  handle$host <- "127.0.0.1"
  handle$port <- port
  handle$token <- token
  handle$endpoint <- paste0(
    "http://127.0.0.1:",
    port,
    "/"
  )
  handle$url <- handle$endpoint
  handle$launch_url <- paste0(
    handle$endpoint,
    "?rpackit_token=",
    utils::URLencode(token, reserved = TRUE)
  )
  handle$network_token_enforced <- FALSE
  handle$session_dir <- session
  handle$control_path <- control
  handle$stdout_path <- stdout
  handle$stderr_path <- stderr
  handle$stdout_cache <- character()
  handle$stderr_cache <- character()
  handle$events_cache <- list()
  handle$state <- "starting"
  handle$started_at <- Sys.time()
  handle$ready_at <- NULL
  handle$stopped_at <- NULL
  handle$exit_status <- NA_integer_
  handle$forced <- FALSE
  handle$cleanup_confirmed <- FALSE
  handle$runtime_cleanup_confirmed <- NA
  handle$cleaned <- FALSE
  class(handle) <- "rpackit_desktop_process"
  reg.finalizer(
    handle,
    .desktop_process_finalizer,
    onexit = TRUE
  )
  handle
}

#' Start a prepared desktop Shiny application
#'
#' `start_desktop_app()` starts the bundled `Rscript` and generated launcher,
#' waits for a protocol-versioned launcher event, then verifies the Shiny
#' endpoint over HTTP on `127.0.0.1`. It returns only after the endpoint is
#' ready. A random high port and a new session token are generated by default.
#'
#' The token is passed to the app as `RPACKIT_SESSION_TOKEN`. The process
#' handle retains it in `token` and in the sensitive `launch_url` intended for
#' an embedded shell handoff; neither value is returned by
#' [desktop_app_status()] or printed. The token is **not** currently enforced
#' for HTTP or WebSocket access, and is not an authentication credential. The
#' returned object therefore always records `network_token_enforced = FALSE`.
#'
#' The launcher protocol is also suitable for a future Tauri sidecar:
#' `launcher.R --app <path> --port <port> --token <token> --control <path>`.
#' Lifecycle events are newline-delimited JSON on standard output, prefixed by
#' `RPACKIT_EVENT `. Creating the previously absent control path asks the
#' launcher to stop Shiny gracefully.
#'
#' The process handle's `pid` is the process tracked by `processx`; on Windows
#' this can be an `Rscript.exe` wrapper. `runtime_pid` is the valid positive PID
#' reported by the launcher event and may differ. Readiness also captures a
#' create-time-aware `ps` handle for that observed runtime PID so cleanup can
#' verify that the same process stopped. rpackit does not claim that these two
#' observations independently prove other process-tree membership or
#' descendant termination.
#'
#' @param bundle_dir Prepared bundle directory containing `resources/`.
#' @param timeout Seconds to wait for a matching launcher event and an HTTP
#'   response.
#' @param port Loopback port, or `NULL` to select a random high port.
#' @param token Session correlation token containing 16 to 256 URL-safe ASCII
#'   characters, or `NULL` to generate one.
#' @param quiet Suppress the ready summary.
#' @return An `rpackit_desktop_process` handle. Call
#'   [stop_desktop_app()] when finished.
#' @export
start_desktop_app <- function(bundle_dir, timeout = 30, port = NULL,
                              token = NULL, quiet = FALSE) {
  timeout <- .desktop_scalar_timeout(timeout, "timeout")
  quiet <- .desktop_scalar_flag(quiet, "quiet")
  spec <- tryCatch(
    .desktop_launch_spec(bundle_dir),
    error = function(error) {
      .desktop_start_abort(
        conditionMessage(error),
        "validation",
        parent = error
      )
    }
  )
  port <- tryCatch(
    .desktop_launch_port(port),
    error = function(error) {
      .desktop_start_abort(
        conditionMessage(error),
        "port-selection",
        parent = error
      )
    }
  )
  token <- tryCatch(
    .desktop_session_token(token),
    error = function(error) {
      .desktop_start_abort(
        conditionMessage(error),
        "token",
        parent = error
      )
    }
  )
  handle <- .desktop_new_process(spec, port, token)
  .desktop_wait_for_ready(handle, timeout)
  if (!quiet) {
    print(handle)
  }
  invisible(handle)
}

#' Inspect a managed desktop application process
#'
#' `pid` is the process tracked by `processx`; `runtime_pid` is the positive PID
#' reported by the launcher and can differ for a Windows `Rscript.exe` wrapper.
#' `wrapper_alive` and `runtime_alive` report both captured processes; `alive`
#' remains conservatively true if either is alive or runtime liveness is
#' unknown. These values do not prove other process-tree membership.
#'
#' @param process An `rpackit_desktop_process` returned by
#'   [start_desktop_app()].
#' @param tail Maximum number of recent standard-output and standard-error
#'   lines to return.
#' @return An `rpackit_desktop_status` object. Its `url` is the token-free
#'   loopback endpoint; the object never contains the session token or
#'   sensitive launch URL.
#' @export
desktop_app_status <- function(process, tail = 20L) {
  if (!inherits(process, "rpackit_desktop_process") ||
      !is.environment(process)) {
    cli::cli_abort(
      "{.arg process} must be returned by {.fn start_desktop_app}."
    )
  }
  integer_tail <- suppressWarnings(as.integer(tail))
  if (!is.numeric(tail) || length(tail) != 1L || is.na(tail) ||
      !is.finite(tail) || is.na(integer_tail) ||
      tail < 0 || tail != integer_tail) {
    cli::cli_abort("{.arg tail} must be one non-negative whole number.")
  }
  tail <- integer_tail
  wrapper_alive <- isTRUE(process$process$is_alive())
  runtime_alive <- .desktop_runtime_alive(process)
  alive <- .desktop_managed_alive(process)
  exit_status <- .desktop_process_exit_status(process$process)
  if (!alive &&
      .desktop_cleanup_confirmed(process) &&
      process$state %in% c("starting", "ready")) {
    process$state <- if (!is.na(exit_status) && exit_status == 0L) {
      "stopped"
    } else {
      "failed"
    }
    process$exit_status <- exit_status
    process$stopped_at <- Sys.time()
  }
  stdout <- if (isTRUE(process$cleaned)) {
    process$stdout_cache
  } else {
    .desktop_redact_text(
      .desktop_log_lines(process$stdout_path),
      process$token
    )
  }
  stderr <- if (isTRUE(process$cleaned)) {
    process$stderr_cache
  } else {
    .desktop_redact_text(
      .desktop_log_lines(process$stderr_path),
      process$token
    )
  }
  events <- if (isTRUE(process$cleaned)) {
    process$events_cache
  } else {
    .desktop_redact_value(
      .desktop_log_events(process$stdout_path),
      process$token
    )
  }
  structure(
    list(
      state = process$state,
      alive = alive,
      ready = alive && identical(process$state, "ready"),
      pid = process$pid,
      runtime_pid = process$runtime_pid,
      wrapper_alive = wrapper_alive,
      runtime_alive = runtime_alive,
      host = process$host,
      port = process$port,
      endpoint = process$endpoint,
      url = process$url,
      network_token_enforced = FALSE,
      exit_status = exit_status,
      forced = isTRUE(process$forced),
      cleanup_confirmed = .desktop_cleanup_confirmed(process) &&
        isTRUE(process$cleaned),
      cleanup_scope = "tracked-process-observed-runtime-and-session",
      runtime_cleanup_confirmed =
        .desktop_runtime_cleanup_confirmed(process),
      descendant_cleanup_confirmed = NA,
      started_at = process$started_at,
      ready_at = process$ready_at,
      stopped_at = process$stopped_at,
      events = events,
      stdout = utils::tail(stdout, tail),
      stderr = utils::tail(stderr, tail)
    ),
    class = "rpackit_desktop_status"
  )
}

#' Stop a managed desktop application process
#'
#' Requests a graceful Shiny shutdown through the launcher's private control
#' file. If the tracked process remains alive after `timeout`, `processx` is
#' asked to terminate it and its known process tree, with a tracked-process
#' kill as fallback. Cleanup is confirmed only after both the tracked process
#' and the create-time-aware handle captured for the observed launcher runtime
#' have stopped. Other descendant membership and termination are not
#' independently verified. Repeated calls are safe.
#'
#' @param process An `rpackit_desktop_process` returned by
#'   [start_desktop_app()].
#' @param timeout Seconds to wait for graceful shutdown before requesting
#'   termination of the tracked process.
#' @param quiet Suppress the stopped summary.
#' @return An `rpackit_desktop_status` object, invisibly.
#' @export
stop_desktop_app <- function(process, timeout = 5, quiet = FALSE) {
  if (!inherits(process, "rpackit_desktop_process") ||
      !is.environment(process)) {
    cli::cli_abort(
      "{.arg process} must be returned by {.fn start_desktop_app}."
    )
  }
  timeout <- .desktop_scalar_timeout(timeout, "timeout")
  quiet <- .desktop_scalar_flag(quiet, "quiet")
  if (.desktop_managed_alive(process)) {
    control_error <- tryCatch({
      writeLines(
        paste0("stop ", format(Sys.time(), tz = "UTC", usetz = TRUE)),
        process$control_path,
        useBytes = TRUE
      )
      NULL
    }, error = identity)
    if (!is.null(control_error)) {
      process_stopped <- .desktop_terminate_process(process)
      .desktop_cache_process_output(process)
      process$state <- "failed"
      process$exit_status <- .desktop_process_exit_status(process$process)
      process$stopped_at <- Sys.time()
      if (isTRUE(process_stopped)) {
        .desktop_remove_session(process)
      }
      cleanup_note <- if (isTRUE(process_stopped)) {
        paste0(
          "The process tracked by processx and any captured launcher runtime ",
          "were confirmed stopped. Other descendant membership and ",
          "termination are not independently verified."
        )
      } else {
        paste0(
          "Tracked-process cleanup could not be confirmed; retry ",
          "`stop_desktop_app()` with this process handle."
        )
      }
      cli::cli_abort(
        c(
          paste0(
            "Could not request graceful desktop shutdown: ",
            "{conditionMessage(control_error)}"
          ),
          "i" = "{cleanup_note}"
        ),
        class = c(
          "rpackit_desktop_stop_error",
          "rpackit_desktop_error"
        ),
        phase = "control",
        pid = process$pid,
        exit_status = process$exit_status,
        stdout = process$stdout_cache,
        stderr = process$stderr_cache,
        cleanup_confirmed = isTRUE(process_stopped),
        cleanup_scope = "tracked-process-observed-runtime-and-session",
        runtime_cleanup_confirmed =
          .desktop_runtime_cleanup_confirmed(process),
        descendant_cleanup_confirmed = NA,
        process_handle = if (isTRUE(process_stopped)) NULL else process,
        parent = control_error
      )
    }
    deadline <- unclass(Sys.time()) + timeout
    while (.desktop_managed_alive(process) &&
           unclass(Sys.time()) < deadline) {
      Sys.sleep(0.05)
    }
    if (.desktop_managed_alive(process)) {
      process$forced <- TRUE
      .desktop_terminate_process(process)
    } else if (!isTRUE(process$process$is_alive())) {
      process$process$wait(200L)
    }
  }
  process$exit_status <- .desktop_process_exit_status(process$process)
  process$cleanup_confirmed <- .desktop_cleanup_confirmed(process)
  process$runtime_cleanup_confirmed <-
    .desktop_runtime_cleanup_confirmed(process)
  process$state <- if (!isTRUE(process$cleanup_confirmed)) {
    "failed"
  } else if (isTRUE(process$forced)) {
    "terminated"
  } else if (is.na(process$exit_status) || process$exit_status == 0L) {
    "stopped"
  } else {
    "failed"
  }
  if (is.null(process$stopped_at)) {
    process$stopped_at <- Sys.time()
  }
  .desktop_remove_session(process)
  process$cleanup_confirmed <- .desktop_cleanup_confirmed(process) &&
    isTRUE(process$cleaned)
  if (!isTRUE(process$cleanup_confirmed)) {
    process$state <- "failed"
  }
  status <- desktop_app_status(process)
  if (!quiet) {
    print(status)
  }
  invisible(status)
}

#' @export
print.rpackit_desktop_process <- function(x, ...) {
  status <- desktop_app_status(x)
  cli::cli_h1("rpackit desktop process")
  cli::cli_text("State: {status$state}")
  cli::cli_text("Wrapper PID: {status$pid}")
  cli::cli_text("Runtime PID: {status$runtime_pid}")
  cli::cli_text("Endpoint: {status$endpoint}")
  cli::cli_text("Loopback host: 127.0.0.1")
  cli::cli_text("Network token enforcement: not implemented")
  invisible(x)
}

#' @export
print.rpackit_desktop_status <- function(x, ...) {
  cli::cli_h1("rpackit desktop process status")
  cli::cli_text("State: {x$state}")
  cli::cli_text("Wrapper PID: {x$pid}")
  cli::cli_text("Runtime PID: {x$runtime_pid}")
  cli::cli_text("Alive: {if (x$alive) 'yes' else 'no'}")
  cli::cli_text(
    "Network token enforcement: ",
    "{if (x$network_token_enforced) 'yes' else 'not implemented'}"
  )
  invisible(x)
}
