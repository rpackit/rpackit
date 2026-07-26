make_lifecycle_app <- function(code = NULL) {
  path <- tempfile("rpackit-lifecycle-app-")
  dir.create(path)
  if (is.null(code)) {
    code <- c(
      "message(",
      "  'credential environment present: ',",
      "  nzchar(Sys.getenv('RPACKIT_SESSION_TOKEN'))",
      ")",
      "shiny::shinyApp(",
      "  ui = shiny::fluidPage('rpackit ready'),",
      "  server = function(input, output, session) {}",
      ")"
    )
  }
  writeLines(code, file.path(path, "app.R"), useBytes = TRUE)
  path
}

make_lifecycle_spec <- function(app, event_pid_offset = 0L) {
  resources <- tempfile("rpackit-lifecycle-resources-")
  dir.create(resources)
  launcher <- file.path(resources, "launcher.R")
  launcher_lines <- rpackit:::.desktop_launcher_lines()
  if (event_pid_offset != 0L) {
    launcher_lines <- sub(
      "      pid = Sys.getpid(),",
      paste0(
        "      pid = Sys.getpid() + ",
        as.integer(event_pid_offset),
        "L,"
      ),
      launcher_lines,
      fixed = TRUE
    )
  }
  writeLines(
    launcher_lines,
    launcher,
    useBytes = TRUE
  )
  list(
    bundle = resources,
    resources = resources,
    rscript = file.path(
      R.home("bin"),
      if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"
    ),
    library = dirname(find.package("shiny")),
    app = app,
    launcher = launcher,
    manifest = list(
      launcher = list(network_token_enforced = TRUE)
    )
  )
}

make_lifecycle_runtime <- function() {
  path <- tempfile("rpackit-lifecycle-runtime-")
  dir.create(file.path(path, "bin"), recursive = TRUE)
  dir.create(file.path(path, "library"))
  file.create(file.path(
    path,
    "bin",
    if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"
  ))
  path
}

mock_lifecycle_spec <- function(spec, .env = parent.frame()) {
  testthat::local_mocked_bindings(
    .desktop_launch_spec = function(bundle_dir) spec,
    .package = "rpackit",
    .env = .env
  )
}

desktop_http_status <- function(port, token = NULL, path = "/",
                                method = "GET") {
  connection <- socketConnection(
    host = "127.0.0.1",
    port = port,
    server = FALSE,
    blocking = TRUE,
    open = "r+b",
    timeout = 2L
  )
  on.exit(close(connection), add = TRUE)
  authentication <- if (is.null(token)) {
    ""
  } else {
    paste0("Shiny-Shared-Secret: ", token, "\r\n")
  }
  request <- paste0(
    method,
    " ",
    path,
    " HTTP/1.1\r\n",
    "Host: 127.0.0.1:",
    port,
    "\r\n",
    authentication,
    "Connection: close\r\n\r\n"
  )
  writeBin(charToRaw(request), connection)
  flush(connection)
  status_line <- readLines(connection, n = 1L, warn = FALSE)
  if (!length(status_line)) {
    return(NA_integer_)
  }
  as.integer(sub(
    "^HTTP/[0-9.]+ ([0-9]{3}).*$",
    "\\1",
    status_line[[1L]]
  ))
}

desktop_websocket_connect <- function(port, token = NULL) {
  connection <- socketConnection(
    host = "127.0.0.1",
    port = port,
    server = FALSE,
    blocking = TRUE,
    open = "r+b",
    timeout = 2L
  )
  authentication <- if (is.null(token)) {
    ""
  } else {
    paste0("Shiny-Shared-Secret: ", token, "\r\n")
  }
  request <- paste0(
    "GET /websocket/ HTTP/1.1\r\n",
    "Host: 127.0.0.1:",
    port,
    "\r\n",
    "Origin: http://127.0.0.1:",
    port,
    "\r\n",
    "Upgrade: websocket\r\n",
    "Connection: Upgrade\r\n",
    "Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n",
    "Sec-WebSocket-Version: 13\r\n",
    authentication,
    "\r\n"
  )
  writeBin(charToRaw(request), connection)
  flush(connection)
  status_line <- readLines(connection, n = 1L, warn = FALSE)
  repeat {
    line <- readLines(connection, n = 1L, warn = FALSE)
    if (!length(line) || !nzchar(line[[1L]])) {
      break
    }
  }
  status <- if (length(status_line)) {
    as.integer(sub(
      "^HTTP/[0-9.]+ ([0-9]{3}).*$",
      "\\1",
      status_line[[1L]]
    ))
  } else {
    NA_integer_
  }
  list(connection = connection, status = status)
}

desktop_websocket_send_text <- function(connection, text) {
  payload <- charToRaw(enc2utf8(text))
  length <- length(payload)
  if (length <= 125L) {
    header <- as.raw(c(0x81L, 0x80L + length))
  } else if (length <= 65535L) {
    header <- as.raw(c(
      0x81L,
      0x80L + 126L,
      bitwShiftR(length, 8L),
      bitwAnd(length, 0xffL)
    ))
  } else {
    stop("Test WebSocket payload is unexpectedly large.")
  }
  mask <- as.raw(c(0x21L, 0x43L, 0x65L, 0x87L))
  masked <- as.raw(bitwXor(
    as.integer(payload),
    rep(as.integer(mask), length.out = length)
  ))
  writeBin(c(header, mask, masked), connection)
  flush(connection)
  invisible(NULL)
}

desktop_websocket_rejected <- function(connection, timeout = 2) {
  readable <- tryCatch(
    socketSelect(list(connection), write = FALSE, timeout = timeout),
    error = function(error) FALSE
  )
  if (!length(readable) || !isTRUE(readable[[1L]])) {
    return(FALSE)
  }
  header <- tryCatch(
    readBin(connection, what = "raw", n = 2L),
    error = function(error) raw()
  )
  if (!length(header)) {
    return(TRUE)
  }
  length(header) >= 1L &&
    bitwAnd(as.integer(header[[1L]]), 0x0fL) == 0x08L
}

desktop_wait_for_file <- function(path, timeout = 2) {
  deadline <- unclass(Sys.time()) + timeout
  while (!file.exists(path) && unclass(Sys.time()) < deadline) {
    Sys.sleep(0.02)
  }
  file.exists(path)
}

test_that("prepared bundles expose the versioned lifecycle launch spec", {
  app <- make_lifecycle_app()
  output <- tempfile("rpackit-lifecycle-spec-")
  prepare_desktop(
    app,
    make_lifecycle_runtime(),
    output_dir = output,
    install_packages = FALSE,
    verify_runtime = FALSE,
    quiet = TRUE
  )

  spec <- rpackit:::.desktop_launch_spec(output)

  expect_identical(spec$bundle, normalizePath(
    output,
    winslash = "/",
    mustWork = TRUE
  ))
  expect_true(file.exists(spec$launcher))
  expect_true(file.exists(spec$rscript))
  expect_true(dir.exists(spec$library))
  expect_true(dir.exists(spec$app))
  expect_identical(spec$manifest$launcher$protocol_version, "2")
  expect_error(
    rpackit:::.desktop_session_token("not long enough"),
    "URL-safe ASCII"
  )

  manifest_path <- file.path(
    output,
    "resources",
    "rpackit.json"
  )
  manifest <- jsonlite::fromJSON(
    manifest_path,
    simplifyVector = FALSE
  )
  manifest$launcher$readiness$starting_event <- "ready"
  jsonlite::write_json(
    manifest,
    manifest_path,
    auto_unbox = TRUE
  )
  expect_error(
    validate_desktop_bundle(output, quiet = TRUE),
    "supported lifecycle"
  )
  expect_error(
    rpackit:::.desktop_launch_spec(output),
    "supported lifecycle"
  )
})

test_that("managed lifecycle reaches HTTP readiness and stops gracefully", {
  skip_if_not_installed("shiny")
  app <- make_lifecycle_app()
  spec <- make_lifecycle_spec(app)
  mock_lifecycle_spec(spec)

  process <- start_desktop_app(
    spec$bundle,
    timeout = 15,
    quiet = TRUE
  )
  session_token <- process$token
  on.exit({
    if (isTRUE(process$process$is_alive())) {
      stop_desktop_app(process, timeout = 1, quiet = TRUE)
    }
  }, add = TRUE)

  status <- desktop_app_status(process, tail = 50)
  expect_s3_class(process, "rpackit_desktop_process")
  expect_s3_class(status, "rpackit_desktop_status")
  expect_identical(status$state, "ready")
  expect_true(status$alive)
  expect_true(status$ready)
  expect_true(status$pid > 0L)
  expect_true(status$runtime_pid > 0L)
  expect_true(status$wrapper_alive)
  expect_true(status$runtime_alive)
  expect_identical(status$host, "127.0.0.1")
  expect_true(status$port >= 49152L)
  expect_true(status$network_token_enforced)
  expect_false("token" %in% names(status))
  expect_identical(
    status$endpoint,
    paste0("http://127.0.0.1:", status$port, "/")
  )
  expect_identical(status$url, status$endpoint)
  expect_false(file.exists(process$credential_path))
  expect_identical(process$launch_url, status$endpoint)
  launch <- desktop_app_launch_config(process)
  expect_s3_class(launch, "rpackit_desktop_launch_config")
  expect_identical(launch$url, status$endpoint)
  expect_identical(launch$origin, sub("/$", "", status$endpoint))
  expect_identical(
    unname(launch$headers[["Shiny-Shared-Secret"]]),
    session_token
  )
  expect_false(launch$follow_redirects)
  expect_false(any(grepl(
    session_token,
    capture.output(print(launch), type = "message"),
    fixed = TRUE
  )))
  expect_match(
    paste(c(status$stdout, status$stderr), collapse = "\n"),
    "credential environment present: FALSE"
  )
  expect_identical(desktop_http_status(status$port), 403L)
  expect_identical(
    desktop_http_status(status$port, "wrong-session-token-0123456789"),
    403L
  )
  expect_identical(
    desktop_http_status(status$port, session_token),
    200L
  )
  expect_identical(
    desktop_http_status(status$port, path = "/shared/shiny.min.css"),
    403L
  )
  expect_identical(
    desktop_http_status(
      status$port,
      session_token,
      "/shared/shiny.min.css"
    ),
    200L
  )
  flattened_status <- unlist(
    status,
    recursive = TRUE,
    use.names = TRUE
  )
  expect_false(any(grepl(
    session_token,
    c(names(flattened_status), as.character(flattened_status)),
    fixed = TRUE
  )))
  expect_false(any(grepl(
    session_token,
    capture.output(print(process), type = "message"),
    fixed = TRUE
  )))
  expect_false(any(grepl(
    session_token,
    capture.output(print(status), type = "message"),
    fixed = TRUE
  )))
  expect_false(any(grepl(
    session_token,
    process$process$get_cmdline(),
    fixed = TRUE
  )))
  expect_false(any(grepl(
    session_token,
    c(
      readLines(process$stdout_path, warn = FALSE),
      readLines(process$stderr_path, warn = FALSE)
    ),
    fixed = TRUE
  )))
  starting <- Filter(
    function(event) identical(event$event, "starting"),
    status$events
  )
  expect_length(starting, 1L)
  expect_true(starting[[1L]]$token_enforced)
  expect_false("token" %in% names(starting[[1L]]))
  listening <- Filter(
    function(event) identical(event$event, "listening"),
    status$events
  )
  expect_length(listening, 1L)
  expect_true(listening[[1L]]$token_enforced)
  expect_false("token" %in% names(listening[[1L]]))

  session_dir <- process$session_dir
  if (.Platform$OS.type != "windows") {
    session_mode <- as.integer(file.info(session_dir)$mode)
    expect_identical(
      bitwAnd(session_mode, as.integer(as.octmode("0077"))),
      0L
    )
  }
  stopped <- stop_desktop_app(
    process,
    timeout = 5,
    quiet = TRUE
  )
  expect_identical(stopped$state, "stopped")
  expect_false(stopped$alive)
  expect_false(stopped$wrapper_alive)
  expect_false(stopped$runtime_alive)
  expect_false(stopped$forced)
  expect_identical(stopped$exit_status, 0L)
  expect_true(stopped$cleanup_confirmed)
  expect_true(stopped$runtime_cleanup_confirmed)
  expect_true(is.na(stopped$descendant_cleanup_confirmed))
  expect_false(process$process$is_alive())
  expect_false(dir.exists(session_dir))
  expect_null(process$token)
  expect_null(process$launch_headers)
  expect_null(process$launch_url)
  expect_error(
    desktop_app_launch_config(process),
    "must be ready and running"
  )
  stopped_events <- vapply(
    stopped$events,
    function(event) event$event,
    character(1)
  )
  expect_true(all(
    c("starting", "listening", "stopping", "stopped") %in% stopped_events
  ))

  repeated <- stop_desktop_app(process, quiet = TRUE)
  expect_identical(repeated$state, "stopped")
  expect_false(repeated$alive)
  repeated_events <- vapply(
    repeated$events,
    function(event) event$event,
    character(1)
  )
  expect_true("stopped" %in% repeated_events)
})

test_that("authenticated launch preserves directory DESCRIPTION semantics", {
  skip_if_not_installed("shiny")
  marker <- tempfile("rpackit-description-semantics-")
  marker_literal <- paste(utils::capture.output(dput(marker)), collapse = "")
  app <- make_lifecycle_app(c(
    paste0("marker <- ", marker_literal),
    "shiny::shinyApp(",
    "  ui = shiny::fluidPage('description semantics'),",
    "  server = function(input, output, session) {},",
    "  onStart = function() {",
    "    globals <- get('.globals', envir = asNamespace('shiny'))",
    "    writeLines(",
    "      paste(globals$showcaseDefault, globals$IncludeWWW),",
    "      marker",
    "    )",
    "  }",
    ")"
  ))
  writeLines(
    c(
      "Title: rpackit DESCRIPTION semantics fixture",
      "DisplayMode: Showcase",
      "IncludeWWW: False"
    ),
    file.path(app, "DESCRIPTION")
  )
  spec <- make_lifecycle_spec(app)
  mock_lifecycle_spec(spec)
  process <- start_desktop_app(spec$bundle, timeout = 15, quiet = TRUE)
  on.exit({
    if (isTRUE(process$process$is_alive())) {
      stop_desktop_app(process, timeout = 1, quiet = TRUE)
    }
  }, add = TRUE)

  expect_true(desktop_wait_for_file(marker))
  expect_identical(readLines(marker, warn = FALSE), "1 FALSE")

  stopped <- stop_desktop_app(process, timeout = 5, quiet = TRUE)
  expect_identical(stopped$state, "stopped")
})

test_that("WebSocket sessions require the same request-header credential", {
  skip_if_not_installed("shiny")
  marker <- tempfile("rpackit-websocket-session-")
  marker_literal <- paste(utils::capture.output(dput(marker)), collapse = "")
  app <- make_lifecycle_app(c(
    paste0("marker <- ", marker_literal),
    "shiny::shinyApp(",
    "  ui = shiny::fluidPage('authenticated websocket'),",
    "  server = function(input, output, session) {",
    "    writeLines('started', marker)",
    "  }",
    ")"
  ))
  spec <- make_lifecycle_spec(app)
  mock_lifecycle_spec(spec)
  process <- start_desktop_app(spec$bundle, timeout = 15, quiet = TRUE)
  token <- process$token
  on.exit({
    if (isTRUE(process$process$is_alive())) {
      stop_desktop_app(process, timeout = 1, quiet = TRUE)
    }
  }, add = TRUE)

  missing <- desktop_websocket_connect(process$port)
  expect_identical(missing$status, 101L)
  try(
    desktop_websocket_send_text(
      missing$connection,
      '{"method":"init","data":{}}'
    ),
    silent = TRUE
  )
  expect_true(desktop_websocket_rejected(missing$connection))
  expect_false(file.exists(marker))
  try(close(missing$connection), silent = TRUE)

  wrong <- desktop_websocket_connect(
    process$port,
    "wrong-session-token-0123456789"
  )
  expect_identical(wrong$status, 101L)
  try(
    desktop_websocket_send_text(
      wrong$connection,
      '{"method":"init","data":{}}'
    ),
    silent = TRUE
  )
  expect_true(desktop_websocket_rejected(wrong$connection))
  expect_false(file.exists(marker))
  try(close(wrong$connection), silent = TRUE)

  authenticated <- desktop_websocket_connect(process$port, token)
  expect_identical(authenticated$status, 101L)
  desktop_websocket_send_text(
    authenticated$connection,
    '{"method":"init","data":{}}'
  )
  expect_true(desktop_wait_for_file(marker))
  try(close(authenticated$connection), silent = TRUE)

  stopped <- stop_desktop_app(process, timeout = 5, quiet = TRUE)
  expect_identical(stopped$state, "stopped")
})

test_that("authenticated readiness survives noisy application startup", {
  skip_if_not_installed("shiny")
  app <- make_lifecycle_app(c(
    "payload <- list(protocol_version = '2', event = 'app-note')",
    "cat(",
    "  'RPACKIT_EVENT ',",
    "  jsonlite::toJSON(payload, auto_unbox = TRUE),",
    "  '\\n',",
    "  sep = ''",
    ")",
    "writeLines(sprintf('startup-noise-%04d', seq_len(1200L)))",
    "flush.console()",
    "Sys.sleep(0.5)",
    "shiny::shinyApp(",
    "  ui = shiny::fluidPage('rpackit ready after noise'),",
    "  server = function(input, output, session) {}",
    ")"
  ))
  spec <- make_lifecycle_spec(app)
  mock_lifecycle_spec(spec)

  process <- start_desktop_app(
    spec$bundle,
    timeout = 15,
    quiet = TRUE
  )
  on.exit({
    if (isTRUE(process$process$is_alive())) {
      stop_desktop_app(process, timeout = 1, quiet = TRUE)
    }
  }, add = TRUE)

  status <- desktop_app_status(process, tail = 20)
  event_names <- vapply(
    status$events,
    function(event) event$event,
    character(1)
  )
  flattened <- unlist(status, recursive = TRUE, use.names = TRUE)

  expect_true(status$ready)
  expect_true(all(c("starting", "app-note") %in% event_names))
  expect_false(any(grepl(
    process$token,
    c(names(flattened), as.character(flattened)),
    fixed = TRUE
  )))

  stopped <- stop_desktop_app(process, timeout = 5, quiet = TRUE)
  expect_identical(stopped$state, "stopped")
})

test_that("zero stop timeout forcibly stops the tracked process", {
  skip_if_not_installed("shiny")
  app <- make_lifecycle_app()
  spec <- make_lifecycle_spec(app)
  mock_lifecycle_spec(spec)

  process <- start_desktop_app(
    spec$bundle,
    timeout = 15,
    quiet = TRUE
  )
  on.exit({
    if (isTRUE(process$process$is_alive())) {
      rpackit:::.desktop_terminate_process(process)
    }
  }, add = TRUE)
  session_dir <- process$session_dir

  stopped <- stop_desktop_app(
    process,
    timeout = 0,
    quiet = TRUE
  )

  expect_identical(stopped$state, "terminated")
  expect_true(stopped$forced)
  expect_false(stopped$alive)
  expect_false(process$process$is_alive())
  expect_false(dir.exists(session_dir))
})

test_that("launcher runtime PID may differ from the processx wrapper PID", {
  handle <- new.env(parent = emptyenv())
  handle$pid <- as.integer(Sys.getpid() + 1L)
  handle$port <- 54321L
  events <- list(list(
    protocol_version = "2",
    event = "listening",
    pid = Sys.getpid(),
    host = "127.0.0.1",
    port = handle$port,
    token_enforced = TRUE
  ))

  listening <- rpackit:::.desktop_matching_listening_event(
    events,
    handle
  )
  runtime_process <- rpackit:::.desktop_runtime_process(
    listening$runtime_pid
  )

  expect_identical(listening$runtime_pid, as.integer(Sys.getpid()))
  expect_false(identical(listening$runtime_pid, handle$pid))
  expect_false(is.null(runtime_process))
  expect_true(ps::ps_is_running(runtime_process))
})

test_that("observed runtime liveness gates cleanup and overall liveness", {
  process <- new.env(parent = emptyenv())
  process$is_alive <- function() FALSE
  handle <- new.env(parent = emptyenv())
  handle$process <- process
  handle$runtime_pid <- 123L
  handle$runtime_process <- NULL

  expect_true(rpackit:::.desktop_managed_alive(handle))
  expect_false(rpackit:::.desktop_cleanup_confirmed(handle))

  handle$runtime_process <- structure(list(), class = "mock-runtime")
  runtime_state <- TRUE
  local_mocked_bindings(
    .desktop_runtime_alive = function(handle) runtime_state,
    .package = "rpackit"
  )

  expect_true(rpackit:::.desktop_managed_alive(handle))
  expect_false(rpackit:::.desktop_cleanup_confirmed(handle))

  runtime_state <- FALSE
  expect_false(rpackit:::.desktop_managed_alive(handle))
  expect_true(rpackit:::.desktop_cleanup_confirmed(handle))
})

test_that("Windows lifecycle paths receive verified account-private ACLs", {
  skip_if(.Platform$OS.type != "windows")
  session <- tempfile("rpackit-windows-acl-test-")
  expect_true(dir.create(session))
  on.exit(unlink(session, recursive = TRUE, force = TRUE), add = TRUE)

  expect_silent(
    rpackit:::.desktop_restrict_windows_acl(session, directory = TRUE)
  )
  credential <- file.path(session, "credential")
  writeLines("private-session-token-0123456789", credential)
  expect_silent(
    rpackit:::.desktop_restrict_windows_acl(
      credential,
      directory = FALSE
    )
  )
})

test_that("startup cleanup failures retain a retryable process handle", {
  session <- tempfile("rpackit-retained-cleanup-")
  expect_true(dir.create(session))
  on.exit(unlink(session, recursive = TRUE, force = TRUE), add = TRUE)
  process <- new.env(parent = emptyenv())
  process$is_alive <- function() FALSE
  handle <- new.env(parent = emptyenv())
  handle$process <- process
  handle$pid <- 123L
  handle$runtime_process <- NULL
  handle$bundle <- "bundle"
  handle$token <- "private-session-token-0123456789"
  handle$launch_headers <- c(
    `Shiny-Shared-Secret` = handle$token
  )
  handle$launch_url <- "http://127.0.0.1:54321/"
  handle$session_dir <- session
  handle$stdout_cache <- character()
  handle$stderr_cache <- character()
  handle$events_cache <- list()
  handle$cleaned <- FALSE
  local_mocked_bindings(
    .desktop_terminate_process = function(handle) TRUE,
    .desktop_process_exit_status = function(process) 0L,
    .desktop_cache_process_output = function(handle) invisible(handle),
    .desktop_remove_session = function(handle) invisible(handle),
    .package = "rpackit"
  )

  error <- tryCatch(
    rpackit:::.desktop_start_abort(
      "simulated retained directory",
      "cleanup",
      handle = handle
    ),
    rpackit_desktop_start_error = identity
  )

  expect_s3_class(error, "rpackit_desktop_start_error")
  expect_false(error$cleanup_confirmed)
  expect_identical(error$process_handle, handle)
})

test_that("shutdown file-cleanup failures retain a retryable handle", {
  session <- tempfile("rpackit-retained-stop-cleanup-")
  expect_true(dir.create(session))
  on.exit(unlink(session, recursive = TRUE, force = TRUE), add = TRUE)
  process <- new.env(parent = emptyenv())
  handle <- new.env(parent = emptyenv())
  handle$process <- process
  handle$pid <- 123L
  handle$runtime_process <- NULL
  handle$session_dir <- session
  handle$control_path <- file.path(session, "missing", "stop")
  handle$stdout_cache <- character()
  handle$stderr_cache <- character()
  handle$events_cache <- list()
  class(handle) <- "rpackit_desktop_process"
  local_mocked_bindings(
    .desktop_managed_alive = function(handle) TRUE,
    .desktop_terminate_process = function(handle) TRUE,
    .desktop_process_exit_status = function(process) 0L,
    .desktop_cache_process_output = function(handle) invisible(handle),
    .desktop_remove_session = function(handle) invisible(handle),
    .desktop_runtime_cleanup_confirmed = function(handle) NA,
    .package = "rpackit"
  )

  error <- tryCatch(
    suppressWarnings(
      stop_desktop_app(handle, timeout = 0, quiet = TRUE)
    ),
    rpackit_desktop_stop_error = identity
  )

  expect_s3_class(error, "rpackit_desktop_stop_error")
  expect_false(error$cleanup_confirmed)
  expect_identical(error$process_handle, handle)
})

test_that("readiness fails if the runtime process handle cannot be captured", {
  skip_if_not_installed("shiny")
  app <- make_lifecycle_app()
  spec <- make_lifecycle_spec(app)
  mock_lifecycle_spec(spec)
  local_mocked_bindings(
    .desktop_runtime_process = function(pid) NULL,
    .package = "rpackit"
  )

  error <- tryCatch(
    start_desktop_app(
      spec$bundle,
      timeout = 15,
      quiet = TRUE
    ),
    rpackit_desktop_start_error = identity
  )
  handle <- error$process_handle
  on.exit({
    if (!is.null(handle)) {
      try(handle$process$kill_tree(grace = 0), silent = TRUE)
      try(handle$process$wait(1000L), silent = TRUE)
      handle$runtime_pid <- NA_integer_
      rpackit:::.desktop_remove_session(handle)
    }
  }, add = TRUE)

  expect_s3_class(error, "rpackit_desktop_start_error")
  expect_identical(error$phase, "readiness")
  expect_match(conditionMessage(error), "create-time-aware handle")
  expect_false(error$cleanup_confirmed)
  expect_s3_class(handle, "rpackit_desktop_process")
  expect_true(dir.exists(handle$session_dir))
})

test_that("launcher failures are structured and stop the tracked process", {
  skip_if_not_installed("shiny")
  app <- make_lifecycle_app(
    paste0(
      "stop(",
      "'intentional startup failure: ', ",
      "Sys.getenv('RPACKIT_SESSION_TOKEN')",
      ")"
    )
  )
  spec <- make_lifecycle_spec(app)
  mock_lifecycle_spec(spec)
  failure_token <- "failure-session-token-0123456789"

  error <- tryCatch(
    start_desktop_app(
      spec$bundle,
      timeout = 15,
      token = failure_token,
      quiet = TRUE
    ),
    rpackit_desktop_start_error = identity
  )
  retained_handle <- error$process_handle
  on.exit({
    if (!is.null(retained_handle)) {
      try(retained_handle$process$kill_tree(grace = 0), silent = TRUE)
      try(retained_handle$process$wait(1000L), silent = TRUE)
      retained_handle$runtime_pid <- NA_integer_
      rpackit:::.desktop_remove_session(retained_handle)
    }
  }, add = TRUE)

  expect_s3_class(error, "rpackit_desktop_start_error")
  expect_identical(error$phase, "runtime")
  expect_true(error$pid > 0L)
  expect_identical(
    error$cleanup_scope,
    "tracked-process-observed-runtime-and-session"
  )
  if (isTRUE(error$cleanup_confirmed)) {
    expect_true(is.na(error$runtime_cleanup_confirmed))
    expect_null(retained_handle)
  } else {
    expect_true(is.na(error$runtime_cleanup_confirmed))
    expect_s3_class(retained_handle, "rpackit_desktop_process")
    expect_true(dir.exists(retained_handle$session_dir))
  }
  expect_true(is.na(error$descendant_cleanup_confirmed))
  expect_match(
    paste(c(error$stdout, error$stderr), collapse = "\n"),
    "intentional startup failure"
  )
  error_events <- Filter(
    function(event) identical(event$event, "error"),
    error$events
  )
  expect_length(error_events, 1L)
  expect_identical(error_events[[1L]]$phase, "runtime")
  expect_match(conditionMessage(error), "intentional startup failure")
  expect_false(grepl(
    failure_token,
    conditionMessage(error),
    fixed = TRUE
  ))
  expect_false(any(grepl(
    failure_token,
    c(
      error$stdout,
      error$stderr,
      jsonlite::toJSON(error$events, auto_unbox = TRUE)
    ),
    fixed = TRUE
  )))
})

test_that("token redaction covers phase metadata and list field names", {
  token <- "private-session-token-0123456789"
  value <- list()
  value[[token]] <- list(
    phase = token,
    message = token
  )

  redacted <- rpackit:::.desktop_redact_value(value, token)
  flattened <- unlist(redacted, recursive = TRUE, use.names = TRUE)

  expect_false(any(grepl(
    token,
    c(names(flattened), as.character(flattened)),
    fixed = TRUE
  )))

  error <- tryCatch(
    rpackit:::.desktop_start_abort(
      "protocol-shaped application event",
      phase = token,
      token = token
    ),
    rpackit_desktop_start_error = identity
  )

  expect_s3_class(error, "rpackit_desktop_start_error")
  expect_false(grepl(token, error$phase, fixed = TRUE))
  expect_false(grepl(token, conditionMessage(error), fixed = TRUE))
})

test_that("launcher event PIDs must be positive whole-number integers", {
  expect_identical(rpackit:::.desktop_event_pid(123), 123L)
  expect_true(is.na(rpackit:::.desktop_event_pid(0)))
  expect_true(is.na(rpackit:::.desktop_event_pid(-1)))
  expect_true(is.na(rpackit:::.desktop_event_pid(1.5)))
  expect_true(is.na(rpackit:::.desktop_event_pid("123")))
  expect_true(is.na(rpackit:::.desktop_event_pid(c(1, 2))))
})

test_that("private port and token generation preserve caller RNG state", {
  seed_existed <- exists(
    ".Random.seed",
    envir = .GlobalEnv,
    inherits = FALSE
  )
  if (seed_existed) {
    original_seed <- get(
      ".Random.seed",
      envir = .GlobalEnv,
      inherits = FALSE
    )
  }
  on.exit({
    if (seed_existed) {
      assign(".Random.seed", original_seed, envir = .GlobalEnv)
    } else if (exists(
      ".Random.seed",
      envir = .GlobalEnv,
      inherits = FALSE
    )) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)

  set.seed(8675309)
  caller_seed <- .Random.seed
  port <- rpackit:::.desktop_launch_port(NULL)
  first <- rpackit:::.desktop_session_token(NULL)
  second <- rpackit:::.desktop_session_token(NULL)

  expect_identical(.Random.seed, caller_seed)
  expect_true(port >= 49152L && port <= 65535L)
  expect_match(first, "^rp-[0-9a-f]{64}$", perl = TRUE)
  expect_match(second, "^rp-[0-9a-f]{64}$", perl = TRUE)
  expect_false(identical(first, second))

  rm(".Random.seed", envir = .GlobalEnv)
  invisible(rpackit:::.desktop_launch_port(NULL))
  invisible(rpackit:::.desktop_session_token(NULL))
  expect_false(exists(
    ".Random.seed",
    envir = .GlobalEnv,
    inherits = FALSE
  ))
})
