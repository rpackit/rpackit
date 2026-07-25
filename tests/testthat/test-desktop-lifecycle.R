make_lifecycle_app <- function(code = NULL) {
  path <- tempfile("rpackit-lifecycle-app-")
  dir.create(path)
  if (is.null(code)) {
    code <- c(
      "message('app token: ', Sys.getenv('RPACKIT_SESSION_TOKEN'))",
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
    manifest = list()
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
  expect_identical(spec$manifest$launcher$protocol_version, "1")
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
    "lifecycle protocol version 1"
  )
  expect_error(
    rpackit:::.desktop_launch_spec(output),
    "lifecycle protocol version 1"
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
  expect_false(status$network_token_enforced)
  expect_false("token" %in% names(status))
  expect_identical(
    status$endpoint,
    paste0("http://127.0.0.1:", status$port, "/")
  )
  expect_identical(status$url, status$endpoint)
  expect_match(process$launch_url, "rpackit_token=", fixed = TRUE)
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
  starting <- Filter(
    function(event) identical(event$event, "starting"),
    status$events
  )
  expect_length(starting, 1L)
  expect_false(starting[[1L]]$token_enforced)
  expect_false("token" %in% names(starting[[1L]]))

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
  stopped_events <- vapply(
    stopped$events,
    function(event) event$event,
    character(1)
  )
  expect_true(all(c("starting", "stopping", "stopped") %in% stopped_events))

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

test_that("readiness survives noisy startup and redacts event field names", {
  skip_if_not_installed("shiny")
  app <- make_lifecycle_app(c(
    "token <- Sys.getenv('RPACKIT_SESSION_TOKEN')",
    "payload <- list(protocol_version = '1', event = 'app-note')",
    "payload[[token]] <- token",
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
    protocol_version = "1",
    event = "starting",
    pid = Sys.getpid(),
    host = "127.0.0.1",
    port = handle$port,
    token_enforced = FALSE
  ))

  starting <- rpackit:::.desktop_matching_starting_event(
    events,
    handle
  )
  runtime_process <- rpackit:::.desktop_runtime_process(
    starting$runtime_pid
  )

  expect_identical(starting$runtime_pid, as.integer(Sys.getpid()))
  expect_false(identical(starting$runtime_pid, handle$pid))
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
    expect_true(error$runtime_cleanup_confirmed)
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
  expect_match(first, "^[A-Za-z0-9._~-]{16,256}$", perl = TRUE)
  expect_match(second, "^[A-Za-z0-9._~-]{16,256}$", perl = TRUE)
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
