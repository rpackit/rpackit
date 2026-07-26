if (!requireNamespace('jsonlite', quietly = TRUE)) {
  cat(paste0(
    'RPACKIT_EVENT ',
    '{"protocol_version":"1","event":"error",',
    '"phase":"bootstrap",',
    '"message":"The bundled runtime does not contain jsonlite."}',
    '\n'
  ))
  quit(save = 'no', status = 1L, runLast = FALSE)
}
event_prefix <- 'RPACKIT_EVENT '
emit_event <- function(event, fields = list()) {
  payload <- c(
    list(
      protocol_version = '1',
      event = event,
      timestamp = format(Sys.time(), tz = 'UTC', usetz = TRUE)
    ),
    fields
  )
  encoded <- jsonlite::toJSON(
    payload,
    auto_unbox = TRUE,
    null = 'null'
  )
  cat(event_prefix, encoded, '\n', sep = '')
  flush.console()
}
launcher_error <- function(message, phase) {
  stop(
    structure(
      list(message = message, call = NULL, phase = phase),
      class = c('rpackit_launcher_error', 'error', 'condition')
    )
  )
}
main <- function() {
  arguments <- commandArgs(trailingOnly = TRUE)
  if (!length(arguments) || length(arguments) %% 2L != 0L) {
    launcher_error(
      paste0(
        'Usage: launcher.R --app <path> --port <port> ',
        '--token <token> [--control <path>]'
      ),
      'arguments'
    )
  }
  keys <- arguments[seq.int(1L, length(arguments), by = 2L)]
  values <- arguments[seq.int(2L, length(arguments), by = 2L)]
  required <- c('--app', '--port', '--token')
  allowed <- c(required, '--control')
  if (anyDuplicated(keys) || !all(required %in% keys) ||
      any(!keys %in% allowed)) {
    launcher_error(
      paste0(
        'Exactly one --app, --port, and --token argument is required; ',
        '--control may appear once.'
      ),
      'arguments'
    )
  }
  options <- stats::setNames(values, keys)
  app <- options[['--app']]
  if (!dir.exists(app)) {
    launcher_error('The application directory does not exist.', 'app')
  }
  app <- normalizePath(app, winslash = '/', mustWork = TRUE)
  layout_ok <- file.exists(file.path(app, 'app.R')) ||
    all(file.exists(file.path(app, c('ui.R', 'server.R'))))
  if (!layout_ok) {
    launcher_error(
      'The application is not a supported Shiny layout.',
      'app'
    )
  }
  port <- suppressWarnings(as.integer(options[['--port']]))
  if (is.na(port) || port < 1L || port > 65535L) {
    launcher_error(
      '--port must be a whole number between 1 and 65535.',
      'arguments'
    )
  }
  token <- options[['--token']]
  if (is.na(token) || nchar(token, type = 'bytes') < 16L ||
      nchar(token, type = 'bytes') > 256L ||
      !grepl('^[A-Za-z0-9._~-]+$', token)) {
    launcher_error(
      '--token must contain 16 to 256 URL-safe ASCII characters.',
      'arguments'
    )
  }
  control <- options[['--control']]
  if (!is.null(control)) {
    if (is.na(control) || !nzchar(control) || grepl('[\r\n]', control)) {
      launcher_error('--control must be a usable path.', 'arguments')
    }
    control_parent <- dirname(control)
    if (!dir.exists(control_parent)) {
      launcher_error(
        'The --control parent directory does not exist.',
        'arguments'
      )
    }
    control <- file.path(
      normalizePath(control_parent, winslash = '/', mustWork = TRUE),
      basename(control)
    )
    if (file.exists(control) || dir.exists(control)) {
      launcher_error(
        'The --control path must not exist at startup.',
        'arguments'
      )
    }
  }
  runtime_library <- file.path(R.home(), 'library')
  if (!dir.exists(runtime_library)) {
    launcher_error(
      'The bundled R library directory is missing.',
      'runtime'
    )
  }
  .libPaths(unique(c(runtime_library, .libPaths())))
  if (!requireNamespace('shiny', quietly = TRUE)) {
    launcher_error(
      "The bundled runtime does not contain the 'shiny' package.",
      'runtime'
    )
  }
  if (!is.null(control) &&
      !requireNamespace('later', quietly = TRUE)) {
    launcher_error(
      "The bundled runtime does not contain the 'later' package.",
      'runtime'
    )
  }
  Sys.setenv(RPACKIT_SESSION_TOKEN = token)
  if (!is.null(control)) {
    watch_control <- NULL
    watch_control <- function() {
      if (file.exists(control)) {
        emit_event('stopping', list(reason = 'control-file'))
        shiny::stopApp()
      } else {
        later::later(watch_control, delay = 0.1)
      }
      invisible(NULL)
    }
    later::later(watch_control, delay = 0.1)
  }
  emit_event(
    'starting',
    list(
      pid = Sys.getpid(),
      host = '127.0.0.1',
      port = port,
      token_enforced = FALSE,
      graceful_stop = !is.null(control)
    )
  )
  shiny::runApp(
    app,
    host = '127.0.0.1',
    port = port,
    launch.browser = FALSE,
    quiet = TRUE
  )
  emit_event('stopped', list(pid = Sys.getpid()))
  quit(save = 'no', status = 0L, runLast = FALSE)
}
tryCatch(
  main(),
  error = function(error) {
    phase <- if (inherits(error, 'rpackit_launcher_error')) {
      error$phase
    } else {
      'runtime'
    }
    emit_event(
      'error',
      list(
        phase = phase,
        message = conditionMessage(error),
        pid = Sys.getpid()
      )
    )
    message('rpackit launcher error: ', conditionMessage(error))
    quit(save = 'no', status = 1L, runLast = FALSE)
  }
)
