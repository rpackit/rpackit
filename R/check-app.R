.app_r_files <- function(path) {
  files <- list.files(
    path,
    pattern = "\\.[Rr]$",
    recursive = TRUE,
    full.names = TRUE
  )
  normalized <- gsub("\\\\", "/", files)
  files[!grepl("/(renv/library|\\.git|packrat/lib|build|dist)/", normalized)]
}

.target_row <- function(target, status, reasons) {
  data.frame(
    target = target,
    status = status,
    reason = if (length(reasons)) paste(reasons, collapse = "; ") else "",
    stringsAsFactors = FALSE
  )
}

.empty_app_system_calls <- function() {
  data.frame(
    call = character(),
    file = character(),
    line = integer(),
    stringsAsFactors = FALSE
  )
}

.app_parse_children <- function(parse_data, parent, child_rows = NULL) {
  indices <- if (is.null(child_rows)) {
    which(parse_data$parent == parent)
  } else {
    child_rows[[parent + 1L]]
  }
  if (is.null(indices) || !length(indices)) {
    return(parse_data[FALSE, , drop = FALSE])
  }
  children <- parse_data[indices, , drop = FALSE]
  if (nrow(children) > 1L) {
    children <- children[
      order(
        children$line1,
        children$col1,
        children$line2,
        children$col2,
        children$id
      ),
      ,
      drop = FALSE
    ]
  }
  children
}

.app_call_token_name <- function(text) {
  result <- as.character(text)
  backticked <- nchar(result) >= 2L &
    startsWith(result, "`") &
    endsWith(result, "`")
  result[backticked] <- vapply(result[backticked], function(value) {
    symbol <- tryCatch(str2lang(value), error = function(error) NULL)
    if (is.symbol(symbol)) as.character(symbol) else value
  }, character(1), USE.NAMES = FALSE)
  unname(result)
}

.app_system_call_head <- function(parse_data, expression_id, child_rows) {
  children <- .app_parse_children(parse_data, expression_id, child_rows)
  if (!nrow(children)) {
    return(NULL)
  }
  if (nrow(children) == 1L &&
      children$token[[1L]] %in% c("SYMBOL_FUNCTION_CALL", "SYMBOL")) {
    call <- .app_call_token_name(children$text[[1L]])
    if (call %in% c("system", "system2", "shell")) {
      return(list(call = call, token = children[1L, , drop = FALSE]))
    }
    return(NULL)
  }
  if (nrow(children) == 3L &&
      identical(children$token, c("'('", "expr", "')'"))) {
    return(.app_system_call_head(
      parse_data,
      children$id[[2L]],
      child_rows
    ))
  }
  if (nrow(children) == 3L &&
      children$token[[1L]] == "SYMBOL_PACKAGE" &&
      children$token[[2L]] %in% c("NS_GET", "NS_GET_INT") &&
      children$token[[3L]] %in% c("SYMBOL_FUNCTION_CALL", "SYMBOL")) {
    namespace <- .app_call_token_name(children$text[[1L]])
    call <- .app_call_token_name(children$text[[3L]])
    if (identical(namespace, "base") &&
        call %in% c("system", "system2", "shell")) {
      return(list(call = call, token = children[3L, , drop = FALSE]))
    }
  }
  NULL
}

.app_system_calls_in_file <- function(path, app_path) {
  relative_path <- .relative_app_path(path, app_path)
  lines <- .read_dependency_lines(path, relative_path, "R source")
  previous_options <- options(keep.parse.data = TRUE)
  on.exit(options(previous_options), add = TRUE)
  source_file <- srcfilecopy(relative_path, lines, isFile = TRUE)
  expressions <- tryCatch(
    parse(text = lines, srcfile = source_file, keep.source = TRUE),
    error = function(error) {
      cli::cli_abort(
        "Cannot parse R source {.path {relative_path}}: {conditionMessage(error)}",
        class = c(
          "rpackit_app_source_parse_error",
          "rpackit_app_parse_error"
        )
      )
    }
  )
  parse_data <- utils::getParseData(source_file)
  if (is.null(parse_data)) {
    if (length(expressions)) {
      cli::cli_abort(
        "Cannot inspect parsed R source {.path {relative_path}}.",
        class = c(
          "rpackit_app_source_parse_data_error",
          "rpackit_app_parse_error"
        )
      )
    }
    return(.empty_app_system_calls())
  }
  symbol_rows <- which(
    parse_data$token %in% c("SYMBOL_FUNCTION_CALL", "SYMBOL")
  )
  symbol_names <- .app_call_token_name(parse_data$text[symbol_rows])
  candidate_rows <- symbol_rows[
    symbol_names %in% c("system", "system2", "shell")
  ]
  if (!length(candidate_rows)) {
    return(.empty_app_system_calls())
  }
  child_indices <- which(parse_data$parent >= 0L)
  child_groups <- split(
    child_indices,
    parse_data$parent[child_indices]
  )
  child_rows <- vector("list", max(parse_data$id) + 1L)
  child_rows[as.integer(names(child_groups)) + 1L] <- unname(child_groups)
  row_by_id <- integer(max(parse_data$id) + 1L)
  row_by_id[parse_data$id + 1L] <- seq_len(nrow(parse_data))
  calls <- lapply(candidate_rows, function(candidate_row) {
    head_id <- parse_data$parent[[candidate_row]]
    repeat {
      head_row <- row_by_id[[head_id + 1L]]
      parent_id <- parse_data$parent[[head_row]]
      if (parent_id <= 0L) {
        break
      }
      parent_children <- .app_parse_children(
        parse_data,
        parent_id,
        child_rows
      )
      is_group <- nrow(parent_children) == 3L &&
        identical(parent_children$token, c("'('", "expr", "')'")) &&
        parent_children$id[[2L]] == head_id
      if (!is_group) {
        break
      }
      head_id <- parent_id
    }
    head_row <- row_by_id[[head_id + 1L]]
    call_id <- parse_data$parent[[head_row]]
    if (call_id <= 0L) {
      return(NULL)
    }
    call_children <- .app_parse_children(parse_data, call_id, child_rows)
    if (nrow(call_children) < 2L ||
        call_children$token[[1L]] != "expr" ||
        call_children$id[[1L]] != head_id ||
        call_children$token[[2L]] != "'('") {
      return(NULL)
    }
    resolved <- .app_system_call_head(parse_data, head_id, child_rows)
    if (is.null(resolved) ||
        resolved$token$id[[1L]] != parse_data$id[[candidate_row]]) {
      return(NULL)
    }
    resolved
  })
  calls <- calls[!vapply(calls, is.null, logical(1))]
  if (!length(calls)) {
    return(.empty_app_system_calls())
  }
  call_names <- vapply(calls, `[[`, character(1), "call")
  call_tokens <- do.call(rbind, lapply(calls, `[[`, "token"))
  order_index <- order(
    call_tokens$line1,
    call_tokens$col1,
    call_tokens$line2,
    call_tokens$col2,
    call_tokens$id
  )
  data.frame(
    call = call_names[order_index],
    file = rep(relative_path, length(calls)),
    line = as.integer(call_tokens$line1[order_index]),
    stringsAsFactors = FALSE
  )
}

.app_system_calls <- function(files, app_path) {
  if (!length(files)) {
    return(.empty_app_system_calls())
  }
  rows <- lapply(files, .app_system_calls_in_file, app_path = app_path)
  rows <- rows[vapply(rows, nrow, integer(1)) > 0L]
  if (!length(rows)) {
    return(.empty_app_system_calls())
  }
  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  result
}

.app_call_locations <- function(calls, limit = 3L) {
  locations <- unique(ifelse(
    is.na(calls$line),
    calls$file,
    paste0(calls$file, ":", calls$line)
  ))
  shown <- utils::head(locations, limit)
  suffix <- if (length(locations) > limit) {
    paste0(" (+", length(locations) - limit, " more)")
  } else {
    ""
  }
  paste0(paste(shown, collapse = ", "), suffix)
}

#' Inspect a Shiny application and recommend packaging targets
#'
#' Recognizes single-file `app.R` and split `ui.R`/`server.R` layouts. Source
#' inspection identifies package calls and common blockers for browser-only
#' static builds. Direct calls to `system()`, `system2()`, and `shell()` are
#' detected from parsed R syntax rather than raw text, so comments, string
#' contents, object methods, and same-named functions in non-base namespaces
#' do not create false blockers. Calls inside quoted language expressions are
#' conservatively reported because their later evaluation cannot be determined
#' statically. The call, file, and source line are returned in
#' `findings$system_calls`. No application code is executed.
#'
#' @param app_dir Path to the application directory.
#' @param quiet Suppress the human-readable report.
#' @return An `rpackit_app_check` object with the detected layout, dependency
#'   plan, target matrix, recommendations, and structured findings.
#' @export
#' @examples
#' app <- tempfile("shiny-app-")
#' dir.create(app)
#' writeLines(
#'   "shiny::shinyApp(shiny::fluidPage('hello'), function(input, output) {})",
#'   file.path(app, "app.R")
#' )
#' check_app(app)
check_app <- function(app_dir, quiet = FALSE) {
  if (!is.character(app_dir) || length(app_dir) != 1L ||
      is.na(app_dir) || !dir.exists(app_dir)) {
    cli::cli_abort("{.arg app_dir} must be an existing directory.")
  }
  path <- normalizePath(app_dir, winslash = "/", mustWork = TRUE)
  top_level <- list.files(path, all.files = FALSE, recursive = FALSE)
  single_file <- "app.R" %in% top_level
  split_files <- all(c("ui.R", "server.R") %in% top_level)
  app_type <- if (single_file) {
    "shiny-single-file"
  } else if (split_files) {
    "shiny-split"
  } else {
    "unknown"
  }
  files <- .app_r_files(path)
  dependency_plan <- plan_dependencies(path)
  source_references <- dependency_plan$references[
    dependency_plan$references$origin == "source",
    ,
    drop = FALSE
  ]
  packages <- sort(unique(source_references$package))
  direct_dependencies <- dependency_plan$dependencies$package[
    dependency_plan$dependencies$direct
  ]
  risk_packages <- sort(unique(c(packages, direct_dependencies)))
  system_calls <- .app_system_calls(files, path)
  has_system_calls <- nrow(system_calls) > 0L
  has_reticulate <- "reticulate" %in% risk_packages
  native_risk <- intersect(
    risk_packages,
    c(
      "BiocManager", "DESeq2", "Rsamtools", "GenomicRanges", "rJava",
      "sf", "terra", "arrow", "duckdb", "torch", "tensorflow"
    )
  )
  data_files <- list.files(
    path,
    pattern = "\\.(rds|rda|rdata|h5|hdf5|bam|bigwig|parquet|feather)$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  large_data <- if (length(data_files)) {
    data_files[file.info(data_files)$size > 50 * 1024^2]
  } else {
    character()
  }
  structure_valid <- app_type != "unknown"
  static_blockers <- character()
  if (!structure_valid) {
    static_blockers <- c(static_blockers, "unrecognized app layout")
  }
  if (has_system_calls) {
    static_blockers <- c(
      static_blockers,
      paste0(
        "system command calls at ",
        .app_call_locations(system_calls)
      )
    )
  }
  if (has_reticulate) {
    static_blockers <- c(static_blockers, "reticulate/Python dependency")
  }
  if (length(native_risk)) {
    static_blockers <- c(
      static_blockers,
      paste0("native or browser-risk packages: ", paste(native_risk, collapse = ", "))
    )
  }
  if (length(large_data)) {
    static_blockers <- c(static_blockers, "files larger than 50 MiB")
  }
  static_status <- if (length(static_blockers)) "no" else "maybe"
  static_reason <- if (length(static_blockers)) {
    static_blockers
  } else {
    "requires package-level shinylive/webR compatibility verification"
  }
  desktop_risks <- character()
  if (!structure_valid) {
    desktop_risks <- c(desktop_risks, "unrecognized app layout")
  }
  if (has_system_calls) {
    desktop_risks <- c(
      desktop_risks,
      paste0(
        "external commands at ",
        .app_call_locations(system_calls),
        " must be bundled"
      )
    )
  }
  if (has_reticulate) {
    desktop_risks <- c(desktop_risks, "Python runtime must be bundled")
  }
  desktop_status <- if (!structure_valid) {
    "no"
  } else if (length(desktop_risks)) {
    "maybe"
  } else {
    "yes"
  }
  server_status <- if (structure_valid) "yes" else "no"
  targets <- rbind(
    .target_row(
      "portable desktop",
      desktop_status,
      if (length(desktop_risks)) desktop_risks else "supported Shiny layout"
    ),
    .target_row("static web", static_status, static_reason),
    .target_row(
      "dynamic server",
      server_status,
      if (structure_valid) "supported Shiny layout" else "unrecognized app layout"
    )
  )
  findings <- list(
    has_app_r = single_file,
    has_ui_server = split_files,
    has_global_r = "global.R" %in% top_level,
    has_www = dir.exists(file.path(path, "www")),
    has_renv_lock = file.exists(file.path(path, "renv.lock")),
    has_description = file.exists(file.path(path, "DESCRIPTION")),
    has_system_calls = has_system_calls,
    system_calls = system_calls,
    has_reticulate = has_reticulate,
    native_risk_packages = native_risk,
    large_data_files = sub(
      paste0("^", gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", path), "/?"),
      "",
      gsub("\\\\", "/", large_data)
    )
  )
  recommendation <- c(
    if (desktop_status == "yes") {
      "Portable desktop is the strongest offline target."
    },
    if (static_status == "maybe") {
      "Run a shinylive export test before promising static-web support."
    },
    if (static_status == "no") {
      paste0("Do not build static web: ", paste(static_blockers, collapse = "; "), ".")
    },
    if (server_status == "yes") {
      "Dynamic server packaging is supported."
    }
  )
  result <- structure(
    list(
      path = path,
      app_type = app_type,
      r_files = sub(
        paste0("^", gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", path), "/?"),
        "",
        gsub("\\\\", "/", files)
      ),
      packages = packages,
      dependency_plan = dependency_plan,
      findings = findings,
      targets = targets,
      recommendation = recommendation
    ),
    class = "rpackit_app_check"
  )
  if (!isTRUE(quiet)) {
    print(result)
  }
  invisible(result)
}

#' @export
print.rpackit_app_check <- function(x, ...) {
  cli::cli_h1("rpackit app check")
  cli::cli_text("Path: {x$path}")
  cli::cli_text("Detected app type: {x$app_type}")
  cli::cli_h2("Packages")
  if (length(x$packages)) {
    cli::cli_bullets(stats::setNames(as.list(x$packages), rep("*", length(x$packages))))
  } else {
    cli::cli_text("No package calls detected.")
  }
  cli::cli_h2("Target matrix")
  for (index in seq_len(nrow(x$targets))) {
    row <- x$targets[index, ]
    cli::cli_text("{row$target}: {toupper(row$status)} - {row$reason}")
  }
  cli::cli_h2("Recommendation")
  cli::cli_bullets(stats::setNames(as.list(x$recommendation), rep("*", length(x$recommendation))))
  invisible(x)
}
