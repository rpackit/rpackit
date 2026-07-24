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

.read_app_source <- function(files) {
  if (!length(files)) {
    return(character())
  }
  unlist(lapply(files, function(file) {
    tryCatch(
      readLines(file, warn = FALSE, encoding = "UTF-8"),
      error = function(error) character()
    )
  }), use.names = FALSE)
}

.extract_packages <- function(source) {
  text <- paste(source, collapse = "\n")
  call_pattern <- "(?:library|require)\\s*\\(\\s*[\"']?([A-Za-z][A-Za-z0-9.]*)"
  namespace_pattern <- "\\b([A-Za-z][A-Za-z0-9.]*)\\s*:::{0,1}"
  calls <- regmatches(text, gregexpr(call_pattern, text, perl = TRUE))[[1L]]
  call_packages <- if (length(calls) && !identical(calls, character(0))) {
    sub(call_pattern, "\\1", calls, perl = TRUE)
  } else {
    character()
  }
  namespaces <- regmatches(
    text,
    gregexpr(namespace_pattern, text, perl = TRUE)
  )[[1L]]
  namespace_packages <- if (length(namespaces) &&
                            !identical(namespaces, character(0))) {
    sub("\\s*:::{0,1}$", "", namespaces, perl = TRUE)
  } else {
    character()
  }
  sort(unique(c(call_packages, namespace_packages)))
}

.target_row <- function(target, status, reasons) {
  data.frame(
    target = target,
    status = status,
    reason = if (length(reasons)) paste(reasons, collapse = "; ") else "",
    stringsAsFactors = FALSE
  )
}

#' Inspect a Shiny application and recommend packaging targets
#'
#' Recognizes single-file `app.R` and split `ui.R`/`server.R` layouts. Source
#' inspection identifies package calls and common blockers for browser-only
#' static builds. No application code is executed.
#'
#' @param app_dir Path to the application directory.
#' @param quiet Suppress the human-readable report.
#' @return An `rpackit_app_check` object.
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
  source <- .read_app_source(files)
  packages <- .extract_packages(source)
  source_text <- paste(source, collapse = "\n")
  has_system_calls <- grepl(
    "\\b(system|system2|shell)\\s*\\(",
    source_text,
    perl = TRUE
  )
  has_reticulate <- "reticulate" %in% packages ||
    grepl("\\breticulate\\s*::", source_text, perl = TRUE)
  native_risk <- intersect(
    packages,
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
    static_blockers <- c(static_blockers, "system command calls")
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
    desktop_risks <- c(desktop_risks, "external commands must be bundled")
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
