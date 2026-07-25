.empty_dependency_references <- function() {
  data.frame(
    package = character(),
    origin = character(),
    detail = character(),
    requirement = character(),
    file = character(),
    line = integer(),
    stringsAsFactors = FALSE
  )
}

.empty_dependency_diagnostics <- function() {
  data.frame(
    severity = character(),
    code = character(),
    file = character(),
    line = integer(),
    message = character(),
    stringsAsFactors = FALSE
  )
}

.dependency_reference <- function(package, origin, detail, requirement = NA_character_,
                                  file, line = NA_integer_) {
  data.frame(
    package = package,
    origin = origin,
    detail = detail,
    requirement = requirement,
    file = file,
    line = as.integer(line),
    stringsAsFactors = FALSE
  )
}

.dependency_diagnostic <- function(code, file, line, message,
                                   severity = "warning") {
  data.frame(
    severity = severity,
    code = code,
    file = file,
    line = as.integer(line),
    message = message,
    stringsAsFactors = FALSE
  )
}

.collapse_dependency_rows <- function(rows, empty) {
  if (!length(rows)) {
    return(empty())
  }
  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  result
}

.relative_app_path <- function(path, app_path) {
  normalized_path <- gsub(
    "\\\\",
    "/",
    normalizePath(path, winslash = "/", mustWork = FALSE)
  )
  normalized_app <- gsub(
    "\\\\",
    "/",
    normalizePath(app_path, winslash = "/", mustWork = TRUE)
  )
  prefix <- paste0(normalized_app, "/")
  if (startsWith(normalized_path, prefix)) {
    substring(normalized_path, nchar(prefix) + 1L)
  } else {
    basename(normalized_path)
  }
}

.read_dependency_lines <- function(path, relative_path, kind) {
  tryCatch(
    suppressWarnings(readLines(path, warn = FALSE, encoding = "UTF-8")),
    error = function(error) {
      cli::cli_abort(
        "Cannot read {kind} {.path {relative_path}}: {conditionMessage(error)}",
        class = c(
          paste0("rpackit_dependency_", gsub("[^a-z]+", "_", kind), "_read_error"),
          "rpackit_dependency_read_error"
        )
      )
    }
  )
}

.call_package_argument <- function(node, function_name) {
  arguments <- as.list(node)[-1L]
  argument_names <- names(as.list(node))[-1L]
  if (!length(arguments)) {
    return(NULL)
  }
  argument_index <- if ("package" %in% argument_names) {
    which(argument_names == "package")[[1L]]
  } else {
    1L
  }
  if (identical(arguments[argument_index][[1L]], quote(expr = ))) {
    return(NULL)
  }
  package_argument <- arguments[argument_index][[1L]]
  if (is.character(package_argument) && length(package_argument) == 1L) {
    return(package_argument)
  }
  if (function_name %in% c("library", "require") &&
      is.symbol(package_argument)) {
    character_only <- if ("character.only" %in% argument_names) {
      arguments[
        which(argument_names == "character.only")[[1L]]
      ][[1L]]
    } else {
      FALSE
    }
    if (!isTRUE(character_only)) {
      return(as.character(package_argument))
    }
  }
  NULL
}

.source_parse_locations <- function(parse_data) {
  if (is.null(parse_data) || !nrow(parse_data)) {
    return(data.frame(
      key = character(),
      line = integer(),
      stringsAsFactors = FALSE
    ))
  }
  call_rows <- parse_data[
    parse_data$token == "SYMBOL_FUNCTION_CALL" &
      parse_data$text %in% c(
        "library", "require", "requireNamespace", "loadNamespace"
      ),
    ,
    drop = FALSE
  ]
  call_locations <- if (nrow(call_rows)) {
    data.frame(
      key = paste0("call:", call_rows$text),
      line = as.integer(call_rows$line1),
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(key = character(), line = integer(), stringsAsFactors = FALSE)
  }
  package_rows <- parse_data[
    parse_data$token == "SYMBOL_PACKAGE",
    ,
    drop = FALSE
  ]
  namespace_locations <- lapply(seq_len(nrow(package_rows)), function(index) {
    row <- package_rows[index, , drop = FALSE]
    sibling <- parse_data[
      parse_data$parent == row$parent &
        parse_data$token %in% c("NS_GET", "NS_GET_INT"),
      ,
      drop = FALSE
    ]
    operator <- if (nrow(sibling) &&
                    identical(sibling$token[[1L]], "NS_GET_INT")) {
      ":::"
    } else {
      "::"
    }
    data.frame(
      key = paste0("namespace:", row$text, ":", operator),
      line = as.integer(row$line1),
      stringsAsFactors = FALSE
    )
  })
  namespace_locations <- if (length(namespace_locations)) {
    do.call(rbind, namespace_locations)
  } else {
    data.frame(key = character(), line = integer(), stringsAsFactors = FALSE)
  }
  locations <- rbind(call_locations, namespace_locations)
  rownames(locations) <- NULL
  locations
}

.attach_source_locations <- function(records, locations) {
  if (!length(records)) {
    return(records)
  }
  counters <- new.env(parent = emptyenv())
  lapply(records, function(record) {
    key <- record$key
    occurrence <- if (exists(key, envir = counters, inherits = FALSE)) {
      get(key, envir = counters, inherits = FALSE) + 1L
    } else {
      1L
    }
    assign(key, occurrence, envir = counters)
    candidates <- locations$line[locations$key == key]
    record$line <- if (length(candidates) >= occurrence) {
      candidates[[occurrence]]
    } else {
      NA_integer_
    }
    record
  })
}

.parse_r_dependencies <- function(path, app_path) {
  relative_path <- .relative_app_path(path, app_path)
  lines <- .read_dependency_lines(path, relative_path, "R source")
  source_file <- srcfilecopy(relative_path, lines, isFile = TRUE)
  expressions <- tryCatch(
    parse(text = lines, srcfile = source_file, keep.source = TRUE),
    error = function(error) {
      cli::cli_abort(
        "Cannot parse R source {.path {relative_path}}: {conditionMessage(error)}",
        class = c(
          "rpackit_dependency_source_parse_error",
          "rpackit_dependency_parse_error"
        )
      )
    }
  )
  events <- list()
  append_record <- function(package, detail, key) {
    events[[length(events) + 1L]] <<- list(
      type = "reference",
      package = package,
      detail = detail,
      key = key,
      line = NA_integer_
    )
  }
  append_diagnostic <- function(function_name, key) {
    events[[length(events) + 1L]] <<- list(
      type = "diagnostic",
      function_name = function_name,
      key = key,
      line = NA_integer_
    )
  }
  walk <- NULL
  walk_child <- function(child) {
    if (missing(child)) {
      return(invisible(NULL))
    }
    force(child)
    if (identical(child, quote(expr = ))) {
      return(invisible(NULL))
    }
    walk(child)
  }
  walk <- function(node) {
    if (is.call(node)) {
      head <- node[[1L]]
      if (is.symbol(head)) {
        function_name <- as.character(head)
        if (function_name %in% c(
          "library", "require", "requireNamespace", "loadNamespace"
        )) {
          key <- paste0("call:", function_name)
          package <- .call_package_argument(node, function_name)
          if (is.null(package)) {
            append_diagnostic(function_name, key)
          } else {
            append_record(package, function_name, key)
          }
        } else if (function_name %in% c("::", ":::") &&
                   length(node) >= 3L) {
          package_node <- node[[2L]]
          package <- if (is.symbol(package_node) ||
                         (is.character(package_node) &&
                          length(package_node) == 1L)) {
            as.character(package_node)
          } else {
            NULL
          }
          if (!is.null(package)) {
            append_record(
              package,
              function_name,
              paste0("namespace:", package, ":", function_name)
            )
          }
        }
      }
      children <- as.list(node)
      if (length(children)) {
        for (index in seq_along(children)) {
          walk_child(children[index][[1L]])
        }
      }
    } else if (is.expression(node) || is.pairlist(node)) {
      children <- as.list(node)
      for (index in seq_along(children)) {
        walk_child(children[index][[1L]])
      }
    }
    invisible(NULL)
  }
  walk(expressions)
  locations <- .source_parse_locations(utils::getParseData(source_file))
  events <- .attach_source_locations(events, locations)
  records <- events[vapply(events, `[[`, character(1), "type") == "reference"]
  diagnostics <- events[
    vapply(events, `[[`, character(1), "type") == "diagnostic"
  ]
  references <- lapply(records, function(record) {
    .dependency_reference(
      package = record$package,
      origin = "source",
      detail = record$detail,
      file = relative_path,
      line = record$line
    )
  })
  diagnostic_rows <- lapply(diagnostics, function(diagnostic) {
    .dependency_diagnostic(
      code = "dynamic-package-name",
      file = relative_path,
      line = diagnostic$line,
      message = paste0(
        "Could not determine the package name in ",
        diagnostic$function_name,
        "()."
      )
    )
  })
  list(
    references = .collapse_dependency_rows(
      references,
      .empty_dependency_references
    ),
    diagnostics = .collapse_dependency_rows(
      diagnostic_rows,
      .empty_dependency_diagnostics
    )
  )
}

.description_field_line <- function(lines, field) {
  matches <- grep(
    paste0("^", gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", field), "\\s*:"),
    lines,
    perl = TRUE
  )
  if (length(matches)) as.integer(matches[[1L]]) else NA_integer_
}

.parse_description_requirement <- function(value, field, relative_path) {
  value <- trimws(value)
  match <- regexec(
    "^([A-Za-z][A-Za-z0-9.]*)\\s*(?:\\(([^)]+)\\))?$",
    value,
    perl = TRUE
  )
  pieces <- regmatches(value, match)[[1L]]
  if (!length(pieces)) {
    cli::cli_abort(
      "Invalid dependency entry {value} in {field} of {.path {relative_path}}.",
      class = c(
        "rpackit_dependency_description_parse_error",
        "rpackit_dependency_parse_error"
      )
    )
  }
  list(
    package = pieces[[2L]],
    requirement = if (length(pieces) >= 3L && nzchar(pieces[[3L]])) {
      trimws(pieces[[3L]])
    } else {
      NA_character_
    }
  )
}

.parse_description_dependencies <- function(path, app_path, include_suggests) {
  if (!file.exists(path)) {
    return(list(
      references = .empty_dependency_references(),
      r_constraint = NA_character_
    ))
  }
  relative_path <- .relative_app_path(path, app_path)
  lines <- .read_dependency_lines(path, relative_path, "DESCRIPTION")
  connection <- textConnection(lines)
  on.exit(close(connection), add = TRUE)
  description <- tryCatch(
    read.dcf(connection, all = FALSE),
    error = function(error) {
      cli::cli_abort(
        "Cannot parse {.path {relative_path}}: {conditionMessage(error)}",
        class = c(
          "rpackit_dependency_description_parse_error",
          "rpackit_dependency_parse_error"
        )
      )
    }
  )
  if (nrow(description) != 1L) {
    cli::cli_abort(
      "{.path {relative_path}} must contain exactly one DCF record.",
      class = c(
        "rpackit_dependency_description_parse_error",
        "rpackit_dependency_parse_error"
      )
    )
  }
  fields <- c("Depends", "Imports", "LinkingTo")
  if (isTRUE(include_suggests)) {
    fields <- c(fields, "Suggests", "Enhances")
  }
  references <- list()
  r_constraint <- NA_character_
  for (field in fields) {
    if (!field %in% colnames(description) ||
        is.na(description[1L, field]) ||
        !nzchar(trimws(description[1L, field]))) {
      next
    }
    entries <- strsplit(description[1L, field], ",", fixed = TRUE)[[1L]]
    for (entry in entries) {
      requirement <- .parse_description_requirement(
        entry,
        field,
        relative_path
      )
      if (identical(requirement$package, "R")) {
        if (!is.na(requirement$requirement)) {
          r_constraint <- requirement$requirement
        }
        next
      }
      references[[length(references) + 1L]] <- .dependency_reference(
        package = requirement$package,
        origin = "DESCRIPTION",
        detail = field,
        requirement = requirement$requirement,
        file = relative_path,
        line = .description_field_line(lines, field)
      )
    }
  }
  list(
    references = .collapse_dependency_rows(
      references,
      .empty_dependency_references
    ),
    r_constraint = r_constraint
  )
}

.lock_package_line <- function(lines, package) {
  escaped <- gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", package)
  matches <- grep(
    paste0('^\\s*"', escaped, '"\\s*:'),
    lines,
    perl = TRUE
  )
  if (length(matches)) as.integer(matches[[1L]]) else NA_integer_
}

.scalar_lock_value <- function(value, field, package, relative_path,
                               required = FALSE) {
  if (is.null(value)) {
    if (isTRUE(required)) {
      cli::cli_abort(
        "Package {package} in {.path {relative_path}} has no {field} field.",
        class = c(
          "rpackit_dependency_lockfile_parse_error",
          "rpackit_dependency_parse_error"
        )
      )
    }
    return(NA_character_)
  }
  if (!is.character(value) || length(value) != 1L || is.na(value) ||
      !nzchar(value)) {
    cli::cli_abort(
      "Package {package} in {.path {relative_path}} has an invalid {field} field.",
      class = c(
        "rpackit_dependency_lockfile_parse_error",
        "rpackit_dependency_parse_error"
      )
    )
  }
  value
}

.lock_remote <- function(record, package, relative_path) {
  fields <- c(
    "RemoteType", "RemoteHost", "RemoteUsername", "RemoteRepo",
    "RemoteRef", "RemoteSha", "RemoteUrl"
  )
  values <- stats::setNames(lapply(fields, function(field) {
    .scalar_lock_value(
      record[[field]],
      field,
      package,
      relative_path
    )
  }), fields)
  if (!is.na(values$RemoteUrl)) {
    return(values$RemoteUrl)
  }
  parts <- c(
    values$RemoteHost,
    values$RemoteUsername,
    values$RemoteRepo
  )
  repository <- paste(parts[!is.na(parts)], collapse = "/")
  if (!nzchar(repository)) {
    return(NA_character_)
  }
  prefix <- if (!is.na(values$RemoteType)) {
    paste0(values$RemoteType, ":")
  } else {
    ""
  }
  ref <- if (!is.na(values$RemoteRef)) {
    paste0("@", values$RemoteRef)
  } else {
    ""
  }
  sha <- if (!is.na(values$RemoteSha)) {
    paste0("#", values$RemoteSha)
  } else {
    ""
  }
  paste0(prefix, repository, ref, sha)
}

.parse_lockfile_dependencies <- function(path, app_path) {
  if (!file.exists(path)) {
    return(list(
      references = .empty_dependency_references(),
      records = list(),
      r_version = NA_character_
    ))
  }
  relative_path <- .relative_app_path(path, app_path)
  lines <- .read_dependency_lines(path, relative_path, "renv lockfile")
  lock <- tryCatch(
    jsonlite::fromJSON(
      paste(lines, collapse = "\n"),
      simplifyVector = FALSE
    ),
    error = function(error) {
      cli::cli_abort(
        "Cannot parse {.path {relative_path}}: {conditionMessage(error)}",
        class = c(
          "rpackit_dependency_lockfile_parse_error",
          "rpackit_dependency_parse_error"
        )
      )
    }
  )
  if (!is.list(lock) || is.null(lock$Packages) ||
      !is.list(lock$Packages)) {
    cli::cli_abort(
      "{.path {relative_path}} must contain a Packages object.",
      class = c(
        "rpackit_dependency_lockfile_parse_error",
        "rpackit_dependency_parse_error"
      )
    )
  }
  package_names <- names(lock$Packages)
  if (is.null(package_names) && !length(lock$Packages)) {
    package_names <- character()
  }
  if (is.null(package_names) ||
      any(!grepl("^[A-Za-z][A-Za-z0-9.]*$", package_names))) {
    cli::cli_abort(
      "{.path {relative_path}} contains an invalid package name.",
      class = c(
        "rpackit_dependency_lockfile_parse_error",
        "rpackit_dependency_parse_error"
      )
    )
  }
  references <- list()
  records <- list()
  for (package in package_names) {
    record <- lock$Packages[[package]]
    if (!is.list(record)) {
      cli::cli_abort(
        "Package {package} in {.path {relative_path}} must be an object.",
        class = c(
          "rpackit_dependency_lockfile_parse_error",
          "rpackit_dependency_parse_error"
        )
      )
    }
    version <- .scalar_lock_value(
      record$Version,
      "Version",
      package,
      relative_path,
      required = TRUE
    )
    records[[package]] <- list(
      version = version,
      source = .scalar_lock_value(
        record$Source,
        "Source",
        package,
        relative_path
      ),
      repository = .scalar_lock_value(
        record$Repository,
        "Repository",
        package,
        relative_path
      ),
      remote = .lock_remote(record, package, relative_path)
    )
    references[[length(references) + 1L]] <- .dependency_reference(
      package = package,
      origin = "renv.lock",
      detail = if (!is.na(records[[package]]$source)) {
        records[[package]]$source
      } else {
        "package record"
      },
      requirement = version,
      file = relative_path,
      line = .lock_package_line(lines, package)
    )
  }
  r_version <- if (is.list(lock$R)) {
    .scalar_lock_value(
      lock$R$Version,
      "R.Version",
      "R",
      relative_path
    )
  } else {
    NA_character_
  }
  list(
    references = .collapse_dependency_rows(
      references,
      .empty_dependency_references
    ),
    records = records,
    r_version = r_version
  )
}

.combine_dependency_plan <- function(references, lock_records) {
  package_names <- sort(unique(references$package))
  required_roles <- c("Depends", "Imports", "LinkingTo")
  rows <- lapply(package_names, function(package) {
    package_references <- references[references$package == package, , drop = FALSE]
    lock_record <- lock_records[[package]]
    roles <- unique(package_references$detail[
      package_references$origin == "DESCRIPTION"
    ])
    constraints <- unique(package_references$requirement[
      package_references$origin == "DESCRIPTION" &
        !is.na(package_references$requirement)
    ])
    labels <- vapply(seq_len(nrow(package_references)), function(index) {
      reference <- package_references[index, , drop = FALSE]
      if (identical(reference$origin, "source")) {
        location <- if (is.na(reference$line)) {
          reference$file
        } else {
          paste0(reference$file, ":", reference$line)
        }
        paste0("source:", reference$detail, "@", location)
      } else if (identical(reference$origin, "DESCRIPTION")) {
        paste0("DESCRIPTION:", reference$detail)
      } else {
        "renv.lock"
      }
    }, character(1))
    data.frame(
      package = package,
      version = if (is.null(lock_record)) NA_character_ else lock_record$version,
      constraint = if (length(constraints)) {
        paste(constraints, collapse = ", ")
      } else {
        NA_character_
      },
      roles = if (length(roles)) paste(roles, collapse = ", ") else NA_character_,
      direct = any(package_references$origin %in% c("source", "DESCRIPTION")),
      required = !is.null(lock_record) ||
        any(package_references$origin == "source") ||
        any(roles %in% required_roles),
      locked = !is.null(lock_record),
      lock_source = if (is.null(lock_record)) {
        NA_character_
      } else {
        lock_record$source
      },
      repository = if (is.null(lock_record)) {
        NA_character_
      } else {
        lock_record$repository
      },
      remote = if (is.null(lock_record)) {
        NA_character_
      } else {
        lock_record$remote
      },
      provenance = paste(unique(labels), collapse = "; "),
      stringsAsFactors = FALSE
    )
  })
  if (!length(rows)) {
    return(data.frame(
      package = character(),
      version = character(),
      constraint = character(),
      roles = character(),
      direct = logical(),
      required = logical(),
      locked = logical(),
      lock_source = character(),
      repository = character(),
      remote = character(),
      provenance = character(),
      stringsAsFactors = FALSE
    ))
  }
  result <- do.call(rbind, rows)
  rownames(result) <- NULL
  result
}

#' Plan R application dependencies without executing application code
#'
#' `plan_dependencies()` combines three sources of dependency information:
#'
#' 1. `renv.lock` has highest precedence for exact package versions, sources,
#'    and repositories.
#' 2. `DESCRIPTION` supplies direct dependency roles and version constraints.
#' 3. Parsed R calls discover packages that are used but not declared.
#'
#' The sources are complementary rather than mutually exclusive. The
#' `references` table retains every observation, while the `dependencies` table
#' contains one resolved row per package. R source is parsed with [parse()];
#' comments and string contents are never treated as package calls, and
#' application code is never evaluated.
#'
#' In `dependencies`, `version`, `lock_source`, `repository`, and `remote` come
#' from `renv.lock`; `constraint` and `roles` come from `DESCRIPTION`; `direct`
#' marks packages seen in source or DESCRIPTION; and `locked` marks packages
#' present in the lockfile. `provenance` is a compact summary. The normalized
#' `references` table is the authoritative record of each origin, file, source
#' line, role or call type, and version requirement. Non-fatal findings such as
#' a dynamic `library()` package name appear in `diagnostics`.
#'
#' Required DESCRIPTION fields (`Depends`, `Imports`, and `LinkingTo`) are
#' included by default. Set `include_suggests = TRUE` to also include `Suggests`
#' and `Enhances`. A lockfile package that is not directly referenced is retained
#' as a locked transitive dependency.
#'
#' @param app_dir Path to the R application directory.
#' @param include_suggests Include `Suggests` and `Enhances` entries from
#'   `DESCRIPTION`.
#' @return An `rpackit_dependency_plan` object with `dependencies`,
#'   `references`, `diagnostics`, source-file paths, and R version requirements.
#' @export
#' @examples
#' app <- tempfile("rpackit-dependencies-")
#' dir.create(app)
#' writeLines(
#'   c("library(shiny)", "jsonlite::toJSON(list(ready = TRUE))"),
#'   file.path(app, "app.R")
#' )
#' plan_dependencies(app)
plan_dependencies <- function(app_dir, include_suggests = FALSE) {
  if (!is.character(app_dir) || length(app_dir) != 1L ||
      is.na(app_dir) || !dir.exists(app_dir)) {
    cli::cli_abort("{.arg app_dir} must be an existing directory.")
  }
  if (!is.logical(include_suggests) || length(include_suggests) != 1L ||
      is.na(include_suggests)) {
    cli::cli_abort("{.arg include_suggests} must be TRUE or FALSE.")
  }
  path <- normalizePath(app_dir, winslash = "/", mustWork = TRUE)
  r_files <- .app_r_files(path)
  source_results <- lapply(r_files, .parse_r_dependencies, app_path = path)
  source_references <- lapply(source_results, `[[`, "references")
  source_diagnostics <- lapply(source_results, `[[`, "diagnostics")
  description_path <- file.path(path, "DESCRIPTION")
  description <- .parse_description_dependencies(
    description_path,
    path,
    include_suggests
  )
  lockfile_path <- file.path(path, "renv.lock")
  lockfile <- .parse_lockfile_dependencies(lockfile_path, path)
  reference_parts <- c(
    source_references,
    list(description$references, lockfile$references)
  )
  reference_parts <- reference_parts[vapply(reference_parts, nrow, integer(1)) > 0L]
  references <- if (length(reference_parts)) {
    result <- do.call(rbind, reference_parts)
    rownames(result) <- NULL
    result
  } else {
    .empty_dependency_references()
  }
  diagnostics <- source_diagnostics[
    vapply(source_diagnostics, nrow, integer(1)) > 0L
  ]
  diagnostics <- if (length(diagnostics)) {
    result <- do.call(rbind, diagnostics)
    rownames(result) <- NULL
    result
  } else {
    .empty_dependency_diagnostics()
  }
  structure(
    list(
      path = path,
      dependencies = .combine_dependency_plan(
        references,
        lockfile$records
      ),
      references = references,
      diagnostics = diagnostics,
      r_files = unname(vapply(
        r_files,
        .relative_app_path,
        character(1),
        app_path = path
      )),
      description = if (file.exists(description_path)) {
        "DESCRIPTION"
      } else {
        NA_character_
      },
      lockfile = if (file.exists(lockfile_path)) {
        "renv.lock"
      } else {
        NA_character_
      },
      r_constraint = description$r_constraint,
      locked_r_version = lockfile$r_version,
      include_suggests = include_suggests
    ),
    class = "rpackit_dependency_plan"
  )
}

#' @export
print.rpackit_dependency_plan <- function(x, ...) {
  cli::cli_h1("rpackit dependency plan")
  cli::cli_text("Path: {x$path}")
  cli::cli_text(
    "{nrow(x$dependencies)} package{?s}; ",
    "{sum(x$dependencies$direct)} direct; ",
    "{sum(x$dependencies$locked)} locked"
  )
  if (nrow(x$dependencies)) {
    print(x$dependencies, row.names = FALSE)
  } else {
    cli::cli_text("No package dependencies detected.")
  }
  if (nrow(x$diagnostics)) {
    cli::cli_h2("Diagnostics")
    for (index in seq_len(nrow(x$diagnostics))) {
      diagnostic <- x$diagnostics[index, , drop = FALSE]
      location <- if (is.na(diagnostic$line)) {
        diagnostic$file
      } else {
        paste0(diagnostic$file, ":", diagnostic$line)
      }
      cli::cli_bullets(c(
        "!" = paste0(location, " - ", diagnostic$message)
      ))
    }
  }
  invisible(x)
}
