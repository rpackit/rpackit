.rpackit_runtime_registry <- paste0(
  "https://raw.githubusercontent.com/rpackit/portable-r/",
  "main/versions.json"
)

.portable_scalar_string <- function(value, field) {
  if (!is.character(value) || length(value) != 1L || is.na(value) ||
    !nzchar(value)) {
    cli::cli_abort(
      "Portable R field {.field {field}} must be one non-empty string.",
      class = "rpackit_runtime_registry_error"
    )
  }
  value
}

.portable_version <- function(value, argument = "r_version",
                              allow_null = FALSE) {
  if (is.null(value) && isTRUE(allow_null)) {
    return(NULL)
  }
  value <- .portable_scalar_string(value, argument)
  if (!grepl("^[0-9]+\\.[0-9]+\\.[0-9]+$", value)) {
    cli::cli_abort(
      "{.arg {argument}} must use an exact major.minor.patch version.",
      class = "rpackit_runtime_version_error"
    )
  }
  value
}

.portable_platform <- function(value = NULL) {
  if (is.null(value)) {
    value <- .rpackit_platform()$platform
  }
  value <- tolower(.portable_scalar_string(value, "platform"))
  if (!value %in% c("windows", "macos", "linux")) {
    cli::cli_abort(
      paste0(
        "{.arg platform} must be one of {.val windows}, {.val macos}, or ",
        "{.val linux}."
      ),
      class = "rpackit_runtime_platform_error"
    )
  }
  value
}

.portable_arch <- function(value = NULL) {
  if (is.null(value)) {
    value <- .rpackit_platform()$architecture
  }
  value <- tolower(.portable_scalar_string(value, "arch"))
  value <- switch(value,
    amd64 = "x86_64",
    x64 = "x86_64",
    `x86-64` = "x86_64",
    aarch64 = "arm64",
    value
  )
  if (!value %in% c("x86_64", "arm64")) {
    cli::cli_abort(
      "{.arg arch} must be one of {.val x86_64} or {.val arm64}.",
      class = "rpackit_runtime_platform_error"
    )
  }
  value
}

.portable_default_cache <- function() {
  tools::R_user_dir("rpackit", which = "cache")
}

.portable_is_https <- function(value) {
  is.character(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    grepl("^https://", value, ignore.case = TRUE)
}

.portable_validate_https_url <- function(value, field) {
  value <- .portable_scalar_string(value, field)
  if (!.portable_is_https(value)) {
    return(value)
  }
  authority <- sub("^https://", "", value, ignore.case = TRUE)
  authority <- sub("[/?#].*$", "", authority)
  if (grepl("@", authority, fixed = TRUE) ||
    grepl("[?#]", value) ||
    grepl("[\r\n]", value)) {
    cli::cli_abort(
      paste0(
        "HTTPS {field} must not contain user credentials, a query string, ",
        "or a fragment because runtime provenance is stored in cache and ",
        "bundle manifests."
      ),
      class = "rpackit_runtime_registry_error"
    )
  }
  value
}

.portable_has_url_scheme <- function(value) {
  windows_drive <- .Platform$OS.type == "windows" &&
    grepl("^[A-Za-z]:[/\\\\]", value)
  grepl("^[A-Za-z][A-Za-z0-9+.-]*:", value) &&
    !windows_drive
}

.portable_is_unc_path <- function(value) {
  is.character(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    grepl("^[/\\\\]{2}", value)
}

.portable_validate_source_reference <- function(
  value,
  field,
  error_class = "rpackit_runtime_registry_error"
) {
  value <- .portable_scalar_string(value, field)
  if (.portable_is_https(value)) {
    return(.portable_validate_https_url(value, field))
  }
  if (.portable_has_url_scheme(value)) {
    cli::cli_abort(
      "{field} must use HTTPS or a local filesystem path.",
      class = error_class
    )
  }
  if (.portable_is_unc_path(value)) {
    cli::cli_abort(
      "{field} may not use a UNC or network share.",
      class = error_class
    )
  }
  if (grepl("[?#\r\n]", value)) {
    cli::cli_abort(
      paste0(
        "{field} must not contain a query string, fragment, or line break ",
        "because runtime provenance is stored in cache and bundle manifests."
      ),
      class = error_class
    )
  }
  value
}

.portable_safe_relative_path <- function(value, field) {
  value <- .portable_scalar_string(value, field)
  if (grepl("\\\\", value) ||
    startsWith(value, "/") ||
    startsWith(value, "//") ||
    grepl("^[A-Za-z]:", value) ||
    grepl(":", value, fixed = TRUE) ||
    grepl("(^|/)(\\.|\\.\\.)(/|$)", value) ||
    grepl("//", value, fixed = TRUE)) {
    cli::cli_abort(
      paste0(
        "Portable R field {.field {field}} must be a traversal-free relative ",
        "POSIX path."
      ),
      class = "rpackit_runtime_registry_error"
    )
  }
  value
}

.portable_lexical_path <- function(path) {
  path <- path.expand(path)
  windows <- .Platform$OS.type == "windows"
  if (windows) {
    path <- gsub("\\\\", "/", path)
  }
  drive_rooted <- windows && grepl("^[A-Za-z]:/", path)
  current_directory <- function() {
    base <- getwd()
    if (windows) {
      base <- gsub("\\\\", "/", base)
      if (.portable_is_unc_path(base)) {
        cli::cli_abort(
          "Local runtime sources may not be resolved from a UNC working directory.",
          class = "rpackit_runtime_registry_error"
        )
      }
    }
    base
  }
  if (windows && startsWith(path, "/") && !drive_rooted) {
    base <- current_directory()
    if (!grepl("^[A-Za-z]:/", base)) {
      cli::cli_abort(
        "Could not determine the current drive for a rooted runtime source.",
        class = "rpackit_runtime_registry_error"
      )
    }
    path <- paste0(substr(base, 1L, 2L), path)
    drive_rooted <- TRUE
  }
  if (!startsWith(path, "/") && !drive_rooted) {
    base <- current_directory()
    path <- paste0(sub("/+$", "", base), "/", path)
    drive_rooted <- windows && grepl("^[A-Za-z]:/", path)
  }
  if (drive_rooted) {
    prefix <- paste0(toupper(substr(path, 1L, 1L)), ":/")
    remainder <- substring(path, 4L)
  } else {
    prefix <- "/"
    remainder <- sub("^/+", "", path)
  }
  parts <- strsplit(remainder, "/+", perl = TRUE)[[1L]]
  normalized <- character()
  for (part in parts) {
    if (!nzchar(part) || identical(part, ".")) {
      next
    }
    if (identical(part, "..")) {
      if (length(normalized)) {
        normalized <- normalized[-length(normalized)]
      }
      next
    }
    normalized <- c(normalized, part)
  }
  if (!length(normalized)) {
    return(prefix)
  }
  paste0(prefix, paste(normalized, collapse = "/"))
}

.portable_source_label <- function(source, field = "source") {
  source <- .portable_validate_source_reference(source, field)
  if (.portable_is_https(source)) {
    source
  } else {
    .portable_lexical_path(source)
  }
}

.portable_download <- function(source, destination, quiet, context) {
  source <- .portable_scalar_string(source, "source")
  if (.portable_is_https(source)) {
    source <- .portable_validate_https_url(source, context)
    status <- tryCatch(
      suppressWarnings(utils::download.file(
        source,
        destination,
        mode = "wb",
        quiet = quiet,
        method = "libcurl"
      )),
      error = function(error) {
        cli::cli_abort(
          paste0(
            "Could not download {context} from {.url {source}}: ",
            "{conditionMessage(error)}"
          ),
          class = "rpackit_runtime_download_error",
          parent = error
        )
      }
    )
    if (!identical(as.integer(status), 0L) || !file.exists(destination)) {
      cli::cli_abort(
        "Could not download {context} from {.url {source}}.",
        class = "rpackit_runtime_download_error"
      )
    }
    return(invisible(destination))
  }
  if (.portable_has_url_scheme(source)) {
    cli::cli_abort(
      "{context} must use HTTPS; an insecure or unsupported URL was rejected.",
      class = "rpackit_runtime_download_error"
    )
  }
  .portable_validate_source_reference(
    source,
    context,
    error_class = "rpackit_runtime_download_error"
  )
  if (!file.exists(source) || dir.exists(source)) {
    cli::cli_abort(
      "Local {context} does not exist: {.path {source}}.",
      class = "rpackit_runtime_download_error"
    )
  }
  if (!file.copy(source, destination, overwrite = FALSE, copy.mode = TRUE)) {
    cli::cli_abort(
      "Could not copy local {context} {.path {source}}.",
      class = "rpackit_runtime_download_error"
    )
  }
  invisible(destination)
}

.portable_read_json <- function(source, context, quiet = TRUE) {
  temporary <- tempfile("rpackit-runtime-json-", fileext = ".json")
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  .portable_download(source, temporary, quiet, context)
  tryCatch(
    jsonlite::fromJSON(temporary, simplifyVector = FALSE),
    error = function(error) {
      cli::cli_abort(
        paste0(
          "Could not parse {context} from {.path {source}}: ",
          "{conditionMessage(error)}"
        ),
        class = "rpackit_runtime_registry_error",
        parent = error
      )
    }
  )
}

.portable_relative_source <- function(source, relative) {
  relative <- .portable_safe_relative_path(relative, "metadata")
  source <- .portable_source_label(source, "registry")
  if (.portable_is_https(source)) {
    base <- sub("/[^/]*$", "", source)
    return(paste0(base, "/", relative))
  }
  parent <- dirname(normalizePath(
    source,
    winslash = "/",
    mustWork = TRUE
  ))
  resolved <- do.call(
    file.path,
    c(list(parent), as.list(strsplit(
      relative,
      "/",
      fixed = TRUE
    )[[1L]]))
  )
  .portable_validate_source_reference(resolved, "metadata source")
}

.portable_registry_entries <- function(registry, quiet) {
  index <- .portable_read_json(registry, "portable R registry", quiet)
  if (!is.list(index) ||
    !identical(index$schema_version, "1") ||
    !is.list(index$runtimes)) {
    cli::cli_abort(
      paste0(
        "Portable R registry must use schema version 1 and contain a ",
        "runtimes array."
      ),
      class = "rpackit_runtime_registry_error"
    )
  }
  entries <- lapply(seq_along(index$runtimes), function(index_number) {
    entry <- index$runtimes[[index_number]]
    location <- paste0("runtimes[", index_number, "]")
    if (!is.list(entry)) {
      cli::cli_abort(
        "Portable R registry field {.field {location}} must be an object.",
        class = "rpackit_runtime_registry_error"
      )
    }
    result <- list(
      r_version = .portable_version(
        entry$r_version,
        paste0(location, ".r_version")
      ),
      platform = .portable_scalar_string(
        entry$platform,
        paste0(location, ".platform")
      ),
      arch = .portable_scalar_string(
        entry$arch,
        paste0(location, ".arch")
      ),
      status = .portable_scalar_string(
        entry$status,
        paste0(location, ".status")
      ),
      metadata = .portable_safe_relative_path(
        entry$metadata,
        paste0(location, ".metadata")
      )
    )
    if (!result$platform %in% c("windows", "macos", "linux") ||
      !result$arch %in% c("x86_64", "arm64") ||
      !result$status %in% c("prototype", "verified", "deprecated")) {
      cli::cli_abort(
        paste0(
          "Portable R registry field {.field {location}} contains an ",
          "unsupported platform, architecture, or status."
        ),
        class = "rpackit_runtime_registry_error"
      )
    }
    result
  })
  keys <- vapply(entries, function(entry) {
    paste(entry$r_version, entry$platform, entry$arch, sep = "/")
  }, character(1))
  if (anyDuplicated(keys)) {
    cli::cli_abort(
      "Portable R registry contains duplicate runtime entries.",
      class = "rpackit_runtime_registry_error"
    )
  }
  entries
}

.portable_available_text <- function(entries) {
  verified <- Filter(
    function(entry) identical(entry$status, "verified"),
    entries
  )
  if (!length(verified)) {
    return("The registry contains no verified runtimes.")
  }
  labels <- sort(unique(vapply(verified, function(entry) {
    paste(entry$platform, entry$arch, entry$r_version, sep = "/")
  }, character(1))))
  paste0("Available verified runtimes: ", paste(labels, collapse = ", "), ".")
}

.portable_select_entry <- function(entries, platform, arch, r_version) {
  verified <- Filter(
    function(entry) identical(entry$status, "verified"),
    entries
  )
  matching_platform <- Filter(function(entry) {
    identical(entry$platform, platform) && identical(entry$arch, arch)
  }, verified)
  if (!length(matching_platform)) {
    cli::cli_abort(
      c(
        "No verified portable R runtime is available for ",
        "{platform}/{arch}.",
        "i" = .portable_available_text(entries)
      ),
      class = "rpackit_runtime_unavailable_error"
    )
  }
  if (!is.null(r_version)) {
    matching_version <- Filter(
      function(entry) identical(entry$r_version, r_version),
      matching_platform
    )
    if (!length(matching_version)) {
      versions <- sort(unique(vapply(
        matching_platform,
        `[[`,
        character(1),
        "r_version"
      )))
      cli::cli_abort(
        c(
          "No verified portable R {r_version} runtime is available for ",
          "{platform}/{arch}.",
          "i" = paste0(
            "Available versions for this platform: ",
            paste(versions, collapse = ", "),
            "."
          )
        ),
        class = "rpackit_runtime_unavailable_error"
      )
    }
    return(matching_version[[1L]])
  }
  versions <- vapply(
    matching_platform,
    `[[`,
    character(1),
    "r_version"
  )
  matching_platform[[order(
    numeric_version(versions),
    decreasing = TRUE
  )[[1L]]]]
}

.portable_artifact_source <- function(value, metadata_source) {
  value <- .portable_scalar_string(value, "artifact_url")
  if (.portable_is_https(value)) {
    return(.portable_validate_https_url(value, "artifact URL"))
  }
  if (.portable_has_url_scheme(value)) {
    cli::cli_abort(
      "Portable R artifact URLs must use HTTPS.",
      class = "rpackit_runtime_registry_error"
    )
  }
  if (.portable_is_https(metadata_source)) {
    cli::cli_abort(
      "A remote portable R registry must provide an HTTPS artifact URL.",
      class = "rpackit_runtime_registry_error"
    )
  }
  .portable_validate_source_reference(metadata_source, "metadata source")
  .portable_validate_source_reference(value, "artifact URL")
  if (startsWith(value, "/") ||
    grepl("^[A-Za-z]:[/\\\\]", value)) {
    return(value)
  }
  relative <- .portable_safe_relative_path(
    gsub("\\\\", "/", value),
    "artifact_url"
  )
  resolved <- do.call(
    file.path,
    c(
      list(dirname(metadata_source)),
      as.list(strsplit(relative, "/", fixed = TRUE)[[1L]])
    )
  )
  .portable_validate_source_reference(resolved, "artifact path")
}

.portable_metadata <- function(entry, registry, quiet) {
  metadata_source <- .portable_relative_source(registry, entry$metadata)
  metadata <- .portable_read_json(
    metadata_source,
    "portable R metadata",
    quiet
  )
  if (!is.list(metadata) || !identical(metadata$schema_version, "1")) {
    cli::cli_abort(
      "Portable R metadata must use schema version 1.",
      class = "rpackit_runtime_registry_error"
    )
  }
  result <- list(
    schema_version = "1",
    r_version = .portable_version(
      metadata$r_version,
      "metadata.r_version"
    ),
    platform = .portable_scalar_string(
      metadata$platform,
      "metadata.platform"
    ),
    arch = .portable_scalar_string(metadata$arch, "metadata.arch"),
    status = entry$status,
    artifact_url = .portable_scalar_string(
      metadata$artifact_url,
      "metadata.artifact_url"
    ),
    sha256 = tolower(.portable_scalar_string(
      metadata$sha256,
      "metadata.sha256"
    )),
    archive_format = .portable_scalar_string(
      metadata$archive_format,
      "metadata.archive_format"
    ),
    r_home = .portable_safe_relative_path(
      metadata$r_home,
      "metadata.r_home"
    ),
    rscript = .portable_safe_relative_path(
      metadata$rscript,
      "metadata.rscript"
    ),
    library = .portable_safe_relative_path(
      metadata$library,
      "metadata.library"
    ),
    registry = .portable_source_label(registry),
    metadata_source = .portable_source_label(metadata_source)
  )
  for (field in c("r_version", "platform", "arch")) {
    if (!identical(result[[field]], entry[[field]])) {
      cli::cli_abort(
        paste0(
          "Portable R metadata field {.field {field}} does not match its ",
          "registry entry."
        ),
        class = "rpackit_runtime_registry_error"
      )
    }
  }
  if (!grepl("^[a-f0-9]{64}$", result$sha256) ||
    !result$archive_format %in% c("zip", "tar.gz", "tar.zst")) {
    cli::cli_abort(
      "Portable R metadata contains an invalid SHA-256 or archive format.",
      class = "rpackit_runtime_registry_error"
    )
  }
  prefix <- paste0(result$r_home, "/")
  if (!startsWith(result$rscript, prefix) ||
    !startsWith(result$library, prefix)) {
    cli::cli_abort(
      "Portable R runtime paths must be contained within metadata.r_home.",
      class = "rpackit_runtime_registry_error"
    )
  }
  expected_rscript <- paste0(
    result$r_home,
    if (identical(result$platform, "windows")) {
      "/bin/Rscript.exe"
    } else {
      "/bin/Rscript"
    }
  )
  if (grepl("/", result$r_home, fixed = TRUE) ||
    !identical(result$rscript, expected_rscript) ||
    !identical(result$library, paste0(result$r_home, "/library"))) {
    cli::cli_abort(
      paste0(
        "rpackit runtimes require one top-level r_home, the platform ",
        "Rscript under r_home/bin, and r_home/library."
      ),
      class = "rpackit_runtime_registry_error"
    )
  }
  result$artifact_source <- .portable_artifact_source(
    result$artifact_url,
    metadata_source
  )
  result
}

.portable_cache_root <- function(cache_dir) {
  if (is.null(cache_dir)) {
    cache_dir <- .portable_default_cache()
  }
  cache_dir <- .portable_scalar_string(cache_dir, "cache_dir")
  if (file.exists(cache_dir) && !dir.exists(cache_dir)) {
    cli::cli_abort(
      "{.arg cache_dir} points to a file: {.path {cache_dir}}.",
      class = "rpackit_runtime_cache_error"
    )
  }
  if (!dir.exists(cache_dir) &&
    !dir.create(
      cache_dir,
      recursive = TRUE,
      showWarnings = FALSE,
      mode = "0700"
    ) &&
    !dir.exists(cache_dir)) {
    cli::cli_abort(
      "Could not create portable R cache directory {.path {cache_dir}}.",
      class = "rpackit_runtime_cache_error"
    )
  }
  root <- file.path(
    normalizePath(cache_dir, winslash = "/", mustWork = TRUE),
    "runtimes"
  )
  if (!dir.exists(root) &&
    !dir.create(
      root,
      recursive = FALSE,
      showWarnings = FALSE,
      mode = "0700"
    ) &&
    !dir.exists(root)) {
    cli::cli_abort(
      "Could not create portable R runtime cache {.path {root}}.",
      class = "rpackit_runtime_cache_error"
    )
  }
  normalizePath(root, winslash = "/", mustWork = TRUE)
}

.portable_cache_target <- function(root, metadata) {
  registry_key <- substr(
    digest::digest(
      .portable_source_label(metadata$registry, "registry"),
      algo = "sha256",
      serialize = FALSE
    ),
    1L,
    32L
  )
  file.path(
    root,
    paste(
      metadata$platform,
      metadata$arch,
      metadata$r_version,
      metadata$sha256,
      registry_key,
      sep = "-"
    )
  )
}

.portable_cache_marker_value <- function(metadata) {
  metadata[c(
    "schema_version", "r_version", "platform", "arch", "status",
    "artifact_url", "sha256", "archive_format", "r_home", "rscript",
    "library", "registry", "metadata_source"
  )]
}

.portable_cached_metadata <- function(target) {
  if (!dir.exists(target) || nzchar(Sys.readlink(target))) {
    return(NULL)
  }
  target_normalized <- normalizePath(
    target,
    winslash = "/",
    mustWork = TRUE
  )
  root_normalized <- normalizePath(
    dirname(target),
    winslash = "/",
    mustWork = TRUE
  )
  if (identical(target_normalized, root_normalized) ||
    !.rpackit_path_within(target_normalized, root_normalized)) {
    return(NULL)
  }
  marker <- file.path(target, "rpackit-runtime.json")
  if (!file.exists(marker)) {
    return(NULL)
  }
  value <- tryCatch(
    jsonlite::fromJSON(marker, simplifyVector = FALSE),
    error = function(error) NULL
  )
  if (!is.list(value) ||
    !identical(value$schema_version, "1") ||
    !identical(value$status, "verified")) {
    return(NULL)
  }
  required <- c(
    "r_version", "platform", "arch", "artifact_url", "sha256",
    "archive_format", "r_home", "rscript", "library", "registry",
    "metadata_source"
  )
  if (any(vapply(required, function(field) {
    !is.character(value[[field]]) ||
      length(value[[field]]) != 1L ||
      is.na(value[[field]]) ||
      !nzchar(value[[field]])
  }, logical(1)))) {
    return(NULL)
  }
  if (!grepl("^[0-9]+\\.[0-9]+\\.[0-9]+$", value$r_version) ||
    !value$platform %in% c("windows", "macos", "linux") ||
    !value$arch %in% c("x86_64", "arm64") ||
    !identical(value$archive_format, "zip") ||
    !grepl("^[a-f0-9]{64}$", value$sha256)) {
    return(NULL)
  }
  safe_fields <- c("r_home", "rscript", "library")
  safe <- tryCatch(
    {
      for (field in safe_fields) {
        .portable_safe_relative_path(value[[field]], field)
      }
      TRUE
    },
    error = function(error) FALSE
  )
  if (!safe) {
    return(NULL)
  }
  prefix <- paste0(value$r_home, "/")
  if (!startsWith(value$rscript, prefix) ||
    !startsWith(value$library, prefix)) {
    return(NULL)
  }
  expected_rscript <- paste0(
    value$r_home,
    if (identical(value$platform, "windows")) {
      "/bin/Rscript.exe"
    } else {
      "/bin/Rscript"
    }
  )
  if (grepl("/", value$r_home, fixed = TRUE) ||
    !identical(value$rscript, expected_rscript) ||
    !identical(value$library, paste0(value$r_home, "/library"))) {
    return(NULL)
  }
  expected_target <- tryCatch(
    .portable_cache_target(dirname(target), value),
    error = function(error) NULL
  )
  if (is.null(expected_target)) {
    return(NULL)
  }
  if (!identical(
    target_normalized,
    normalizePath(expected_target, winslash = "/", mustWork = FALSE)
  )) {
    return(NULL)
  }
  runtime <- file.path(target, value$r_home)
  rscript <- file.path(target, value$rscript)
  library <- file.path(target, value$library)
  if (!dir.exists(runtime) ||
    !file.exists(rscript) ||
    dir.exists(rscript) ||
    !dir.exists(library)) {
    return(NULL)
  }
  safe_tree <- tryCatch(
    {
      .portable_validate_extracted_tree(target, value)
      TRUE
    },
    error = function(error) FALSE
  )
  if (!safe_tree) {
    return(NULL)
  }
  value$path <- normalizePath(runtime, winslash = "/", mustWork = TRUE)
  value$cache_path <- normalizePath(target, winslash = "/", mustWork = TRUE)
  value
}

.portable_cached_runtimes <- function(root, platform = NULL, arch = NULL) {
  targets <- list.dirs(
    root,
    recursive = FALSE,
    full.names = TRUE
  )
  if (!is.null(platform) && !is.null(arch)) {
    prefix <- paste0(platform, "-", arch, "-")
    targets <- targets[startsWith(basename(targets), prefix)]
  }
  runtimes <- lapply(targets, .portable_cached_metadata)
  Filter(Negate(is.null), runtimes)
}

.portable_overlay_cached_metadata <- function(cached, metadata) {
  identity_fields <- c(
    "r_version", "platform", "arch", "status", "sha256",
    "archive_format"
  )
  if (is.null(cached) || any(vapply(identity_fields, function(field) {
    !identical(cached[[field]], metadata[[field]])
  }, logical(1)))) {
    return(NULL)
  }
  runtime <- file.path(cached$cache_path, metadata$r_home)
  rscript <- file.path(cached$cache_path, metadata$rscript)
  library <- file.path(cached$cache_path, metadata$library)
  if (!dir.exists(runtime) ||
    !file.exists(rscript) ||
    dir.exists(rscript) ||
    !dir.exists(library)) {
    return(NULL)
  }
  metadata$path <- normalizePath(runtime, winslash = "/", mustWork = TRUE)
  metadata$cache_path <- cached$cache_path
  metadata
}

.portable_runtime_result <- function(metadata, cache_hit) {
  structure(
    list(
      path = metadata$path,
      r_version = metadata$r_version,
      platform = metadata$platform,
      arch = metadata$arch,
      status = metadata$status,
      artifact_url = metadata$artifact_url,
      sha256 = metadata$sha256,
      archive_format = metadata$archive_format,
      registry = metadata$registry,
      metadata_source = metadata$metadata_source,
      cache_path = metadata$cache_path,
      cache_hit = isTRUE(cache_hit)
    ),
    class = "rpackit_portable_runtime"
  )
}

.portable_select_cached <- function(root, platform, arch, r_version,
                                    registry) {
  registry_label <- .portable_source_label(registry)
  cached <- Filter(function(value) {
    identical(value$platform, platform) &&
      identical(value$arch, arch) &&
      identical(value$registry, registry_label) &&
      (is.null(r_version) || identical(value$r_version, r_version))
  }, .portable_cached_runtimes(root, platform, arch))
  if (!length(cached)) {
    requested <- if (is.null(r_version)) {
      paste0(platform, "/", arch)
    } else {
      paste0(platform, "/", arch, "/", r_version)
    }
    cli::cli_abort(
      c(
        paste0(
          "No checksum-verified cached portable R runtime from ",
          "{.path {registry_label}} matches {requested}."
        ),
        "i" = "Run again with {.code offline = FALSE} to populate the cache."
      ),
      class = "rpackit_runtime_offline_error"
    )
  }
  versions <- vapply(cached, `[[`, character(1), "r_version")
  newest <- as.character(max(numeric_version(versions)))
  cached <- cached[versions == newest]
  if (length(cached) != 1L) {
    cli::cli_abort(
      paste0(
        "Multiple cached portable R {newest} artifacts match ",
        "{platform}/{arch}; specify an online registry to select the ",
        "current verified artifact."
      ),
      class = "rpackit_runtime_cache_error"
    )
  }
  .portable_runtime_result(cached[[1L]], cache_hit = TRUE)
}

.portable_validate_zip_entry_types <- function(entries, types) {
  if (!is.character(entries) ||
    !is.character(types) ||
    length(entries) != length(types) ||
    anyNA(entries) ||
    anyNA(types)) {
    cli::cli_abort(
      "Portable R ZIP archive has an invalid typed file listing.",
      class = "rpackit_runtime_archive_error"
    )
  }
  unsafe <- !types %in% c("file", "directory")
  if (any(unsafe)) {
    detail <- paste0(
      entries[unsafe],
      " (",
      types[unsafe],
      ")"
    )
    cli::cli_abort(
      paste0(
        "Portable R ZIP archives may contain files and directories only; ",
        "rejected: ",
        paste(detail, collapse = ", "),
        "."
      ),
      class = "rpackit_runtime_archive_error"
    )
  }
  invisible(entries)
}

.portable_archive_entries <- function(archive, archive_format) {
  if (!identical(archive_format, "zip")) {
    cli::cli_abort(
      "Only ZIP archives have a link-aware extraction contract.",
      class = "rpackit_runtime_archive_error"
    )
  }
  listing <- tryCatch(
    zip::zip_list(archive),
    error = function(error) {
      cli::cli_abort(
        paste0(
          "Could not inspect portable R ZIP archive: ",
          "{conditionMessage(error)}"
        ),
        class = "rpackit_runtime_archive_error",
        parent = error
      )
    }
  )
  .portable_validate_zip_entry_types(
    listing$filename,
    listing$type
  )
  listing$filename
}

.portable_validate_archive_entries <- function(entries, r_home) {
  if (!is.character(entries) || !length(entries) || anyNA(entries)) {
    cli::cli_abort(
      "Portable R archive is empty or has an invalid file listing.",
      class = "rpackit_runtime_archive_error"
    )
  }
  for (entry in entries) {
    path <- sub("/+$", "", entry)
    if (!nzchar(path) ||
      grepl("\\\\", path) ||
      grepl(":", path, fixed = TRUE) ||
      startsWith(path, "/") ||
      startsWith(path, "//") ||
      grepl("^[A-Za-z]:", path)) {
      cli::cli_abort(
        "Portable R archive contains an unsafe path: {.path {entry}}.",
        class = "rpackit_runtime_archive_error"
      )
    }
    pieces <- strsplit(path, "/", fixed = TRUE)[[1L]]
    if (any(!nzchar(pieces)) ||
      any(pieces %in% c(".", "..")) ||
      !identical(pieces[[1L]], r_home)) {
      cli::cli_abort(
        paste0(
          "Portable R archive entry escapes or disagrees with its declared ",
          "runtime root: {.path {entry}}."
        ),
        class = "rpackit_runtime_archive_error"
      )
    }
  }
  invisible(entries)
}

.portable_extract_archive <- function(archive, archive_format, destination) {
  if (!identical(archive_format, "zip")) {
    cli::cli_abort(
      "Only ZIP archives have a link-aware extraction contract.",
      class = "rpackit_runtime_archive_error"
    )
  }
  tryCatch(
    zip::unzip(
      archive,
      overwrite = FALSE,
      junkpaths = FALSE,
      exdir = destination
    ),
    error = function(error) {
      cli::cli_abort(
        "Could not extract portable R archive: {conditionMessage(error)}",
        class = "rpackit_runtime_archive_error",
        parent = error
      )
    }
  )
  invisible(destination)
}

.portable_validate_extracted_tree <- function(stage, metadata) {
  entries <- list.files(
    stage,
    recursive = TRUE,
    full.names = TRUE,
    all.files = TRUE,
    no.. = TRUE,
    include.dirs = TRUE
  )
  stage_normalized <- normalizePath(
    stage,
    winslash = "/",
    mustWork = TRUE
  )
  for (entry in entries) {
    link <- Sys.readlink(entry)
    if (nzchar(link)) {
      cli::cli_abort(
        paste0(
          "Portable R archives may not contain symbolic links: ",
          "{.path {entry}}."
        ),
        class = "rpackit_runtime_archive_error"
      )
    }
    normalized <- normalizePath(entry, winslash = "/", mustWork = TRUE)
    if (!.rpackit_path_within(normalized, stage_normalized)) {
      cli::cli_abort(
        "Portable R archive extracted outside its private staging directory.",
        class = "rpackit_runtime_archive_error"
      )
    }
  }
  runtime <- file.path(stage, metadata$r_home)
  rscript <- file.path(stage, metadata$rscript)
  library <- file.path(stage, metadata$library)
  if (!dir.exists(runtime) ||
    !file.exists(rscript) ||
    dir.exists(rscript) ||
    !dir.exists(library)) {
    cli::cli_abort(
      paste0(
        "Portable R archive does not contain the runtime paths declared by ",
        "its verified metadata."
      ),
      class = "rpackit_runtime_archive_error"
    )
  }
  invisible(runtime)
}

.portable_publish_stage <- function(stage, target) {
  file.rename(stage, target)
}

.portable_populate_cache <- function(metadata, root, quiet) {
  target <- .portable_cache_target(root, metadata)
  cached <- if (dir.exists(target)) {
    .portable_cached_metadata(target)
  } else {
    NULL
  }
  if (!is.null(cached)) {
    selected_cached <- .portable_overlay_cached_metadata(cached, metadata)
    if (is.null(selected_cached)) {
      cli::cli_abort(
        paste0(
          "Selected portable R metadata disagrees with the paths in its ",
          "checksum-identified cache entry."
        ),
        class = "rpackit_runtime_registry_error"
      )
    }
    return(.portable_runtime_result(selected_cached, cache_hit = TRUE))
  }
  if (dir.exists(target) || file.exists(target)) {
    cli::cli_abort(
      c(
        "Portable R cache entry is incomplete or invalid: {.path {target}}.",
        "i" = "Move or remove this single cache entry, then retry."
      ),
      class = "rpackit_runtime_cache_error"
    )
  }
  if (!identical(metadata$archive_format, "zip")) {
    cli::cli_abort(
      paste0(
        "Automatic extraction supports verified ZIP runtimes only; ",
        "{.val {metadata$archive_format}} archives can contain link targets ",
        "that cannot yet be proven safe before extraction."
      ),
      class = "rpackit_runtime_archive_error"
    )
  }
  stage <- tempfile(".rpackit-runtime-stage-", tmpdir = root)
  archive_extension <- switch(metadata$archive_format,
    zip = ".zip",
    tar.gz = ".tar.gz",
    tar.zst = ".tar.zst"
  )
  archive <- tempfile(
    ".rpackit-runtime-download-",
    tmpdir = root,
    fileext = archive_extension
  )
  if (!dir.create(stage, recursive = FALSE, showWarnings = FALSE)) {
    cli::cli_abort(
      "Could not create private portable R staging directory.",
      class = "rpackit_runtime_cache_error"
    )
  }
  on.exit(
    {
      if (file.exists(archive)) {
        unlink(archive, force = TRUE)
      }
      if (dir.exists(stage)) {
        unlink(stage, recursive = TRUE, force = TRUE)
      }
    },
    add = TRUE
  )

  if (!quiet) {
    cli::cli_progress_message(
      paste0(
        "Downloading portable R {metadata$r_version} for ",
        "{metadata$platform}/{metadata$arch}"
      )
    )
  }
  .portable_download(
    metadata$artifact_source,
    archive,
    quiet,
    "portable R artifact"
  )
  actual_sha256 <- digest::digest(
    file = archive,
    algo = "sha256"
  )
  if (!is.character(actual_sha256) ||
    length(actual_sha256) != 1L ||
    is.na(actual_sha256) ||
    !identical(tolower(actual_sha256), metadata$sha256)) {
    cli::cli_abort(
      c(
        "Portable R artifact SHA-256 verification failed.",
        "x" = "Expected: {metadata$sha256}",
        "x" = "Observed: {actual_sha256}"
      ),
      class = "rpackit_runtime_checksum_error"
    )
  }
  entries <- .portable_archive_entries(
    archive,
    metadata$archive_format
  )
  .portable_validate_archive_entries(entries, metadata$r_home)
  if (!quiet) {
    cli::cli_progress_message("Extracting verified portable R runtime")
  }
  .portable_extract_archive(
    archive,
    metadata$archive_format,
    stage
  )
  .portable_validate_extracted_tree(stage, metadata)
  marker <- .portable_cache_marker_value(metadata)
  jsonlite::write_json(
    marker,
    file.path(stage, "rpackit-runtime.json"),
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )
  cache_hit <- FALSE
  if (!.portable_publish_stage(stage, target)) {
    raced <- if (dir.exists(target)) {
      .portable_cached_metadata(target)
    } else {
      NULL
    }
    if (is.null(raced)) {
      cli::cli_abort(
        paste0(
          "Could not atomically publish verified runtime to ",
          "{.path {target}}."
        ),
        class = "rpackit_runtime_cache_error"
      )
    }
    cache_hit <- TRUE
    unlink(stage, recursive = TRUE, force = TRUE)
  }
  cached <- .portable_cached_metadata(target)
  if (is.null(cached)) {
    cli::cli_abort(
      "Published portable R cache entry failed validation.",
      class = "rpackit_runtime_cache_error"
    )
  }
  selected_cached <- .portable_overlay_cached_metadata(cached, metadata)
  if (is.null(selected_cached)) {
    cli::cli_abort(
      paste0(
        "Published runtime does not contain the paths declared by the ",
        "selected metadata."
      ),
      class = "rpackit_runtime_registry_error"
    )
  }
  .portable_runtime_result(selected_cached, cache_hit = cache_hit)
}

#' Resolve a verified portable R runtime
#'
#' `resolve_portable_runtime()` selects a `verified` runtime from the rpackit
#' portable-R registry, downloads its archive over HTTPS, verifies its SHA-256,
#' checks every archive path before extraction, and atomically publishes the
#' extracted runtime to a user cache. A valid cache entry is reused without
#' downloading the artifact again.
#'
#' Automatic extraction currently accepts ZIP artifacts only. Tar archives can
#' encode link targets that base R cannot prove safe before extraction, so
#' `tar.gz` and `tar.zst` metadata fail closed until a link-aware extractor is
#' part of the runtime contract. ZIP entries are inspected with their types;
#' symbolic links and all types other than ordinary files and directories are
#' rejected before extraction.
#'
#' `registry` may be an HTTPS `versions.json` URL or a local file. Local
#' registries may refer to local metadata and artifact files, which supports
#' air-gapped mirrors and deterministic tests. This local transport override
#' uses the schema-v1 field set but is not valid public-registry metadata.
#' HTTP, UNC shares, and HTTPS URLs containing credentials, query strings, or
#' fragments are rejected so distributable provenance cannot expose secrets.
#'
#' With `offline = TRUE`, no registry or artifact is read. A cache entry must
#' match the requested registry source, platform, architecture, and optional R
#' version. Cache entries are structurally revalidated and links or path escapes
#' are rejected. The user cache contains executable code and must not be
#' writable by untrusted users.
#'
#' rpackit accepts the portable-R schema's conventional desktop subset: one
#' top-level R home, `bin/Rscript.exe` on Windows or `bin/Rscript` elsewhere,
#' and a runtime-local `library` directory.
#'
#' @param r_version Exact R major.minor.patch version, or `NULL` for the newest
#'   verified version available for `platform` and `arch`.
#' @param platform Runtime platform. Defaults to the current platform.
#' @param arch Runtime architecture. Defaults to the current architecture.
#' @param registry HTTPS URL or local path to a portable-R schema-v1 registry.
#' @param cache_dir Cache directory. Defaults to rpackit's user cache.
#' @param offline Reuse a matching same-registry cache entry without reading
#'   any registry or artifact.
#' @param quiet Suppress download and extraction progress.
#' @return An `rpackit_portable_runtime` object. `path` is the extracted R home
#'   accepted by [prepare_desktop()]. The object also records `r_version`,
#'   `platform`, `arch`, registry and metadata sources, artifact URL, SHA-256,
#'   archive format, cache path, and whether the cache was reused.
#' @export
#' @examples
#' \dontrun{
#' runtime <- resolve_portable_runtime()
#' runtime$path
#' }
resolve_portable_runtime <- function(
  r_version = NULL,
  platform = NULL,
  arch = NULL,
  registry = getOption(
    "rpackit.runtime_registry",
    .rpackit_runtime_registry
  ),
  cache_dir = NULL,
  offline = FALSE,
  quiet = FALSE
) {
  r_version <- .portable_version(
    r_version,
    "r_version",
    allow_null = TRUE
  )
  platform <- .portable_platform(platform)
  arch <- .portable_arch(arch)
  registry <- .portable_scalar_string(registry, "registry")
  for (value in list(offline = offline, quiet = quiet)) {
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      cli::cli_abort(
        "{.arg offline} and {.arg quiet} must each be TRUE or FALSE."
      )
    }
  }
  root <- .portable_cache_root(cache_dir)
  registry <- .portable_source_label(registry, "registry")
  if (isTRUE(offline)) {
    result <- .portable_select_cached(
      root,
      platform,
      arch,
      r_version,
      registry
    )
    if (!quiet) {
      print(result)
    }
    return(invisible(result))
  }
  entries <- .portable_registry_entries(registry, quiet)
  selected <- .portable_select_entry(
    entries,
    platform,
    arch,
    r_version
  )
  metadata <- .portable_metadata(selected, registry, quiet)
  result <- .portable_populate_cache(metadata, root, quiet)
  if (!quiet) {
    print(result)
  }
  invisible(result)
}

#' @export
print.rpackit_portable_runtime <- function(x, ...) {
  cli::cli_h1("rpackit portable R runtime")
  cli::cli_text("R: {x$r_version}")
  cli::cli_text("Platform: {x$platform} {x$arch}")
  cli::cli_text("Path: {x$path}")
  cli::cli_text(
    "Cache: {if (x$cache_hit) 'reused verified entry' else 'verified and added'}"
  )
  cli::cli_text("SHA-256: {x$sha256}")
  invisible(x)
}
