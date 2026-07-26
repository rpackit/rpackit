.rpackit_platform <- function() {
  system <- tolower(Sys.info()[["sysname"]])
  platform <- switch(
    system,
    darwin = "macos",
    windows = "windows",
    linux = "linux",
    system
  )
  machine <- tolower(Sys.info()[["machine"]])
  architecture <- if (machine %in% c("amd64", "x86_64", "x86-64", "x64")) {
    "x86_64"
  } else if (machine %in% c("arm64", "aarch64")) {
    "arm64"
  } else {
    machine
  }
  list(platform = platform, architecture = architecture)
}

.tool_version <- function(command, arguments = "--version") {
  path <- Sys.which(command)
  if (!nzchar(path)) {
    return(list(available = FALSE, version = NA_character_, path = NA_character_))
  }
  output <- tryCatch(
    suppressWarnings(system2(
      path,
      arguments,
      stdout = TRUE,
      stderr = TRUE
    )),
    error = function(error) character()
  )
  status <- attr(output, "status")
  available <- is.null(status) || identical(as.integer(status), 0L)
  version <- if (length(output)) trimws(output[[1L]]) else NA_character_
  list(available = available, version = version, path = unname(path))
}

.package_manager_probe <- function() {
  for (command in c("npm", "pnpm", "yarn")) {
    result <- .tool_version(command, "--version")
    if (isTRUE(result$available)) {
      result$version <- paste(command, result$version)
      return(result)
    }
  }
  list(available = FALSE, version = NA_character_, path = NA_character_)
}

.windows_msvc_probe <- function() {
  compiler <- .tool_version("cl", character())
  if (isTRUE(compiler$available)) {
    return(compiler)
  }
  program_files_x86 <- Sys.getenv("ProgramFiles(x86)", unset = "")
  vswhere <- if (nzchar(program_files_x86)) {
    file.path(
      program_files_x86,
      "Microsoft Visual Studio",
      "Installer",
      "vswhere.exe"
    )
  } else {
    ""
  }
  if (!nzchar(vswhere) || !file.exists(vswhere)) {
    return(list(
      available = FALSE,
      version = NA_character_,
      path = NA_character_
    ))
  }
  output <- tryCatch(
    suppressWarnings(system2(
      vswhere,
      c(
        "-latest",
        "-products", "*",
        "-requires", "Microsoft.VisualStudio.Component.VC.Tools.x86.x64",
        "-property", "installationPath"
      ),
      stdout = TRUE,
      stderr = TRUE
    )),
    error = function(error) character()
  )
  installation <- output[nzchar(trimws(output))]
  available <- length(installation) > 0L &&
    dir.exists(installation[[1L]])
  list(
    available = available,
    version = if (available) basename(installation[[1L]]) else NA_character_,
    path = if (available) installation[[1L]] else NA_character_
  )
}

.windows_webview2_probe <- function() {
  roots <- unique(c(
    Sys.getenv("ProgramFiles(x86)", unset = ""),
    Sys.getenv("ProgramFiles", unset = "")
  ))
  roots <- roots[nzchar(roots)]
  candidates <- unlist(lapply(roots, function(root) {
    Sys.glob(file.path(
      root,
      "Microsoft",
      "EdgeWebView",
      "Application",
      "*",
      "msedgewebview2.exe"
    ))
  }), use.names = FALSE)
  if (!length(candidates)) {
    return(list(
      available = FALSE,
      version = NA_character_,
      path = NA_character_
    ))
  }
  versions <- basename(dirname(candidates))
  order_index <- order(versions, decreasing = TRUE)
  list(
    available = TRUE,
    version = versions[order_index[[1L]]],
    path = candidates[order_index[[1L]]]
  )
}

.native_desktop_probe <- function(platform) {
  switch(
    platform,
    windows = .windows_msvc_probe(),
    macos = .tool_version("xcodebuild", "-version"),
    linux = .tool_version(
      "pkg-config",
      c("--modversion", "webkit2gtk-4.1")
    ),
    list(available = FALSE, version = NA_character_, path = NA_character_)
  )
}

#' Inspect the local rpackit build environment
#'
#' Reports the current platform and availability of tools used by R package,
#' desktop, and server build targets. The function is read-only and does not
#' install missing software.
#'
#' @param quiet Suppress the human-readable summary.
#' @return An `rpackit_doctor` object.
#' @export
#' @examples
#' doctor()
doctor <- function(quiet = FALSE) {
  platform <- .rpackit_platform()
  specs <- list(
    R = list(command = file.path(R.home("bin"), "R"), args = "--version",
             purpose = "R application checks"),
    Rscript = list(command = file.path(R.home("bin"), "Rscript"),
                   args = "--version", purpose = "R launchers"),
    Git = list(command = "git", args = "--version",
               purpose = "source and release workflows"),
    Node = list(command = "node", args = "--version",
                purpose = "desktop frontend tooling"),
    Cargo = list(command = "cargo", args = "--version",
                 purpose = "Tauri desktop builds"),
    TauriCLI = list(command = "cargo", args = c("tauri", "--version"),
                    purpose = "Tauri desktop builds"),
    PackageManager = list(probe = .package_manager_probe(),
                          purpose = "Tauri frontend dependencies"),
    NativeDesktop = list(probe = .native_desktop_probe(platform$platform),
                         purpose = "platform-native desktop toolchain"),
    Docker = list(command = "docker", args = "--version",
                  purpose = "dynamic server bundles"),
    GitHubCLI = list(command = "gh", args = "--version",
                     purpose = "release workflows")
  )
  if (platform$platform == "windows") {
    specs$WebView2 <- list(
      probe = .windows_webview2_probe(),
      purpose = "Windows desktop webview"
    )
  }
  rows <- lapply(names(specs), function(name) {
    spec <- specs[[name]]
    result <- if (!is.null(spec$probe)) {
      spec$probe
    } else {
      .tool_version(spec$command, spec$args)
    }
    data.frame(
      tool = name,
      available = result$available,
      version = result$version,
      path = result$path,
      purpose = spec$purpose,
      stringsAsFactors = FALSE
    )
  })
  tools <- do.call(rbind, rows)
  desktop_requirements <- c(
    "Node",
    "Cargo",
    "TauriCLI",
    "PackageManager",
    "NativeDesktop"
  )
  if (platform$platform == "windows") {
    desktop_requirements <- c(desktop_requirements, "WebView2")
  }
  capabilities <- data.frame(
    task = c(
      "app inspection",
      "portable Windows runtime verification",
      "Tauri desktop build",
      "Docker server build"
    ),
    supported = c(
      TRUE,
      platform$platform == "windows",
      all(tools$available[tools$tool %in% desktop_requirements]) &&
        all(desktop_requirements %in% tools$tool),
      tools$available[tools$tool == "Docker"]
    ),
    stringsAsFactors = FALSE
  )
  result <- structure(
    list(
      platform = platform$platform,
      architecture = platform$architecture,
      r_version = as.character(getRversion()),
      tools = tools,
      capabilities = capabilities
    ),
    class = "rpackit_doctor"
  )
  if (!isTRUE(quiet)) {
    print(result)
  }
  invisible(result)
}

#' @export
print.rpackit_doctor <- function(x, ...) {
  cli::cli_h1("rpackit doctor")
  cli::cli_text("Platform: {x$platform} {x$architecture}")
  cli::cli_text("R: {x$r_version}")
  cli::cli_h2("Tools")
  for (index in seq_len(nrow(x$tools))) {
    row <- x$tools[index, ]
    marker <- if (isTRUE(row$available)) cli::col_green("\u2713") else cli::col_red("\u2717")
    version <- if (isTRUE(row$available) && !is.na(row$version)) {
      paste0(" - ", row$version)
    } else {
      ""
    }
    cli::cli_text("{marker} {row$tool}{version}")
  }
  cli::cli_h2("Supported tasks")
  for (index in seq_len(nrow(x$capabilities))) {
    row <- x$capabilities[index, ]
    marker <- if (isTRUE(row$supported)) cli::col_green("\u2713") else cli::col_yellow("-")
    cli::cli_text("{marker} {row$task}")
  }
  invisible(x)
}
