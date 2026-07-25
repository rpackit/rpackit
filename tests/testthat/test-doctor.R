test_that("doctor reports platform, tools, and capabilities", {
  result <- doctor(quiet = TRUE)

  expect_s3_class(result, "rpackit_doctor")
  expect_true(result$platform %in% c("windows", "macos", "linux"))
  expect_true(all(c("R", "Rscript", "Git") %in% result$tools$tool))
  expect_true(result$tools$available[result$tools$tool == "R"])
  expect_true("app inspection" %in% result$capabilities$task)
  expect_true(
    all(c("TauriCLI", "PackageManager", "NativeDesktop") %in%
          result$tools$tool)
  )
  desktop_requirements <- c(
    "Node", "Cargo", "TauriCLI", "PackageManager", "NativeDesktop"
  )
  if (result$platform == "windows") {
    desktop_requirements <- c(desktop_requirements, "WebView2")
  }
  expected <- all(
    result$tools$available[
      match(desktop_requirements, result$tools$tool)
    ]
  )
  expect_identical(
    result$capabilities$supported[
      result$capabilities$task == "Tauri desktop build"
    ],
    expected
  )
})

test_that("doctor does not infer Tauri support from Cargo alone", {
  testthat::local_mocked_bindings(
    .tool_version = function(command, arguments = "--version") {
      tauri <- identical(command, "cargo") &&
        identical(arguments, c("tauri", "--version"))
      list(
        available = !tauri,
        version = if (tauri) NA_character_ else "available",
        path = if (tauri) NA_character_ else command
      )
    },
    .package_manager_probe = function() {
      list(available = TRUE, version = "npm", path = "npm")
    },
    .native_desktop_probe = function(platform) {
      list(available = TRUE, version = "native", path = "native")
    },
    .windows_webview2_probe = function() {
      list(available = TRUE, version = "webview", path = "webview")
    },
    .package = "rpackit"
  )

  result <- doctor(quiet = TRUE)
  expect_false(
    result$capabilities$supported[
      result$capabilities$task == "Tauri desktop build"
    ]
  )
  expect_false(
    result$tools$available[result$tools$tool == "TauriCLI"]
  )
})
