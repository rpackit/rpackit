make_app <- function(lines, layout = "app.R") {
  path <- tempfile("rpackit-app-")
  dir.create(path)
  if (layout == "app.R") {
    writeLines(lines, file.path(path, "app.R"))
  } else {
    writeLines("ui <- shiny::fluidPage('hello')", file.path(path, "ui.R"))
    writeLines(lines, file.path(path, "server.R"))
  }
  path
}

test_that("check_app recognizes a standard Shiny app", {
  path <- make_app(c(
    "library(shiny)",
    "shinyApp(fluidPage('hello'), function(input, output) {})"
  ))
  result <- check_app(path, quiet = TRUE)

  expect_s3_class(result, "rpackit_app_check")
  expect_identical(result$app_type, "shiny-single-file")
  expect_true("shiny" %in% result$packages)
  expect_identical(
    result$targets$status[result$targets$target == "portable desktop"],
    "yes"
  )
  expect_identical(
    result$targets$status[result$targets$target == "static web"],
    "maybe"
  )
})

test_that("check_app recognizes split Shiny layout", {
  path <- make_app(
    "server <- function(input, output) {}",
    layout = "split"
  )
  result <- check_app(path, quiet = TRUE)
  expect_identical(result$app_type, "shiny-split")
  expect_true(result$findings$has_ui_server)
})

test_that("static blockers and runtime risks are reported", {
  path <- make_app(c(
    "library(reticulate)",
    "Rsamtools::scanBam('reads.bam')",
    "system2('tool', '--version')"
  ))
  result <- check_app(path, quiet = TRUE)

  expect_identical(
    result$targets$status[result$targets$target == "static web"],
    "no"
  )
  expect_identical(
    result$targets$status[result$targets$target == "portable desktop"],
    "maybe"
  )
  expect_true(result$findings$has_reticulate)
  expect_true(result$findings$has_system_calls)
  expect_true("Rsamtools" %in% result$findings$native_risk_packages)
})

test_that("unknown layouts are rejected for build targets", {
  path <- tempfile("rpackit-not-app-")
  dir.create(path)
  writeLines("print('hello')", file.path(path, "analysis.R"))
  result <- check_app(path, quiet = TRUE)

  expect_identical(result$app_type, "unknown")
  expect_true(all(result$targets$status == "no"))
})

test_that("missing directories fail clearly", {
  expect_error(check_app(tempfile(), quiet = TRUE), "existing directory")
})
