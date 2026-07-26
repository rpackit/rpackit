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

test_that("system-call findings come from syntax rather than raw text", {
  path <- make_app(c(
    "# system('comment only')",
    "message(\"system2('quoted example')\")",
    "tool <- list(system = function(...) NULL)",
    "tool$system('object method')",
    "notbase::shell('foreign namespace')",
    "shiny::shinyApp(",
    "  shiny::fluidPage('hello'),",
    "  function(input, output) {}",
    ")"
  ))

  result <- check_app(path, quiet = TRUE)

  expect_false(result$findings$has_system_calls)
  expect_identical(
    result$findings$system_calls,
    data.frame(
      call = character(),
      file = character(),
      line = integer(),
      stringsAsFactors = FALSE
    )
  )
  expect_identical(
    result$targets$status[result$targets$target == "static web"],
    "maybe"
  )
  expect_identical(
    result$targets$status[result$targets$target == "portable desktop"],
    "yes"
  )
})

test_that("system-call findings identify direct base calls and locations", {
  path <- make_app(c(
    "system2('tool', '--version')",
    "base::system('tool')",
    "base:::shell('tool')",
    "notbase::system('foreign namespace')",
    "tool <- list(system = function(...) NULL)",
    "tool$system('object method')"
  ))

  result <- check_app(path, quiet = TRUE)
  calls <- result$findings$system_calls

  expect_true(result$findings$has_system_calls)
  expect_identical(calls$call, c("system2", "system", "shell"))
  expect_identical(calls$file, rep("app.R", 3L))
  expect_identical(calls$line, 1:3)
  expect_identical(
    result$targets$status[result$targets$target == "static web"],
    "no"
  )
  expect_match(
    result$targets$reason[
      result$targets$target == "portable desktop"
    ],
    "app[.]R:1, app[.]R:2, app[.]R:3"
  )
})

test_that("system-call parsing is independent of the parse-data option", {
  path <- make_app("system2('tool', '--version')")
  previous_options <- options(keep.parse.data = FALSE)
  on.exit(options(previous_options), add = TRUE)

  result <- check_app(path, quiet = TRUE)

  expect_false(getOption("keep.parse.data"))
  expect_true(result$findings$has_system_calls)
  expect_identical(result$findings$system_calls$call, "system2")
  expect_identical(result$findings$system_calls$line, 1L)
})

test_that("backticked and parenthesized direct base calls are detected", {
  path <- make_app(c(
    "`system`('tool')",
    "base::`system2`('tool')",
    "(shell)('tool')",
    "((system2))('tool')",
    "(base::system)('tool')",
    "notbase::`system`('foreign namespace')",
    "(notbase::system)('foreign namespace')",
    "(tool$system)('object method')"
  ))

  result <- check_app(path, quiet = TRUE)
  calls <- result$findings$system_calls

  expect_identical(
    calls$call,
    c("system", "system2", "shell", "system2", "system")
  )
  expect_identical(calls$file, rep("app.R", 5L))
  expect_identical(calls$line, 1:5)
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
