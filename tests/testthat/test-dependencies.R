make_dependency_app <- function(files = list()) {
  path <- tempfile("rpackit-dependencies-")
  dir.create(path)
  for (name in names(files)) {
    destination <- file.path(path, name)
    dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
    writeLines(files[[name]], destination, useBytes = TRUE)
  }
  path
}

dependency_row <- function(plan, package) {
  plan$dependencies[plan$dependencies$package == package, , drop = FALSE]
}

test_that("source parsing finds static package calls without false positives", {
  path <- make_dependency_app(list(
    "app.R" = c(
      "# library(commentpkg)",
      "message(\"require(stringpkg)\")",
      "message(\"reticulate::py_run_string('pass')\")",
      "library(shiny)",
      "require(package = \"dplyr\")",
      "library(package = ggplot2)",
      "requireNamespace(\"jsonlite\", quietly = TRUE)",
      "jsonlite::toJSON(list(ready = TRUE))",
      "rlang:::is_string(\"ready\")",
      "loadNamespace(\"cli\")",
      "pkg <- \"later\"",
      "library(pkg, character.only = TRUE)",
      "requireNamespace(pkg)"
    )
  ))

  plan <- plan_dependencies(path)

  expect_s3_class(plan, "rpackit_dependency_plan")
  expect_setequal(
    plan$dependencies$package,
    c("shiny", "dplyr", "ggplot2", "jsonlite", "rlang", "cli")
  )
  expect_false(any(c("commentpkg", "stringpkg", "reticulate", "later") %in%
                     plan$dependencies$package))
  expect_true(all(plan$dependencies$direct))
  expect_identical(
    plan$references$detail[plan$references$package == "rlang"],
    ":::"
  )
  expect_identical(
    plan$references$line[
      plan$references$package == "ggplot2" &
        plan$references$detail == "library"
    ],
    6L
  )
  expect_identical(nrow(plan$diagnostics), 2L)
  expect_true(all(plan$diagnostics$code == "dynamic-package-name"))
  expect_identical(plan$diagnostics$line, c(12L, 13L))
})

test_that("DESCRIPTION and renv.lock retain precedence and provenance", {
  path <- make_dependency_app(list(
    "app.R" = "dplyr::filter(data.frame(x = 1), x == 1)",
    "DESCRIPTION" = c(
      "Package: dependencyfixture",
      "Version: 0.0.1",
      "Title: Dependency Fixture",
      "Description: Exercises dependency planning.",
      "License: MIT",
      "Depends: R (>= 4.4.0)",
      "Imports: dplyr (>= 1.1.0), cli",
      "LinkingTo: Rcpp",
      "Suggests: testthat"
    ),
    "renv.lock" = c(
      "{",
      '  "R": {"Version": "4.6.1"},',
      '  "Packages": {',
      '    "dplyr": {',
      '      "Version": "1.1.4",',
      '      "Source": "Repository",',
      '      "Repository": "CRAN"',
      "    },",
      '    "cli": {',
      '      "Version": "3.6.5",',
      '      "Source": "Repository",',
      '      "Repository": "CRAN"',
      "    },",
      '    "Rcpp": {',
      '      "Version": "1.1.0",',
      '      "Source": "Repository",',
      '      "Repository": "CRAN"',
      "    },",
      '    "rlang": {',
      '      "Version": "1.1.6",',
      '      "Source": "Repository",',
      '      "Repository": "CRAN"',
      "    },",
      '    "devpkg": {',
      '      "Version": "0.2.0",',
      '      "Source": "GitHub",',
      '      "RemoteType": "github",',
      '      "RemoteHost": "api.github.com",',
      '      "RemoteUsername": "rpackit",',
      '      "RemoteRepo": "devpkg",',
      '      "RemoteRef": "main",',
      '      "RemoteSha": "abc123"',
      "    }",
      "  }",
      "}"
    )
  ))

  plan <- plan_dependencies(path)
  dplyr <- dependency_row(plan, "dplyr")
  rlang <- dependency_row(plan, "rlang")

  expect_identical(dplyr$version, "1.1.4")
  expect_identical(dplyr$constraint, ">= 1.1.0")
  expect_identical(dplyr$roles, "Imports")
  expect_true(dplyr$direct)
  expect_true(dplyr$locked)
  expect_match(dplyr$provenance, "source::")
  expect_match(dplyr$provenance, "DESCRIPTION:Imports", fixed = TRUE)
  expect_match(dplyr$provenance, "renv.lock", fixed = TRUE)

  expect_false(rlang$direct)
  expect_true(rlang$required)
  expect_true(rlang$locked)
  expect_identical(rlang$version, "1.1.6")
  expect_identical(plan$r_constraint, ">= 4.4.0")
  expect_identical(plan$locked_r_version, "4.6.1")
  expect_identical(
    dependency_row(plan, "devpkg")$remote,
    "github:api.github.com/rpackit/devpkg@main#abc123"
  )
  expect_false("testthat" %in% plan$dependencies$package)

  with_suggests <- plan_dependencies(path, include_suggests = TRUE)
  testthat <- dependency_row(with_suggests, "testthat")
  expect_identical(testthat$roles, "Suggests")
  expect_true(testthat$direct)
  expect_false(testthat$required)
})

test_that("source parse failures identify the file and do not execute code", {
  invalid <- make_dependency_app(list(
    "R/broken.R" = c("library(shiny)", "if (TRUE) {")
  ))
  expect_error(
    plan_dependencies(invalid),
    regexp = "R/broken.R",
    class = "rpackit_dependency_source_parse_error"
  )

  safe <- make_dependency_app()
  marker <- file.path(safe, "executed")
  marker_for_r <- gsub("\\\\", "/", marker)
  writeLines(
    c(
      sprintf("file.create(%s)", dQuote(marker_for_r)),
      "stop('application source must not run')",
      "library(shiny)"
    ),
    file.path(safe, "app.R")
  )
  plan <- plan_dependencies(safe)
  expect_true("shiny" %in% plan$dependencies$package)
  expect_false(file.exists(marker))
})

test_that("read, DESCRIPTION, and lockfile failures are explicit", {
  expect_error(
    rpackit:::.read_dependency_lines(
      tempfile("missing-source-"),
      "R/missing.R",
      "R source"
    ),
    regexp = "Cannot read.*R/missing.R",
    class = "rpackit_dependency_read_error"
  )

  bad_description <- make_dependency_app(list(
    "DESCRIPTION" = c(
      "Package: dependencyfixture",
      "Version: 0.0.1",
      "Imports: goodpkg, not a package"
    )
  ))
  expect_error(
    plan_dependencies(bad_description),
    regexp = "Invalid dependency entry",
    class = "rpackit_dependency_description_parse_error"
  )

  bad_lock <- make_dependency_app(list(
    "renv.lock" = '{"Packages": {"shiny": {"Version": }}}'
  ))
  expect_error(
    plan_dependencies(bad_lock),
    regexp = "Cannot parse.*renv.lock",
    class = "rpackit_dependency_lockfile_parse_error"
  )

  missing_version <- make_dependency_app(list(
    "renv.lock" = '{"Packages": {"shiny": {"Source": "Repository"}}}'
  ))
  expect_error(
    plan_dependencies(missing_version),
    regexp = "has no Version field",
    class = "rpackit_dependency_lockfile_parse_error"
  )
})

test_that("empty applications and arguments have stable behavior", {
  path <- make_dependency_app()
  plan <- plan_dependencies(path)

  expect_identical(nrow(plan$dependencies), 0L)
  expect_identical(nrow(plan$references), 0L)
  expect_identical(nrow(plan$diagnostics), 0L)
  expect_identical(plan$r_files, character())
  expect_error(plan_dependencies(tempfile()), "existing directory")
  expect_error(
    plan_dependencies(path, include_suggests = NA),
    "must be TRUE or FALSE"
  )
})

test_that("check_app keeps source package output and exposes the full plan", {
  path <- make_dependency_app(list(
    "app.R" = c(
      "# library(commentpkg)",
      "message(\"require(stringpkg)\")",
      "shiny::shinyApp(shiny::fluidPage('hello'), function(input, output) {})"
    ),
    "DESCRIPTION" = c(
      "Package: dependencyfixture",
      "Version: 0.0.1",
      "Title: Dependency Fixture",
      "Description: Exercises check app integration.",
      "License: MIT",
      "Imports: reticulate"
    )
  ))

  result <- check_app(path, quiet = TRUE)

  expect_identical(result$packages, "shiny")
  expect_s3_class(result$dependency_plan, "rpackit_dependency_plan")
  expect_true("reticulate" %in% result$dependency_plan$dependencies$package)
  expect_true(result$findings$has_reticulate)
  expect_false(any(c("commentpkg", "stringpkg") %in% result$packages))

  string_only <- make_dependency_app(list(
    "app.R" = c(
      "message(\"reticulate::py_run_string('pass')\")",
      "shiny::shinyApp(shiny::fluidPage('hello'), function(input, output) {})"
    )
  ))
  string_result <- check_app(string_only, quiet = TRUE)
  expect_false(string_result$findings$has_reticulate)
})
