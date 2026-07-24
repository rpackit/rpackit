test_that("doctor reports platform, tools, and capabilities", {
  result <- doctor(quiet = TRUE)

  expect_s3_class(result, "rpackit_doctor")
  expect_true(result$platform %in% c("windows", "macos", "linux"))
  expect_true(all(c("R", "Rscript", "Git") %in% result$tools$tool))
  expect_true(result$tools$available[result$tools$tool == "R"])
  expect_true("app inspection" %in% result$capabilities$task)
})
