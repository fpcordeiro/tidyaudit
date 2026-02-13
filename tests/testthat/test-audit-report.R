test_that("audit_report works with console format", {
  trail <- audit_trail("report_test")
  mtcars |> audit_tap(trail, "raw")
  dplyr::filter(mtcars, mpg > 20) |> audit_tap(trail, "filtered")
  dplyr::mutate(dplyr::filter(mtcars, mpg > 20), mpg2 = mpg * 2) |>
    audit_tap(trail, "final")

  output <- capture.output(audit_report(trail), type = "message")
  combined <- paste(output, collapse = "\n")
  expect_true(grepl("Audit Report", combined))
})

test_that("audit_report returns trail invisibly", {
  trail <- audit_trail("invisible_test")
  mtcars |> audit_tap(trail, "raw")

  capture.output(result <- audit_report(trail), type = "message")
  expect_identical(result, trail)
})

test_that("audit_report errors on non-trail", {
  expect_error(audit_report("not_a_trail"), "audit_trail")
})

test_that("audit_report errors on rmd format", {
  trail <- audit_trail("rmd_test")
  expect_error(audit_report(trail, format = "rmd"), "not yet implemented")
})

test_that("audit_report works with empty trail", {
  trail <- audit_trail("empty_report")
  output <- capture.output(audit_report(trail), type = "message")
  combined <- paste(output, collapse = "\n")
  expect_true(grepl("No snapshots", combined))
})

test_that("audit_report shows custom diagnostics", {
  trail <- audit_trail("custom_report")
  mtcars |> audit_tap(trail, "raw", .fns = list(
    n_cyl_types = ~length(unique(.x$cyl))
  ))

  output <- capture.output(audit_report(trail), type = "message")
  combined <- paste(output, collapse = "\n")
  expect_true(grepl("Custom Diagnostics", combined))
  expect_true(grepl("n_cyl_types", combined))
})

test_that("audit_report shows unnamed .fns with auto-generated names", {
  trail <- audit_trail("unnamed_fns_report")
  mtcars |> audit_tap(trail, "raw", .fns = list(nrow, ~ncol(.x)))

  output <- capture.output(audit_report(trail), type = "message")
  combined <- paste(output, collapse = "\n")
  expect_true(grepl("Custom Diagnostics", combined))
  expect_true(grepl("fn_1", combined))
  expect_true(grepl("fn_2", combined))
})
