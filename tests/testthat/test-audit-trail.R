test_that("audit_trail() creates a valid trail object", {
  trail <- audit_trail("test_trail")

  expect_s3_class(trail, "audit_trail")
  expect_s3_class(trail, "environment")
  expect_equal(trail$name, "test_trail")
  expect_s3_class(trail$created_at, "POSIXct")
  expect_equal(trail$snapshots, list())
  expect_equal(trail$labels, character())
})

test_that("audit_trail() auto-generates name when NULL", {
  trail <- audit_trail()

  expect_true(grepl("^trail_", trail$name))
})

test_that("audit_trail uses reference semantics", {
  trail <- audit_trail("ref_test")
  trail_ref <- trail

  # Modify via one reference
  trail$snapshots[[1]] <- list(label = "test")

  # Should be visible via the other reference
  expect_equal(trail_ref$snapshots[[1]]$label, "test")
})

test_that("print.audit_trail works with no snapshots", {
  trail <- audit_trail("empty")

  output <- capture.output(print(trail), type = "message")
  combined <- paste(output, collapse = "\n")

  expect_true(grepl("Audit Trail", combined))
  expect_true(grepl("Snapshots: 0", combined))
  expect_true(grepl("No snapshots recorded", combined))
})

test_that("print.audit_trail works with snapshots", {
  trail <- audit_trail("print_test")
  mtcars |> audit_tap(trail, "raw")
  dplyr::filter(mtcars, mpg > 20) |> audit_tap(trail, "filtered")

  output <- capture.output(print(trail), type = "message")
  combined <- paste(output, collapse = "\n")

  expect_true(grepl("print_test", combined))
  expect_true(grepl("raw", combined))
  expect_true(grepl("filtered", combined))
})
