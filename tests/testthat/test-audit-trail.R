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

test_that(".is_named_scalars() detects Case 2 correctly", {
  # TRUE cases
  expect_true(tidyaudit:::.is_named_scalars(c(a = 1, b = 2)))
  expect_true(tidyaudit:::.is_named_scalars(c(x = "foo", y = "bar")))
  expect_true(tidyaudit:::.is_named_scalars(list(n = 5L, pct = 98.2)))

  # FALSE cases
  expect_false(tidyaudit:::.is_named_scalars(42))
  expect_false(tidyaudit:::.is_named_scalars(c(1, 2, 3)))
  expect_false(tidyaudit:::.is_named_scalars(list(a = 1:3)))
  expect_false(tidyaudit:::.is_named_scalars(data.frame(a = 1, b = 2)))
  expect_false(tidyaudit:::.is_named_scalars(list(a = list(x = 1))))
  expect_false(tidyaudit:::.is_named_scalars(setNames(character(0), character(0))))  # zero-length
})

test_that(".format_custom_result() renders Case 1 (scalar)", {
  result <- tidyaudit:::.format_custom_result("n_rows", 42L)
  expect_true(grepl("\u21b3 n_rows: 42", result))
})

test_that(".format_custom_result() renders Case 2 (named vector)", {
  result <- tidyaudit:::.format_custom_result("check", c(a = 1, b = 2))
  expect_true(grepl("\u21b3 check:", result))
  expect_true(grepl("a=1", result))
  expect_true(grepl("b=2", result))
})

test_that(".format_custom_result() renders Case 2 (named list of scalars)", {
  result <- tidyaudit:::.format_custom_result("quality", list(n_nulls = 5L, pct = 98.2))
  expect_true(grepl("\u21b3 quality:", result))
  expect_true(grepl("n_nulls=5", result))
  expect_true(grepl("pct=98.2", result))
})

test_that(".format_custom_result() truncates Case 2 output beyond 60 chars", {
  long_result <- setNames(as.double(1:20), paste0("p", 1:20))
  result <- tidyaudit:::.format_custom_result("deciles", long_result)
  value_portion <- sub(".*\u21b3 deciles: ", "", result)
  expect_true(endsWith(value_portion, "..."))
  expect_equal(nchar(value_portion), 63L)  # exactly 60 chars + "..."
})

test_that(".format_custom_result() renders Case 3 (complex)", {
  result <- tidyaudit:::.format_custom_result("report", data.frame(x = 1))
  expect_true(grepl("\u21b3 report:", result))
  expect_true(grepl("complex", result))
  expect_true(grepl("audit_report", result))

  result2 <- tidyaudit:::.format_custom_result("vec", 1:5)  # unnamed length-5 vector
  expect_true(grepl("complex", result2))
})

test_that("print.audit_trail shows inline annotations for custom diagnostics", {
  trail <- audit_trail("custom_test")

  mtcars |> audit_tap(trail, "raw", .fns = list(
    n_rows  = ~nrow(.),
    quality = ~c(n_na = sum(is.na(.)), n_cols = ncol(.))
  ))

  dplyr::filter(mtcars, mpg > 20) |> audit_tap(trail, "filtered", .fns = list(
    n_rows  = ~nrow(.),
    quality = ~c(n_na = sum(is.na(.)), n_cols = ncol(.))
  ))

  output <- capture.output(print(trail), type = "message")
  combined <- paste(output, collapse = "\n")

  expect_true(grepl("\u21b3 n_rows", combined))
  expect_true(grepl("\u21b3 quality", combined))

  expect_equal(sum(grepl("\u21b3 n_rows", output)), 2L)
})

test_that("print.audit_trail shows no annotation lines when .fns is not used", {
  trail <- audit_trail("no_custom")
  mtcars |> audit_tap(trail, "raw")

  output <- capture.output(print(trail), type = "message")
  combined <- paste(output, collapse = "\n")

  expect_false(grepl("\u21b3", combined))
})

test_that("print.audit_trail shows annotation only for snapshots with custom diagnostics", {
  trail <- audit_trail("partial_custom")
  mtcars |> audit_tap(trail, "raw")
  mtcars |> audit_tap(trail, "checked", .fns = list(n = ~nrow(.)))

  output <- capture.output(print(trail), type = "message")

  expect_equal(sum(grepl("\u21b3", output)), 1L)
})

test_that("print.audit_trail respects show_custom = FALSE", {
  trail <- audit_trail("show_custom_test")
  mtcars |> audit_tap(trail, "raw", .fns = list(n = ~nrow(.)))

  output <- capture.output(print(trail, show_custom = FALSE), type = "message")
  combined <- paste(output, collapse = "\n")

  expect_false(grepl("\u21b3", combined))
  expect_true(grepl("raw", combined))  # row still present
})
