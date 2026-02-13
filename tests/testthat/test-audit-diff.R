test_that("audit_diff computes correct deltas", {
  trail <- audit_trail("diff_test")
  mtcars |> audit_tap(trail, "raw")
  dplyr::filter(mtcars, mpg > 20) |> audit_tap(trail, "filtered")

  diff_obj <- audit_diff(trail, "raw", "filtered")

  expect_s3_class(diff_obj, "audit_diff")
  expect_equal(diff_obj$from_label, "raw")
  expect_equal(diff_obj$to_label, "filtered")
  expect_true(diff_obj$row_delta < 0)
  expect_equal(diff_obj$col_delta, 0L)
  expect_equal(diff_obj$from_nrow, nrow(mtcars))
  expect_equal(diff_obj$to_nrow, sum(mtcars$mpg > 20))
})

test_that("audit_diff works with integer indices", {
  trail <- audit_trail("idx_test")
  mtcars |> audit_tap(trail, "raw")
  dplyr::filter(mtcars, mpg > 20) |> audit_tap(trail, "filtered")

  diff_obj <- audit_diff(trail, 1L, 2L)

  expect_equal(diff_obj$from_label, "raw")
  expect_equal(diff_obj$to_label, "filtered")
})

test_that("audit_diff detects columns added/removed", {
  trail <- audit_trail("col_diff")
  mtcars |> audit_tap(trail, "raw")
  dplyr::select(mtcars, -cyl, -disp) |>
    dplyr::mutate(new_col = 1) |>
    audit_tap(trail, "modified")

  diff_obj <- audit_diff(trail, "raw", "modified")

  expect_true("new_col" %in% diff_obj$cols_added)
  expect_true("cyl" %in% diff_obj$cols_removed)
  expect_true("disp" %in% diff_obj$cols_removed)
})

test_that("audit_diff detects type changes", {
  trail <- audit_trail("type_diff")
  df <- data.frame(x = 1:5, y = letters[1:5], stringsAsFactors = FALSE)
  df |> audit_tap(trail, "before")
  dplyr::mutate(df, x = as.character(x)) |> audit_tap(trail, "after")

  diff_obj <- audit_diff(trail, "before", "after")

  expect_false(is.null(diff_obj$type_changes))
  expect_true("x" %in% diff_obj$type_changes$column)
  expect_equal(diff_obj$type_changes$from[diff_obj$type_changes$column == "x"], "integer")
  expect_equal(diff_obj$type_changes$to[diff_obj$type_changes$column == "x"], "character")
})

test_that("audit_diff detects NA changes per column", {
  trail <- audit_trail("na_diff")
  df <- data.frame(x = 1:10, y = c(1:5, rep(NA, 5)))
  df |> audit_tap(trail, "before")
  df[1:5, ] |> audit_tap(trail, "after")

  diff_obj <- audit_diff(trail, "before", "after")

  expect_false(is.null(diff_obj$na_changes))
  # y had 5 NAs, now has 0
  y_change <- diff_obj$na_changes[diff_obj$na_changes$column == "y", ]
  expect_equal(y_change$from_na, 5L)
  expect_equal(y_change$to_na, 0L)
  expect_equal(y_change$delta, -5L)
})

test_that("audit_diff computes numeric shifts", {
  trail <- audit_trail("num_diff")
  df <- data.frame(x = 1:100, y = rnorm(100, mean = 10))
  df |> audit_tap(trail, "before")
  dplyr::mutate(df, x = x * 2) |> audit_tap(trail, "after")

  diff_obj <- audit_diff(trail, "before", "after")

  expect_false(is.null(diff_obj$numeric_shifts))
  x_shift <- diff_obj$numeric_shifts[diff_obj$numeric_shifts$column == "x", ]
  expect_true(x_shift$mean_shift > 0)
})

test_that("audit_diff errors on invalid label", {
  trail <- audit_trail("err_test")
  mtcars |> audit_tap(trail, "raw")

  expect_error(audit_diff(trail, "raw", "nonexistent"), "not found")
})

test_that("audit_diff errors on out-of-range index", {
  trail <- audit_trail("err_test2")
  mtcars |> audit_tap(trail, "raw")

  expect_error(audit_diff(trail, 1L, 5L), "out of range")
})

test_that("print.audit_diff produces output", {
  trail <- audit_trail("print_diff")
  mtcars |> audit_tap(trail, "raw")
  dplyr::filter(mtcars, mpg > 20) |> audit_tap(trail, "filtered")

  diff_obj <- audit_diff(trail, "raw", "filtered")
  output <- capture.output(print(diff_obj), type = "message")
  combined <- paste(output, collapse = "\n")
  expect_true(grepl("Audit Diff", combined))
})

test_that("audit_diff rejects non-integerish numeric refs", {
  trail <- audit_trail("integerish_test")
  mtcars |> audit_tap(trail, "raw")

  expect_error(audit_diff(trail, 1.5, 1L), "whole number")
  expect_error(audit_diff(trail, 1L, 1.9), "whole number")
  expect_error(audit_diff(trail, NA_real_, 1L), "whole number")
  expect_error(audit_diff(trail, 1L, Inf), "whole number")
})

test_that("audit_diff comparing same snapshot to itself gives zero deltas", {
  trail <- audit_trail("self_diff")
  mtcars |> audit_tap(trail, "raw")

  diff_obj <- audit_diff(trail, "raw", "raw")
  expect_equal(diff_obj$row_delta, 0L)
  expect_equal(diff_obj$col_delta, 0L)
  expect_equal(diff_obj$na_delta, 0L)
  expect_length(diff_obj$cols_added, 0L)
  expect_length(diff_obj$cols_removed, 0L)
  expect_null(diff_obj$type_changes)
  expect_null(diff_obj$na_changes)
})

test_that("audit_diff compares non-consecutive snapshots", {
  trail <- audit_trail("nonconsec_test")
  mtcars |> audit_tap(trail, "s1")
  dplyr::filter(mtcars, mpg > 20) |> audit_tap(trail, "s2")
  dplyr::select(dplyr::filter(mtcars, mpg > 20), -cyl) |> audit_tap(trail, "s3")

  # Compare s1 to s3 directly, skipping s2
  diff_obj <- audit_diff(trail, "s1", "s3")
  expect_true(diff_obj$row_delta < 0L)
  expect_equal(diff_obj$col_delta, -1L)
  expect_true("cyl" %in% diff_obj$cols_removed)
})
