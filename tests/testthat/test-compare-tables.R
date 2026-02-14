test_that("compare_tables detects identical tables", {
  x <- data.frame(id = 1:3, value = c(10, 20, 30))
  result <- compare_tables(x, x)

  expect_s3_class(result, "compare_tbl")
  expect_equal(result$nrow_x, 3L)
  expect_equal(result$nrow_y, 3L)
  expect_equal(length(result$only_x), 0L)
  expect_equal(length(result$only_y), 0L)
})

test_that("compare_tables detects column differences", {
  x <- data.frame(id = 1:3, value = 1:3, extra_x = 4:6)
  y <- data.frame(id = 1:3, value = 1:3, extra_y = 7:9)
  result <- compare_tables(x, y)

  expect_equal(result$only_x, "extra_x")
  expect_equal(result$only_y, "extra_y")
  expect_true("id" %in% result$common_columns)
  expect_true("value" %in% result$common_columns)
})

test_that("compare_tables detects row count differences", {
  x <- data.frame(id = 1:5, value = 1:5)
  y <- data.frame(id = 1:3, value = 1:3)
  result <- compare_tables(x, y)

  expect_equal(result$nrow_x, 5L)
  expect_equal(result$nrow_y, 3L)
})

test_that("compare_tables detects type mismatches", {
  x <- data.frame(id = 1:3, mixed = c(1L, 2L, 3L))
  y <- data.frame(id = 1:3, mixed = c("a", "b", "c"), stringsAsFactors = FALSE)
  result <- compare_tables(x, y)

  expect_false(is.null(result$type_mismatches))
  expect_equal(nrow(result$type_mismatches), 1L)
  expect_equal(result$type_mismatches$column, "mixed")
})

test_that("compare_tables auto-detects keys", {
  x <- data.frame(id = c(1L, 2L, 3L), name = c("a", "b", "c"),
                   value = c(10.0, 20.0, 30.0), stringsAsFactors = FALSE)
  y <- data.frame(id = c(1L, 2L, 3L), name = c("a", "b", "c"),
                   value = c(10.1, 20.0, 30.5), stringsAsFactors = FALSE)
  result <- compare_tables(x, y)

  expect_false(is.null(result$key_summary))
  expect_true(result$key_summary$auto)
  expect_true("id" %in% result$key_summary$keys)
  expect_true("name" %in% result$key_summary$keys)
})

test_that("compare_tables works with explicit key_cols", {
  x <- data.frame(id = 1:3, value = c(10.0, 20.0, 30.0))
  y <- data.frame(id = 1:3, value = c(10.1, 20.0, 30.5))
  result <- compare_tables(x, y, key_cols = "id")

  expect_false(is.null(result$key_summary))
  expect_false(result$key_summary$auto)
  expect_equal(result$key_summary$keys, "id")
})

test_that("compare_tables computes numeric discrepancies with keys", {
  x <- data.frame(id = 1:3, value = c(10.0, 20.0, 30.0))
  y <- data.frame(id = 1:3, value = c(10.5, 20.0, 30.5))
  result <- compare_tables(x, y, key_cols = "id")

  expect_false(is.null(result$numeric_summary))
  expect_equal(result$numeric_method, "keys")
  expect_equal(result$rows_matched, 3L)
  expect_equal(result$numeric_summary$column, "value")
})

test_that("compare_tables computes numeric discrepancies by row index", {
  x <- data.frame(value = c(10.0, 20.0, 30.0))
  y <- data.frame(value = c(10.5, 20.0, 30.5))
  result <- compare_tables(x, y)

  expect_equal(result$numeric_method, "row_index")
  expect_false(is.null(result$numeric_summary))
})

test_that("compare_tables errors with no common columns", {
  x <- data.frame(a = 1:3)
  y <- data.frame(b = 1:3)
  expect_error(compare_tables(x, y), "No matching column names")
})

test_that("compare_tables errors with invalid key_cols", {
  x <- data.frame(id = 1:3)
  y <- data.frame(id = 1:3)
  expect_error(compare_tables(x, y, key_cols = "nonexistent"), "not present")
})

test_that("compare_tables key overlap summary is correct", {
  x <- data.frame(id = c(1L, 2L, 3L), value = c(10, 20, 30))
  y <- data.frame(id = c(2L, 3L, 4L), value = c(40, 50, 60))
  result <- compare_tables(x, y, key_cols = "id")

  expect_equal(result$key_summary$matches, 2L)
  expect_equal(result$key_summary$only_x, 1L)
  expect_equal(result$key_summary$only_y, 1L)
})

test_that("print.compare_tbl produces output", {
  x <- data.frame(id = 1:3, value = c(10.0, 20.0, 30.0))
  y <- data.frame(id = 1:3, value = c(10.1, 20.0, 30.5))
  result <- compare_tables(x, y, key_cols = "id")

  output <- capture.output(print(result), type = "message")
  combined <- paste(output, collapse = "\n")
  expect_true(grepl("Table Comparison", combined))
})

test_that("compare_tables works with tibbles", {
  skip_if_not_installed("dplyr")
  x <- dplyr::tibble(id = 1:3, value = c(10.0, 20.0, 30.0))
  y <- dplyr::tibble(id = 1:3, value = c(10.1, 20.0, 30.5))
  result <- compare_tables(x, y, key_cols = "id")

  expect_s3_class(result, "compare_tbl")
  expect_false(is.null(result$numeric_summary))
})

test_that("compare_tables handles no numeric columns", {
  x <- data.frame(id = c("a", "b", "c"), stringsAsFactors = FALSE)
  y <- data.frame(id = c("a", "b", "d"), stringsAsFactors = FALSE)
  result <- compare_tables(x, y)

  expect_true(is.na(result$numeric_method))
  expect_null(result$numeric_summary)
})

test_that("compare_tables warns when key columns are non-unique", {
  x <- data.frame(id = c(1L, 1L, 2L), value = c(10.0, 20.0, 30.0))
  y <- data.frame(id = c(1L, 2L), value = c(10.1, 30.5))

  expect_warning(
    compare_tables(x, y, key_cols = "id"),
    "not unique"
  )
})
