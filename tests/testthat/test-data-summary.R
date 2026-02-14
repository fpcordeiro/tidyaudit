test_that("summarize_column works for numeric", {
  result <- summarize_column(c(1, 2, 3, NA, 5))

  expect_equal(result[["type"]], "numeric")
  expect_equal(result[["missing"]], "1")
  expect_equal(result[["n_unique"]], "4")
  expect_false(is.na(result[["mean"]]))
  expect_false(is.na(result[["min"]]))
  expect_false(is.na(result[["max"]]))
})

test_that("summarize_column works for character", {
  result <- summarize_column(c("a", "b", "a", "c"))

  expect_equal(result[["type"]], "character")
  expect_equal(result[["n_unique"]], "3")
  expect_equal(result[["missing"]], "0")
  expect_equal(result[["most_frequent"]], "a")
  expect_true(is.na(result[["mean"]]))
})

test_that("summarize_column works for logical", {
  result <- summarize_column(c(TRUE, FALSE, TRUE, NA))

  expect_equal(result[["type"]], "logical")
  expect_equal(result[["missing"]], "1")
  expect_false(is.na(result[["mean"]]))
})

test_that("summarize_column works for factor", {
  result <- summarize_column(factor(c("x", "y", "x")))

  expect_equal(result[["type"]], "factor")
  expect_equal(result[["n_unique"]], "2")
  expect_equal(result[["most_frequent"]], "x")
})

test_that("summarize_column works for Date", {
  result <- summarize_column(as.Date(c("2023-01-01", "2023-06-15", NA)))

  expect_equal(result[["type"]], "Date")
  expect_equal(result[["missing"]], "1")
  expect_equal(result[["min"]], "2023-01-01")
  expect_equal(result[["max"]], "2023-06-15")
})

test_that("summarize_column handles all-NA", {
  result <- summarize_column(c(NA_real_, NA_real_))

  expect_equal(result[["type"]], "numeric")
  expect_equal(result[["missing"]], "2")
  expect_equal(result[["n_unique"]], "0")
  expect_true(is.na(result[["mean"]]))
})

test_that("summarize_column returns named character vector", {
  result <- summarize_column(1:5)
  expect_true(is.character(result))
  expect_equal(length(result), 14L)
  expect_true(all(c("type", "n_unique", "missing", "mean") %in% names(result)))
})

test_that("get_summary_table returns data.frame with all columns", {
  df <- data.frame(
    id = 1:5,
    value = c(10, 20, 30, 40, 50),
    name = c("a", "b", "c", "d", "e"),
    stringsAsFactors = FALSE
  )
  result <- get_summary_table(df)

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 3L)
  expect_true("variable" %in% names(result))
  expect_true("type" %in% names(result))
})

test_that("get_summary_table filters by cols", {
  df <- data.frame(id = 1:5, value = 1:5, name = letters[1:5],
                   stringsAsFactors = FALSE)
  result <- get_summary_table(df, cols = c("id", "value"))

  expect_equal(nrow(result), 2L)
  expect_true(all(result$variable %in% c("id", "value")))
})

test_that("get_summary_table errors on missing cols", {
  df <- data.frame(id = 1:3)
  expect_error(get_summary_table(df, cols = "nonexistent"), "not found")
})

test_that("diagnose_nas returns correct structure", {
  df <- data.frame(
    a = c(1, NA, 3),
    b = c(NA, NA, "x"),
    c = c(TRUE, FALSE, TRUE),
    stringsAsFactors = FALSE
  )
  result <- diagnose_nas(df)

  expect_s3_class(result, "diagnose_na")
  expect_equal(result$n_cols, 3L)
  expect_equal(result$n_with_na, 2L)

  tbl <- result$table
  expect_true(is.data.frame(tbl))
  expect_equal(nrow(tbl), 3L)
  expect_true(all(c("variable", "n_na", "pct_na", "n_valid") %in% names(tbl)))
})

test_that("diagnose_nas sorted by pct_na descending", {
  df <- data.frame(
    a = c(1, NA, 3),       # 33.3% NA
    b = c(NA, NA, "x"),    # 66.7% NA
    c = c(TRUE, FALSE, TRUE), # 0% NA
    stringsAsFactors = FALSE
  )
  result <- diagnose_nas(df)

  expect_equal(result$table$variable[1], "b")  # highest NA percentage first
  expect_equal(result$table$variable[3], "c")  # lowest NA last
})

test_that("diagnose_nas handles no NAs", {
  df <- data.frame(a = 1:3, b = 4:6)
  result <- diagnose_nas(df)

  expect_equal(result$n_with_na, 0L)
  expect_true(all(result$table$n_na == 0L))
})

test_that("diagnose_nas handles all NAs", {
  df <- data.frame(a = c(NA, NA), b = c(NA, NA))
  result <- diagnose_nas(df)

  expect_equal(result$n_with_na, 2L)
  expect_true(all(result$table$pct_na == 100))
})

test_that("print.diagnose_na produces output", {
  df <- data.frame(a = c(1, NA, 3), b = c(NA, NA, "x"),
                   stringsAsFactors = FALSE)
  result <- diagnose_nas(df)

  output <- capture.output(print(result), type = "message")
  combined <- paste(output, collapse = "\n")
  expect_true(grepl("Missing Value", combined))
  expect_true(grepl("2 of 2", combined))
})

test_that("diagnose_nas works with tibbles", {
  skip_if_not_installed("dplyr")
  df <- dplyr::tibble(a = c(1, NA, 3), b = c(NA, NA, "x"))
  result <- diagnose_nas(df)

  expect_s3_class(result, "diagnose_na")
  expect_equal(result$n_with_na, 2L)
})

test_that("diagnose_nas handles zero-row data.frame without NaN", {
  df <- data.frame(a = integer(0), b = character(0), stringsAsFactors = FALSE)
  result <- diagnose_nas(df)

  expect_s3_class(result, "diagnose_na")
  expect_equal(result$n_cols, 2L)
  expect_equal(result$n_with_na, 0L)
  expect_true(all(result$table$pct_na == 0))
  expect_false(any(is.nan(result$table$pct_na)))
})
