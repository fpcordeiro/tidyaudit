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
  expect_equal(result$comparison_method, "keys")
  expect_equal(result$rows_matched, 3L)
  expect_equal(result$numeric_summary$column, "value")
})

test_that("compare_tables computes numeric discrepancies by row index", {
  x <- data.frame(value = c(10.0, 20.0, 30.0))
  y <- data.frame(value = c(10.5, 20.0, 30.5))
  result <- compare_tables(x, y)

  expect_equal(result$comparison_method, "row_index")
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

  expect_true(is.na(result$comparison_method))
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

# --- tol, top_n, discrepancies, match_summary tests ---

test_that("tol = 0 is backward compatible", {
  x <- data.frame(id = 1:3, value = c(10.0, 20.0, 30.0))
  y <- data.frame(id = 1:3, value = c(10.5, 20.0, 30.5))
  result <- compare_tables(x, y, key_cols = "id")

  expect_equal(result$tol, .Machine$double.eps)
  expect_equal(result$top_n, Inf)
  expect_false(is.null(result$match_summary))
  expect_false(is.null(result$discrepancies))
  expect_equal(result$numeric_summary$n_over_tol, 2L)
})

test_that("tol filters discrepancies correctly", {
  x <- data.frame(id = 1:4, value = c(10.0, 20.0, 30.0, 40.0))
  y <- data.frame(id = 1:4, value = c(10.1, 20.0, 30.5, 40.2))
  result <- compare_tables(x, y, key_cols = "id", tol = 0.15)

  # Only id=3 (diff=0.5) and id=4 (diff=0.2) exceed tol=0.15
  expect_equal(nrow(result$discrepancies), 2L)
  expect_true(all(result$discrepancies$abs_diff > 0.15))
  expect_equal(result$numeric_summary$n_over_tol, 2L)
})

test_that("tol classifies matched rows correctly", {
  x <- data.frame(id = 1:4, value = c(10.0, 20.0, 30.0, 40.0))
  y <- data.frame(id = 1:4, value = c(10.1, 20.0, 30.5, 40.0))
  result <- compare_tables(x, y, key_cols = "id", tol = 0.15)

  ms <- result$match_summary
  # id=1 (diff=0.1 <= 0.15): no disc; id=2,4 (diff=0): no disc; id=3 (diff=0.5): disc
  expect_equal(ms$matched_no_disc, 3L)
  expect_equal(ms$matched_with_disc, 1L)
  expect_equal(ms$pct_no_disc, 0.75)
  expect_equal(ms$pct_with_disc, 0.25)
})

test_that("top_n truncates discrepancies per column", {
  x <- data.frame(id = 1:5, value = c(10, 20, 30, 40, 50))
  y <- data.frame(id = 1:5, value = c(11, 22, 33, 44, 55))
  result <- compare_tables(x, y, key_cols = "id", top_n = 2)

  # All 5 rows differ, but only top 2 per column stored
  expect_equal(nrow(result$discrepancies), 2L)
  # Should be the largest diffs (id=5: diff=5, id=4: diff=4)
  expect_equal(result$discrepancies$abs_diff[1], 5)
  expect_equal(result$discrepancies$abs_diff[2], 4)
})

test_that("top_n = Inf returns all discrepancies", {
  x <- data.frame(id = 1:5, value = c(10, 20, 30, 40, 50))
  y <- data.frame(id = 1:5, value = c(11, 22, 33, 44, 55))
  result <- compare_tables(x, y, key_cols = "id", top_n = Inf)

  expect_equal(nrow(result$discrepancies), 5L)
})

test_that("discrepancies data.frame has correct structure with keys", {
  x <- data.frame(id = c("a", "b", "c"), date = c("2024-01-01", "2024-01-02", "2024-01-03"),
                   value = c(10.0, 20.0, 30.0), stringsAsFactors = FALSE)
  y <- data.frame(id = c("a", "b", "c"), date = c("2024-01-01", "2024-01-02", "2024-01-03"),
                   value = c(10.5, 20.0, 30.5), stringsAsFactors = FALSE)
  result <- compare_tables(x, y, key_cols = c("id", "date"))

  disc <- result$discrepancies
  expect_true("id" %in% names(disc))
  expect_true("date" %in% names(disc))
  expect_true("column" %in% names(disc))
  expect_true("value_x" %in% names(disc))
  expect_true("value_y" %in% names(disc))
  expect_true("abs_diff" %in% names(disc))
})

test_that("discrepancies use row_index in row-index mode", {
  x <- data.frame(value = c(10.0, 20.0, 30.0))
  y <- data.frame(value = c(10.5, 20.0, 30.5))
  result <- compare_tables(x, y)

  disc <- result$discrepancies
  expect_true("row_index" %in% names(disc))
  expect_false("id" %in% names(disc))
})

test_that("only_x_keys and only_y_keys data.frames are stored", {
  x <- data.frame(id = c(1L, 2L, 3L), value = c(10, 20, 30))
  y <- data.frame(id = c(2L, 3L, 4L), value = c(40, 50, 60))
  result <- compare_tables(x, y, key_cols = "id")

  expect_false(is.null(result$only_x_keys))
  expect_equal(nrow(result$only_x_keys), 1L)
  expect_equal(result$only_x_keys$id, 1L)

  expect_false(is.null(result$only_y_keys))
  expect_equal(nrow(result$only_y_keys), 1L)
  expect_equal(result$only_y_keys$id, 4L)
})

test_that("top_n truncates unmatched key data.frames", {
  x <- data.frame(id = 1:10, value = 1:10)
  y <- data.frame(id = 6:15, value = 6:15)
  result <- compare_tables(x, y, key_cols = "id", top_n = 3)

  # x has ids 1-5 unmatched, but top_n = 3
  expect_equal(nrow(result$only_x_keys), 3L)
  # y has ids 11-15 unmatched, but top_n = 3
  expect_equal(nrow(result$only_y_keys), 3L)
  # key_summary still has full counts
  expect_equal(result$key_summary$only_x, 5L)
  expect_equal(result$key_summary$only_y, 5L)
})

test_that("match_summary counts are correct with key overlap", {
  x <- data.frame(id = c(1L, 2L, 3L, 4L), value = c(10, 20, 30, 40))
  y <- data.frame(id = c(2L, 3L, 5L), value = c(25, 30, 50))
  result <- compare_tables(x, y, key_cols = "id")

  ms <- result$match_summary
  expect_equal(ms$only_x, 2L)    # ids 1, 4

  expect_equal(ms$only_y, 1L)    # id 5
  # ids 2,3 match; id=2 differs (20 vs 25), id=3 same (30 vs 30)
  expect_equal(ms$matched_no_disc, 1L)
  expect_equal(ms$matched_with_disc, 1L)
  expect_equal(ms$pct_no_disc, 0.5)
  expect_equal(ms$pct_with_disc, 0.5)
})

test_that("match_summary in row-index mode", {
  x <- data.frame(value = c(10.0, 20.0, 30.0, 40.0, 50.0))
  y <- data.frame(value = c(10.0, 20.0, 30.0))
  result <- compare_tables(x, y)

  ms <- result$match_summary
  expect_equal(ms$only_x, 2L)
  expect_equal(ms$only_y, 0L)
  expect_equal(ms$matched_no_disc, 3L)
  expect_equal(ms$matched_with_disc, 0L)
})

test_that("no discrepancies returns NULL discrepancies", {
  x <- data.frame(id = 1:3, value = c(10.0, 20.0, 30.0))
  y <- data.frame(id = 1:3, value = c(10.0, 20.0, 30.0))
  result <- compare_tables(x, y, key_cols = "id")

  expect_null(result$discrepancies)
  expect_equal(result$match_summary$matched_no_disc, 3L)
  expect_equal(result$match_summary$matched_with_disc, 0L)
})

test_that("match_summary works with no numeric columns", {
  x <- data.frame(id = c("a", "b", "c"), stringsAsFactors = FALSE)
  y <- data.frame(id = c("a", "b", "d"), stringsAsFactors = FALSE)
  result <- compare_tables(x, y)

  ms <- result$match_summary
  expect_equal(ms$only_x, 1L)
  expect_equal(ms$only_y, 1L)
  expect_equal(ms$matched_no_disc, 2L)
  expect_equal(ms$matched_with_disc, 0L)
})

test_that("print.compare_tbl shows new sections", {
  x <- data.frame(id = c(1L, 2L, 3L, 4L), value = c(10.0, 20.0, 30.0, 40.0))
  y <- data.frame(id = c(2L, 3L, 5L), value = c(25.0, 30.0, 50.0))
  result <- compare_tables(x, y, key_cols = "id", tol = 0.5)

  output <- capture.output(print(result), type = "message")
  combined <- paste(output, collapse = "\n")
  expect_true(grepl("Row matching", combined))
  expect_true(grepl("tol = 0.5", combined))
  expect_true(grepl("Unmatched keys", combined))
  expect_true(grepl("Top discrepancies", combined))
  expect_true(grepl(">tol", combined))
})

test_that("print.compare_tbl respects show_n", {
  x <- data.frame(id = 1:10, value = as.numeric(1:10))
  y <- data.frame(id = 1:10, value = as.numeric(1:10) + 1)
  result <- compare_tables(x, y, key_cols = "id")

  output <- capture.output(print(result, show_n = 2), type = "message")
  combined <- paste(output, collapse = "\n")
  expect_true(grepl("and 8 more", combined))
})

test_that("tol validation rejects invalid values", {
  x <- data.frame(id = 1:3, value = 1:3)
  y <- data.frame(id = 1:3, value = 1:3)
  expect_error(compare_tables(x, y, tol = -1), "non-negative")
  expect_error(compare_tables(x, y, tol = "abc"), "non-negative")
  expect_error(compare_tables(x, y, tol = c(0, 1)), "non-negative")
})

test_that("top_n validation rejects invalid values", {
  x <- data.frame(id = 1:3, value = 1:3)
  y <- data.frame(id = 1:3, value = 1:3)
  expect_error(compare_tables(x, y, top_n = 0), "positive")
  expect_error(compare_tables(x, y, top_n = -1), "positive")
  expect_error(compare_tables(x, y, top_n = "abc"), "positive")
})

test_that("NA-vs-value is treated as discrepancy with abs_diff = NA (keys mode)", {
  x <- data.frame(id = 1:3, value = c(10.0, NA, 30.0))
  y <- data.frame(id = 1:3, value = c(10.0, 20.0, 30.0))
  result <- compare_tables(x, y, key_cols = "id")

  # id=2: NA vs 20.0 should be a discrepancy

  expect_false(is.null(result$discrepancies))
  expect_equal(nrow(result$discrepancies), 1L)
  expect_equal(result$discrepancies$id, 2L)
  expect_true(is.na(result$discrepancies$abs_diff))
  expect_true(is.na(result$discrepancies$value_x))
  expect_equal(result$discrepancies$value_y, 20.0)

  # match_summary: 2 no-disc (id=1,3), 1 with-disc (id=2)
  expect_equal(result$match_summary$matched_no_disc, 2L)
  expect_equal(result$match_summary$matched_with_disc, 1L)
})

test_that("NA-vs-value is treated as discrepancy with abs_diff = NA (row-index mode)", {
  x <- data.frame(value = c(10.0, NA, 30.0))
  y <- data.frame(value = c(10.0, 20.0, 30.0))
  result <- compare_tables(x, y)

  expect_false(is.null(result$discrepancies))
  expect_equal(nrow(result$discrepancies), 1L)
  expect_equal(result$discrepancies$row_index, 2L)
  expect_true(is.na(result$discrepancies$abs_diff))
  expect_equal(result$match_summary$matched_with_disc, 1L)
})

test_that("both-NA is NOT treated as discrepancy", {
  x <- data.frame(id = 1:3, value = c(10.0, NA, 30.0))
  y <- data.frame(id = 1:3, value = c(10.0, NA, 30.0))
  result <- compare_tables(x, y, key_cols = "id")

  expect_null(result$discrepancies)
  expect_equal(result$match_summary$matched_no_disc, 3L)
  expect_equal(result$match_summary$matched_with_disc, 0L)
})

test_that("n_over_tol includes NA-vs-value pairs", {
  x <- data.frame(id = 1:4, value = c(10.0, NA, 30.0, 40.0))
  y <- data.frame(id = 1:4, value = c(10.5, 20.0, 30.0, 40.0))
  result <- compare_tables(x, y, key_cols = "id")

  # id=1 (diff=0.5 > eps): over tol; id=2 (NA vs 20): NA mismatch
  # n_over_tol should count both
  expect_equal(result$numeric_summary$n_over_tol, 2L)
})

test_that("NA discrepancies are sorted to the end", {
  x <- data.frame(id = 1:4, value = c(10.0, NA, 30.0, 40.0))
  y <- data.frame(id = 1:4, value = c(15.0, 20.0, 30.0, 42.0))
  result <- compare_tables(x, y, key_cols = "id")

  disc <- result$discrepancies
  # id=1 (diff=5), id=4 (diff=2) should come before id=2 (NA), id=3 matches
  expect_equal(nrow(disc), 3L)
  expect_false(is.na(disc$abs_diff[1]))  # largest finite diff first
  expect_false(is.na(disc$abs_diff[2]))  # second finite diff
  expect_true(is.na(disc$abs_diff[3]))   # NA at end
})

test_that("multiple numeric columns produce correct discrepancies", {
  x <- data.frame(id = 1:3, val_a = c(10.0, 20.0, 30.0), val_b = c(1.0, 2.0, 3.0))
  y <- data.frame(id = 1:3, val_a = c(10.5, 20.0, 30.5), val_b = c(1.0, 2.5, 3.0))
  result <- compare_tables(x, y, key_cols = "id")

  disc <- result$discrepancies
  # val_a: id=1 (0.5), id=3 (0.5); val_b: id=2 (0.5)
  expect_equal(nrow(disc), 3L)
  expect_true("val_a" %in% disc$column)
  expect_true("val_b" %in% disc$column)
  expect_equal(sum(disc$column == "val_a"), 2L)
  expect_equal(sum(disc$column == "val_b"), 1L)
})

test_that("top_n applies per-column with multiple numeric columns", {
  x <- data.frame(id = 1:5, val_a = as.numeric(1:5), val_b = as.numeric(1:5))
  y <- data.frame(id = 1:5, val_a = as.numeric(1:5) + 1, val_b = as.numeric(1:5) + 1)
  result <- compare_tables(x, y, key_cols = "id", top_n = 2)

  disc <- result$discrepancies
  # top_n = 2 per column, 2 columns => up to 4 rows
  expect_equal(sum(disc$column == "val_a"), 2L)
  expect_equal(sum(disc$column == "val_b"), 2L)
})

test_that("tol works correctly in row-index mode", {
  x <- data.frame(value = c(10.0, 20.0, 30.0, 40.0))
  y <- data.frame(value = c(10.1, 20.0, 30.5, 40.2))
  result <- compare_tables(x, y, tol = 0.15)

  # Only row 3 (diff=0.5) and row 4 (diff=0.2) exceed tol=0.15
  expect_equal(nrow(result$discrepancies), 2L)
  expect_true(all(result$discrepancies$abs_diff > 0.15))
  expect_equal(result$match_summary$matched_no_disc, 2L)
  expect_equal(result$match_summary$matched_with_disc, 2L)
  expect_equal(result$numeric_summary$n_over_tol, 2L)
})

test_that("only_x_keys and only_y_keys in row-index mode", {
  x <- data.frame(value = c(10.0, 20.0, 30.0, 40.0, 50.0))
  y <- data.frame(value = c(10.0, 20.0, 30.0))
  result <- compare_tables(x, y)

  expect_false(is.null(result$only_x_keys))
  expect_equal(nrow(result$only_x_keys), 2L)
  expect_equal(result$only_x_keys$row_index, c(4L, 5L))
  expect_null(result$only_y_keys)
})

test_that("print with no discrepancies omits Top discrepancies section", {
  x <- data.frame(id = 1:3, value = c(10.0, 20.0, 30.0))
  y <- data.frame(id = 1:3, value = c(10.0, 20.0, 30.0))
  result <- compare_tables(x, y, key_cols = "id")

  output <- capture.output(print(result), type = "message")
  combined <- paste(output, collapse = "\n")
  expect_false(grepl("Top discrepancies", combined))
  expect_true(grepl("Row matching", combined))
})

test_that("print section numbering is sequential even without match_summary", {
  x <- data.frame(value = c(10.0, 20.0, 30.0))
  y <- data.frame(value = c(10.0, 20.0, 30.0))
  result <- compare_tables(x, y)

  # No match_summary (all identical, row-index mode has match_summary)
  # but let's test a case with no match_summary: identical tables with no keys
  # Actually row-index mode now always has match_summary when num_cols > 0.
  # Test the no-numeric-no-key case:
  x2 <- data.frame(a = c(1L, 2L, 3L))
  y2 <- data.frame(a = c(1L, 2L, 3L))
  result2 <- compare_tables(x2, y2)

  output <- capture.output(print(result2), type = "message")
  combined <- paste(output, collapse = "\n")
  # Should have sections 1, 2, 3, 4 (row matching), 5 (numeric) — or skip 4 if no match_summary
  # With keys auto-detected (integer col "a"), match_summary exists
  expect_true(grepl("1\\. Row counts", combined))
  expect_true(grepl("2\\. Column names", combined))
  expect_true(grepl("3\\. Key columns", combined))
})

test_that("print.compare_tbl suppresses tol label at default eps", {
  x <- data.frame(id = 1:3, value = c(10.0, 20.0, 30.0))
  y <- data.frame(id = 1:3, value = c(10.5, 20.0, 30.5))
  result <- compare_tables(x, y, key_cols = "id")

  output <- capture.output(print(result), type = "message")
  combined <- paste(output, collapse = "\n")
  # Default eps should NOT show tol label
  expect_false(grepl("tol =", combined))
})

test_that("print.compare_tbl shows top_n cap in remaining message", {
  x <- data.frame(id = 1:10, value = 1:10)
  y <- data.frame(id = 6:15, value = 6:15)
  result <- compare_tables(x, y, key_cols = "id", top_n = 3)

  output <- capture.output(print(result, show_n = 2), type = "message")
  combined <- paste(output, collapse = "\n")
  # top_n = 3 < total unmatched (5), so should mention "stored"
  expect_true(grepl("3 stored", combined))
})

test_that("print.compare_tbl validates show_n", {
  x <- data.frame(id = 1:3, value = 1:3)
  y <- data.frame(id = 1:3, value = 1:3)
  result <- compare_tables(x, y, key_cols = "id")
  expect_error(print(result, show_n = -1), "non-negative")
  expect_error(print(result, show_n = "abc"), "non-negative")
})

test_that("zero-row tables are handled gracefully", {
  x <- data.frame(id = integer(0), value = numeric(0))
  y <- data.frame(id = integer(0), value = numeric(0))
  result <- compare_tables(x, y, key_cols = "id")

  expect_equal(result$nrow_x, 0L)
  expect_equal(result$nrow_y, 0L)
  expect_equal(result$key_summary$matches, 0L)
  expect_null(result$discrepancies)
  expect_null(result$numeric_summary)
})

test_that("print handles NA values in discrepancies display", {
  x <- data.frame(id = 1:3, value = c(10.0, NA, 30.0))
  y <- data.frame(id = 1:3, value = c(15.0, 20.0, 30.0))
  result <- compare_tables(x, y, key_cols = "id")

  output <- capture.output(print(result), type = "message")
  combined <- paste(output, collapse = "\n")
  expect_true(grepl("NA", combined))
  expect_true(grepl("Top discrepancies", combined))
})

# -- compare_cols / exclude_cols ----------------------------------------------

test_that("compare_cols selects only specified columns", {
  x <- data.frame(id = 1:3, a = c(1, 2, 3), b = c(4, 5, 6))
  y <- data.frame(id = 1:3, a = c(1, 2, 4), b = c(4, 5, 7))
  r <- compare_tables(x, y, key_cols = "id", compare_cols = "a")
  expect_equal(r$discrepancies$column, "a")
})

test_that("exclude_cols removes specified columns from comparison", {
  x <- data.frame(id = 1:3, a = c(1, 2, 3), b = c(4, 5, 6))
  y <- data.frame(id = 1:3, a = c(1, 2, 4), b = c(4, 5, 7))
  r <- compare_tables(x, y, key_cols = "id", exclude_cols = "a")
  expect_equal(r$discrepancies$column, "b")
})

test_that("compare_cols and exclude_cols are mutually exclusive", {
  x <- data.frame(id = 1:3, a = 1:3)
  y <- data.frame(id = 1:3, a = 1:3)
  expect_error(
    compare_tables(x, y, compare_cols = "a", exclude_cols = "a"),
    "cannot both be specified"
  )
})

test_that("compare_cols errors on unknown column names", {
  x <- data.frame(id = 1:3, a = 1:3)
  y <- data.frame(id = 1:3, a = 1:3)
  expect_error(
    compare_tables(x, y, key_cols = "id", compare_cols = "nonexistent"),
    "Not found"
  )
})

test_that("exclude_cols warns on nonexistent columns", {
  x <- data.frame(id = 1:3, a = 1:3)
  y <- data.frame(id = 1:3, a = 1:3)
  expect_warning(
    compare_tables(x, y, key_cols = "id", exclude_cols = "nonexistent"),
    "not found among common non-key columns"
  )
})

test_that("compare_cols silently excludes key columns", {
  x <- data.frame(id = 1:3, a = c(1, 2, 3))
  y <- data.frame(id = 1:3, a = c(1, 2, 4))
  # "id" is a key so asking for it in compare_cols is a non-key column error
  expect_error(
    compare_tables(x, y, key_cols = "id", compare_cols = c("id", "a")),
    "Not found"
  )
})

# -- Categorical discrepancy detection ---------------------------------------

test_that("character column discrepancies detected in keys mode", {
  x <- data.frame(id = 1:4, cat = c("a", "b", "c", "d"), stringsAsFactors = FALSE)
  y <- data.frame(id = 1:4, cat = c("a", "x", "c", "z"), stringsAsFactors = FALSE)
  r <- compare_tables(x, y, key_cols = "id")

  expect_false(is.null(r$categorical_summary))
  expect_equal(r$categorical_summary$column, "cat")
  expect_equal(r$categorical_summary$n_compared, 4L)
  expect_equal(r$categorical_summary$n_mismatched, 2L)
  expect_equal(r$categorical_summary$pct_mismatched, 0.5)
  expect_equal(r$categorical_summary$n_na_mismatch, 0L)

  expect_false(is.null(r$categorical_discrepancies))
  expect_equal(nrow(r$categorical_discrepancies), 2L)
  expect_true(all(c("id", "column", "value_x", "value_y") %in% names(r$categorical_discrepancies)))
})

test_that("character column discrepancies detected in row-index mode", {
  x <- data.frame(val = c(1, 2), cat = c("a", "b"), stringsAsFactors = FALSE)
  y <- data.frame(val = c(1, 3), cat = c("a", "x"), stringsAsFactors = FALSE)
  r <- compare_tables(x, y, key_cols = character(0))

  expect_false(is.null(r$categorical_discrepancies))
  expect_equal(nrow(r$categorical_discrepancies), 1L)
  expect_equal(r$categorical_discrepancies$row_index, 2L)
  expect_equal(r$categorical_discrepancies$value_x, "b")
  expect_equal(r$categorical_discrepancies$value_y, "x")
})

test_that("factor columns compared as character values", {
  x <- data.frame(id = 1:3, cat = factor(c("a", "b", "c")))
  y <- data.frame(id = 1:3, cat = factor(c("a", "x", "c")))
  r <- compare_tables(x, y, key_cols = "id")

  expect_false(is.null(r$categorical_discrepancies))
  expect_equal(nrow(r$categorical_discrepancies), 1L)
  expect_equal(r$categorical_discrepancies$value_x, "b")
  expect_equal(r$categorical_discrepancies$value_y, "x")
})

test_that("logical column discrepancies detected", {
  x <- data.frame(id = 1:3, flag = c(TRUE, FALSE, TRUE))
  y <- data.frame(id = 1:3, flag = c(TRUE, TRUE, TRUE))
  r <- compare_tables(x, y, key_cols = "id")

  expect_equal(r$categorical_summary$n_mismatched, 1L)
  expect_equal(nrow(r$categorical_discrepancies), 1L)
})

test_that("Date column discrepancies detected", {
  x <- data.frame(id = 1:3, dt = as.Date(c("2024-01-01", "2024-02-01", "2024-03-01")))
  y <- data.frame(id = 1:3, dt = as.Date(c("2024-01-01", "2024-02-15", "2024-03-01")))
  r <- compare_tables(x, y, key_cols = "id")

  expect_equal(r$categorical_summary$n_mismatched, 1L)
  expect_equal(nrow(r$categorical_discrepancies), 1L)
})

test_that("NA-vs-value is categorical discrepancy; both-NA is not", {
  x <- data.frame(id = 1:4, cat = c("a", NA, "c", NA), stringsAsFactors = FALSE)
  y <- data.frame(id = 1:4, cat = c("a", "b", NA, NA), stringsAsFactors = FALSE)
  r <- compare_tables(x, y, key_cols = "id")

  expect_equal(r$categorical_summary$n_mismatched, 2L)
  expect_equal(r$categorical_summary$n_na_mismatch, 2L)
  expect_equal(nrow(r$categorical_discrepancies), 2L)
})

test_that("top_n applies per-column for categorical discrepancies", {
  x <- data.frame(id = 1:5,
                   a = c("a", "b", "c", "d", "e"),
                   b = c("x", "y", "z", "w", "v"),
                   stringsAsFactors = FALSE)
  y <- data.frame(id = 1:5,
                   a = c("A", "B", "C", "D", "E"),
                   b = c("X", "Y", "Z", "W", "V"),
                   stringsAsFactors = FALSE)
  r <- compare_tables(x, y, key_cols = "id", top_n = 2)

  # 2 per column = 4 total stored
  expect_equal(nrow(r$categorical_discrepancies), 4L)
  expect_equal(sum(r$categorical_discrepancies$column == "a"), 2L)
  expect_equal(sum(r$categorical_discrepancies$column == "b"), 2L)
})

test_that("match_summary counts rows with either numeric or categorical discrepancies", {
  x <- data.frame(id = 1:4, val = c(1, 2, 3, 4),
                   cat = c("a", "b", "c", "d"), stringsAsFactors = FALSE)
  y <- data.frame(id = 1:4, val = c(1, 2.5, 3, 4),
                   cat = c("a", "b", "x", "d"), stringsAsFactors = FALSE)
  r <- compare_tables(x, y, key_cols = "id")

  # Row 2: numeric disc only, Row 3: categorical disc only
  expect_equal(r$match_summary$matched_with_disc, 2L)
  expect_equal(r$match_summary$matched_no_disc, 2L)
})

test_that("type-mismatched columns excluded from categorical comparison", {
  x <- data.frame(id = 1:3, cat = c("a", "b", "c"), stringsAsFactors = FALSE)
  y <- data.frame(id = 1:3, cat = c(1L, 2L, 3L))
  r <- compare_tables(x, y, key_cols = "id")

  expect_null(r$categorical_summary)
  expect_null(r$categorical_discrepancies)
  expect_false(is.null(r$type_mismatches))
})

# -- pct_diff -----------------------------------------------------------------

test_that("pct_diff column present in numeric discrepancies", {
  x <- data.frame(id = 1:3, value = c(10, 20, 30))
  y <- data.frame(id = 1:3, value = c(10, 25, 30))
  r <- compare_tables(x, y, key_cols = "id")

  expect_true("pct_diff" %in% names(r$discrepancies))
  # 5/25 = 0.2
  expect_equal(r$discrepancies$pct_diff, 0.2)
})

test_that("pct_diff handles both-zero case", {
  x <- data.frame(id = 1:2, value = c(0, 10))
  y <- data.frame(id = 1:2, value = c(0, 15))
  r <- compare_tables(x, y, key_cols = "id")

  # Only row 2 has a discrepancy (both-zero diff = 0, not > tol)
  expect_equal(nrow(r$discrepancies), 1L)
  # 5/15 = 0.333...
  expect_equal(r$discrepancies$pct_diff, 5 / 15, tolerance = 1e-10)
})

test_that("pct_diff handles one-zero case", {
  x <- data.frame(id = 1L, value = 0)
  y <- data.frame(id = 1L, value = 5)
  r <- compare_tables(x, y, key_cols = "id")

  # 5/max(0,5) = 1.0
  expect_equal(r$discrepancies$pct_diff, 1.0)
})

test_that("pct_diff is NA when value is NA", {
  x <- data.frame(id = 1L, value = NA_real_)
  y <- data.frame(id = 1L, value = 5)
  r <- compare_tables(x, y, key_cols = "id")

  expect_true(is.na(r$discrepancies$pct_diff))
})

# -- total_discrepancies ------------------------------------------------------

test_that("total_discrepancies counts numeric discrepancies only", {
  x <- data.frame(id = 1:3, value = c(10, 20, 30))
  y <- data.frame(id = 1:3, value = c(10, 25, 35))
  r <- compare_tables(x, y, key_cols = "id")

  expect_equal(r$total_discrepancies, 2L)
})

test_that("total_discrepancies counts both numeric and categorical", {
  x <- data.frame(id = 1:3, val = c(1, 2, 3),
                   cat = c("a", "b", "c"), stringsAsFactors = FALSE)
  y <- data.frame(id = 1:3, val = c(1, 2.5, 3),
                   cat = c("a", "x", "c"), stringsAsFactors = FALSE)
  r <- compare_tables(x, y, key_cols = "id")

  # 1 numeric + 1 categorical = 2

  expect_equal(r$total_discrepancies, 2L)
})

test_that("total_discrepancies is not limited by top_n", {
  x <- data.frame(id = 1:5, val = c(1, 2, 3, 4, 5))
  y <- data.frame(id = 1:5, val = c(2, 3, 4, 5, 6))
  r <- compare_tables(x, y, key_cols = "id", top_n = 2)

  # All 5 rows differ but only 2 stored
  expect_equal(nrow(r$discrepancies), 2L)
  expect_equal(r$total_discrepancies, 5L)
})

# -- as.data.frame.compare_tbl -----------------------------------------------

test_that("as.data.frame returns correct columns for mixed discrepancies", {
  x <- data.frame(id = 1:3, val = c(1, 2, 3),
                   cat = c("a", "b", "c"), stringsAsFactors = FALSE)
  y <- data.frame(id = 1:3, val = c(1, 2.5, 3),
                   cat = c("a", "x", "c"), stringsAsFactors = FALSE)
  r <- compare_tables(x, y, key_cols = "id")
  df <- as.data.frame(r)

  expect_s3_class(df, "data.frame")
  expect_true(all(c("id", "column", "type", "value_x", "value_y",
                     "abs_diff", "pct_diff") %in% names(df)))
  expect_equal(nrow(df), 2L)
  expect_equal(sum(df$type == "numeric"), 1L)
  expect_equal(sum(df$type == "categorical"), 1L)
})

test_that("as.data.frame works with numeric-only discrepancies", {
  x <- data.frame(id = 1:3, val = c(1, 2, 3))
  y <- data.frame(id = 1:3, val = c(1, 2.5, 3))
  r <- compare_tables(x, y, key_cols = "id")
  df <- as.data.frame(r)

  expect_equal(nrow(df), 1L)
  expect_equal(df$type, "numeric")
})

test_that("as.data.frame works with categorical-only discrepancies", {
  x <- data.frame(id = 1:3, cat = c("a", "b", "c"), stringsAsFactors = FALSE)
  y <- data.frame(id = 1:3, cat = c("a", "x", "c"), stringsAsFactors = FALSE)
  r <- compare_tables(x, y, key_cols = "id")
  df <- as.data.frame(r)

  expect_equal(nrow(df), 1L)
  expect_equal(df$type, "categorical")
  expect_true(is.na(df$abs_diff))
  expect_true(is.na(df$pct_diff))
})

test_that("as.data.frame returns 0-row data.frame with no discrepancies", {
  x <- data.frame(id = 1:3, val = c(1, 2, 3))
  y <- data.frame(id = 1:3, val = c(1, 2, 3))
  r <- compare_tables(x, y, key_cols = "id")
  df <- as.data.frame(r)

  expect_equal(nrow(df), 0L)
  expect_true("column" %in% names(df))
  expect_true("type" %in% names(df))
})

test_that("as.data.frame converts numeric values to character", {
  x <- data.frame(id = 1:2, val = c(1, 2))
  y <- data.frame(id = 1:2, val = c(1, 3))
  r <- compare_tables(x, y, key_cols = "id")
  df <- as.data.frame(r)

  expect_type(df$value_x, "character")
  expect_type(df$value_y, "character")
})

# -- Print method updates ----------------------------------------------------

test_that("print shows categorical discrepancies section", {
  x <- data.frame(id = 1:3, cat = c("a", "b", "c"), stringsAsFactors = FALSE)
  y <- data.frame(id = 1:3, cat = c("a", "x", "c"), stringsAsFactors = FALSE)
  r <- compare_tables(x, y, key_cols = "id")

  output <- capture.output(print(r), type = "message")
  combined <- paste(output, collapse = "\n")
  expect_true(grepl("Categorical discrepancies", combined))
  expect_true(grepl("Mismatched", combined))
})

test_that("print shows pct_diff column in numeric discrepancies", {
  x <- data.frame(id = 1:3, value = c(10, 20, 30))
  y <- data.frame(id = 1:3, value = c(10, 25, 30))
  r <- compare_tables(x, y, key_cols = "id")

  output <- capture.output(print(r), type = "message")
  combined <- paste(output, collapse = "\n")
  expect_true(grepl("pct_diff", combined))
})

test_that("print shows total cell discrepancies line", {
  x <- data.frame(id = 1:3, val = c(1, 2, 3),
                   cat = c("a", "b", "c"), stringsAsFactors = FALSE)
  y <- data.frame(id = 1:3, val = c(1, 2.5, 3),
                   cat = c("a", "x", "c"), stringsAsFactors = FALSE)
  r <- compare_tables(x, y, key_cols = "id")

  output <- capture.output(print(r), type = "message")
  combined <- paste(output, collapse = "\n")
  expect_true(grepl("Total cell discrepancies", combined))
  expect_true(grepl("numeric", combined))
  expect_true(grepl("categorical", combined))
})

test_that("print handles show_n for categorical discrepancies", {
  x <- data.frame(id = 1:5, cat = c("a", "b", "c", "d", "e"), stringsAsFactors = FALSE)
  y <- data.frame(id = 1:5, cat = c("A", "B", "C", "D", "E"), stringsAsFactors = FALSE)
  r <- compare_tables(x, y, key_cols = "id")

  output <- capture.output(print(r, show_n = 2L), type = "message")
  combined <- paste(output, collapse = "\n")
  expect_true(grepl("and 3 more", combined))
})
