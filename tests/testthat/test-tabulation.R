# -- One-way tabulation -------------------------------------------------------

test_that("one-way tab returns correct structure", {
  result <- tab(mtcars, cyl)

  expect_s3_class(result, "tidyaudit_tab")
  expect_equal(result$type, "oneway")
  expect_equal(result$var_name, "cyl")
  expect_equal(result$n_obs, 32L)
  expect_equal(result$n_missing, 0L)
  expect_false(result$weighted)
  expect_null(result$wt_name)

  tbl <- result$table
  expect_true("Total" %in% tbl$Value)
  expect_equal(tbl$Freq[tbl$Value == "Total"], 32)
  expect_equal(tbl$Percent[tbl$Value == "Total"], 100.0)
})

test_that("one-way tab frequencies are correct", {
  result <- tab(mtcars, cyl)
  tbl <- result$table[result$table$Value != "Total", ]

  expect_equal(tbl$Freq[tbl$Value == "4"], 11)
  expect_equal(tbl$Freq[tbl$Value == "6"], 7)
  expect_equal(tbl$Freq[tbl$Value == "8"], 14)
})

test_that("one-way tab cumulative percentage does not exceed 100", {
  result <- tab(mtcars, cyl)
  tbl <- result$table[result$table$Value != "Total", ]
  expect_true(all(tbl$Cum_Percent <= 100.0))
})

test_that("one-way tab prints without error", {
  result <- tab(mtcars, cyl)
  output <- capture.output(print(result), type = "message")
  expect_true(any(grepl("Tabulation: cyl", output)))
  expect_true(any(grepl("Total", output)))
})

# -- Sorting ------------------------------------------------------------------

test_that(".sort = 'freq_desc' sorts by descending frequency", {
  result <- tab(mtcars, cyl, .sort = "freq_desc")
  tbl <- result$table[result$table$Value != "Total", ]
  expect_equal(tbl$Value[1], "8")
  expect_equal(tbl$Value[2], "4")
  expect_equal(tbl$Value[3], "6")
})

test_that(".sort = 'freq_asc' sorts by ascending frequency", {
  result <- tab(mtcars, cyl, .sort = "freq_asc")
  tbl <- result$table[result$table$Value != "Total", ]
  expect_equal(tbl$Value[1], "6")
})

test_that(".sort = 'value_desc' reverses alphabetical order", {
  result <- tab(mtcars, cyl, .sort = "value_desc")
  tbl <- result$table[result$table$Value != "Total", ]
  expect_equal(tbl$Value[1], "8")
  expect_equal(tbl$Value[3], "4")
})

# -- Cutoff -------------------------------------------------------------------

test_that(".cutoff top-N groups remaining into (Other)", {
  result <- tab(mtcars, cyl, .sort = "freq_desc", .cutoff = 2)
  tbl <- result$table

  expect_true("(Other)" %in% tbl$Value)
  expect_equal(result$n_other, 1L)
  data_rows <- tbl[!tbl$Value %in% c("Total", "(Other)"), ]
  expect_equal(nrow(data_rows), 2L)
})

test_that(".cutoff proportional threshold works", {
  result <- tab(mtcars, cyl, .cutoff = 0.5)
  tbl <- result$table
  # cyl=8 has 43.8% — need at least 50%, so top 2 by freq (8=14, 4=11) kept
  expect_true("(Other)" %in% tbl$Value)
  data_rows <- tbl[!tbl$Value %in% c("Total", "(Other)"), ]
  expect_equal(nrow(data_rows), 2L)
  # The kept values should cumulatively reach >= 50%
  expect_gte(sum(data_rows$Freq) / 32, 0.5)
})

test_that(".cutoff = 0 errors", {
  expect_error(tab(mtcars, cyl, .cutoff = 0), "positive")
})

test_that(".cutoff = -1 errors", {
  expect_error(tab(mtcars, cyl, .cutoff = -1), "positive")
})

# -- Weighted -----------------------------------------------------------------

test_that("weighted tabulation uses .wt sums", {
  result <- tab(mtcars, cyl, .wt = mpg)

  expect_true(result$weighted)
  expect_equal(result$wt_name, "mpg")

  tbl <- result$table
  total <- tbl$Freq[tbl$Value == "Total"]
  expect_equal(total, sum(mtcars$mpg), tolerance = 0.1)
})

test_that("weighted tabulation prints weight name in header", {
  result <- tab(mtcars, cyl, .wt = mpg)
  output <- capture.output(print(result), type = "message")
  expect_true(any(grepl("weighted by mpg", output)))
})

# -- NA handling --------------------------------------------------------------

test_that(".na = 'include' shows NA as category", {
  df <- data.frame(x = c("a", "b", NA, "a"), stringsAsFactors = FALSE)
  result <- tab(df, x, .na = "include")

  expect_equal(result$n_missing, 1L)
  expect_true("<NA>" %in% result$table$Value)
})

test_that(".na = 'exclude' drops NAs", {
  df <- data.frame(x = c("a", "b", NA, "a"), stringsAsFactors = FALSE)
  result <- tab(df, x, .na = "exclude")

  expect_false("<NA>" %in% result$table$Value)
  total <- result$table$Freq[result$table$Value == "Total"]
  expect_equal(total, 3)
})

test_that(".na = 'only' shows only NA rows", {
  df <- data.frame(x = c("a", "b", NA, "a"), stringsAsFactors = FALSE)
  result <- tab(df, x, .na = "only")
  tbl <- result$table[result$table$Value != "Total", ]
  expect_equal(nrow(tbl), 1L)
  expect_equal(tbl$Value, "<NA>")
})

# -- Two-way crosstabulation --------------------------------------------------

test_that("two-way tab returns correct structure", {
  result <- tab(mtcars, cyl, gear)

  expect_s3_class(result, "tidyaudit_tab")
  expect_equal(result$type, "twoway")
  expect_equal(result$var1_name, "cyl")
  expect_equal(result$var2_name, "gear")
  expect_equal(result$display, "count")
  expect_equal(result$grand_total, 32)
})

test_that("two-way tab matrix has correct dimensions", {
  result <- tab(mtcars, cyl, gear)
  mat <- result$matrix

  # 3 cyl values + Total row = 4 rows
  expect_equal(nrow(mat), 4L)
  # cyl + 3 gear values + Total = 5 columns
  expect_equal(ncol(mat), 5L)
  expect_true("Total" %in% names(mat))
  expect_true("Total" %in% mat[[1L]])
})

test_that("two-way tab prints without error", {
  result <- tab(mtcars, cyl, gear)
  output <- capture.output(print(result), type = "message")
  expect_true(any(grepl("Crosstabulation", output)))
})

test_that(".display = 'row_pct' shows percentages", {
  result <- tab(mtcars, cyl, gear, .display = "row_pct")
  mat <- result$matrix
  # First data row, second column (first gear value) should contain %
  cell <- mat[1, 2]
  expect_true(grepl("%", cell))
})

test_that(".display = 'col_pct' works", {
  result <- tab(mtcars, cyl, gear, .display = "col_pct")
  mat <- result$matrix
  cell <- mat[1, 2]
  expect_true(grepl("%", cell))
})

test_that(".display = 'total_pct' works", {
  result <- tab(mtcars, cyl, gear, .display = "total_pct")
  mat <- result$matrix
  cell <- mat[1, 2]
  expect_true(grepl("%", cell))
})

test_that("two-way weighted works", {
  result <- tab(mtcars, cyl, gear, .wt = mpg)
  expect_true(result$weighted)
  expect_equal(result$grand_total, sum(mtcars$mpg), tolerance = 0.1)
})

# -- as.data.frame ------------------------------------------------------------

test_that("as.data.frame one-way returns data without Total", {
  result <- tab(mtcars, cyl)
  df <- as.data.frame(result)

  expect_false("Total" %in% df$Value)
  expect_equal(nrow(df), 3L)
  expect_true("Freq" %in% names(df))
})

test_that("as.data.frame two-way returns numeric counts without Total", {
  result <- tab(mtcars, cyl, gear)
  df <- as.data.frame(result)

  expect_false("Total" %in% df[[1L]])
  expect_false("Total" %in% names(df))
  expect_equal(nrow(df), 3L)
  # Values should be numeric, not formatted strings
  expect_true(is.numeric(df[[2L]]))
})

# -- Edge cases ---------------------------------------------------------------

test_that("tab errors on 0 variables", {
  expect_error(tab(mtcars), "At least one variable")
})

test_that("tab errors on 3+ variables", {
  expect_error(tab(mtcars, cyl, gear, am), "at most 2")
})

test_that("tab errors on non-existent column", {
  expect_error(tab(mtcars, nonexistent), "not found")
})

test_that("tab errors on non-data.frame", {
  expect_error(tab(1:10, x), "data.frame")
})

test_that("tab works with factor variables", {
  df <- data.frame(
    x = factor(c("b", "a", "c", "a", "b"), levels = c("c", "b", "a"))
  )
  result <- tab(df, x, .sort = "value_asc")
  tbl <- result$table[result$table$Value != "Total", ]
  # Factor level order: c, b, a
  expect_equal(tbl$Value[1], "c")
  expect_equal(tbl$Value[2], "b")
  expect_equal(tbl$Value[3], "a")
})

test_that("tab with all NAs and .na = 'exclude' produces empty table", {
  df <- data.frame(x = c(NA, NA, NA))
  result <- tab(df, x, .na = "exclude")
  tbl <- result$table[result$table$Value != "Total", ]
  expect_equal(nrow(tbl), 0L)
})

test_that("high-cardinality warning fires", {
  df <- data.frame(x = as.character(seq_len(60)), stringsAsFactors = FALSE)
  expect_warning(tab(df, x), "unique values")
})

test_that("n_missing counts rows where any variable is NA", {
  df <- data.frame(
    x = c("a", NA,  "b", "a", NA),
    y = c(NA,  "p", "q", "p", NA),
    stringsAsFactors = FALSE
  )
  # Rows with any NA: 1 (y=NA), 2 (x=NA), 5 (both NA) = 3 rows
  result <- tab(df, x, y)
  expect_equal(result$n_missing, 3L)
})

test_that("numeric values sort correctly with .na = 'include'", {
  df <- data.frame(x = c(2, 10, 100, 2, NA, 10))
  result <- tab(df, x, .sort = "value_asc", .na = "include")
  tbl <- result$table[result$table$Value != "Total", ]
  # <NA> should be last; numeric values should sort as 2, 10, 100 (not "10", "100", "2")
  non_na <- tbl$Value[tbl$Value != "<NA>"]
  expect_equal(non_na, c("2", "10", "100"))
})

test_that("piping works: df |> tab(var)", {
  result <- mtcars |> tab(cyl)
  expect_s3_class(result, "tidyaudit_tab")
  expect_equal(result$table$Freq[result$table$Value == "Total"], 32)
})
