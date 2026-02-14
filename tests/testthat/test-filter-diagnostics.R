test_that("filter_keep keeps matching rows", {
  df <- data.frame(id = 1:6, value = c(10, 20, 30, 40, 50, 60))
  result <- filter_keep(df, value > 30, .quiet = TRUE)

  expect_equal(nrow(result), 3L)
  expect_true(all(result$value > 30))
})

test_that("filter_keep prints diagnostics", {
  df <- data.frame(id = 1:10, value = 1:10)

  output <- capture.output(
    result <- filter_keep(df, value > 5),
    type = "message"
  )
  combined <- paste(output, collapse = "\n")
  expect_true(grepl("filter_keep", combined))
  expect_true(grepl("Dropped", combined))
  expect_true(grepl("5", combined))  # 5 rows dropped
})

test_that("filter_keep suppresses output with .quiet", {
  df <- data.frame(id = 1:10, value = 1:10)

  output <- capture.output(
    result <- filter_keep(df, value > 5, .quiet = TRUE),
    type = "message"
  )
  expect_equal(length(output), 0L)
})

test_that("filter_keep tracks .stat", {
  df <- data.frame(id = 1:4, sales = c(100, 200, 300, 400), keep = c(TRUE, FALSE, TRUE, FALSE))

  output <- capture.output(
    result <- filter_keep(df, keep == TRUE, .stat = sales),
    type = "message"
  )
  combined <- paste(output, collapse = "\n")
  expect_true(grepl("sales", combined))
  expect_equal(nrow(result), 2L)
})

test_that("filter_keep warns at threshold", {
  df <- data.frame(id = 1:100, value = 1:100)

  expect_warning(
    filter_keep(df, value > 90, .quiet = TRUE, .warn_threshold = 0.5),
    "exceeds threshold"
  )
})

test_that("filter_keep does not warn below threshold", {
  df <- data.frame(id = 1:100, value = 1:100)

  expect_no_warning(
    filter_keep(df, value > 10, .quiet = TRUE, .warn_threshold = 0.95)
  )
})

test_that("filter_keep works with multiple conditions", {
  df <- data.frame(id = 1:10, x = 1:10, y = 10:1)
  result <- filter_keep(df, x > 3, y > 3, .quiet = TRUE)

  expect_true(all(result$x > 3 & result$y > 3))
})

test_that("filter_drop drops matching rows", {
  df <- data.frame(id = 1:6, bad = c(FALSE, TRUE, FALSE, TRUE, FALSE, TRUE))
  result <- filter_drop(df, bad == TRUE, .quiet = TRUE)

  expect_equal(nrow(result), 3L)
  expect_true(all(!result$bad))
})

test_that("filter_drop prints diagnostics", {
  df <- data.frame(id = 1:10, value = 1:10)

  output <- capture.output(
    result <- filter_drop(df, value > 8),
    type = "message"
  )
  combined <- paste(output, collapse = "\n")
  expect_true(grepl("filter_drop", combined))
  expect_true(grepl("Dropped", combined))
})

test_that("filter_drop tracks .stat", {
  df <- data.frame(id = 1:4, sales = c(100, 200, 300, 400),
                   bad = c(TRUE, FALSE, TRUE, FALSE))

  output <- capture.output(
    result <- filter_drop(df, bad == TRUE, .stat = sales),
    type = "message"
  )
  combined <- paste(output, collapse = "\n")
  expect_true(grepl("sales", combined))
  # Dropped rows 1 and 3 (sales = 100 + 300 = 400)
  expect_equal(nrow(result), 2L)
})

test_that("filter_drop warns at threshold", {
  df <- data.frame(id = 1:10, value = 1:10)

  expect_warning(
    filter_drop(df, value < 9, .quiet = TRUE, .warn_threshold = 0.5),
    "exceeds threshold"
  )
})

test_that("filter_keep preserves tibble class", {
  skip_if_not_installed("dplyr")
  df <- dplyr::tibble(id = 1:5, value = 1:5)
  result <- filter_keep(df, value > 2, .quiet = TRUE)

  expect_s3_class(result, "tbl_df")
})

test_that("filter_drop preserves tibble class", {
  skip_if_not_installed("dplyr")
  df <- dplyr::tibble(id = 1:5, value = 1:5)
  result <- filter_drop(df, value < 3, .quiet = TRUE)

  expect_s3_class(result, "tbl_df")
})

test_that("filter_keep handles zero-row result", {
  df <- data.frame(id = 1:5, value = 1:5)
  result <- filter_keep(df, value > 100, .quiet = TRUE)

  expect_equal(nrow(result), 0L)
  expect_equal(ncol(result), ncol(df))
})

test_that("filter_drop with all rows matching drops everything", {
  df <- data.frame(id = 1:5, value = 1:5)
  result <- filter_drop(df, value > 0, .quiet = TRUE)

  expect_equal(nrow(result), 0L)
})

test_that("filter_drop works with multiple predicates", {
  df <- data.frame(a = 1:6, b = 6:1)
  # Drop rows where a > 2 AND b < 5 (rows 3,4,5,6)
  result <- filter_drop(df, a > 2, b < 5, .quiet = TRUE)

  expect_equal(nrow(result), 2L)
  # Kept rows should NOT satisfy both conditions
  expect_true(all(!(result$a > 2 & result$b < 5)))
})

test_that("filter_drop with no predicates returns all rows", {
  df <- data.frame(id = 1:5, value = 1:5)
  result <- filter_drop(df, .quiet = TRUE)

  expect_equal(nrow(result), 5L)
})

test_that("filter_drop with multiple predicates and .stat", {
  df <- data.frame(a = 1:6, b = 6:1, sales = c(10, 20, 30, 40, 50, 60))
  output <- capture.output(
    result <- filter_drop(df, a > 3, b < 4, .stat = sales),
    type = "message"
  )
  combined <- paste(output, collapse = "\n")
  expect_true(grepl("sales", combined))
  # Rows matching a > 3 & b < 4 are rows 4,5,6
  expect_equal(nrow(result), 3L)
})
