test_that("validate_join detects one-to-one relationship", {
  x <- data.frame(id = 1:3, val = c("a", "b", "c"), stringsAsFactors = FALSE)
  y <- data.frame(id = 2:4, score = c(10, 20, 30))
  result <- validate_join(x, y, by = "id")

  expect_s3_class(result, "validate_join")
  expect_equal(result$relation, "one-to-one")
  expect_equal(result$counts$x_rows, 3L)
  expect_equal(result$counts$y_rows, 3L)
  expect_equal(result$counts$n_key_overlap, 2L)
  expect_equal(result$counts$n_only_x, 1L)
  expect_equal(result$counts$n_only_y, 1L)
})

test_that("validate_join detects many-to-one relationship", {
  x <- data.frame(id = c(1L, 2L, 3L, 3L), val = c("a", "b", "c", "d"),
                   stringsAsFactors = FALSE)
  y <- data.frame(id = c(2L, 3L, 4L), score = c(10, 20, 30))
  result <- validate_join(x, y, by = "id")

  expect_equal(result$relation, "many-to-one")
  expect_true(result$duplicates$x_has_dups)
  expect_false(result$duplicates$y_has_dups)
})

test_that("validate_join detects no matches", {
  x <- data.frame(id = 1:3)
  y <- data.frame(id = 4:6)
  result <- validate_join(x, y, by = "id")

  expect_equal(result$relation, "no matches")
  expect_equal(result$counts$n_key_overlap, 0L)
})

test_that("validate_join works with named by vector", {
  x <- data.frame(key_x = c(1L, 2L, 3L), val = c("a", "b", "c"),
                   stringsAsFactors = FALSE)
  y <- data.frame(key_y = c(2L, 3L, 4L), score = c(10, 20, 30))
  result <- validate_join(x, y, by = c("key_x" = "key_y"))

  expect_equal(result$by_x, "key_x")
  expect_equal(result$by_y, "key_y")
  expect_equal(result$counts$n_key_overlap, 2L)
})

test_that("validate_join tracks stat column (same name)", {
  x <- data.frame(id = 1:4, revenue = c(100, 200, 300, 400))
  y <- data.frame(id = 2:5, revenue = c(10, 20, 30, 40))
  result <- validate_join(x, y, by = "id", stat = "revenue")

  expect_false(is.null(result$stat))
  expect_equal(result$stat$stat_col_x, "revenue")
  expect_equal(result$stat$stat_col_y, "revenue")
  expect_equal(result$stat$x$total, 1000)
  expect_equal(result$stat$y$total, 100)
})

test_that("validate_join tracks stat with stat_x and stat_y", {
  x <- data.frame(id = 1:3, sales = c(100, 200, 300))
  y <- data.frame(id = 2:4, cost = c(10, 20, 30))
  result <- validate_join(x, y, by = "id", stat_x = "sales", stat_y = "cost")

  expect_equal(result$stat$stat_col_x, "sales")
  expect_equal(result$stat$stat_col_y, "cost")
  expect_false(is.null(result$stat$x))
  expect_false(is.null(result$stat$y))
})

test_that("validate_join errors on missing key columns", {
  x <- data.frame(id = 1:3)
  y <- data.frame(id = 1:3)

  expect_error(validate_join(x, y, by = "nonexistent"), "missing key")
})

test_that("validate_join errors without by argument", {
  x <- data.frame(id = 1:3)
  y <- data.frame(id = 1:3)

  expect_error(validate_join(x, y), "must be provided")
})

test_that("validate_join errors on non-numeric stat", {
  x <- data.frame(id = 1:3, name = c("a", "b", "c"), stringsAsFactors = FALSE)
  y <- data.frame(id = 1:3)

  expect_error(validate_join(x, y, by = "id", stat = "name"), "not numeric")
})

test_that("validate_join has correct match rates", {
  x <- data.frame(id = 1:10, val = 1:10)
  y <- data.frame(id = 6:15, val = 6:15)
  result <- validate_join(x, y, by = "id")

  expect_equal(result$counts$match_rate_x, 50)
  expect_equal(result$counts$match_rate_y, 50)
})

test_that("validate_join summary_table is a data.frame", {
  x <- data.frame(id = 1:3)
  y <- data.frame(id = 2:4)
  result <- validate_join(x, y, by = "id")

  expect_true(is.data.frame(result$summary_table))
  expect_true("Item" %in% names(result$summary_table))
  expect_true("Value" %in% names(result$summary_table))
})

test_that("print.validate_join produces output", {
  x <- data.frame(id = 1:3)
  y <- data.frame(id = 2:4)
  result <- validate_join(x, y, by = "id")

  output <- capture.output(print(result), type = "message")
  combined <- paste(output, collapse = "\n")
  expect_true(grepl("Join Validation", combined))
})

test_that("summary.validate_join returns summary_table", {
  x <- data.frame(id = 1:3)
  y <- data.frame(id = 2:4)
  result <- validate_join(x, y, by = "id")

  st <- capture.output(s <- summary(result), type = "message")
  expect_true(is.data.frame(s))
})

test_that("validate_join works with composite keys", {
  x <- data.frame(a = c(1L, 1L, 2L), b = c("x", "y", "x"),
                   stringsAsFactors = FALSE)
  y <- data.frame(a = c(1L, 2L), b = c("x", "x"),
                   stringsAsFactors = FALSE)
  result <- validate_join(x, y, by = c("a", "b"))

  expect_equal(result$counts$n_key_overlap, 2L)
  expect_equal(result$counts$n_only_x, 1L)
})

test_that("validate_join works with tibbles", {
  skip_if_not_installed("dplyr")
  x <- dplyr::tibble(id = 1:3, val = c(10, 20, 30))
  y <- dplyr::tibble(id = 2:4, val = c(40, 50, 60))
  result <- validate_join(x, y, by = "id")

  expect_s3_class(result, "validate_join")
  expect_equal(result$relation, "one-to-one")
})

test_that("validate_join stat_x only tracks x side", {
  x <- data.frame(id = 1:3, sales = c(100, 200, 300))
  y <- data.frame(id = 2:4, cost = c(10, 20, 30))
  result <- validate_join(x, y, by = "id", stat_x = "sales")

  expect_false(is.null(result$stat))
  expect_false(is.null(result$stat$x))
  expect_null(result$stat$y)
  expect_equal(result$stat$stat_col_x, "sales")
})

test_that("validate_join stat_y only tracks y side", {
  x <- data.frame(id = 1:3, sales = c(100, 200, 300))
  y <- data.frame(id = 2:4, cost = c(10, 20, 30))
  result <- validate_join(x, y, by = "id", stat_y = "cost")

  expect_false(is.null(result$stat))
  expect_null(result$stat$x)
  expect_false(is.null(result$stat$y))
  expect_equal(result$stat$stat_col_y, "cost")
})

test_that("validate_join stat_x/stat_y override stat", {
  x <- data.frame(id = 1:3, sales = c(100, 200, 300), revenue = c(1, 2, 3))
  y <- data.frame(id = 2:4, cost = c(10, 20, 30), revenue = c(4, 5, 6))
  # stat_x/stat_y should take precedence over stat
  result <- validate_join(x, y, by = "id", stat = "revenue",
                          stat_x = "sales", stat_y = "cost")

  expect_equal(result$stat$stat_col_x, "sales")
  expect_equal(result$stat$stat_col_y, "cost")
})
