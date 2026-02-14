test_that("validate_primary_keys detects valid primary key", {
  df <- data.frame(id = 1:4, group = c("A", "A", "B", "B"),
                   stringsAsFactors = FALSE)
  result <- validate_primary_keys(df, "id")

  expect_s3_class(result, "validate_pk")
  expect_true(result$is_primary_key)
  expect_equal(result$n_rows, 4L)
  expect_equal(result$n_unique_keys, 4L)
  expect_equal(result$n_duplicate_keys, 0L)
})

test_that("validate_primary_keys detects non-primary key", {
  df <- data.frame(id = 1:4, group = c("A", "A", "B", "B"),
                   stringsAsFactors = FALSE)
  result <- validate_primary_keys(df, "group")

  expect_false(result$is_primary_key)
  expect_equal(result$n_unique_keys, 2L)
  expect_equal(result$n_duplicate_keys, 2L)
  expect_true(nrow(result$duplicate_keys) > 0L)
})

test_that("validate_primary_keys works with composite keys", {
  df <- data.frame(a = c(1L, 1L, 2L, 2L), b = c("x", "y", "x", "y"),
                   stringsAsFactors = FALSE)
  result <- validate_primary_keys(df, c("a", "b"))

  expect_true(result$is_primary_key)
  expect_equal(result$n_unique_keys, 4L)
})

test_that("validate_primary_keys warns about numeric double keys", {
  df <- data.frame(id = c(1.1, 2.2, 3.3))
  expect_warning(result <- validate_primary_keys(df, "id"), "numeric")
  expect_true(result$has_numeric_keys)
})

test_that("validate_primary_keys errors on missing columns", {
  df <- data.frame(id = 1:3)
  expect_error(validate_primary_keys(df, "nonexistent"), "not found")
})

test_that("validate_primary_keys errors on non-character keys", {
  df <- data.frame(id = 1:3)
  expect_error(validate_primary_keys(df, 1), "character vector")
})

test_that("print.validate_pk produces output for valid PK", {
  df <- data.frame(id = 1:3)
  result <- validate_primary_keys(df, "id")

  output <- capture.output(print(result), type = "message")
  combined <- paste(output, collapse = "\n")
  expect_true(grepl("Primary Key", combined))
  expect_true(grepl("YES", combined))
})

test_that("print.validate_pk shows duplicates for invalid PK", {
  df <- data.frame(id = c(1L, 1L, 2L))
  result <- validate_primary_keys(df, "id")

  output <- capture.output(print(result), type = "message")
  combined <- paste(output, collapse = "\n")
  expect_true(grepl("NO", combined))
})

test_that("validate_primary_keys works with tibbles", {
  skip_if_not_installed("dplyr")
  df <- dplyr::tibble(id = 1:5, val = letters[1:5])
  result <- validate_primary_keys(df, "id")
  expect_true(result$is_primary_key)
})

# validate_var_relationship tests

test_that("validate_var_relationship detects one-to-one", {
  df <- data.frame(a = c(1L, 2L, 3L), b = c("x", "y", "z"),
                   stringsAsFactors = FALSE)
  result <- validate_var_relationship(df, "a", "b")

  expect_s3_class(result, "validate_var_rel")
  expect_equal(result$relation, "one-to-one")
  expect_false(result$var1_has_dups)
  expect_false(result$var2_has_dups)
})

test_that("validate_var_relationship detects one-to-many", {
  # a=1 maps to {x, y}, a=2 maps to {z}: var1_has_dups = TRUE
  # b=x maps to {1}, b=y maps to {1}, b=z maps to {2}: var2_has_dups = FALSE
  df <- data.frame(a = c(1L, 1L, 2L), b = c("x", "y", "z"),
                   stringsAsFactors = FALSE)
  result <- validate_var_relationship(df, "a", "b")

  expect_equal(result$relation, "one-to-many")
  expect_true(result$var1_has_dups)
  expect_false(result$var2_has_dups)
})

test_that("validate_var_relationship detects many-to-one", {
  df <- data.frame(dept = c("Sales", "Sales", "Eng", "Eng"),
                   person = c(1L, 2L, 3L, 4L),
                   stringsAsFactors = FALSE)
  result <- validate_var_relationship(df, "person", "dept")

  expect_equal(result$relation, "many-to-one")
})

test_that("validate_var_relationship detects many-to-many", {
  df <- data.frame(a = c(1L, 1L, 2L, 2L), b = c("x", "y", "x", "y"),
                   stringsAsFactors = FALSE)
  result <- validate_var_relationship(df, "a", "b")

  expect_equal(result$relation, "many-to-many")
  expect_true(result$var1_has_dups)
  expect_true(result$var2_has_dups)
})

test_that("validate_var_relationship rejects numeric double columns", {
  df <- data.frame(a = c(1.1, 2.2), b = c("x", "y"), stringsAsFactors = FALSE)
  expect_error(validate_var_relationship(df, "a", "b"), "character, integer, or factor")
})

test_that("validate_var_relationship errors on missing variable", {
  df <- data.frame(a = 1:3)
  expect_error(validate_var_relationship(df, "a", "nonexistent"), "not found")
})

test_that("print.validate_var_rel produces output", {
  df <- data.frame(a = c(1L, 2L, 3L), b = c("x", "y", "z"),
                   stringsAsFactors = FALSE)
  result <- validate_var_relationship(df, "a", "b")

  output <- capture.output(print(result), type = "message")
  combined <- paste(output, collapse = "\n")
  expect_true(grepl("Variable Relationship", combined))
  expect_true(grepl("ONE-TO-ONE", combined))
})

test_that("validate_var_relationship works with factors", {
  df <- data.frame(a = factor(c("x", "y", "z")), b = c(1L, 2L, 3L))
  result <- validate_var_relationship(df, "a", "b")

  expect_equal(result$relation, "one-to-one")
})
