test_that("diagnose_strings detects basic issues", {
  skip_if_not_installed("stringi")
  x <- c("Apple", "APPLE", "apple", "  Microsoft ", "Google", NA, "")
  result <- diagnose_strings(x)

  expect_s3_class(result, "diagnose_strings")
  expect_equal(result$n_total, 7L)
  expect_equal(result$n_na, 1L)
  expect_equal(result$n_empty, 1L)
})

test_that("diagnose_strings detects whitespace issues", {
  skip_if_not_installed("stringi")
  x <- c("  leading", "trailing  ", "  both  ", "normal", "   ")
  result <- diagnose_strings(x)

  expect_equal(result$n_leading_ws, 2L)   # "  leading", "  both  "
  expect_equal(result$n_trailing_ws, 2L)  # "trailing  ", "  both  "
  expect_equal(result$n_whitespace_only, 1L)  # "   "
})

test_that("diagnose_strings detects case variants", {
  skip_if_not_installed("stringi")
  x <- c("Apple", "APPLE", "apple", "Google", "google")
  result <- diagnose_strings(x)

  expect_equal(result$n_case_variant_groups, 2L)
  expect_true(nrow(result$case_variant_examples) > 0L)
})

test_that("diagnose_strings handles all-NA input", {
  skip_if_not_installed("stringi")
  x <- c(NA_character_, NA_character_)
  result <- diagnose_strings(x)

  expect_equal(result$n_total, 2L)
  expect_equal(result$n_na, 2L)
  expect_equal(result$n_empty, 0L)
  expect_equal(result$n_case_variant_groups, 0L)
})

test_that("diagnose_strings handles clean input", {
  skip_if_not_installed("stringi")
  x <- c("alpha", "beta", "gamma")
  result <- diagnose_strings(x)

  expect_equal(result$n_na, 0L)
  expect_equal(result$n_empty, 0L)
  expect_equal(result$n_whitespace_only, 0L)
  expect_equal(result$n_leading_ws, 0L)
  expect_equal(result$n_trailing_ws, 0L)
  expect_equal(result$n_case_variant_groups, 0L)
})

test_that("diagnose_strings captures variable name", {
  skip_if_not_installed("stringi")
  my_var <- c("a", "b")
  result <- diagnose_strings(my_var)

  expect_equal(result$name, "my_var")
})

test_that("diagnose_strings uses explicit name", {
  skip_if_not_installed("stringi")
  result <- diagnose_strings(c("a", "b"), name = "test_col")

  expect_equal(result$name, "test_col")
})

test_that("print.diagnose_strings produces output", {
  skip_if_not_installed("stringi")
  x <- c("Apple", "APPLE", NA, "")
  result <- diagnose_strings(x)

  output <- capture.output(print(result), type = "message")
  combined <- paste(output, collapse = "\n")
  expect_true(grepl("String Column Diagnosis", combined))
})

test_that("diagnose_strings detects non-ASCII", {
  skip_if_not_installed("stringi")
  x <- c("hello", "caf\u00e9", "na\u00efve")
  result <- diagnose_strings(x)

  expect_equal(result$n_non_ascii, 2L)
})
