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

# audit_transform tests

test_that("audit_transform reports changes from trimws", {
  x <- c("  hello ", "world", "  foo  ", NA)
  result <- audit_transform(x, trimws)

  expect_s3_class(result, "audit_transform")
  expect_equal(result$n_total, 4L)
  expect_equal(result$n_na, 1L)
  expect_equal(result$n_changed, 2L)  # "  hello " and "  foo  " change
  expect_equal(result$n_unchanged, 2L) # "world" and NA unchanged
  expect_equal(length(result$cleaned), 4L)
  expect_equal(result$cleaned[1], "hello")
  expect_equal(result$cleaned[3], "foo")
  expect_true(is.na(result$cleaned[4]))
})

test_that("audit_transform captures function name", {
  x <- c("a", "b")
  result <- audit_transform(x, toupper)

  expect_equal(result$clean_fn_name, "toupper")
  expect_equal(result$cleaned, c("A", "B"))
  expect_equal(result$n_changed, 2L)
})

test_that("audit_transform handles no changes", {
  x <- c("HELLO", "WORLD")
  result <- audit_transform(x, toupper)

  expect_equal(result$n_changed, 0L)
  expect_equal(nrow(result$change_examples), 0L)
  expect_equal(result$pct_changed, 0)
})

test_that("audit_transform provides change examples", {
  x <- c("  a  ", "b", "  c  ", "d", "  e  ")
  result <- audit_transform(x, trimws)

  expect_true(nrow(result$change_examples) > 0L)
  expect_true(all(c("before", "after") %in% names(result$change_examples)))
})

test_that("audit_transform handles all-NA input", {
  x <- c(NA_character_, NA_character_)
  result <- audit_transform(x, toupper)

  expect_equal(result$n_changed, 0L)
  expect_equal(result$n_na, 2L)
  expect_equal(result$pct_changed, 0)
})

test_that("audit_transform captures custom name", {
  result <- audit_transform(c("a", "b"), toupper, name = "test_col")
  expect_equal(result$name, "test_col")
})

test_that("print.audit_transform produces output", {
  x <- c("  hello ", "world")
  result <- audit_transform(x, trimws)

  output <- capture.output(print(result), type = "message")
  combined <- paste(output, collapse = "\n")
  expect_true(grepl("String Transformation Audit", combined))
  expect_true(grepl("trimws", combined))
})

test_that("audit_transform pct_changed is correct", {
  x <- c("a", "b", "c", "d", NA)
  # toupper changes all 4 non-NA values
  result <- audit_transform(x, toupper)

  expect_equal(result$pct_changed, 100)
})

test_that("audit_transform change examples limited to 10", {
  x <- paste0("item_", 1:20)
  result <- audit_transform(x, toupper)

  expect_true(nrow(result$change_examples) <= 10L)
})

test_that("audit_transform errors when clean_fn returns wrong length", {
  expect_error(
    audit_transform(c("a", "b", "c"), function(x) x[1]),
    "same length"
  )
})

test_that("audit_transform errors when clean_fn returns longer vector", {
  expect_error(
    audit_transform(c("a", "b"), function(x) rep(x, 2)),
    "same length"
  )
})
