test_that("audit_transform [character] reports changes from trimws", {
  x <- c("  hello ", "world", "  foo  ", NA)
  result <- audit_transform(x, trimws)

  expect_s3_class(result, "audit_transform")
  expect_equal(result$type_class, "character")
  expect_equal(result$n_total, 4L)
  expect_equal(result$n_na_before, 1L)
  expect_equal(result$n_changed, 2L)  # "  hello " and "  foo  " change
  expect_equal(result$n_unchanged, 2L) # "world" and NA unchanged
  expect_equal(length(result$cleaned), 4L)
  expect_equal(result$cleaned[1], "hello")
  expect_equal(result$cleaned[3], "foo")
  expect_true(is.na(result$cleaned[4]))
  expect_null(result$diagnostics)
})

test_that("audit_transform [character] captures function name", {
  x <- c("a", "b")
  result <- audit_transform(x, toupper)

  expect_equal(result$clean_fn_name, "toupper")
  expect_equal(result$cleaned, c("A", "B"))
  expect_equal(result$n_changed, 2L)
})

test_that("audit_transform [character] handles no changes", {
  x <- c("HELLO", "WORLD")
  result <- audit_transform(x, toupper)

  expect_equal(result$n_changed, 0L)
  expect_equal(nrow(result$change_examples), 0L)
  expect_equal(result$pct_changed, 0)
})

test_that("audit_transform [character] provides change examples", {
  x <- c("  a  ", "b", "  c  ", "d", "  e  ")
  result <- audit_transform(x, trimws)

  expect_true(nrow(result$change_examples) > 0L)
  expect_true(all(c("before", "after") %in% names(result$change_examples)))
})

test_that("audit_transform [character] handles all-NA input", {
  x <- c(NA_character_, NA_character_)
  result <- audit_transform(x, toupper)

  expect_equal(result$n_changed, 0L)
  expect_equal(result$n_na_before, 2L)
  expect_equal(result$pct_changed, 0)
})

test_that("audit_transform [character] captures custom name", {
  result <- audit_transform(c("a", "b"), toupper, name = "test_col")
  expect_equal(result$name, "test_col")
})

test_that("print.audit_transform [character] produces output", {
  x <- c("  hello ", "world")
  result <- audit_transform(x, trimws)

  output <- capture.output(print(result), type = "message")
  combined <- paste(output, collapse = "\n")
  expect_true(grepl("Transformation Audit", combined))
  expect_true(grepl("character", combined))
  expect_true(grepl("trimws", combined))
})

test_that("audit_transform [character] pct_changed uses n_total as denominator", {
  # 4 of 5 elements change (NA stays NA); pct = 4/5 * 100 = 80
  x <- c("a", "b", "c", "d", NA)
  result <- audit_transform(x, toupper)

  expect_equal(result$pct_changed, 80)
})

test_that("audit_transform [character] change examples limited to 10", {
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

# ---------------------------------------------------------------------------
# Numeric
# ---------------------------------------------------------------------------

test_that("audit_transform [numeric] detects type and computes diagnostics", {
  x <- c(1, 2, NA, 4)
  result <- audit_transform(x, function(v) v * 2)

  expect_s3_class(result, "audit_transform")
  expect_equal(result$type_class, "numeric")
  expect_equal(result$n_total, 4L)
  expect_equal(result$n_changed, 3L)   # 1→2, 2→4, 4→8 (NA stays NA)
  expect_equal(result$n_unchanged, 1L) # NA→NA counts as unchanged
  expect_equal(result$n_na_before, 1L)
  expect_equal(result$n_na_after, 1L)
  expect_equal(result$cleaned, c(2, 4, NA, 8))
  expect_false(is.null(result$diagnostics))
})

test_that("audit_transform [numeric] diagnostics fields are correct", {
  x <- c(1, 2, 3, 4)
  result <- audit_transform(x, function(v) v * 2)
  d <- result$diagnostics

  expect_equal(d$mean_before, 2.5)
  expect_equal(d$mean_after, 5.0)
  expect_equal(d$min_before, 1)
  expect_equal(d$max_before, 4)
  expect_equal(d$min_after, 2)
  expect_equal(d$max_after, 8)
  expect_equal(d$n_nan_before, 0L)
  expect_equal(d$n_inf_before, 0L)
  expect_true(!is.null(d$mean_abs_delta))
  expect_equal(d$mean_abs_delta, 2.5)  # avg delta = mean(1,2,3,4) = 2.5
})

test_that("audit_transform [numeric] n_beyond_tol uses tolerance", {
  x <- c(1.0, 2.0, 3.0)
  # Add a tiny delta below default tolerance to one element
  tiny <- sqrt(.Machine$double.eps) / 10
  result <- audit_transform(x, function(v) v + c(0, tiny, 1))

  d <- result$diagnostics
  expect_equal(d$n_beyond_tol, 1L)  # only the +1 exceeds tolerance
})

test_that("audit_transform [numeric] handles all-NA", {
  x <- c(NA_real_, NA_real_)
  result <- audit_transform(x, function(v) v + 1)

  expect_equal(result$n_changed, 0L)
  expect_equal(result$n_na_before, 2L)
  expect_true(is.na(result$diagnostics$mean_before))
})

test_that("audit_transform [numeric] zero changes", {
  x <- c(1.0, 2.0, 3.0)
  result <- audit_transform(x, function(v) v)

  expect_equal(result$n_changed, 0L)
  expect_equal(result$diagnostics$mean_abs_delta, 0)
})

test_that("print.audit_transform [numeric] mentions type and summary", {
  x <- c(1, 2, 3)
  result <- audit_transform(x, function(v) v * 2)
  output <- capture.output(print(result), type = "message")
  combined <- paste(output, collapse = "\n")
  expect_true(grepl("numeric", combined))
  expect_true(grepl("Mean", combined))
})

# ---------------------------------------------------------------------------
# Date
# ---------------------------------------------------------------------------

test_that("audit_transform [Date] detects type and computes range diagnostics", {
  x <- as.Date(c("2020-01-01", "2020-06-15", "2020-12-31", NA))
  result <- audit_transform(x, function(d) d + 1L)

  expect_equal(result$type_class, "Date")
  expect_equal(result$n_changed, 3L)
  expect_equal(result$n_na_before, 1L)
  d <- result$diagnostics
  expect_false(is.null(d))
  expect_equal(d$min_before, as.Date("2020-01-01"))
  expect_equal(d$max_before, as.Date("2020-12-31"))
  expect_equal(d$range_days_before, 365)
})

test_that("print.audit_transform [Date] mentions type and date range", {
  x <- as.Date(c("2020-01-01", "2020-12-31"))
  result <- audit_transform(x, function(d) d + 10L)
  output <- capture.output(print(result), type = "message")
  combined <- paste(output, collapse = "\n")
  expect_true(grepl("Date", combined))
  expect_true(grepl("Span", combined))
})

# ---------------------------------------------------------------------------
# Factor
# ---------------------------------------------------------------------------

test_that("audit_transform [factor] detects level changes", {
  f <- factor(c("a", "b", "b", "c", "c"))
  # Rename level "c" → "d": removes "c", adds "d", same length
  result <- audit_transform(f, function(v) {
    levels(v)[levels(v) == "c"] <- "d"
    v
  })

  expect_equal(result$type_class, "factor")
  d <- result$diagnostics
  expect_true("c" %in% d$levels_removed)
  expect_true("d" %in% d$levels_added)
})

test_that("audit_transform [factor] level_counts have correct structure", {
  f <- factor(c("a", "b", "a"))
  result <- audit_transform(f, function(v) factor(toupper(as.character(v))))
  d <- result$diagnostics

  expect_true(is.data.frame(d$level_counts_before))
  expect_true(is.data.frame(d$level_counts_after))
  expect_true("Level" %in% names(d$level_counts_before))
})

test_that("print.audit_transform [factor] mentions type", {
  f <- factor(c("a", "b", "a"))
  result <- audit_transform(f, function(v) factor(toupper(as.character(v))))
  output <- capture.output(print(result), type = "message")
  combined <- paste(output, collapse = "\n")
  expect_true(grepl("factor", combined))
})

# ---------------------------------------------------------------------------
# Logical
# ---------------------------------------------------------------------------

test_that("audit_transform [logical] flips TRUE/FALSE correctly", {
  x <- c(TRUE, FALSE, TRUE, NA, FALSE)
  result <- audit_transform(x, `!`)

  expect_equal(result$type_class, "logical")
  expect_equal(result$n_changed, 4L)  # 4 non-NA values all flip
  expect_equal(result$n_na_before, 1L)
  d <- result$diagnostics
  expect_equal(d$n_true_before,  2L)
  expect_equal(d$n_false_before, 2L)
  expect_equal(d$n_true_after,   2L)
  expect_equal(d$n_false_after,  2L)
})

test_that("print.audit_transform [logical] shows balance table", {
  x <- c(TRUE, FALSE, TRUE)
  result <- audit_transform(x, `!`)
  output <- capture.output(print(result), type = "message")
  combined <- paste(output, collapse = "\n")
  expect_true(grepl("logical", combined))
  expect_true(grepl("TRUE", combined))
  expect_true(grepl("FALSE", combined))
})

# ---------------------------------------------------------------------------
# Pre-computed vector
# ---------------------------------------------------------------------------

test_that("audit_transform accepts pre-computed vector", {
  x <- c(1, 2, NA, 4)
  cleaned <- c(2, 4, NA, 8)
  result <- audit_transform(x, cleaned)

  expect_equal(result$clean_fn_name, "<pre-computed>")
  expect_equal(result$cleaned, cleaned)
  expect_equal(result$n_changed, 3L)
})

test_that("audit_transform errors on pre-computed vector of wrong length", {
  expect_error(
    audit_transform(c(1, 2, 3), c(1, 2)),
    "same length"
  )
})

# ---------------------------------------------------------------------------
# Edge cases — empty vector
# ---------------------------------------------------------------------------

test_that("audit_transform handles empty character vector", {
  result <- audit_transform(character(0), toupper)

  expect_equal(result$n_total, 0L)
  expect_equal(result$n_changed, 0L)
  expect_equal(result$pct_changed, 0)
  expect_equal(nrow(result$change_examples), 0L)
})

test_that("audit_transform handles empty numeric vector", {
  result <- audit_transform(numeric(0), function(v) v * 2)

  expect_equal(result$type_class, "numeric")
  expect_equal(result$n_total, 0L)
  expect_equal(result$pct_changed, 0)
})

# ---------------------------------------------------------------------------
# Integer input → classified as "numeric"
# ---------------------------------------------------------------------------

test_that("audit_transform classifies integer as numeric", {
  x <- 1L:5L
  result <- audit_transform(x, rev)

  expect_equal(result$type_class, "numeric")
  expect_false(is.null(result$diagnostics))
  expect_equal(result$diagnostics$mean_before, 3)
})

# ---------------------------------------------------------------------------
# POSIXct
# ---------------------------------------------------------------------------

test_that("audit_transform [POSIXct] detects type and range diagnostics", {
  x <- as.POSIXct(c("2020-01-01 00:00:00", "2020-06-15 12:00:00",
                     "2020-12-31 23:59:59"), tz = "UTC")
  result <- audit_transform(x, function(v) v + 3600)  # add 1 hour

  expect_equal(result$type_class, "POSIXct")
  expect_equal(result$n_changed, 3L)
  d <- result$diagnostics
  expect_false(is.null(d))
  expect_true(!is.na(d$range_days_before))
  expect_true(!is.na(d$range_days_after))
})

# ---------------------------------------------------------------------------
# Numeric: NaN and Inf
# ---------------------------------------------------------------------------

test_that("audit_transform [numeric] counts NaN and Inf correctly", {
  x <- c(1, NaN, Inf, -Inf, NA)
  result <- audit_transform(x, function(v) v * 2)
  d <- result$diagnostics

  expect_equal(d$n_nan_before, 1L)
  expect_equal(d$n_inf_before, 2L)  # Inf and -Inf
  expect_equal(d$n_nan_after, 1L)
  expect_equal(d$n_inf_after, 2L)
})

# ---------------------------------------------------------------------------
# Factor: clean_fn returns character (not factor)
# ---------------------------------------------------------------------------

test_that("audit_transform [factor] handles clean_fn returning character", {
  f <- factor(c("a", "b", "c"))
  result <- audit_transform(f, function(v) as.character(v))

  expect_equal(result$type_class, "factor")
  d <- result$diagnostics
  # levels_after derived from unique values of character output
  expect_true(is.character(d$levels_after))
  expect_equal(sort(d$levels_after), c("a", "b", "c"))
})

# ---------------------------------------------------------------------------
# Transform introduces new NAs (value → NA)
# ---------------------------------------------------------------------------

test_that("audit_transform detects value-to-NA transitions as changed", {
  x <- c(1, 2, 3)
  result <- audit_transform(x, function(v) ifelse(v == 2, NA_real_, v))

  expect_equal(result$n_changed, 1L)
  expect_equal(result$n_na_before, 0L)
  expect_equal(result$n_na_after, 1L)
  # pct_changed = 1/3 * 100
  expect_equal(result$pct_changed, 100 / 3)
})

test_that("audit_transform detects NA-to-value transitions as changed", {
  x <- c(NA_real_, 2, 3)
  result <- audit_transform(x, function(v) ifelse(is.na(v), 99, v))

  expect_equal(result$n_changed, 1L)
  expect_equal(result$n_na_before, 1L)
  expect_equal(result$n_na_after, 0L)
})

# ---------------------------------------------------------------------------
# Custom .tolerance
# ---------------------------------------------------------------------------

test_that("audit_transform [numeric] respects custom .tolerance", {
  x <- c(1.0, 2.0, 3.0)
  # Delta of 0.5 exceeds tight tolerance of 0.1 but not loose tolerance of 1.0
  result_tight <- audit_transform(x, function(v) v + 0.5, .tolerance = 0.1)
  result_loose <- audit_transform(x, function(v) v + 0.5, .tolerance = 1.0)

  expect_equal(result_tight$diagnostics$n_beyond_tol, 3L)
  expect_equal(result_loose$diagnostics$n_beyond_tol, 0L)
})
