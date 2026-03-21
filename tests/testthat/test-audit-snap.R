test_that(".build_snapshot captures correct dimensions", {
  snap <- tidyaudit:::.build_snapshot(mtcars, label = "test", index = 1L)

  expect_s3_class(snap, "audit_snap")
  expect_equal(snap$label, "test")
  expect_equal(snap$index, 1L)
  expect_equal(snap$nrow, 32L)
  expect_equal(snap$ncol, 11L)
  expect_equal(snap$type, "tap")
  expect_equal(snap$all_columns, names(mtcars))
})

test_that(".build_snapshot captures schema", {
  df <- data.frame(x = 1:5, y = c("a", "b", "c", NA, "e"),
                   z = c(1.1, 2.2, NA, 4.4, 5.5),
                   stringsAsFactors = FALSE)
  snap <- tidyaudit:::.build_snapshot(df, label = "test", index = 1L)

  expect_equal(nrow(snap$schema), 3L)
  expect_equal(snap$schema$column, c("x", "y", "z"))
  expect_equal(snap$schema$n_na, c(0L, 1L, 1L))
  expect_equal(snap$total_nas, 2L)
})

test_that(".build_snapshot computes numeric summaries", {
  df <- data.frame(x = 1:10, y = letters[1:10], stringsAsFactors = FALSE)
  snap <- tidyaudit:::.build_snapshot(df, label = "test", index = 1L)

  expect_false(is.null(snap$numeric_summary))
  expect_equal(snap$numeric_summary$column, "x")
  expect_equal(snap$numeric_summary$mean, 5.5)
  expect_equal(snap$numeric_summary$min, 1)
  expect_equal(snap$numeric_summary$max, 10)
})

test_that(".build_snapshot handles all-NA numeric column", {
  df <- data.frame(x = rep(NA_real_, 5))
  snap <- tidyaudit:::.build_snapshot(df, label = "test", index = 1L)

  expect_false(is.null(snap$numeric_summary))
  expect_true(is.na(snap$numeric_summary$mean))
})

test_that(".build_snapshot handles no numeric columns", {
  df <- data.frame(x = letters[1:5], y = letters[6:10], stringsAsFactors = FALSE)
  snap <- tidyaudit:::.build_snapshot(df, label = "test", index = 1L)

  expect_null(snap$numeric_summary)
})

test_that(".build_snapshot computes on ungrouped data", {
  grouped <- dplyr::group_by(mtcars, cyl)
  snap <- tidyaudit:::.build_snapshot(grouped, label = "test", index = 1L)

  expect_equal(snap$nrow, 32L)
})

test_that("print.audit_snap produces output", {
  snap <- tidyaudit:::.build_snapshot(mtcars, label = "test", index = 1L)
  output <- capture.output(print(snap), type = "message")
  combined <- paste(output, collapse = "\n")
  expect_true(grepl("Snapshot", combined))
})

# ── Snapshot controls ─────────────────────────────────────────────────────────

test_that(".numeric_summary = FALSE skips numeric summary", {
  snap <- tidyaudit:::.build_snapshot(mtcars, label = "test", index = 1L,
                                       .numeric_summary = FALSE)
  expect_null(snap$numeric_summary)
  # Schema is still populated
  expect_equal(nrow(snap$schema), 11L)
  # Core invariants still computed
  expect_equal(snap$nrow, 32L)
  expect_equal(snap$ncol, 11L)
})

test_that(".cols_include filters schema to specified columns", {
  df <- data.frame(a = 1:5, b = letters[1:5], c = 1.1:5.1,
                   stringsAsFactors = FALSE)
  snap <- tidyaudit:::.build_snapshot(df, label = "test", index = 1L,
                                       .cols_include = c("a", "c"))
  expect_equal(snap$schema$column, c("a", "c"))
  expect_equal(nrow(snap$schema), 2L)
  # nrow/ncol from full data
  expect_equal(snap$ncol, 3L)
  # all_columns always has full set
  expect_equal(snap$all_columns, c("a", "b", "c"))
})

test_that(".cols_exclude removes specified columns from schema", {
  df <- data.frame(a = 1:5, b = letters[1:5], c = 1.1:5.1,
                   stringsAsFactors = FALSE)
  snap <- tidyaudit:::.build_snapshot(df, label = "test", index = 1L,
                                       .cols_exclude = "b")
  expect_equal(snap$schema$column, c("a", "c"))
  expect_equal(snap$ncol, 3L)
})

test_that("total_nas always computed from all columns", {
  df <- data.frame(a = c(1, NA, 3), b = c(NA, NA, "x"),
                   stringsAsFactors = FALSE)
  snap <- tidyaudit:::.build_snapshot(df, label = "test", index = 1L,
                                       .cols_include = "a")
  # total_nas counts NAs across ALL columns (a has 1, b has 2 = 3 total)
  expect_equal(snap$total_nas, 3L)
  # But schema only has column "a"
  expect_equal(snap$schema$column, "a")
  expect_equal(snap$schema$n_na, 1L)
})

test_that(".cols_include limits numeric summary scope", {
  df <- data.frame(x = 1:10, y = 11:20, z = letters[1:10],
                   stringsAsFactors = FALSE)
  snap <- tidyaudit:::.build_snapshot(df, label = "test", index = 1L,
                                       .cols_include = c("x", "z"))
  # Only x is numeric in the filtered schema
  expect_equal(snap$numeric_summary$column, "x")
})

test_that("non-existent column names warn and are excluded", {
  df <- data.frame(a = 1:3, b = 4:6)
  expect_warning(
    snap <- tidyaudit:::.build_snapshot(df, label = "test", index = 1L,
                                         .cols_include = c("a", "nonexistent")),
    "not found in data"
  )
  expect_equal(snap$schema$column, "a")
})

test_that("empty .cols_include warns", {
  df <- data.frame(a = 1:3, b = 4:6)
  expect_warning(
    snap <- tidyaudit:::.build_snapshot(df, label = "test", index = 1L,
                                         .cols_include = character(0)),
    "empty"
  )
  expect_equal(nrow(snap$schema), 0L)
  # Invariants still computed from full data
  expect_equal(snap$ncol, 2L)
  expect_equal(snap$nrow, 3L)
})

test_that("both .cols_include and .cols_exclude errors", {
  expect_error(
    tidyaudit:::.build_snapshot(mtcars, label = "test", index = 1L,
                                 .cols_include = "mpg", .cols_exclude = "cyl"),
    "cols_include.*cols_exclude"
  )
})

test_that(".numeric_summary must be logical", {
  expect_error(
    tidyaudit:::.build_snapshot(mtcars, label = "test", index = 1L,
                                 .numeric_summary = "yes"),
    "numeric_summary"
  )
})

test_that("controls field stored when non-default", {
  snap <- tidyaudit:::.build_snapshot(mtcars, label = "test", index = 1L,
                                       .numeric_summary = FALSE)
  expect_false(is.null(snap$controls))
  expect_false(snap$controls$numeric_summary)

  snap2 <- tidyaudit:::.build_snapshot(mtcars, label = "test", index = 1L,
                                        .cols_include = c("mpg", "cyl"))
  expect_false(is.null(snap2$controls))
  expect_equal(snap2$controls$cols_include, c("mpg", "cyl"))
})

test_that("controls field is NULL when all defaults", {
  snap <- tidyaudit:::.build_snapshot(mtcars, label = "test", index = 1L)
  expect_null(snap$controls)
})

test_that("print.audit_snap shows 'N of M' when schema is filtered", {
  snap <- tidyaudit:::.build_snapshot(mtcars, label = "test", index = 1L,
                                       .cols_include = c("mpg", "cyl"))
  output <- capture.output(print(snap), type = "message")
  combined <- paste(output, collapse = "\n")
  expect_true(grepl("2 of 11", combined))
})

test_that("print.audit_snap shows plain count when schema is unfiltered", {
  snap <- tidyaudit:::.build_snapshot(mtcars, label = "test", index = 1L)
  output <- capture.output(print(snap), type = "message")
  combined <- paste(output, collapse = "\n")
  expect_true(grepl("Columns \\(11\\)", combined))
  expect_false(grepl("of", combined))
})

test_that(".decompose_pipe handles simple symbol", {
  result <- tidyaudit:::.decompose_pipe(quote(df))
  expect_equal(result, "df")
})

test_that(".decompose_pipe handles nested calls", {
  # Simulates: df |> filter(x > 0) |> mutate(y = 1)
  # Which R parses as: mutate(filter(df, x > 0), y = 1)
  expr <- quote(mutate(filter(df, x > 0), y = 1))
  result <- tidyaudit:::.decompose_pipe(expr)

  expect_equal(length(result), 3L)
  expect_equal(result[1], "df")
  expect_equal(result[2], "filter(., x > 0)")
  expect_equal(result[3], "mutate(., y = 1)")
})

test_that(".capture_pipeline truncates long chains", {
  # Build a deeply nested expression (8 steps)
  expr <- quote(f8(f7(f6(f5(f4(f3(f2(f1(df)))))))))
  result <- tidyaudit:::.capture_pipeline(expr, .max_steps = 5L)

  # Should have "..." prefix + 5 steps
  expect_equal(result[1], "...")
  expect_equal(length(result), 6L)
})

test_that(".capture_pipeline returns short chains intact", {
  expr <- quote(filter(df, x > 0))
  result <- tidyaudit:::.capture_pipeline(expr, .max_steps = 5L)

  expect_equal(length(result), 2L)
  expect_equal(result[1], "df")
  expect_equal(result[2], "filter(., x > 0)")
})
