test_that(".build_snapshot captures correct dimensions", {
  snap <- tidyaudit:::.build_snapshot(mtcars, label = "test", index = 1L)

  expect_s3_class(snap, "audit_snap")
  expect_equal(snap$label, "test")
  expect_equal(snap$index, 1L)
  expect_equal(snap$nrow, 32L)
  expect_equal(snap$ncol, 11L)
  expect_equal(snap$type, "tap")
})

test_that(".build_snapshot captures column info", {
  df <- data.frame(x = 1:5, y = c("a", "b", "c", NA, "e"),
                   z = c(1.1, 2.2, NA, 4.4, 5.5),
                   stringsAsFactors = FALSE)
  snap <- tidyaudit:::.build_snapshot(df, label = "test", index = 1L)

  expect_equal(nrow(snap$col_info), 3L)
  expect_equal(snap$col_info$column, c("x", "y", "z"))
  expect_equal(snap$col_info$n_na, c(0L, 1L, 1L))
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
