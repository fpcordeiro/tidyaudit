# ---------------------------------------------------------------------------
# Test data
# ---------------------------------------------------------------------------

df <- data.frame(
  id     = 1:10,
  amount = c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000),
  flag   = rep(c(TRUE, FALSE), 5),
  stringsAsFactors = FALSE
)

# ---------------------------------------------------------------------------
# filter_tap — basic with trail
# ---------------------------------------------------------------------------

test_that("filter_tap produces correct filter result", {
  trail <- audit_trail("ft_basic")
  result <- filter_tap(df, amount > 300, .trail = trail, .label = "big")

  expected <- dplyr::filter(df, amount > 300)
  expect_equal(result, expected)
})

test_that("filter_tap records enriched snapshot in trail", {
  trail <- audit_trail("ft_enriched")
  suppressMessages(
    result <- filter_tap(df, amount > 300, .trail = trail, .label = "big")
  )

  expect_equal(length(trail$snapshots), 1L)
  expect_equal(trail$labels, "big")

  snap <- trail$snapshots[[1]]
  expect_s3_class(snap, "audit_snap")
  expect_equal(snap$type, "filter")
  expect_equal(snap$label, "big")

  diag <- snap$diagnostics
  expect_equal(diag$filter_type, "keep")
  expect_equal(diag$n_dropped, 3L)
  expect_equal(diag$n_total, 10L)
  expect_equal(diag$pct_dropped, 30)
  expect_equal(diag$expr_label, "amount > 300")
})

test_that("filter_tap with stat tracks stat column", {
  trail <- audit_trail("ft_stat")
  suppressMessages(
    result <- filter_tap(df, amount > 500, .trail = trail, .label = "big",
                          .stat = amount)
  )

  diag <- trail$snapshots[[1]]$diagnostics
  expect_equal(diag$stat_col, "amount")
  expect_true(!is.null(diag$stat_dropped))
  expect_true(!is.null(diag$stat_total))
  expect_equal(diag$stat_total, sum(df$amount))
  expect_equal(diag$stat_dropped, sum(df$amount[df$amount <= 500]))
})

# ---------------------------------------------------------------------------
# filter_out_tap — inverse logic
# ---------------------------------------------------------------------------

test_that("filter_out_tap drops matching rows", {
  trail <- audit_trail("fot_basic")
  suppressMessages(
    result <- filter_out_tap(df, flag == FALSE,
                              .trail = trail, .label = "flagged")
  )

  expected <- dplyr::filter_out(df, flag == FALSE)
  expect_equal(result, expected)
})

test_that("filter_out_tap records enriched snapshot", {
  trail <- audit_trail("fot_enriched")
  suppressMessages(
    result <- filter_out_tap(df, amount > 500,
                              .trail = trail, .label = "small_only")
  )

  snap <- trail$snapshots[[1]]
  expect_equal(snap$type, "filter")

  diag <- snap$diagnostics
  expect_equal(diag$filter_type, "drop")
  expect_equal(diag$n_dropped, 5L)  # 600-1000 dropped
  expect_equal(diag$n_total, 10L)
  expect_equal(diag$pct_dropped, 50)
})

test_that("filter_out_tap with stat tracks dropped stat", {
  trail <- audit_trail("fot_stat")
  suppressMessages(
    result <- filter_out_tap(df, amount > 500,
                              .trail = trail, .label = "low",
                              .stat = amount)
  )

  diag <- trail$snapshots[[1]]$diagnostics
  expect_equal(diag$stat_total, sum(df$amount))
  expect_equal(diag$stat_dropped, sum(df$amount[df$amount > 500]))
})

# ---------------------------------------------------------------------------
# NULL trail — plain filter
# ---------------------------------------------------------------------------

test_that("filter_tap without trail performs plain dplyr::filter", {
  result <- filter_tap(df, amount > 300)
  expected <- dplyr::filter(df, amount > 300)
  expect_equal(result, expected)
})

test_that("filter_out_tap without trail performs plain dplyr::filter_out", {
  result <- filter_out_tap(df, amount > 300)
  expected <- dplyr::filter_out(df, amount > 300)
  expect_equal(result, expected)
})

# ---------------------------------------------------------------------------
# NULL trail + diagnostic args → delegates to filter_keep/filter_drop
# ---------------------------------------------------------------------------

test_that("filter_tap without trail + .stat delegates to filter_keep", {
  output <- capture.output(
    result <- filter_tap(df, amount > 300, .stat = amount),
    type = "message"
  )
  expected <- dplyr::filter(df, amount > 300)
  expect_equal(result, expected)
  expect_true(any(grepl("Dropped|filter_keep", output)))
})

test_that("filter_out_tap without trail + .stat delegates to filter_drop", {
  output <- capture.output(
    result <- filter_out_tap(df, amount > 300, .stat = amount),
    type = "message"
  )
  expected <- dplyr::filter_out(df, amount > 300)
  expect_equal(result, expected)
  expect_true(any(grepl("Dropped|filter_drop", output)))
})

# ---------------------------------------------------------------------------
# NULL trail + .label → warns
# ---------------------------------------------------------------------------

test_that("filter_tap without trail + .label warns", {
  expect_warning(
    filter_tap(df, amount > 300, .label = "foo"),
    "ignored"
  )
})

test_that("filter_out_tap without trail + .label warns", {
  expect_warning(
    filter_out_tap(df, amount > 300, .label = "bar"),
    "ignored"
  )
})

# ---------------------------------------------------------------------------
# .warn_threshold
# ---------------------------------------------------------------------------

test_that("filter_tap warns when threshold exceeded", {
  trail <- audit_trail("ft_warn")
  expect_warning(
    suppressMessages(
      filter_tap(df, amount > 200, .trail = trail, .label = "w",
                  .warn_threshold = 0.1)
    ),
    "exceeds threshold"
  )
})

test_that("filter_tap does not warn when threshold not exceeded", {
  trail <- audit_trail("ft_nowarn")
  expect_no_warning(
    suppressMessages(
      filter_tap(df, amount > 900, .trail = trail, .label = "nw",
                  .warn_threshold = 0.95)
    )
  )
})

# ---------------------------------------------------------------------------
# .quiet
# ---------------------------------------------------------------------------

test_that("filter_tap .quiet suppresses diagnostic output", {
  trail <- audit_trail("ft_quiet")
  output <- capture.output(
    result <- filter_tap(df, amount > 300, .trail = trail, .label = "q",
                          .quiet = TRUE),
    type = "message"
  )
  # Only trail-level output should be absent
  expect_false(any(grepl("filter_tap:", output)))
})

# ---------------------------------------------------------------------------
# Label management
# ---------------------------------------------------------------------------

test_that("filter_tap auto-generates labels", {
  trail <- audit_trail("ft_autolbl")
  suppressMessages(
    filter_tap(df, amount > 300, .trail = trail)
  )
  expect_equal(trail$labels, "filter_1")
})

test_that("filter_out_tap auto-generates labels", {
  trail <- audit_trail("fot_autolbl")
  suppressMessages(
    filter_out_tap(df, amount > 300, .trail = trail)
  )
  expect_equal(trail$labels, "filter_out_1")
})

test_that("filter_tap rejects duplicate labels", {
  trail <- audit_trail("ft_dup")
  suppressMessages(
    filter_tap(df, amount > 300, .trail = trail, .label = "f")
  )
  expect_error(
    filter_tap(df, amount > 500, .trail = trail, .label = "f"),
    "already exists"
  )
})

test_that("filter_tap rejects invalid trail", {
  expect_error(
    filter_tap(df, amount > 300, .trail = "not_a_trail"),
    "audit_trail"
  )
})

# ---------------------------------------------------------------------------
# Change detection
# ---------------------------------------------------------------------------

test_that("filter_tap computes changes from previous snapshot", {
  trail <- audit_trail("ft_changes")
  df |> audit_tap(trail, "raw")
  suppressMessages(
    filter_tap(df, amount > 300, .trail = trail, .label = "filtered")
  )

  snap2 <- trail$snapshots[[2]]
  expect_true(!is.null(snap2$changes))
  expect_true(snap2$changes$row_delta < 0L)
})

# ---------------------------------------------------------------------------
# Pipeline capture
# ---------------------------------------------------------------------------

test_that("filter_tap captures pipeline", {
  trail <- audit_trail("ft_pipeline")
  suppressMessages(
    result <- df |>
      dplyr::mutate(x = 1) |>
      filter_tap(amount > 300, .trail = trail, .label = "f")
  )

  snap <- trail$snapshots[[1]]
  expect_true(!is.null(snap$pipeline))
  expect_true(length(snap$pipeline) > 0L)
})

# ---------------------------------------------------------------------------
# Mixed pipeline
# ---------------------------------------------------------------------------

test_that("mixed pipeline with audit_tap + join_tap + filter_tap works", {
  customers <- data.frame(id = c(1L, 5L, 7L), name = c("A", "B", "C"))

  trail <- audit_trail("mixed")
  suppressMessages(
    result <- df |>
      audit_tap(trail, "raw") |>
      left_join_tap(customers, by = "id", .trail = trail, .label = "joined") |>
      filter_tap(amount > 300, .trail = trail, .label = "big_only")
  )

  expect_equal(length(trail$snapshots), 3L)
  expect_equal(trail$snapshots[[1]]$type, "tap")
  expect_equal(trail$snapshots[[2]]$type, "join")
  expect_equal(trail$snapshots[[3]]$type, "filter")

  # Print should work
  output <- capture.output(print(trail), type = "message")
  expect_true(any(grepl("left_join", output)))
  expect_true(any(grepl("filter", output)))
})

# ---------------------------------------------------------------------------
# audit_diff across filter snapshots
# ---------------------------------------------------------------------------

test_that("audit_diff works between tap and filter snapshots", {
  trail <- audit_trail("diff_filter")
  df |> audit_tap(trail, "raw")
  suppressMessages(
    filter_tap(df, amount > 500, .trail = trail, .label = "big")
  )

  diff_obj <- audit_diff(trail, "raw", "big")
  expect_s3_class(diff_obj, "audit_diff")
  expect_true(diff_obj$row_delta < 0L)
})

# ---------------------------------------------------------------------------
# Multiple conditions
# ---------------------------------------------------------------------------

test_that("filter_tap handles multiple conditions", {
  trail <- audit_trail("ft_multi")
  suppressMessages(
    result <- filter_tap(df, amount > 300, flag == TRUE,
                          .trail = trail, .label = "multi")
  )

  expected <- dplyr::filter(df, amount > 300, flag == TRUE)
  expect_equal(result, expected)

  diag <- trail$snapshots[[1]]$diagnostics
  expect_true(grepl("&", diag$expr_label))
})

# ---------------------------------------------------------------------------
# n_na_in_filter
# ---------------------------------------------------------------------------

test_that("filter_tap counts NAs in filter condition", {
  df_na <- data.frame(
    id = 1:6,
    val = c(10, NA, 30, NA, 50, 60)
  )
  trail <- audit_trail("ft_na")
  suppressMessages(
    result <- filter_tap(df_na, val > 20, .trail = trail, .label = "naf")
  )

  diag <- trail$snapshots[[1]]$diagnostics
  expect_equal(diag$n_na_in_filter, 2L)
})

# ---------------------------------------------------------------------------
# Zero-row data
# ---------------------------------------------------------------------------

test_that("filter_tap handles zero-row data", {
  empty <- df[0, ]
  trail <- audit_trail("ft_empty")
  suppressMessages(
    result <- filter_tap(empty, amount > 300, .trail = trail, .label = "e")
  )
  expect_equal(nrow(result), 0L)
  expect_equal(trail$snapshots[[1]]$diagnostics$n_dropped, 0L)
  expect_equal(trail$snapshots[[1]]$diagnostics$pct_dropped, 0)
})

# ---------------------------------------------------------------------------
# Filter that drops all rows
# ---------------------------------------------------------------------------

test_that("filter_tap handles condition that drops all rows", {
  trail <- audit_trail("ft_all")
  suppressMessages(
    result <- filter_tap(df, amount > 99999, .trail = trail, .label = "none")
  )
  expect_equal(nrow(result), 0L)
  expect_equal(trail$snapshots[[1]]$diagnostics$n_dropped, 10L)
  expect_equal(trail$snapshots[[1]]$diagnostics$pct_dropped, 100)
})

# ---------------------------------------------------------------------------
# Grouped tibble
# ---------------------------------------------------------------------------

test_that("filter_tap preserves grouping", {
  grouped <- dplyr::group_by(df, flag)
  trail <- audit_trail("ft_grouped")
  suppressMessages(
    result <- filter_tap(grouped, amount > 300, .trail = trail, .label = "g")
  )
  expect_true(dplyr::is_grouped_df(result))
  expect_equal(dplyr::group_vars(result), "flag")
})

# ---------------------------------------------------------------------------
# Nested pipe chain
# ---------------------------------------------------------------------------

test_that("filter_tap and filter_out_tap work in nested pipes", {
  trail <- audit_trail("ft_nested")
  suppressMessages(
    result <- df |>
      audit_tap(trail, "raw") |>
      filter_tap(amount > 200, .trail = trail, .label = "f1") |>
      filter_out_tap(flag == FALSE, .trail = trail, .label = "f2")
  )

  expect_equal(length(trail$snapshots), 3L)
  expect_equal(trail$labels, c("raw", "f1", "f2"))
  # Correct ordering
  expect_equal(trail$snapshots[[1]]$index, 1L)
  expect_equal(trail$snapshots[[2]]$index, 2L)
  expect_equal(trail$snapshots[[3]]$index, 3L)
})

# ---------------------------------------------------------------------------
# print.audit_trail Type column rendering
# ---------------------------------------------------------------------------

test_that("print.audit_trail renders Type column for filter snapshots", {
  trail <- audit_trail("type_col")
  df |> audit_tap(trail, "raw")
  suppressMessages(
    filter_tap(df, amount > 300, .trail = trail, .label = "f")
  )
  output <- capture.output(print(trail), type = "message")
  # Should show filter info in the Type column
  expect_true(any(grepl("filter.*dropped.*rows", output)))
})

# ---------------------------------------------------------------------------
# .data validation
# ---------------------------------------------------------------------------

test_that("filter_tap rejects non-data.frame input", {
  trail <- audit_trail("bad_data")
  expect_error(
    filter_tap(list(a = 1), a > 1, .trail = trail),
    "data.frame"
  )
})

test_that("filter_out_tap rejects non-data.frame input", {
  trail <- audit_trail("bad_data2")
  expect_error(
    filter_out_tap(42, a > 1, .trail = trail),
    "data.frame"
  )
})

# ---------------------------------------------------------------------------
# Label validation
# ---------------------------------------------------------------------------

test_that("filter_tap rejects NA label", {
  trail <- audit_trail("na_label")
  expect_error(
    filter_tap(df, amount > 300, .trail = trail, .label = NA_character_),
    "single character string"
  )
})

test_that("filter_tap rejects multi-element label", {
  trail <- audit_trail("multi_label")
  expect_error(
    filter_tap(df, amount > 300, .trail = trail, .label = c("a", "b")),
    "single character string"
  )
})

test_that("filter_out_tap rejects NA label", {
  trail <- audit_trail("na_label2")
  expect_error(
    filter_out_tap(df, amount > 300, .trail = trail, .label = NA_character_),
    "single character string"
  )
})

# ---------------------------------------------------------------------------
# filter_out_tap with .warn_threshold
# ---------------------------------------------------------------------------

test_that("filter_out_tap warns when threshold exceeded", {
  trail <- audit_trail("fot_warn")
  expect_warning(
    suppressMessages(
      filter_out_tap(df, amount > 200, .trail = trail, .label = "w",
                      .warn_threshold = 0.1)
    ),
    "exceeds threshold"
  )
})

test_that("filter_out_tap does not warn when threshold not exceeded", {
  trail <- audit_trail("fot_nowarn")
  expect_no_warning(
    suppressMessages(
      filter_out_tap(df, amount > 900, .trail = trail, .label = "nw",
                      .warn_threshold = 0.95)
    )
  )
})

# ---------------------------------------------------------------------------
# filter_out_tap with .quiet
# ---------------------------------------------------------------------------

test_that("filter_out_tap .quiet suppresses diagnostic output", {
  trail <- audit_trail("fot_quiet")
  output <- capture.output(
    result <- filter_out_tap(df, amount > 300, .trail = trail, .label = "q",
                              .quiet = TRUE),
    type = "message"
  )
  expect_false(any(grepl("filter_out_tap:", output)))
})

# ---------------------------------------------------------------------------
# Zero conditions with trail
# ---------------------------------------------------------------------------

test_that("filter_tap with zero conditions and trail records no-op snapshot", {
  trail <- audit_trail("ft_zero")
  df |> audit_tap(trail, "raw")
  suppressMessages(
    result <- filter_tap(df, .trail = trail, .label = "noop")
  )
  expect_equal(result, df)
  expect_equal(length(trail$snapshots), 2L)
  diag <- trail$snapshots[[2]]$diagnostics
  expect_equal(diag$n_dropped, 0L)
  expect_equal(diag$pct_dropped, 0)
})

test_that("filter_out_tap with zero conditions and trail records no-op snapshot", {
  trail <- audit_trail("fot_zero")
  df |> audit_tap(trail, "raw")
  suppressMessages(
    result <- filter_out_tap(df, .trail = trail, .label = "noop")
  )
  expect_equal(result, df)
  expect_equal(length(trail$snapshots), 2L)
  diag <- trail$snapshots[[2]]$diagnostics
  expect_equal(diag$n_dropped, 0L)
})

# ---------------------------------------------------------------------------
# audit_report with mixed snapshot types
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# .warn_threshold input validation in filter_tap / filter_out_tap (Fix 1)
# ---------------------------------------------------------------------------

test_that("filter_tap rejects NA_real_ .warn_threshold", {
  trail <- audit_trail("wt_na1")
  expect_error(
    filter_tap(df, amount > 300, .trail = trail, .warn_threshold = NA_real_),
    "warn_threshold"
  )
})

test_that("filter_tap rejects length-2 .warn_threshold", {
  trail <- audit_trail("wt_len")
  expect_error(
    filter_tap(df, amount > 300, .trail = trail, .warn_threshold = c(0.1, 0.2)),
    "warn_threshold"
  )
})

test_that("filter_tap rejects character .warn_threshold", {
  trail <- audit_trail("wt_char")
  expect_error(
    filter_tap(df, amount > 300, .trail = trail, .warn_threshold = "0.5"),
    "warn_threshold"
  )
})

test_that("filter_out_tap rejects NA_real_ .warn_threshold", {
  trail <- audit_trail("wt_na2")
  expect_error(
    filter_out_tap(df, amount > 300, .trail = trail, .warn_threshold = NA_real_),
    "warn_threshold"
  )
})

test_that("filter_tap rejects invalid .warn_threshold without trail too", {
  # NULL trail delegation path also validates
  expect_error(
    filter_tap(df, amount > 300, .warn_threshold = NA_real_),
    "warn_threshold"
  )
})

test_that("audit_report works with mixed snapshot types (tap + join + filter)", {
  customers <- data.frame(id = c(1L, 5L, 7L), name = c("A", "B", "C"))

  trail <- audit_trail("report_mixed")
  suppressMessages({
    df |>
      audit_tap(trail, "raw") |>
      left_join_tap(customers, by = "id", .trail = trail, .label = "joined") |>
      filter_tap(amount > 300, .trail = trail, .label = "big")
  })

  expect_equal(length(trail$snapshots), 3L)

  # audit_report should work without error
  output <- capture.output(audit_report(trail), type = "message")
  expect_true(any(grepl("Audit Report", output)))
  expect_true(any(grepl("left_join", output)))
  expect_true(any(grepl("filter", output)))
  expect_true(any(grepl("Detailed Diffs", output)))
})
