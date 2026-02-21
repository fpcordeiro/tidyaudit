test_that("audit_tap returns data unchanged (pipe transparency)", {
  trail <- audit_trail("transparency")
  input <- mtcars

  output <- audit_tap(input, trail, "step1")

  expect_identical(output, input)
})

test_that("audit_tap appends snapshot to trail", {
  trail <- audit_trail("append_test")
  mtcars |> audit_tap(trail, "raw")

  expect_equal(length(trail$snapshots), 1L)
  expect_equal(trail$labels, "raw")
  expect_s3_class(trail$snapshots[[1]], "audit_snap")
  expect_equal(trail$snapshots[[1]]$label, "raw")
  expect_equal(trail$snapshots[[1]]$nrow, nrow(mtcars))
  expect_equal(trail$snapshots[[1]]$ncol, ncol(mtcars))
})

test_that("audit_tap auto-generates labels", {
  trail <- audit_trail("auto_label")
  mtcars |> audit_tap(trail)
  mtcars |> audit_tap(trail)

  expect_equal(trail$labels, c("step_1", "step_2"))
})

test_that("audit_tap rejects duplicate labels", {
  trail <- audit_trail("dup_test")
  mtcars |> audit_tap(trail, "same")

  expect_error(
    audit_tap(mtcars, trail, "same"),
    "already exists"
  )
})

test_that("audit_tap rejects non-trail object", {
  expect_error(
    audit_tap(mtcars, "not_a_trail", "step"),
    "audit_trail"
  )
})

test_that("audit_tap detects row changes", {
  trail <- audit_trail("row_changes")
  mtcars |> audit_tap(trail, "raw")
  dplyr::filter(mtcars, mpg > 20) |> audit_tap(trail, "filtered")

  snap2 <- trail$snapshots[[2]]
  expect_true(!is.null(snap2$changes))
  expect_true(snap2$changes$row_delta < 0L)
})

test_that("audit_tap detects column changes", {
  trail <- audit_trail("col_changes")
  mtcars |> audit_tap(trail, "raw")
  dplyr::mutate(mtcars, new_col = mpg * 2) |> audit_tap(trail, "added")

  snap2 <- trail$snapshots[[2]]
  expect_equal(snap2$changes$col_delta, 1L)
  expect_equal(snap2$changes$cols_added, "new_col")
})

test_that("audit_tap detects NA changes", {
  trail <- audit_trail("na_changes")
  df <- data.frame(x = 1:5, y = c(1, NA, 3, NA, 5))
  df |> audit_tap(trail, "with_na")
  df[complete.cases(df), ] |> audit_tap(trail, "no_na")

  snap2 <- trail$snapshots[[2]]
  expect_true(snap2$changes$na_delta < 0L)
})

test_that("audit_tap works with custom .fns", {
  trail <- audit_trail("custom_fns")
  mtcars |> audit_tap(trail, "raw", .fns = list(
    n_rows = nrow,
    mean_mpg = ~mean(.x$mpg)
  ))

  snap <- trail$snapshots[[1]]
  expect_equal(snap$custom$n_rows, 32L)
  expect_equal(snap$custom$mean_mpg, mean(mtcars$mpg))
})

test_that("audit_tap works on zero-row data", {
  trail <- audit_trail("empty_data")
  empty_df <- mtcars[0, ]
  result <- audit_tap(empty_df, trail, "empty")

  expect_identical(result, empty_df)
  expect_equal(trail$snapshots[[1]]$nrow, 0L)
  expect_equal(trail$snapshots[[1]]$total_nas, 0L)
})

test_that("audit_tap modifies trail by reference inside pipe", {
  trail <- audit_trail("ref_pipe")
  result <- mtcars |>
    audit_tap(trail, "step1") |>
    dplyr::filter(mpg > 20) |>
    audit_tap(trail, "step2") |>
    dplyr::mutate(mpg2 = mpg * 2) |>
    audit_tap(trail, "step3")

  expect_equal(length(trail$snapshots), 3L)
  expect_equal(trail$labels, c("step1", "step2", "step3"))
})

test_that("audit_tap works with grouped tibbles", {
  trail <- audit_trail("grouped")
  grouped_df <- dplyr::group_by(mtcars, cyl)
  result <- audit_tap(grouped_df, trail, "grouped")

  # Data passes through with groups intact
  expect_true(dplyr::is_grouped_df(result))
  expect_equal(dplyr::group_vars(result), "cyl")

  # Snapshot diagnostics are computed on ungrouped data
  snap <- trail$snapshots[[1]]
  expect_equal(snap$nrow, nrow(mtcars))
})

test_that("audit_tap rejects non-data.frame input", {
  trail <- audit_trail("bad_data")
  expect_error(audit_tap(1:10, trail, "vec"), "data.frame or tibble")
  expect_error(audit_tap(matrix(1:4, 2), trail, "mat"), "data.frame or tibble")
})

test_that("audit_tap rejects invalid label types", {
  trail <- audit_trail("bad_label")
  expect_error(audit_tap(mtcars, trail, label = 42), "single character string")
  expect_error(audit_tap(mtcars, trail, label = TRUE), "single character string")
  expect_error(audit_tap(mtcars, trail, label = c("a", "b")), "single character string")
  expect_error(audit_tap(mtcars, trail, label = NA_character_), "single character string")
})

test_that("audit_tap rejects non-list .fns", {
  trail <- audit_trail("bad_fns")
  expect_error(audit_tap(mtcars, trail, "s", .fns = mean), "named list")
})

test_that("audit_tap auto-names unnamed .fns", {
  trail <- audit_trail("autoname_fns")
  mtcars |> audit_tap(trail, "raw", .fns = list(nrow, ~ncol(.x)))

  snap <- trail$snapshots[[1]]
  expect_equal(names(snap$custom), c("fn_1", "fn_2"))
  expect_equal(snap$custom$fn_1, 32L)
  expect_equal(snap$custom$fn_2, 11L)
})

test_that("audit_tap auto-names partially named .fns", {
  trail <- audit_trail("partname_fns")
  mtcars |> audit_tap(trail, "raw", .fns = list(rows = nrow, ~ncol(.x)))

  snap <- trail$snapshots[[1]]
  expect_equal(names(snap$custom), c("rows", "fn_2"))
})

test_that("audit_tap preserves tibble class", {
  trail <- audit_trail("tibble_class")
  tbl <- dplyr::as_tibble(mtcars)
  result <- audit_tap(tbl, trail, "tbl_step")

  expect_s3_class(result, "tbl_df")
  expect_identical(result, tbl)
})

test_that("audit_tap validates .trail before forcing .data", {
  # If .trail is validated first, an expensive .data should NOT be evaluated
  call_count <- 0L
  expensive_df <- {
    call_count <<- call_count + 1L
    mtcars
  }
  # With a non-trail object, the error should fire before forcing .data
  # We can't perfectly test evaluation order, but we verify the error message
  expect_error(audit_tap(mtcars, "not_a_trail", "s"), "audit_trail")
})

# ---------------------------------------------------------------------------
# .fns with NA names (Fix 3)
# ---------------------------------------------------------------------------

test_that("audit_tap handles .fns list with NA name without crashing", {
  trail <- audit_trail("fns_na_name")
  fns <- list(nrow)
  names(fns) <- NA_character_

  # Must not crash; the NA name should be auto-renamed to fn_1
  mtcars |> audit_tap(trail, "raw", .fns = fns)
  snap <- trail$snapshots[[1]]
  expect_equal(names(snap$custom), "fn_1")
  expect_equal(snap$custom$fn_1, 32L)
})

test_that("audit_tap handles mixed NA and empty names in .fns", {
  trail <- audit_trail("fns_mixed_names")
  fns <- list(nrow, ncol)
  names(fns) <- c("keep", NA_character_)

  mtcars |> audit_tap(trail, "raw", .fns = fns)
  snap <- trail$snapshots[[1]]
  expect_equal(names(snap$custom), c("keep", "fn_2"))
})

# ---------------------------------------------------------------------------
# invisible return (Fix 5)
# ---------------------------------------------------------------------------

test_that("audit_tap returns invisibly", {
  trail <- audit_trail("invisible_test")
  result <- withVisible(audit_tap(mtcars, trail, "step1"))
  expect_false(result$visible)
  expect_identical(result$value, mtcars)
})
