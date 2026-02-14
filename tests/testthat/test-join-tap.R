# ---------------------------------------------------------------------------
# Test data
# ---------------------------------------------------------------------------

orders <- data.frame(
  id     = c(1L, 2L, 3L, 4L),
  amount = c(100, 200, 300, 400),
  stringsAsFactors = FALSE
)

customers <- data.frame(
  id   = c(2L, 3L, 5L),
  name = c("Alice", "Bob", "Charlie"),
  stringsAsFactors = FALSE
)

# ---------------------------------------------------------------------------
# left_join_tap — basic with trail
# ---------------------------------------------------------------------------

test_that("left_join_tap produces correct join result", {
  trail <- audit_trail("lj_basic")
  result <- left_join_tap(orders, customers, by = "id",
                           .trail = trail, .label = "joined")

  expected <- dplyr::left_join(orders, customers, by = "id")
  expect_equal(result, expected)
})

test_that("left_join_tap records enriched snapshot in trail", {
  trail <- audit_trail("lj_enriched")
  result <- left_join_tap(orders, customers, by = "id",
                           .trail = trail, .label = "joined")

  expect_equal(length(trail$snapshots), 1L)
  expect_equal(trail$labels, "joined")

  snap <- trail$snapshots[[1]]
  expect_s3_class(snap, "audit_snap")
  expect_equal(snap$type, "join")
  expect_equal(snap$label, "joined")

  diag <- snap$diagnostics
  expect_equal(diag$join_type, "left_join")
  expect_equal(diag$relation, "one-to-one")
  expect_true(!is.null(diag$match_rate$x))
  expect_true(!is.null(diag$match_rate$y))
})

test_that("left_join_tap with stat tracks stat column", {
  # stat requires the column to exist in both tables
  x_stat <- data.frame(id = 1:4, amount = c(100, 200, 300, 400))
  y_stat <- data.frame(id = c(2L, 3L, 5L), amount = c(50, 75, 100))
  trail <- audit_trail("lj_stat")
  result <- left_join_tap(x_stat, y_stat, by = "id",
                           .trail = trail, .label = "joined",
                           .stat = "amount")

  snap <- trail$snapshots[[1]]
  expect_true(!is.null(snap$diagnostics$stat))
})

# ---------------------------------------------------------------------------
# All six join types
# ---------------------------------------------------------------------------

test_that("right_join_tap produces correct result and enriched snapshot", {
  trail <- audit_trail("rj")
  result <- right_join_tap(orders, customers, by = "id",
                            .trail = trail, .label = "rj")
  expected <- dplyr::right_join(orders, customers, by = "id")
  expect_equal(result, expected)
  expect_equal(trail$snapshots[[1]]$diagnostics$join_type, "right_join")
})

test_that("inner_join_tap produces correct result and enriched snapshot", {
  trail <- audit_trail("ij")
  result <- inner_join_tap(orders, customers, by = "id",
                            .trail = trail, .label = "ij")
  expected <- dplyr::inner_join(orders, customers, by = "id")
  expect_equal(result, expected)
  expect_equal(trail$snapshots[[1]]$diagnostics$join_type, "inner_join")
})

test_that("full_join_tap produces correct result and enriched snapshot", {
  trail <- audit_trail("fj")
  result <- full_join_tap(orders, customers, by = "id",
                           .trail = trail, .label = "fj")
  expected <- dplyr::full_join(orders, customers, by = "id")
  expect_equal(result, expected)
  expect_equal(trail$snapshots[[1]]$diagnostics$join_type, "full_join")
})

test_that("anti_join_tap produces correct result and enriched snapshot", {
  trail <- audit_trail("aj")
  result <- anti_join_tap(orders, customers, by = "id",
                           .trail = trail, .label = "aj")
  expected <- dplyr::anti_join(orders, customers, by = "id")
  expect_equal(result, expected)
  expect_equal(trail$snapshots[[1]]$diagnostics$join_type, "anti_join")
})

test_that("semi_join_tap produces correct result and enriched snapshot", {
  trail <- audit_trail("sj")
  result <- semi_join_tap(orders, customers, by = "id",
                           .trail = trail, .label = "sj")
  expected <- dplyr::semi_join(orders, customers, by = "id")
  expect_equal(result, expected)
  expect_equal(trail$snapshots[[1]]$diagnostics$join_type, "semi_join")
})

# ---------------------------------------------------------------------------
# inner_join_tap row loss
# ---------------------------------------------------------------------------

test_that("inner_join_tap captures partial overlap diagnostics", {
  trail <- audit_trail("ij_partial")
  result <- inner_join_tap(orders, customers, by = "id",
                            .trail = trail, .label = "inner")
  diag <- trail$snapshots[[1]]$diagnostics
  expect_true(diag$n_only_x > 0)
  expect_true(diag$n_only_y > 0)
})

# ---------------------------------------------------------------------------
# Passthrough of dplyr join args via ...
# ---------------------------------------------------------------------------

test_that("join_tap passes suffix and other args through", {
  trail <- audit_trail("suffix_test")
  x <- data.frame(id = 1:3, value = 10:12)
  y <- data.frame(id = 2:4, value = 20:22)
  result <- left_join_tap(x, y, by = "id", suffix = c("_left", "_right"),
                           .trail = trail, .label = "sfx")
  expect_true("value_left" %in% names(result))
  expect_true("value_right" %in% names(result))
})

# ---------------------------------------------------------------------------
# Join tap without trail
# ---------------------------------------------------------------------------

test_that("join_tap without trail performs plain dplyr join", {
  result <- left_join_tap(orders, customers, by = "id")
  expected <- dplyr::left_join(orders, customers, by = "id")
  expect_equal(result, expected)
})

test_that("join_tap without trail + .stat prints diagnostics", {
  # stat requires the column in both tables
  x_stat <- data.frame(id = 1:4, amount = c(100, 200, 300, 400))
  y_stat <- data.frame(id = c(2L, 3L, 5L), amount = c(50, 75, 100))
  output <- capture.output(
    result <- left_join_tap(x_stat, y_stat, by = "id", .stat = "amount"),
    type = "message"
  )
  expected <- dplyr::left_join(x_stat, y_stat, by = "id")
  expect_equal(result, expected)
  # validate_join output should appear
  expect_true(any(grepl("Join Validation|Relationship|Match rate", output)))
})

test_that("join_tap without trail + .label warns", {
  expect_warning(
    left_join_tap(orders, customers, by = "id", .label = "foo"),
    "ignored"
  )
})

# ---------------------------------------------------------------------------
# join_by() equality — enriched diagnostics
# ---------------------------------------------------------------------------

test_that("join_tap with join_by() equality produces enriched diagnostics", {
  trail <- audit_trail("jb_eq")
  result <- left_join_tap(orders, customers,
                           by = dplyr::join_by(id),
                           .trail = trail, .label = "jb")
  diag <- trail$snapshots[[1]]$diagnostics
  expect_equal(diag$join_type, "left_join")
  expect_true(!is.null(diag$relation))
  expect_true(!is.null(diag$match_rate$x))
})

test_that("join_tap with join_by(a == b) produces enriched diagnostics", {
  x <- data.frame(key_a = 1:3, val = 10:12)
  y <- data.frame(key_b = 2:4, score = 20:22)
  trail <- audit_trail("jb_eq2")
  result <- left_join_tap(x, y,
                           by = dplyr::join_by(key_a == key_b),
                           .trail = trail, .label = "jb2")
  diag <- trail$snapshots[[1]]$diagnostics
  expect_true(!is.null(diag$relation))
})

# ---------------------------------------------------------------------------
# join_by() non-equi — basic snapshot only
# ---------------------------------------------------------------------------

test_that("join_tap with non-equi join_by produces basic snapshot", {
  x <- data.frame(id = 1:3, start = c(1, 5, 10))
  y <- data.frame(id = 1:3, threshold = c(2, 4, 8))
  trail <- audit_trail("jb_neq")
  result <- left_join_tap(x, y,
                           by = dplyr::join_by(id, start >= threshold),
                           .trail = trail, .label = "neq")
  snap <- trail$snapshots[[1]]
  expect_equal(snap$type, "join")
  diag <- snap$diagnostics
  expect_equal(diag$join_type, "left_join")
  # No relation or match_rate for non-equi
  expect_null(diag$relation)
  expect_null(diag$match_rate)
})

# ---------------------------------------------------------------------------
# Label management
# ---------------------------------------------------------------------------

test_that("join_tap auto-generates labels", {
  trail <- audit_trail("auto_lbl")
  left_join_tap(orders, customers, by = "id", .trail = trail)
  expect_equal(trail$labels, "left_join_1")
})

test_that("join_tap rejects duplicate labels", {
  trail <- audit_trail("dup_lbl")
  left_join_tap(orders, customers, by = "id", .trail = trail, .label = "j")
  expect_error(
    left_join_tap(orders, customers, by = "id", .trail = trail, .label = "j"),
    "already exists"
  )
})

test_that("join_tap rejects invalid trail", {
  expect_error(
    left_join_tap(orders, customers, by = "id", .trail = "not_a_trail"),
    "audit_trail"
  )
})

# ---------------------------------------------------------------------------
# Change detection
# ---------------------------------------------------------------------------

test_that("join_tap computes changes from previous snapshot", {
  trail <- audit_trail("changes")
  orders |> audit_tap(trail, "raw")
  left_join_tap(orders, customers, by = "id", .trail = trail, .label = "joined")

  snap2 <- trail$snapshots[[2]]
  expect_true(!is.null(snap2$changes))
  expect_true(snap2$changes$col_delta > 0L)  # join adds columns
})

# ---------------------------------------------------------------------------
# Trail timeline with mixed types
# ---------------------------------------------------------------------------

test_that("mixed pipeline with audit_tap + join_tap + filter_tap renders correctly", {
  trail <- audit_trail("mixed")
  result <- orders |>
    audit_tap(trail, "raw") |>
    left_join_tap(customers, by = "id", .trail = trail, .label = "joined") |>
    dplyr::filter(amount > 100) |>
    audit_tap(trail, "filtered")

  expect_equal(length(trail$snapshots), 3L)
  expect_equal(trail$snapshots[[1]]$type, "tap")
  expect_equal(trail$snapshots[[2]]$type, "join")
  expect_equal(trail$snapshots[[3]]$type, "tap")

  # Print should work without error
  output <- capture.output(print(trail), type = "message")
  expect_true(any(grepl("left_join", output)))
})

# ---------------------------------------------------------------------------
# audit_diff across typed snapshots
# ---------------------------------------------------------------------------

test_that("audit_diff works across join and tap snapshots", {
  trail <- audit_trail("diff_typed")
  orders |> audit_tap(trail, "raw")
  left_join_tap(orders, customers, by = "id", .trail = trail, .label = "joined")

  diff_obj <- audit_diff(trail, "raw", "joined")
  expect_s3_class(diff_obj, "audit_diff")
  expect_true(diff_obj$col_delta > 0L)
})

# ---------------------------------------------------------------------------
# Pipeline capture in join_tap
# ---------------------------------------------------------------------------

test_that("join_tap captures pipeline", {
  trail <- audit_trail("pipeline")
  result <- orders |>
    dplyr::mutate(x = 1) |>
    left_join_tap(customers, by = "id", .trail = trail, .label = "j")

  snap <- trail$snapshots[[1]]
  expect_true(!is.null(snap$pipeline))
  expect_true(length(snap$pipeline) > 0L)
})

# ---------------------------------------------------------------------------
# Grouped tibble handling
# ---------------------------------------------------------------------------

test_that("join_tap preserves grouping on left table", {
  grouped_orders <- dplyr::group_by(orders, id)
  trail <- audit_trail("grouped")
  result <- left_join_tap(grouped_orders, customers, by = "id",
                           .trail = trail, .label = "gj")

  # Result should be a grouped tibble (dplyr preserves groups on left)
  expect_true(dplyr::is_grouped_df(result))
})

# ---------------------------------------------------------------------------
# Named by vector (different column names)
# ---------------------------------------------------------------------------

test_that("join_tap works with named by vector", {
  x <- data.frame(key_x = 1:3, val = 10:12)
  y <- data.frame(key_y = 2:4, score = 20:22)
  trail <- audit_trail("named_by")
  result <- left_join_tap(x, y, by = c("key_x" = "key_y"),
                           .trail = trail, .label = "nb")
  expected <- dplyr::left_join(x, y, by = c("key_x" = "key_y"))
  expect_equal(result, expected)

  diag <- trail$snapshots[[1]]$diagnostics
  expect_true(!is.null(diag$relation))
})

# ---------------------------------------------------------------------------
# Duplicate key detection
# ---------------------------------------------------------------------------

test_that("join_tap detects duplicate keys", {
  x <- data.frame(id = c(1L, 2L, 2L, 3L), val = 1:4)
  y <- data.frame(id = c(2L, 3L), score = c(10, 20))
  trail <- audit_trail("dups")
  result <- left_join_tap(x, y, by = "id", .trail = trail, .label = "d")

  diag <- trail$snapshots[[1]]$diagnostics
  expect_true(diag$x_has_dups)
  expect_false(diag$y_has_dups)
  expect_equal(diag$relation, "many-to-one")
})

# ---------------------------------------------------------------------------
# .normalize_by internal helper
# ---------------------------------------------------------------------------

test_that(".normalize_by handles character vectors", {
  expect_equal(tidyaudit:::.normalize_by("id"), "id")
  expect_equal(tidyaudit:::.normalize_by(c("a" = "b")), c("a" = "b"))
  expect_equal(tidyaudit:::.normalize_by(c("x", "y")), c("x", "y"))
})

test_that(".normalize_by handles NULL", {
  expect_null(tidyaudit:::.normalize_by(NULL))
})

test_that(".normalize_by handles equality join_by", {
  jb <- dplyr::join_by(id)
  result <- tidyaudit:::.normalize_by(jb)
  expect_equal(result, c("id" = "id"))

  jb2 <- dplyr::join_by(a == b)
  result2 <- tidyaudit:::.normalize_by(jb2)
  expect_equal(result2, c("a" = "b"))
})

test_that(".normalize_by returns NULL for non-equi join_by", {
  jb <- dplyr::join_by(x >= y)
  expect_null(tidyaudit:::.normalize_by(jb))
})

test_that(".normalize_by returns NULL for unknown types", {
  expect_null(tidyaudit:::.normalize_by(42))
  expect_null(tidyaudit:::.normalize_by(list(a = 1)))
})

# ---------------------------------------------------------------------------
# Multiple snapshots via pipe chain
# ---------------------------------------------------------------------------

test_that("multiple join_tap calls in a pipe chain work correctly", {
  lookup_a <- data.frame(id = 1:3, a_val = c("x", "y", "z"))
  lookup_b <- data.frame(id = 2:4, b_val = c(10, 20, 30))

  trail <- audit_trail("chain")
  result <- orders |>
    audit_tap(trail, "raw") |>
    left_join_tap(lookup_a, by = "id", .trail = trail, .label = "join_a") |>
    left_join_tap(lookup_b, by = "id", .trail = trail, .label = "join_b")

  expect_equal(length(trail$snapshots), 3L)
  expect_equal(trail$labels, c("raw", "join_a", "join_b"))

  # Verify correct ordering
  expect_equal(trail$snapshots[[1]]$index, 1L)
  expect_equal(trail$snapshots[[2]]$index, 2L)
  expect_equal(trail$snapshots[[3]]$index, 3L)

  # Changes should be computed
  expect_true(!is.null(trail$snapshots[[2]]$changes))
  expect_true(!is.null(trail$snapshots[[3]]$changes))
})

# ---------------------------------------------------------------------------
# Zero-row tables
# ---------------------------------------------------------------------------

test_that("join_tap handles zero-row tables", {
  empty <- orders[0, ]
  trail <- audit_trail("empty_join")
  result <- left_join_tap(empty, customers, by = "id",
                           .trail = trail, .label = "ej")
  expect_equal(nrow(result), 0L)
  expect_equal(trail$snapshots[[1]]$nrow, 0L)
})

# ---------------------------------------------------------------------------
# y_name capture
# ---------------------------------------------------------------------------

test_that("join_tap captures y_name in diagnostics", {
  trail <- audit_trail("yname")
  left_join_tap(orders, customers, by = "id", .trail = trail, .label = "yn")
  expect_equal(trail$snapshots[[1]]$diagnostics$y_name, "customers")
})

# ---------------------------------------------------------------------------
# .data validation
# ---------------------------------------------------------------------------

test_that("join_tap rejects non-data.frame input", {
  trail <- audit_trail("bad_data")
  expect_error(
    left_join_tap(list(a = 1), customers, by = "id", .trail = trail),
    "data.frame"
  )
  expect_error(
    left_join_tap(42, customers, by = "id", .trail = trail),
    "data.frame"
  )
})

# ---------------------------------------------------------------------------
# Label validation
# ---------------------------------------------------------------------------

test_that("join_tap rejects NA label", {
  trail <- audit_trail("na_label")
  expect_error(
    left_join_tap(orders, customers, by = "id",
                   .trail = trail, .label = NA_character_),
    "single character string"
  )
})

test_that("join_tap rejects multi-element label", {
  trail <- audit_trail("multi_label")
  expect_error(
    left_join_tap(orders, customers, by = "id",
                   .trail = trail, .label = c("a", "b")),
    "single character string"
  )
})

# ---------------------------------------------------------------------------
# Natural join (by = NULL)
# ---------------------------------------------------------------------------

test_that("join_tap with by = NULL performs natural join with basic snapshot", {
  trail <- audit_trail("natural_join")
  result <- left_join_tap(orders, customers, .trail = trail, .label = "nj")

  expected <- suppressMessages(dplyr::left_join(orders, customers, by = "id"))
  expect_equal(result, expected)

  # Natural join => by is NULL => no enriched diagnostics
  snap <- trail$snapshots[[1]]
  expect_equal(snap$type, "join")
  diag <- snap$diagnostics
  expect_equal(diag$join_type, "left_join")
  expect_null(diag$relation)
  expect_null(diag$match_rate)
})

# ---------------------------------------------------------------------------
# validate_join error handling (no-trail mode)
# ---------------------------------------------------------------------------

test_that("join_tap no-trail warns when validate_join fails", {
  # .stat referencing a column that doesn't exist in one table
  expect_warning(
    result <- left_join_tap(orders, customers, by = "id",
                             .stat = "nonexistent_col"),
    "validate_join.*failed"
  )
  # Join itself should still succeed
  expected <- dplyr::left_join(orders, customers, by = "id")
  expect_equal(result, expected)
})

# ---------------------------------------------------------------------------
# validate_join error handling (with trail)
# ---------------------------------------------------------------------------

test_that("join_tap with trail degrades gracefully when validate_join fails", {
  trail <- audit_trail("vj_fail")
  # .stat referencing a column that doesn't exist
  result <- left_join_tap(orders, customers, by = "id",
                           .trail = trail, .label = "graceful",
                           .stat = "nonexistent_col")

  # Join result should be correct
  expected <- dplyr::left_join(orders, customers, by = "id")
  expect_equal(result, expected)

  # Snapshot should have basic diagnostics (no enriched)
  snap <- trail$snapshots[[1]]
  expect_equal(snap$type, "join")
  expect_equal(snap$diagnostics$join_type, "left_join")
  expect_null(snap$diagnostics$relation)
})
