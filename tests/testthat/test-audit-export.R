# Shared fixture: 3-snapshot trail with join, filter, and custom diagnostics
make_export_trail <- function() {
  trail   <- audit_trail("export_test")
  orders  <- data.frame(id = 1:4, amount = c(100, 200, 300, 400))
  lookup  <- data.frame(id = c(2L, 3L, 5L), name = c("A", "B", "C"),
                        stringsAsFactors = FALSE)
  orders |>
    audit_tap(trail, "raw") |>
    left_join_tap(lookup, by = "id", .trail = trail, .label = "joined") |>
    dplyr::filter(amount > 100) |>
    audit_tap(trail, "filtered", .fns = list(n = nrow))
  trail
}


# ── trail_to_list ─────────────────────────────────────────────────────────────

test_that("trail_to_list returns a plain list with correct top-level keys", {
  lst <- trail_to_list(make_export_trail())
  expect_type(lst, "list")
  expect_named(lst, c("name", "created_at", "n_snapshots", "snapshots"))
  expect_equal(lst$name, "export_test")
  expect_equal(lst$n_snapshots, 3L)
  expect_type(lst$created_at, "character")
  expect_true(grepl("T", lst$created_at))
})

test_that("trail_to_list snapshots are named by label", {
  lst <- trail_to_list(make_export_trail())
  expect_named(lst$snapshots, c("raw", "joined", "filtered"))
})

test_that("trail_to_list converts schema data.frame to list of rows", {
  lst <- trail_to_list(make_export_trail())
  schema <- lst$snapshots$raw$schema
  expect_type(schema, "list")
  expect_true(is.list(schema[[1]]))
  expect_true("column" %in% names(schema[[1]]))
  expect_true("type" %in% names(schema[[1]]))
  expect_true("n_na" %in% names(schema[[1]]))
})

test_that("trail_to_list converts POSIXct timestamps to ISO 8601 strings", {
  lst <- trail_to_list(make_export_trail())
  expect_type(lst$snapshots$raw$timestamp, "character")
  expect_true(grepl("T", lst$snapshots$raw$timestamp))
})

test_that("trail_to_list handles NULL numeric_summary (character-only data)", {
  trail <- audit_trail("no_num")
  data.frame(x = letters[1:3], stringsAsFactors = FALSE) |>
    audit_tap(trail, "text_only")
  lst <- trail_to_list(trail)
  expect_null(lst$snapshots$text_only$numeric_summary)
})

test_that("trail_to_list has NULL changes on the first snapshot", {
  lst <- trail_to_list(make_export_trail())
  expect_null(lst$snapshots$raw$changes)
})

test_that("trail_to_list converts type_changes in changes to list of rows", {
  trail <- audit_trail("type_change")
  data.frame(x = 1:3) |> audit_tap(trail, "before")
  data.frame(x = as.character(1:3), stringsAsFactors = FALSE) |>
    audit_tap(trail, "after")
  lst <- trail_to_list(trail)
  tc <- lst$snapshots$after$changes$type_changes
  expect_type(tc, "list")
  expect_true(is.list(tc[[1]]))
  expect_true("column" %in% names(tc[[1]]))
})

test_that("trail_to_list output is JSON-serialisable", {
  skip_if_not_installed("jsonlite")
  lst <- trail_to_list(make_export_trail())
  expect_no_error(jsonlite::toJSON(lst, auto_unbox = TRUE))
})

test_that("trail_to_list rejects non-trail input", {
  expect_error(trail_to_list(list()), "audit_trail")
  expect_error(trail_to_list("not a trail"), "audit_trail")
})

test_that("trail_to_list on empty trail returns empty snapshots list", {
  trail <- audit_trail("empty")
  lst   <- trail_to_list(trail)
  expect_equal(lst$n_snapshots, 0L)
  expect_equal(length(lst$snapshots), 0L)
})


# ── trail_to_df ───────────────────────────────────────────────────────────────

test_that("trail_to_df returns a data.frame", {
  df <- trail_to_df(make_export_trail())
  expect_s3_class(df, "data.frame")
  expect_false(inherits(df, "tbl_df"))
})

test_that("trail_to_df has one row per snapshot", {
  df <- trail_to_df(make_export_trail())
  expect_equal(nrow(df), 3L)
})

test_that("trail_to_df has the expected column names", {
  df <- trail_to_df(make_export_trail())
  expected <- c("index", "label", "type", "timestamp", "nrow", "ncol",
                "total_nas", "all_columns", "schema", "numeric_summary",
                "changes", "diagnostics", "custom", "pipeline", "controls")
  expect_named(df, expected)
})

test_that("trail_to_df scalar columns have correct types", {
  df <- trail_to_df(make_export_trail())
  expect_type(df$index, "integer")
  expect_type(df$label, "character")
  expect_type(df$type, "character")
  expect_s3_class(df$timestamp, "POSIXct")
  expect_type(df$nrow, "integer")
  expect_type(df$ncol, "integer")
  expect_type(df$total_nas, "integer")
})

test_that("trail_to_df schema is a list-column of data.frames", {
  df <- trail_to_df(make_export_trail())
  expect_type(df$schema, "list")
  expect_s3_class(df$schema[[1]], "data.frame")
})

test_that("trail_to_df changes is NULL for the first row", {
  df <- trail_to_df(make_export_trail())
  expect_null(df$changes[[1]])
})

test_that("trail_to_df attaches trail metadata as attributes", {
  df <- trail_to_df(make_export_trail())
  expect_equal(attr(df, "trail_name"), "export_test")
  expect_s3_class(attr(df, "created_at"), "POSIXct")
})

test_that("trail_to_df on empty trail returns zero-row data.frame with correct columns", {
  df <- trail_to_df(audit_trail("empty"))
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 0L)
  expect_equal(ncol(df), 15L)
})

test_that("trail_to_df diagnostics is NULL for plain taps", {
  trail <- audit_trail("plain")
  mtcars |> audit_tap(trail, "step1")
  df <- trail_to_df(trail)
  expect_null(df$diagnostics[[1]])
})

test_that("trail_to_df rejects non-trail input", {
  expect_error(trail_to_df(list()), "audit_trail")
})


# ── write_trail / read_trail — RDS ───────────────────────────────────────────

test_that("write_trail (rds) creates a file", {
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp))
  write_trail(make_export_trail(), tmp)
  expect_true(file.exists(tmp))
})

test_that("write_trail returns .trail invisibly", {
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp))
  trail  <- make_export_trail()
  result <- withVisible(write_trail(trail, tmp))
  expect_false(result$visible)
  expect_identical(result$value, trail)
})

test_that("read_trail (rds) restores audit_trail class and structure", {
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp))
  original <- make_export_trail()
  write_trail(original, tmp)
  restored <- read_trail(tmp)

  expect_s3_class(restored, "audit_trail")
  expect_s3_class(restored, "environment")
  expect_equal(restored$name, "export_test")
  expect_s3_class(restored$created_at, "POSIXct")
  expect_equal(length(restored$snapshots), 3L)
  expect_equal(restored$labels, c("raw", "joined", "filtered"))
})

test_that("read_trail (rds) restores audit_snap S3 class on every snapshot", {
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp))
  write_trail(make_export_trail(), tmp)
  restored <- read_trail(tmp)
  for (snap in restored$snapshots) {
    expect_s3_class(snap, "audit_snap")
  }
})

test_that("read_trail (rds) preserves snapshot scalar fields", {
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp))
  original <- make_export_trail()
  write_trail(original, tmp)
  restored <- read_trail(tmp)

  orig_raw <- original$snapshots[[1]]
  rest_raw <- restored$snapshots[[1]]
  expect_equal(rest_raw$label,     orig_raw$label)
  expect_equal(rest_raw$nrow,      orig_raw$nrow)
  expect_equal(rest_raw$ncol,      orig_raw$ncol)
  expect_equal(rest_raw$total_nas, orig_raw$total_nas)
  expect_equal(rest_raw$type,      orig_raw$type)
})

test_that("read_trail (rds) preserves schema data.frame", {
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp))
  original <- make_export_trail()
  write_trail(original, tmp)
  restored <- read_trail(tmp)
  expect_equal(restored$snapshots[[1]]$schema, original$snapshots[[1]]$schema)
})

test_that("read_trail (rds) preserves changes list", {
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp))
  original <- make_export_trail()
  write_trail(original, tmp)
  restored <- read_trail(tmp)

  expect_null(restored$snapshots[[1]]$changes)
  expect_equal(
    restored$snapshots[[2]]$changes$row_delta,
    original$snapshots[[2]]$changes$row_delta
  )
})

test_that("read_trail (rds) preserves custom diagnostics", {
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp))
  original <- make_export_trail()
  write_trail(original, tmp)
  restored <- read_trail(tmp)
  # 3rd snapshot has .fns = list(n = nrow)
  expect_equal(restored$snapshots[[3]]$custom$n,
               original$snapshots[[3]]$custom$n)
})

test_that("read_trail auto-detects rds format from extension", {
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp))
  write_trail(make_export_trail(), tmp)
  expect_s3_class(read_trail(tmp), "audit_trail")
})

test_that("read_trail with explicit format = 'rds' works for non-.rds extension", {
  tmp <- tempfile(fileext = ".dat")
  on.exit(unlink(tmp))
  write_trail(make_export_trail(), tmp, format = "rds")
  restored <- read_trail(tmp, format = "rds")
  expect_s3_class(restored, "audit_trail")
  expect_equal(length(restored$snapshots), 3L)
})


# ── write_trail / read_trail — JSON ──────────────────────────────────────────

test_that("write_trail (json) creates a valid JSON file", {
  skip_if_not_installed("jsonlite")
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp))
  write_trail(make_export_trail(), tmp, format = "json")
  expect_true(file.exists(tmp))
  expect_no_error(jsonlite::fromJSON(tmp))
})

test_that("read_trail (json) restores audit_trail class and snapshot count", {
  skip_if_not_installed("jsonlite")
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp))
  write_trail(make_export_trail(), tmp, format = "json")
  restored <- read_trail(tmp)

  expect_s3_class(restored, "audit_trail")
  expect_equal(restored$name, "export_test")
  expect_equal(length(restored$snapshots), 3L)
  expect_equal(restored$labels, c("raw", "joined", "filtered"))
})

test_that("read_trail (json) restores audit_snap S3 class on every snapshot", {
  skip_if_not_installed("jsonlite")
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp))
  write_trail(make_export_trail(), tmp, format = "json")
  restored <- read_trail(tmp)
  for (snap in restored$snapshots) {
    expect_s3_class(snap, "audit_snap")
  }
})

test_that("read_trail (json) restores scalar fields with correct types", {
  skip_if_not_installed("jsonlite")
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp))
  write_trail(make_export_trail(), tmp, format = "json")
  restored <- read_trail(tmp)

  snap <- restored$snapshots[[1]]
  expect_type(snap$index, "integer")
  expect_type(snap$nrow, "integer")
  expect_type(snap$ncol, "integer")
  expect_type(snap$total_nas, "integer")
  expect_s3_class(snap$timestamp, "POSIXct")
})

test_that("read_trail (json) restores schema as data.frame with correct column names", {
  skip_if_not_installed("jsonlite")
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp))
  write_trail(make_export_trail(), tmp, format = "json")
  restored <- read_trail(tmp)

  schema <- restored$snapshots[[1]]$schema
  expect_s3_class(schema, "data.frame")
  expect_named(schema, c("column", "type", "n_na"))
  expect_type(schema$n_na, "integer")
})

test_that("read_trail (json) restores type_changes as data.frame", {
  skip_if_not_installed("jsonlite")
  trail <- audit_trail("type_change_json")
  data.frame(x = 1:3) |> audit_tap(trail, "before")
  data.frame(x = as.character(1:3), stringsAsFactors = FALSE) |>
    audit_tap(trail, "after")

  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp))
  write_trail(trail, tmp, format = "json")
  restored <- read_trail(tmp)

  tc <- restored$snapshots[[2]]$changes$type_changes
  expect_s3_class(tc, "data.frame")
  expect_named(tc, c("column", "from", "to"))
})

test_that("read_trail (json) restores NULL changes on first snapshot", {
  skip_if_not_installed("jsonlite")
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp))
  write_trail(make_export_trail(), tmp, format = "json")
  restored <- read_trail(tmp)
  expect_null(restored$snapshots[[1]]$changes)
})

test_that("read_trail (json) restores changes integer deltas correctly", {
  skip_if_not_installed("jsonlite")
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp))
  original <- make_export_trail()
  write_trail(original, tmp, format = "json")
  restored <- read_trail(tmp)

  orig_delta <- original$snapshots[[2]]$changes$row_delta
  rest_delta <- restored$snapshots[[2]]$changes$row_delta
  expect_equal(rest_delta, orig_delta)
  expect_type(rest_delta, "integer")
})

test_that("read_trail auto-detects json format from extension", {
  skip_if_not_installed("jsonlite")
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp))
  write_trail(make_export_trail(), tmp, format = "json")
  expect_s3_class(read_trail(tmp), "audit_trail")
})

test_that("read_trail (json) restores numeric_summary values as double", {
  skip_if_not_installed("jsonlite")
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp))
  original <- make_export_trail()
  write_trail(original, tmp, format = "json")
  restored <- read_trail(tmp)

  orig_ns <- original$snapshots[[1]]$numeric_summary
  rest_ns <- restored$snapshots[[1]]$numeric_summary
  expect_s3_class(rest_ns, "data.frame")
  expect_equal(rest_ns$min,    orig_ns$min)
  expect_equal(rest_ns$median, orig_ns$median)
  expect_type(rest_ns$min,    "double")
  expect_type(rest_ns$median, "double")
  expect_type(rest_ns$max,    "double")
})

test_that("read_trail (json) restores custom diagnostics", {
  skip_if_not_installed("jsonlite")
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp))
  original <- make_export_trail()
  write_trail(original, tmp, format = "json")
  restored <- read_trail(tmp)
  # 3rd snapshot has .fns = list(n = nrow)
  expect_equal(restored$snapshots[[3]]$custom$n,
               original$snapshots[[3]]$custom$n)
})

test_that(".parse_posixct returns a POSIXct-classed NA for NULL input", {
  result <- tidyaudit:::.parse_posixct(NULL)
  expect_s3_class(result, "POSIXct")
  expect_true(is.na(result))
})

test_that("read_trail (json) restores schema as 0-row data.frame for zero-column input", {
  skip_if_not_installed("jsonlite")
  trail <- audit_trail("zero_cols")
  data.frame(row.names = 1:3) |> audit_tap(trail, "empty")

  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp))
  write_trail(trail, tmp, format = "json")
  restored <- read_trail(tmp)

  schema <- restored$snapshots[[1]]$schema
  expect_s3_class(schema, "data.frame")
  expect_named(schema, c("column", "type", "n_na"))
  expect_equal(nrow(schema), 0L)
})


# ── Error handling ────────────────────────────────────────────────────────────

test_that("write_trail errors on non-trail input", {
  expect_error(write_trail(list(), tempfile()), "audit_trail")
})

test_that("write_trail errors on invalid file argument", {
  trail <- make_export_trail()
  expect_error(write_trail(trail, NA_character_), "file")
  expect_error(write_trail(trail, c("a", "b")),   "file")
})

test_that("read_trail errors when file does not exist", {
  expect_error(read_trail("/nonexistent/path/trail.rds"), "not found")
})

test_that("read_trail errors on unknown extension without explicit format", {
  tmp <- tempfile(fileext = ".csv")
  file.create(tmp)
  on.exit(unlink(tmp))
  expect_error(read_trail(tmp), "infer")
})

test_that("read_trail errors on invalid file argument", {
  expect_error(read_trail(NA_character_), "file")
})


# ── audit_export ─────────────────────────────────────────────────────────────

test_that("audit_export creates an HTML file", {
  skip_if_not_installed("jsonlite")
  tmp <- tempfile(fileext = ".html")
  on.exit(unlink(tmp))
  trail <- make_export_trail()
  audit_export(trail, file = tmp)
  expect_true(file.exists(tmp))
})

test_that("audit_export output is valid HTML containing the trail JSON", {
  skip_if_not_installed("jsonlite")
  tmp <- tempfile(fileext = ".html")
  on.exit(unlink(tmp))
  trail <- make_export_trail()
  audit_export(trail, file = tmp)
  content <- paste(readLines(tmp), collapse = "\n")
  expect_true(grepl("<!DOCTYPE html>", content, fixed = TRUE))
  expect_true(grepl("TRAIL_DATA", content, fixed = TRUE))
  expect_true(grepl("export_test", content, fixed = TRUE))
})

test_that("audit_export returns the file path invisibly", {
  skip_if_not_installed("jsonlite")
  tmp <- tempfile(fileext = ".html")
  on.exit(unlink(tmp))
  trail <- make_export_trail()
  result <- withVisible(audit_export(trail, file = tmp))
  expect_false(result$visible)
  expect_equal(result$value, tmp)
})

test_that("audit_export embeds all snapshot labels in the output", {
  skip_if_not_installed("jsonlite")
  tmp <- tempfile(fileext = ".html")
  on.exit(unlink(tmp))
  trail <- make_export_trail()
  audit_export(trail, file = tmp)
  content <- paste(readLines(tmp), collapse = "\n")
  expect_true(grepl('"raw"', content, fixed = TRUE))
  expect_true(grepl('"joined"', content, fixed = TRUE))
  expect_true(grepl('"filtered"', content, fixed = TRUE))
})

test_that("audit_export errors on non-trail input", {
  expect_error(audit_export(list()), "audit_trail")
})

test_that("audit_export errors on invalid file argument", {
  skip_if_not_installed("jsonlite")
  trail <- make_export_trail()
  expect_error(audit_export(trail, file = NA_character_), "file")
  expect_error(audit_export(trail, file = c("a.html", "b.html")), "file")
})

test_that("audit_export works with empty trail", {
  skip_if_not_installed("jsonlite")
  tmp <- tempfile(fileext = ".html")
  on.exit(unlink(tmp))
  trail <- audit_trail("empty")
  audit_export(trail, file = tmp)
  expect_true(file.exists(tmp))
  content <- paste(readLines(tmp), collapse = "\n")
  expect_true(grepl("TRAIL_DATA", content, fixed = TRUE))
})

test_that("audit_export embeds join diagnostics", {
  skip_if_not_installed("jsonlite")
  tmp <- tempfile(fileext = ".html")
  on.exit(unlink(tmp))
  trail <- make_export_trail()
  audit_export(trail, file = tmp)
  content <- paste(readLines(tmp), collapse = "\n")
  expect_true(grepl("match_rate", content, fixed = TRUE))
})

test_that("audit_export embeds filter diagnostics when present", {
  skip_if_not_installed("jsonlite")
  tmp <- tempfile(fileext = ".html")
  on.exit(unlink(tmp))
  trail <- audit_trail("filter_test")
  data.frame(id = 1:10, amount = 1:10 * 100) |>
    audit_tap(trail, "raw") |>
    filter_tap(amount > 300, .trail = trail, .label = "filtered")
  audit_export(trail, file = tmp)
  content <- paste(readLines(tmp), collapse = "\n")
  expect_true(grepl("pct_dropped", content, fixed = TRUE))
})

test_that("audit_export with file=NULL creates a temp file", {
  skip_if_not_installed("jsonlite")
  trail <- make_export_trail()
  # Stub browseURL to prevent opening a browser in tests
  local_mocked_bindings(browseURL = function(...) invisible(NULL), .package = "utils")
  result <- audit_export(trail)
  on.exit(unlink(result))
  expect_true(file.exists(result))
  expect_true(grepl("\\.html$", result))
})

test_that("audit_export escapes </script> in trail content", {
  skip_if_not_installed("jsonlite")
  tmp <- tempfile(fileext = ".html")
  on.exit(unlink(tmp))
  trail <- audit_trail("<script>alert(1)</script>")
  mtcars |> audit_tap(trail, "<b>bad</b>")
  audit_export(trail, file = tmp)
  content <- paste(readLines(tmp), collapse = "\n")
  # A raw </script> inside the JSON blob would break the HTML <script> tag.
  # The R function escapes it to <\/script>.
  # Count </script> occurrences — there should be exactly 1 (the real closing tag).
  expect_equal(length(gregexpr("</script>", content, fixed = TRUE)[[1L]]), 1L)
})

# ---------------------------------------------------------------------------
# Snapshot controls serialization
# ---------------------------------------------------------------------------

test_that("JSON round-trip preserves controls", {
  skip_if_not_installed("jsonlite")
  trail <- audit_trail("controls_json")
  mtcars |> audit_tap(trail, "raw", .numeric_summary = FALSE,
                      .cols_include = c("mpg", "cyl"))
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp))
  write_trail(trail, tmp, format = "json")
  restored <- read_trail(tmp, format = "json")
  snap <- restored$snapshots[[1]]
  expect_null(snap$numeric_summary)
  expect_equal(snap$schema$column, c("mpg", "cyl"))
  expect_false(is.null(snap$controls))
  # jsonlite may deserialize character vectors as lists; compare via unlist
  expect_equal(unlist(snap$controls$cols_include), c("mpg", "cyl"))
  expect_false(snap$controls$numeric_summary)
})

test_that("RDS round-trip preserves controls", {
  trail <- audit_trail("controls_rds")
  mtcars |> audit_tap(trail, "raw", .cols_exclude = c("disp", "hp"))
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp))
  write_trail(trail, tmp, format = "rds")
  restored <- read_trail(tmp, format = "rds")
  snap <- restored$snapshots[[1]]
  expect_false("disp" %in% snap$schema$column)
  expect_false("hp" %in% snap$schema$column)
  expect_equal(snap$controls$cols_exclude, c("disp", "hp"))
})

test_that("trail_to_df includes controls column", {
  trail <- audit_trail("controls_df")
  mtcars |> audit_tap(trail, "raw", .numeric_summary = FALSE)
  df <- trail_to_df(trail)
  expect_true("controls" %in% names(df))
  expect_false(is.null(df$controls[[1]]))
})

# ---------------------------------------------------------------------------
# all_columns serialization
# ---------------------------------------------------------------------------

test_that("JSON round-trip preserves all_columns", {
  skip_if_not_installed("jsonlite")
  trail <- audit_trail("allcols_json")
  mtcars |> audit_tap(trail, "raw", .cols_include = c("mpg", "cyl"))
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp))
  write_trail(trail, tmp, format = "json")
  restored <- read_trail(tmp, format = "json")
  snap <- restored$snapshots[[1]]
  expect_equal(snap$all_columns, names(mtcars))
  expect_equal(snap$schema$column, c("mpg", "cyl"))
})

test_that("RDS round-trip preserves all_columns", {
  trail <- audit_trail("allcols_rds")
  mtcars |> audit_tap(trail, "raw", .cols_exclude = "disp")
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp))
  write_trail(trail, tmp, format = "rds")
  restored <- read_trail(tmp, format = "rds")
  snap <- restored$snapshots[[1]]
  expect_equal(snap$all_columns, names(mtcars))
  expect_false("disp" %in% snap$schema$column)
})

test_that("trail_to_df includes all_columns list-column", {
  trail <- audit_trail("allcols_df")
  mtcars |> audit_tap(trail, "raw")
  df <- trail_to_df(trail)
  expect_true("all_columns" %in% names(df))
  expect_equal(df$all_columns[[1]], names(mtcars))
})
