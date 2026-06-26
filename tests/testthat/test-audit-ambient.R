# Stage 2: capture core — collision / aliasing / lifecycle battery.

# Return the snapshot for an object_name (optionally a specific version).
snap_by <- function(trail, object_name, version = NULL) {
  hits <- Filter(function(s) identical(s$object_name, object_name) &&
                   (is.null(version) || identical(s$version, version)),
                 trail$snapshots)
  if (length(hits) == 0L) return(NULL)
  hits[[length(hits)]]
}

events_of <- function(trail) trail$events


test_that("basic block captures one snapshot per object with correct parents", {
  e <- new.env()
  trail <- audit_record({
    raw    <- dplyr::as_tibble(mtcars)
    clean  <- dplyr::filter(raw, mpg > 20)
    lookup <- data.frame(cyl = c(4, 6, 8), lab = c("s", "m", "l"),
                         stringsAsFactors = FALSE)
    joined <- dplyr::left_join(clean, lookup, by = "cyl")
  }, env = e)

  expect_s3_class(trail, "audit_trail")
  expect_equal(length(trail$snapshots), 4L)

  expect_equal(snap_by(trail, "raw")$parent_snapshot_ids, character(0))
  expect_equal(snap_by(trail, "clean")$parent_snapshot_ids,
               snap_by(trail, "raw")$snapshot_id)
  expect_setequal(snap_by(trail, "joined")$parent_snapshot_ids,
                  c(snap_by(trail, "clean")$snapshot_id,
                    snap_by(trail, "lookup")$snapshot_id))
})

test_that("source line is recorded per object", {
  e <- new.env()
  trail <- audit_record({
    a <- data.frame(x = 1:3)
  }, env = e)
  expect_match(snap_by(trail, "a")$source, "a <- data.frame")
})


# ── Self-overwrite: parents resolved from the PRE-eval registry ──────────────

test_that("self-overwrite links to the previous version, not the new one", {
  e <- new.env()
  trail <- audit_record({
    df <- data.frame(x = 1:3)
    df <- dplyr::mutate(df, y = x + 1)
  }, env = e)

  v1 <- snap_by(trail, "df", 1L)
  v2 <- snap_by(trail, "df", 2L)
  expect_equal(v2$version, 2L)
  expect_equal(v2$event, "update")
  expect_equal(v2$object_id, v1$object_id)         # same binding stream
  expect_equal(v2$parent_snapshot_ids, v1$snapshot_id)
})

test_that("self-join links to previous self plus the other table", {
  e <- new.env()
  trail <- audit_record({
    df     <- data.frame(cyl = c(4, 6), n = 1:2)
    lookup <- data.frame(cyl = c(4, 6), lab = c("a", "b"), stringsAsFactors = FALSE)
    df     <- dplyr::left_join(df, lookup, by = "cyl")
  }, env = e)
  v1 <- snap_by(trail, "df", 1L)
  v2 <- snap_by(trail, "df", 2L)
  expect_setequal(v2$parent_snapshot_ids,
                  c(v1$snapshot_id, snap_by(trail, "lookup")$snapshot_id))
})


# ── Column-name vs df-name collision ─────────────────────────────────────────

test_that("a df sharing a name with a referenced column is not a false parent", {
  e <- new.env()
  trail <- audit_record({
    mpg   <- data.frame(z = 1:3)        # a tracked df literally named 'mpg'
    raw   <- dplyr::as_tibble(mtcars)   # has a column 'mpg'
    clean <- dplyr::filter(raw, mpg > 20)
  }, env = e)

  mpg_id <- snap_by(trail, "mpg")$snapshot_id
  parents <- snap_by(trail, "clean")$parent_snapshot_ids
  expect_equal(parents, snap_by(trail, "raw")$snapshot_id)
  expect_false(mpg_id %in% parents)
})


# ── Subassignment: root-binding target extraction ────────────────────────────

test_that("$<- subassignment attributes to the root binding", {
  e <- new.env()
  trail <- audit_record({
    df <- data.frame(x = 1:3)
    df$y <- df$x * 2
  }, env = e)
  v2 <- snap_by(trail, "df", 2L)
  expect_equal(v2$version, 2L)
  expect_equal(v2$event, "update")
  expect_equal(v2$ncol, 2L)
  expect_equal(v2$parent_snapshot_ids, snap_by(trail, "df", 1L)$snapshot_id)
})

test_that("names(df) <- replacement attributes to the root binding", {
  e <- new.env()
  trail <- audit_record({
    df <- data.frame(x = 1:3)
    names(df) <- "a"
  }, env = e)
  v2 <- snap_by(trail, "df", 2L)
  expect_equal(v2$event, "update")
  expect_true("a" %in% v2$all_columns)
})


# ── Binding-stream object_id ─────────────────────────────────────────────────

test_that("distinct assignment targets get distinct object_ids", {
  e <- new.env()
  trail <- audit_record({
    a <- data.frame(x = 1)
    b <- data.frame(x = 1)
  }, env = e)
  expect_false(identical(snap_by(trail, "a")$object_id,
                         snap_by(trail, "b")$object_id))
})

test_that("re-creating a deleted binding starts a new object_id", {
  e <- new.env()
  trail <- audit_record({
    df <- data.frame(x = 1)
    rm(df)
    df <- data.frame(y = 2)
  }, env = e)
  creates <- Filter(function(s) s$object_name == "df" && s$event == "create",
                    trail$snapshots)
  expect_equal(length(creates), 2L)
  expect_false(identical(creates[[1]]$object_id, creates[[2]]$object_id))
})


# ── Lifecycle events ─────────────────────────────────────────────────────────

test_that("rm() records a delete event", {
  e <- new.env()
  trail <- audit_record({
    df <- data.frame(x = 1:3)
    rm(df)
  }, env = e)
  expect_equal(snap_by(trail, "df", 2L)$event, "delete")
  expect_false("df" %in% names(trail$registry))
})

test_that("rebinding a df to a non-df records a retire event", {
  e <- new.env()
  trail <- audit_record({
    df <- data.frame(x = 1:3)
    df <- 42
  }, env = e)
  expect_equal(snap_by(trail, "df", 2L)$event, "retire")
})

test_that("an assignment with no metadata change is unchanged_assignment", {
  e <- new.env()
  trail <- audit_record({
    df <- data.frame(x = c(1, 2, 3))
    df <- dplyr::mutate(df, x = x * 2)   # same shape/types/NAs
  }, env = e, level = "metadata")
  expect_equal(snap_by(trail, "df", 2L)$event, "unchanged_assignment")
})

test_that("column_hash level detects a metadata-stable value change", {
  e <- new.env()
  trail <- audit_record({
    df <- data.frame(x = c(1, 2, 3))
    df <- dplyr::mutate(df, x = x * 2)
  }, env = e, level = "column_hash")
  expect_equal(snap_by(trail, "df", 2L)$event, "update")
})


# ── Filtering + non-data.frame assignments ───────────────────────────────────

test_that("non-data.frame assignments produce no snapshot", {
  e <- new.env()
  trail <- audit_record({
    x  <- 5
    df <- data.frame(a = 1)
  }, env = e)
  expect_equal(length(trail$snapshots), 1L)
  expect_equal(snap_by(trail, "df")$object_name, "df")
})

test_that("ignore patterns exclude matching objects", {
  e <- new.env()
  trail <- audit_record({
    tmp_scratch <- data.frame(a = 1)
    keep        <- data.frame(b = 2)
  }, env = e, ignore = "^tmp_")
  expect_null(snap_by(trail, "tmp_scratch"))
  expect_false(is.null(snap_by(trail, "keep")))
})

test_that("watch restricts tracking to named objects", {
  e <- new.env()
  trail <- audit_record({
    a <- data.frame(x = 1)
    b <- data.frame(y = 2)
  }, env = e, watch = "a")
  expect_false(is.null(snap_by(trail, "a")))
  expect_null(snap_by(trail, "b"))
})


# ── Baseline of pre-existing data.frames ─────────────────────────────────────

test_that("pre-existing data.frames are baselined and become parents", {
  e <- new.env()
  e$raw <- data.frame(id = 1:3, mpg = c(10, 25, 30))
  trail <- audit_record({
    clean <- dplyr::filter(raw, mpg > 20)
  }, env = e)
  expect_equal(snap_by(trail, "raw")$source, "<pre-existing>")
  expect_equal(snap_by(trail, "clean")$parent_snapshot_ids,
               snap_by(trail, "raw")$snapshot_id)
})


# ── Warnings / errors ────────────────────────────────────────────────────────

test_that("warnings are captured in the step event log", {
  e <- new.env()
  trail <- suppressWarnings(audit_record({
    df <- data.frame(a = 1)
    warning("boom")
  }, env = e))
  warns <- unlist(lapply(trail$events, `[[`, "warnings"))
  expect_true(any(grepl("boom", warns)))
})

test_that("an error aborts by default (re-thrown like normal evaluation)", {
  e <- new.env()
  expect_error(
    audit_record({
      df <- data.frame(a = 1)
      stop("nope")
    }, env = e),
    "nope"
  )
})

test_that("continue_on_error records the error and keeps going", {
  e <- new.env()
  trail <- audit_record({
    df  <- data.frame(a = 1)
    stop("nope")
    df2 <- data.frame(b = 2)
  }, env = e, continue_on_error = TRUE)
  errs <- unlist(lapply(trail$events, `[[`, "error"))
  expect_true(any(grepl("nope", errs)))
  expect_false(is.null(snap_by(trail, "df2")))
})


# ── Change detection between versions ────────────────────────────────────────

test_that("consecutive versions of an object carry a changes diff", {
  e <- new.env()
  trail <- audit_record({
    df <- data.frame(x = 1:4)
    df <- dplyr::filter(df, x > 2)
  }, env = e)
  v2 <- snap_by(trail, "df", 2L)
  expect_false(is.null(v2$changes))
  expect_equal(v2$changes$row_delta, -2L)
})


# ── Stage 3: audit_source ────────────────────────────────────────────────────

write_script <- function(lines) {
  f <- tempfile(fileext = ".R")
  writeLines(lines, f)
  f
}

test_that("audit_source captures one snapshot per top-level statement", {
  f <- write_script(c(
    "raw   <- dplyr::as_tibble(mtcars)",
    "clean <- dplyr::filter(raw, mpg > 20)"
  ))
  on.exit(unlink(f))
  trail <- audit_source(f)
  expect_equal(length(trail$snapshots), 2L)
  expect_equal(snap_by(trail, "clean")$parent_snapshot_ids,
               snap_by(trail, "raw")$snapshot_id)
})

test_that("audit_source records srcref line numbers", {
  f <- write_script(c(
    "a <- data.frame(x = 1)",
    "b <- data.frame(y = 2)"
  ))
  on.exit(unlink(f))
  trail <- audit_source(f)
  expect_equal(snap_by(trail, "b")$srcref$line1, 2L)
})

test_that("audit_source evaluates each statement exactly once", {
  e <- new.env(parent = globalenv())
  e$hits <- 0L
  f <- write_script(c(
    "hits <- hits + 1L",
    "df <- data.frame(a = hits)"
  ))
  on.exit(unlink(f))
  audit_source(f, env = e)
  expect_equal(e$hits, 1L)            # evaluated once, not re-run by capture
})

test_that("audit_source defaults to a sandbox env, not the caller workspace", {
  f <- write_script("sentinel_obj <- data.frame(a = 1)")
  on.exit(unlink(f))
  audit_source(f)
  expect_false(exists("sentinel_obj", envir = globalenv(), inherits = FALSE))
})

test_that("audit_source rethrows errors by default", {
  f <- write_script(c("df <- data.frame(a = 1)", "stop('boom')"))
  on.exit(unlink(f))
  expect_error(audit_source(f), "boom")
})

test_that("audit_source rejects a missing file", {
  expect_error(audit_source(tempfile(fileext = ".R")), "not found")
})

test_that("audit_record and audit_source produce the same trail shape", {
  lines <- c(
    "raw   <- dplyr::as_tibble(mtcars)",
    "clean <- dplyr::filter(raw, mpg > 20)"
  )
  f <- write_script(lines)
  on.exit(unlink(f))
  ts <- audit_source(f)
  e  <- new.env(parent = globalenv())
  tr <- audit_record({
    raw   <- dplyr::as_tibble(mtcars)
    clean <- dplyr::filter(raw, mpg > 20)
  }, env = e)

  shape <- function(t) {
    data.frame(
      name   = vapply(t$snapshots, `[[`, character(1), "object_name"),
      event  = vapply(t$snapshots, `[[`, character(1), "event"),
      nrow   = vapply(t$snapshots, `[[`, integer(1), "nrow"),
      stringsAsFactors = FALSE
    )
  }
  expect_equal(shape(ts), shape(tr))
})


# ── Stage 3: .audit_observe_step never re-evaluates ──────────────────────────

test_that("observe step captures state without re-running the statement", {
  e <- new.env(parent = globalenv())
  # Simulate the REPL having already evaluated the statement exactly once.
  e$hits <- 1L
  e$df   <- data.frame(a = e$hits)
  trail  <- audit_trail("observe")
  tidyaudit:::.audit_init(trail, tidyaudit:::.audit_opts())

  expr <- quote(df <- {
    hits <- hits + 1L
    data.frame(a = hits)
  })
  tidyaudit:::.audit_observe_step(expr, e, trail, trail$opts)

  expect_equal(e$hits, 1L)                       # NOT incremented a second time
  expect_equal(length(trail$snapshots), 1L)      # but df was captured
  expect_equal(snap_by(trail, "df")$object_name, "df")
})


# ── Stage 3: audit_start / audit_stop state management ───────────────────────

test_that("audit_stop without a session errors", {
  expect_null(tidyaudit:::the$active)
  expect_error(audit_stop(), "No active")
})

test_that("audit_start then audit_stop manages session state and returns trail", {
  on.exit({
    if (!is.null(tidyaudit:::the$active)) suppressMessages(audit_stop())
  }, add = TRUE)
  e <- new.env(parent = globalenv())
  suppressMessages(audit_start("sess", env = e))
  expect_false(is.null(tidyaudit:::the$active))
  expect_error(suppressMessages(audit_start("again")), "already active")
  trail <- suppressMessages(audit_stop())
  expect_s3_class(trail, "audit_trail")
  expect_null(tidyaudit:::the$active)
})

test_that("audit_source granularity vs source() is documented (per-statement)", {
  # source() is one top-level task, so audit_start() under source() would record
  # a single combined step. audit_source() instead loops per statement. We
  # assert the per-statement contract here; the source() limitation is covered
  # by documentation in ?audit_start.
  f <- write_script(c(
    "a <- data.frame(x = 1)",
    "b <- data.frame(y = 2)",
    "d <- data.frame(z = 3)"
  ))
  on.exit(unlink(f))
  trail <- audit_source(f)
  expect_equal(length(unique(vapply(trail$snapshots, `[[`, character(1), "step_id"))), 3L)
})


# ── Reproducibility: auditing must not touch the global RNG ───────────────────

test_that("audited execution does not perturb the global RNG state", {
  set.seed(123)
  before <- .Random.seed
  e <- new.env(parent = globalenv())
  audit_record({ df <- data.frame(x = 1:3) }, env = e, level = "column_hash")
  expect_identical(.Random.seed, before)
})
