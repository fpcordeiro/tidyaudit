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


# ── Review fixes: parent resolution ──────────────────────────────────────────

test_that("a fresh rebind that does not read the old value gets no parent", {
  e <- new.env()
  trail <- audit_record({
    df <- data.frame(a = 1)
    df <- data.frame(b = 2)   # unrelated value; reuses the name only
  }, env = e)
  v2 <- snap_by(trail, "df", 2L)
  expect_equal(v2$event, "update")               # same binding stream, new version
  expect_equal(v2$parent_snapshot_ids, character(0))  # NOT derived from old df
})

test_that("a real join parent is kept even when it shares a name with a column", {
  e <- new.env()
  trail <- audit_record({
    raw    <- data.frame(id = 1:3, lookup = c(9, 9, 9))   # column named 'lookup'
    lookup <- data.frame(id = 1:3, val = c("a", "b", "c"))
    joined <- dplyr::left_join(raw, lookup, by = "id")
  }, env = e)
  expect_setequal(
    snap_by(trail, "joined")$parent_snapshot_ids,
    c(snap_by(trail, "raw")$snapshot_id, snap_by(trail, "lookup")$snapshot_id)
  )
})

test_that("operands of mask operators are not treated as parents", {
  e <- new.env()
  trail <- audit_record({
    mpg   <- data.frame(z = 1:3)
    raw   <- dplyr::as_tibble(mtcars)
    clean <- dplyr::filter(raw, mpg > 20)        # mpg is a column here
  }, env = e)
  expect_false(snap_by(trail, "mpg")$snapshot_id %in%
                 snap_by(trail, "clean")$parent_snapshot_ids)
})

test_that("a data-masked column in a named arg is not a false parent", {
  e <- new.env()
  trail <- audit_record({
    mpg   <- data.frame(z = 1:3)
    raw   <- dplyr::as_tibble(mtcars)
    clean <- dplyr::mutate(raw, z = mpg)         # mpg is a column reference
  }, env = e)
  expect_equal(snap_by(trail, "clean")$parent_snapshot_ids,
               snap_by(trail, "raw")$snapshot_id)
})

test_that("extraction-based derivation links to the root object", {
  e <- new.env()
  trail <- audit_record({
    raw   <- data.frame(x = 1:3, y = 4:6)
    clean <- raw[raw$x > 1, ]
  }, env = e)
  expect_equal(snap_by(trail, "clean")$parent_snapshot_ids,
               snap_by(trail, "raw")$snapshot_id)
})

# ── Function-aware parent extraction ─────────────────────────────────────────

test_that("constructor named args descend into extraction (data.frame)", {
  e <- new.env()
  trail <- audit_record({
    raw <- data.frame(x = 1:3, y = 4:6)
    out <- data.frame(x = raw$x)
  }, env = e)
  expect_equal(snap_by(trail, "out")$parent_snapshot_ids,
               snap_by(trail, "raw")$snapshot_id)
})

test_that("constructor named args descend into extraction (tibble)", {
  e <- new.env()
  trail <- audit_record({
    raw <- data.frame(x = 1:3, y = 4:6)
    out <- dplyr::tibble(x = raw$x)
  }, env = e)
  expect_equal(snap_by(trail, "out")$parent_snapshot_ids,
               snap_by(trail, "raw")$snapshot_id)
})

test_that("bind_rows links all named data arguments as parents", {
  e <- new.env()
  trail <- audit_record({
    a   <- data.frame(x = 1:2)
    b   <- data.frame(x = 3:4)
    out <- dplyr::bind_rows(first = a, second = b)
  }, env = e)
  expect_setequal(snap_by(trail, "out")$parent_snapshot_ids,
                  c(snap_by(trail, "a")$snapshot_id,
                    snap_by(trail, "b")$snapshot_id))
})

test_that("named join arguments (x =, y =) resolve both parents", {
  e <- new.env()
  trail <- audit_record({
    clean  <- data.frame(id = 1:3, n = 1:3)
    lookup <- data.frame(id = 1:3, lab = c("a", "b", "c"),
                         stringsAsFactors = FALSE)
    out    <- dplyr::left_join(x = clean, y = lookup, by = "id")
  }, env = e)
  expect_setequal(snap_by(trail, "out")$parent_snapshot_ids,
                  c(snap_by(trail, "clean")$snapshot_id,
                    snap_by(trail, "lookup")$snapshot_id))
})

test_that("tidyselect arg in a one-data verb is not a false parent", {
  e <- new.env()
  trail <- audit_record({
    mpg   <- data.frame(z = 1:3)            # tracked df sharing a column name
    raw   <- data.frame(mpg = 1:3, cyl = 4:6)
    clean <- dplyr::select(raw, mpg)        # mpg here is a tidyselect column
  }, env = e)
  parents <- snap_by(trail, "clean")$parent_snapshot_ids
  expect_equal(parents, snap_by(trail, "raw")$snapshot_id)
  expect_false(snap_by(trail, "mpg")$snapshot_id %in% parents)
})

test_that("in-place column assignment links prior self and extracted source", {
  e <- new.env()
  trail <- audit_record({
    df       <- data.frame(id = 1:3)
    lookup   <- data.frame(v = 4:6)
    df$new   <- lookup$v
  }, env = e)
  v2 <- snap_by(trail, "df", 2L)
  expect_setequal(v2$parent_snapshot_ids,
                  c(snap_by(trail, "df", 1L)$snapshot_id,
                    snap_by(trail, "lookup")$snapshot_id))
})

test_that("unknown helper call walks a named data argument", {
  e <- new.env()
  e$helper <- function(...) data.frame(...)
  trail <- audit_record({
    raw <- data.frame(x = 1:3)
    out <- helper(x = raw)            # custom helper, named data arg
  }, env = e)
  expect_equal(snap_by(trail, "out")$parent_snapshot_ids,
               snap_by(trail, "raw")$snapshot_id)
})

test_that("unknown helper call walks a named extraction argument", {
  e <- new.env()
  e$helper <- function(...) data.frame(...)
  trail <- audit_record({
    raw <- data.frame(x = 1:3, y = 4:6)
    out <- helper(z = raw$x)          # custom helper, named extraction arg
  }, env = e)
  expect_equal(snap_by(trail, "out")$parent_snapshot_ids,
               snap_by(trail, "raw")$snapshot_id)
})

test_that("one-data verb resolves a named primary argument", {
  e <- new.env()
  trail <- audit_record({
    raw   <- data.frame(x = 1:3, y = 4:6)
    clean <- subset(x = raw, x > 1)   # primary arg named `x`, not positional
  }, env = e)
  expect_equal(snap_by(trail, "clean")$parent_snapshot_ids,
               snap_by(trail, "raw")$snapshot_id)
})


# ── Review fixes: terminal snapshots in report paths ─────────────────────────

test_that("delete carries forward the last-known NA count", {
  e <- new.env()
  trail <- audit_record({
    df <- data.frame(x = c(1, NA, 3))
    rm(df)
  }, env = e)
  term <- snap_by(trail, "df", 2L)
  expect_equal(term$event, "delete")
  expect_equal(term$total_nas, 1L)
})

test_that("print.audit_snap and audit_report handle delete/retire without error", {
  e <- new.env()
  trail <- audit_record({
    df <- data.frame(x = c(1, NA, 3))
    rm(df)
  }, env = e)
  term <- snap_by(trail, "df", 2L)
  expect_no_error(capture.output(print(term), type = "message"))
  expect_no_error(capture.output(audit_report(trail), type = "message"))

  e2 <- new.env()
  trail2 <- audit_record({
    g <- data.frame(y = 1:2)
    g <- 42                    # retire
  }, env = e2)
  expect_equal(snap_by(trail2, "g", 2L)$event, "retire")
  expect_no_error(capture.output(audit_report(trail2), type = "message"))
})


# ── Review fixes: hash evidence is recorded and serialised ───────────────────

test_that("metadata level records no evidence", {
  e <- new.env()
  trail <- audit_record({ df <- data.frame(x = 1:3) }, env = e)
  expect_null(snap_by(trail, "df")$evidence)
})

test_that("column_hash level records evidence metadata and a hash", {
  e <- new.env()
  trail <- audit_record({ df <- data.frame(x = 1:3) }, env = e, level = "column_hash")
  ev <- snap_by(trail, "df")$evidence
  expect_equal(ev$level, "column_hash")
  expect_true(grepl("xxhash", ev$algorithm))
  expect_true(grepl("not privacy", ev$salt_policy))
  expect_false(is.null(ev$hash))
})

test_that("evidence round-trips through RDS and JSON", {
  skip_if_not_installed("jsonlite")
  e <- new.env()
  trail <- audit_record({ df <- data.frame(x = 1:3) }, env = e, level = "column_hash")
  rds <- tempfile(fileext = ".rds"); json <- tempfile(fileext = ".json")
  on.exit(unlink(c(rds, json)))
  write_trail(trail, rds)
  write_trail(trail, json, format = "json")
  expect_equal(read_trail(rds)$snapshots[[1]]$evidence$level, "column_hash")
  expect_equal(read_trail(json)$snapshots[[1]]$evidence$level, "column_hash")
})


# ── Review fixes: audit_start callback + source() granularity ────────────────

test_that("the audit_start callback observes a completed statement", {
  on.exit({
    if (!is.null(tidyaudit:::the$active)) suppressMessages(audit_stop())
  }, add = TRUE)
  e <- new.env(parent = globalenv())
  suppressMessages(audit_start("sess", env = e))
  # Simulate the REPL completing one statement and firing the task callback.
  e$df <- data.frame(a = 1)
  tidyaudit:::the$active$handler(quote(df <- data.frame(a = 1)), e$df, TRUE, TRUE)
  trail <- suppressMessages(audit_stop())
  expect_equal(snap_by(trail, "df")$object_name, "df")
})

test_that("under audit_start, source() is observed as a single combined step", {
  # R evaluates `source(f)` as ONE top-level task, so the callback fires once
  # with that single expression — collapsing all of the script's objects into
  # one step. (audit_source() instead loops per statement; see its test above.)
  #
  # NOTE: this exercises the observe path deterministically; it does not drive
  # addTaskCallback() itself, which only fires from the live top-level REPL.
  # The end-to-end callback behaviour is confirmed by manual verification:
  #   audit_start(); source("multi_stmt.R"); audit_stop()  # -> one combined step
  # (and is documented in ?audit_start).
  f <- write_script(c(
    "a <- data.frame(x = 1)",
    "b <- data.frame(y = 2)",
    "d <- data.frame(z = 3)"
  ))
  on.exit(unlink(f))
  e <- new.env(parent = globalenv())
  trail <- audit_trail("src")
  tidyaudit:::.audit_init(trail, tidyaudit:::.audit_opts())
  source(f, local = e)
  tidyaudit:::.audit_observe_step(quote(source(f, local = e)), e, trail, trail$opts)

  expect_equal(length(trail$snapshots), 3L)   # all three captured
  step_ids <- vapply(trail$snapshots, function(s) s$step_id, character(1))
  expect_equal(length(unique(step_ids)), 1L)  # but as ONE step
})
