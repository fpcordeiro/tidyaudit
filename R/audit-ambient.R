# Audited execution: capture top-level data-frame lineage without per-step taps.
#
# Granularity is TOP-LEVEL STATEMENT lineage, not every transformation. A
# multi-verb pipe assigned in one statement is a single audited step; a loop
# yields one snapshot after it finishes. We do not instrument inside pipelines,
# loops, or function bodies. For intra-pipeline detail, the explicit taps
# (audit_tap(), *_join_tap(), filter_tap()) remain the tool and compose inside
# an audited run.
#
# Two step functions, never shared (a callback receives an already-evaluated
# expression — re-evaluating would duplicate side effects):
#   .audit_eval_step()    — owned evaluation, for audit_record() / audit_source()
#   .audit_observe_step() — observe only, for the audit_start() task callback
# Both feed the side-effect-free .audit_capture() diff/record core.

# Package-internal state for audit_start()/audit_stop().
the <- new.env(parent = emptyenv())
the$active <- NULL


# ── Options ──────────────────────────────────────────────────────────────────

#' @noRd
.audit_opts <- function(watch = "data.frames", ignore = NULL,
                        level = "metadata", profile = "standard",
                        keys = NULL, numeric_summary = TRUE,
                        continue_on_error = FALSE,
                        max_rows = Inf, max_cols = Inf) {
  list(
    watch = watch, ignore = ignore, level = level, profile = profile,
    keys = keys, numeric_summary = numeric_summary,
    continue_on_error = continue_on_error,
    max_rows = max_rows, max_cols = max_cols,
    # Per-run random salt: hashes are comparable within a run but not across
    # runs, and unsalted hashes of small categorical columns are dictionary-
    # attackable — so never present these as privacy-preserving.
    salt = as.character(sample.int(.Machine$integer.max, 1L))
  )
}

#' @noRd
.audit_init <- function(trail, opts) {
  trail$registry <- list()
  trail$events   <- list()
  trail$opts     <- opts
  trail$keys     <- opts$keys
  invisible(trail)
}

#' Sequential ID generator scoped to a trail (s1, o1, step1, ...).
#' @noRd
.audit_next_id <- function(trail, prefix) {
  key <- paste0(".n_", prefix)
  n <- (trail[[key]] %||% 0L) + 1L
  assign(key, n, envir = trail)
  paste0(prefix, n)
}


# ── Fingerprinting (metadata-only by default) ────────────────────────────────

#' @noRd
.audit_hash <- function(df, level, salt) {
  payload <- switch(
    level,
    sample_hash = {
      n <- nrow(df)
      idx <- if (n > 100L) unique(round(seq(1, n, length.out = 100L))) else seq_len(n)
      df[idx, , drop = FALSE]
    },
    column_hash = lapply(df, function(col) rlang::hash(list(salt, col))),
    full_hash   = df,
    df
  )
  rlang::hash(list(salt, payload))
}

#' Lightweight fingerprint used for change detection. Never copies rows unless
#' a hash `level` is requested. `profile` gates the cost of the NA scan.
#' @noRd
.audit_fingerprint <- function(df, profile = "standard", level = "metadata",
                               salt = NULL) {
  fp <- list(
    nrow     = nrow(df),
    ncol     = ncol(df),
    colnames = names(df),
    coltypes = vapply(df, function(x) class(x)[[1L]], character(1))
  )
  if (profile %in% c("standard", "deep")) {
    fp$total_nas <- sum(vapply(df, function(x) sum(is.na(x)), integer(1)))
  }
  if (!identical(level, "metadata")) {
    fp$hash <- .audit_hash(df, level, salt)
  }
  fp
}


# ── Static analysis of a top-level statement ─────────────────────────────────

#' Root binding of an assignment LHS: df$x -> "df", names(df) -> "df".
#' @noRd
.audit_root_symbol <- function(lhs) {
  while (is.call(lhs)) lhs <- lhs[[2L]]
  if (is.symbol(lhs)) as.character(lhs) else NULL
}

#' Assignment target(s) and removals for a statement.
#' Handles `<-`/`=`/`<<-`, replacement forms ($<-, [<-, names<-, attr<-, ...),
#' `assign("x", ...)`, and `rm(a, b)`.
#' @noRd
.audit_assign_targets <- function(expr) {
  targets <- character(0)
  removed <- character(0)
  if (is.call(expr) && is.symbol(expr[[1L]])) {
    op <- as.character(expr[[1L]])
    if (op %in% c("<-", "=", "<<-")) {
      root <- .audit_root_symbol(expr[[2L]])
      if (!is.null(root)) targets <- root
    } else if (op == "assign" && length(expr) >= 2L && is.character(expr[[2L]])) {
      targets <- expr[[2L]]
    } else if (op == "rm") {
      args <- as.list(expr)[-1L]
      removed <- unlist(lapply(args, function(a) {
        if (is.symbol(a)) as.character(a) else character(0)
      }))
    }
  }
  list(targets = targets, removed = removed %||% character(0))
}

#' Structured srcref (file/line/col), or NULL when unavailable.
#' @noRd
.audit_srcref <- function(expr) {
  sr <- attr(expr, "srcref")
  if (is.null(sr)) return(NULL)
  sf <- attr(sr, "srcfile")
  list(
    file  = if (!is.null(sf) && !is.null(sf$filename)) sf$filename else NA_character_,
    line1 = sr[1L], col1 = sr[2L], line2 = sr[3L], col2 = sr[4L]
  )
}


# ── Environment scanning + name filters ──────────────────────────────────────

#' @noRd
.audit_filter_names <- function(nms, opts) {
  if (length(nms) == 0L) return(nms)
  if (!is.null(opts$watch) && !identical(opts$watch, "data.frames")) {
    nms <- intersect(nms, opts$watch)
  }
  if (!is.null(opts$ignore) && length(opts$ignore) > 0L) {
    drop <- vapply(nms, function(n) {
      any(vapply(opts$ignore, function(p) grepl(p, n), logical(1)))
    }, logical(1))
    nms <- nms[!drop]
  }
  nms
}

#' Names of data.frames currently bound in `env` (dot-names excluded), filtered.
#' @noRd
.audit_env_dfs <- function(env, opts) {
  nms <- ls(env, all.names = FALSE)
  if (length(nms) == 0L) return(character(0))
  is_df <- vapply(nms, function(n) {
    is.data.frame(get(n, envir = env, inherits = FALSE))
  }, logical(1))
  .audit_filter_names(nms[is_df], opts)
}


# ── Parent (lineage) resolution ──────────────────────────────────────────────

#' Resolve parent snapshot IDs from the RHS symbols of a statement, using the
#' PRE-evaluation registry so self-overwrites link to the previous version.
#' Only tracked data.frames count, and a candidate that is a column of another
#' candidate is dropped (avoids column-name vs df-name collisions).
#' @noRd
.audit_resolve_parents <- function(rhs_syms, reg_before) {
  cand <- intersect(rhs_syms, names(reg_before))
  if (length(cand) == 0L) return(character(0))
  if (length(cand) > 1L) {
    is_col_of_other <- vapply(cand, function(n) {
      others <- setdiff(cand, n)
      any(vapply(others, function(o) {
        n %in% (reg_before[[o]]$fingerprint$colnames %||% character(0))
      }, logical(1)))
    }, logical(1))
    cand <- cand[!is_col_of_other]
  }
  ids <- vapply(cand, function(n) reg_before[[n]]$snapshot_id, character(1))
  unique(unname(ids))
}


# ── Snapshot construction + recording ────────────────────────────────────────

#' @noRd
.audit_unique_label <- function(trail, base) {
  lbl <- base
  if (lbl %in% trail$labels) {
    k <- 2L
    while (paste0(base, " (", k, ")") %in% trail$labels) k <- k + 1L
    lbl <- paste0(base, " (", k, ")")
  }
  lbl
}

#' Most recent existing snapshot for an object_id, or NULL.
#' @noRd
.audit_prev_snapshot <- function(trail, object_id) {
  snaps <- trail$snapshots
  if (length(snaps) == 0L) return(NULL)
  for (i in rev(seq_along(snaps))) {
    if (identical(snaps[[i]]$object_id, object_id)) return(snaps[[i]])
  }
  NULL
}

#' Lightweight terminal snapshot for delete/retire events: no live data, so
#' carry forward the last-known shape from the registry fingerprint.
#' @noRd
.audit_terminal_snap <- function(label, index, reg_entry, lineage) {
  fp <- reg_entry$fingerprint
  snap <- list(
    label           = label,
    index           = index,
    timestamp       = Sys.time(),
    type            = "tap",
    nrow            = if (!is.null(fp)) as.integer(fp$nrow) else NA_integer_,
    ncol            = if (!is.null(fp)) as.integer(fp$ncol) else NA_integer_,
    all_columns     = fp$colnames %||% character(0),
    schema          = data.frame(column = character(), type = character(),
                                 n_na = integer(), stringsAsFactors = FALSE),
    total_nas       = NA_integer_,
    numeric_summary = NULL,
    diagnostics     = NULL,
    pipeline        = NULL,
    changes         = NULL,
    custom          = NULL,
    controls        = NULL,
    snapshot_id         = lineage$snapshot_id,
    object_id           = lineage$object_id,
    object_name         = lineage$object_name,
    version             = lineage$version,
    step_id             = lineage$step_id,
    event               = lineage$event,
    source              = lineage$source,
    srcref              = lineage$srcref,
    parent_snapshot_ids = lineage$parent_snapshot_ids,
    level               = lineage$level
  )
  structure(snap, class = c("audit_snap", "list"))
}

#' Record one classified event onto the trail and update the live registry.
#' @noRd
.audit_record_event <- function(ev, parent_ids, reg_before, trail, opts,
                                step_id, source, srcref) {
  nm <- ev$name
  snapshot_id <- .audit_next_id(trail, "s")
  if (nm %in% names(reg_before)) {
    object_id <- reg_before[[nm]]$object_id
    version   <- reg_before[[nm]]$version + 1L
  } else {
    object_id <- .audit_next_id(trail, "o")
    version   <- 1L
  }
  index <- length(trail$snapshots) + 1L
  label <- .audit_unique_label(trail, nm)

  lineage <- list(
    snapshot_id = snapshot_id, object_id = object_id, object_name = nm,
    version = version, step_id = step_id, event = ev$event,
    source = source, srcref = srcref,
    parent_snapshot_ids = parent_ids, level = opts$level
  )

  if (ev$event %in% c("delete", "retire")) {
    snap <- .audit_terminal_snap(label, index, reg_before[[nm]], lineage)
  } else {
    snap <- .build_snapshot(ev$value, label = label, index = index,
                            .numeric_summary = opts$numeric_summary,
                            lineage = lineage)
    prev <- .audit_prev_snapshot(trail, object_id)
    if (!is.null(prev)) snap$changes <- .detect_changes(prev, snap)
  }

  trail$snapshots[[index]] <- snap
  trail$labels <- c(trail$labels, label)

  if (ev$event %in% c("delete", "retire")) {
    # End the binding stream: a later re-assignment starts a new object_id.
    trail$registry[[nm]] <- NULL
  } else {
    trail$registry[[nm]] <- list(object_id = object_id, snapshot_id = snapshot_id,
                                 version = version, fingerprint = ev$fingerprint)
  }
  invisible(snap)
}


# ── The diff/record core (side-effect free) ──────────────────────────────────

#' Diff `env` against `reg_before` after a statement and record snapshots.
#' Pure observation — never evaluates anything.
#' @noRd
.audit_capture <- function(expr, env, reg_before, trail, opts, step_id,
                           srcref = NULL, warnings = character(0),
                           messages = character(0), error = NULL) {
  src <- paste(deparse(expr), collapse = "\n")
  tgt <- .audit_assign_targets(expr)

  current_dfs <- .audit_env_dfs(env, opts)
  candidates  <- unique(c(current_dfs, names(reg_before), tgt$targets, tgt$removed))
  candidates  <- .audit_filter_names(candidates, opts)

  rhs_syms   <- all.vars(expr)
  parent_ids <- .audit_resolve_parents(rhs_syms, reg_before)

  events <- list()
  for (nm in candidates) {
    in_env  <- exists(nm, envir = env, inherits = FALSE)
    val     <- if (in_env) get(nm, envir = env, inherits = FALSE) else NULL
    is_df   <- in_env && is.data.frame(val)
    tracked <- nm %in% names(reg_before)
    target  <- nm %in% tgt$targets
    removed <- nm %in% tgt$removed

    if (is_df) {
      fp <- .audit_fingerprint(val, opts$profile, opts$level, opts$salt)
      if (!tracked) {
        events[[nm]] <- list(name = nm, event = "create", value = val,
                             fingerprint = fp)
      } else {
        changed <- !identical(fp, reg_before[[nm]]$fingerprint)
        event <- if (target && !changed) {
          "unchanged_assignment"
        } else if (target || changed) {
          "update"
        } else {
          NA_character_
        }
        if (!is.na(event)) {
          events[[nm]] <- list(name = nm, event = event, value = val,
                               fingerprint = fp)
        }
      }
    } else if (tracked) {
      event <- if (!in_env || removed) "delete" else "retire"
      events[[nm]] <- list(name = nm, event = event, value = NULL,
                           fingerprint = NULL)
    }
  }

  for (ev in events) {
    .audit_record_event(ev, parent_ids, reg_before, trail, opts,
                        step_id = step_id, source = src, srcref = srcref)
  }

  # Step-level record (errors/warnings/messages feed the HTML report).
  trail$events[[length(trail$events) + 1L]] <- list(
    step_id   = step_id,
    source    = src,
    srcref    = srcref,
    warnings  = warnings,
    messages  = messages,
    error     = error,
    snapshots = names(events)
  )
  invisible(names(events))
}


# ── Step functions ───────────────────────────────────────────────────────────

#' Owned evaluation step (audit_record / audit_source). Evaluates once, capturing
#' warnings/messages without suppressing them and errors via tryCatch; records,
#' then rethrows the error unless `continue_on_error`.
#' @noRd
.audit_eval_step <- function(expr, env, trail, opts) {
  reg_before <- trail$registry %||% list()
  srcref     <- .audit_srcref(expr)
  step_id    <- .audit_next_id(trail, "step")

  warnings <- character(0)
  messages <- character(0)
  err      <- NULL

  withCallingHandlers(
    tryCatch(
      eval(expr, envir = env),
      error = function(e) err <<- conditionMessage(e)
    ),
    warning = function(w) warnings <<- c(warnings, conditionMessage(w)),
    message = function(m) messages <<- c(messages, conditionMessage(m))
  )

  .audit_capture(expr, env, reg_before, trail, opts, step_id = step_id,
                 srcref = srcref, warnings = warnings, messages = messages,
                 error = err)

  if (!is.null(err) && !isTRUE(opts$continue_on_error)) {
    stop(err, call. = FALSE)
  }
  invisible(NULL)
}

#' Observe-only step (audit_start task callback). The REPL already evaluated the
#' statement — this NEVER re-evaluates it.
#' @noRd
.audit_observe_step <- function(expr, env, trail, opts) {
  reg_before <- trail$registry %||% list()
  step_id    <- .audit_next_id(trail, "step")
  tryCatch(
    .audit_capture(expr, env, reg_before, trail, opts, step_id = step_id,
                   srcref = .audit_srcref(expr)),
    error = function(e) NULL  # capture must never break the REPL
  )
  invisible(NULL)
}

#' Snapshot pre-existing data.frames as baseline `create` events (no parents).
#' @noRd
.audit_baseline <- function(env, trail, opts) {
  dfs <- .audit_env_dfs(env, opts)
  if (length(dfs) == 0L) return(invisible(NULL))
  step_id <- .audit_next_id(trail, "step")
  for (nm in dfs) {
    val <- get(nm, envir = env, inherits = FALSE)
    fp  <- .audit_fingerprint(val, opts$profile, opts$level, opts$salt)
    .audit_record_event(
      list(name = nm, event = "create", value = val, fingerprint = fp),
      parent_ids = character(0), reg_before = list(), trail = trail, opts = opts,
      step_id = step_id, source = "<pre-existing>", srcref = NULL
    )
  }
  invisible(NULL)
}


# ── Exported: audit_record ───────────────────────────────────────────────────

#' Record Data-Frame Lineage for a Block of Code
#'
#' Evaluates a block of top-level statements and records a versioned audit trail
#' of every data.frame created or changed along the way — without per-step taps.
#' Capture granularity is **top-level statement lineage**: a multi-verb pipe
#' assigned in one statement is a single step, and a loop yields one snapshot
#' after it. For intra-pipeline detail, use the explicit taps ([audit_tap()],
#' `*_join_tap()`, [filter_tap()]), which compose inside an audited run.
#'
#' Capture is metadata-only by default (shape, types, NA counts); raw rows never
#' enter the trail unless a hash `level` above `"metadata"` is requested.
#'
#' @param expr A braced block of statements, e.g. `{ x <- ...; y <- ... }`.
#'   Captured unevaluated and evaluated statement by statement in `env`.
#' @param name Optional trail name. If `NULL`, a timestamped name is generated.
#' @param env Environment in which to evaluate the block. Defaults to the
#'   caller's environment.
#' @param watch Either `"data.frames"` (the default — track every data.frame)
#'   or a character vector of object names to restrict tracking to.
#' @param ignore Optional character vector of regular expressions; objects whose
#'   names match any pattern are skipped (e.g. scratch variables).
#' @param level Evidence level: `"metadata"` (default, privacy-safe) detects
#'   shape/type/NA changes only; `"sample_hash"`, `"column_hash"`, and
#'   `"full_hash"` additionally detect value-only changes by hashing data with a
#'   per-run salt. Salted hashes are *not* a privacy guarantee.
#' @param profile Cost profile for fingerprinting: `"cheap"` (shape + types),
#'   `"standard"` (default; adds NA counts), or `"deep"`.
#' @param keys Optional named list mapping object names to key column(s), used by
#'   the HTML report to flag primary-key status.
#' @param numeric_summary Logical; passed to the snapshot builder. If `FALSE`,
#'   skip numeric quantile summaries.
#' @param continue_on_error Logical. If `FALSE` (default), an error in a
#'   statement is recorded and then re-thrown so the block aborts like normal R
#'   evaluation. If `TRUE`, the error is recorded and evaluation continues.
#'
#' @returns An [audit_trail()] populated with versioned, lineage-aware snapshots.
#'
#' @examples
#' trail <- audit_record({
#'   raw    <- dplyr::as_tibble(mtcars)
#'   clean  <- dplyr::filter(raw, mpg > 20)
#'   joined <- dplyr::left_join(clean,
#'                              data.frame(cyl = c(4, 6, 8)), by = "cyl")
#' })
#' print(trail)
#'
#' @family audited execution
#' @seealso [audit_source()], [audit_start()]
#' @export
audit_record <- function(expr, name = NULL, env = parent.frame(),
                         watch = "data.frames", ignore = NULL,
                         level = c("metadata", "sample_hash", "column_hash", "full_hash"),
                         profile = c("standard", "cheap", "deep"),
                         keys = NULL, numeric_summary = TRUE,
                         continue_on_error = FALSE) {
  level   <- match.arg(level)
  profile <- match.arg(profile)
  captured <- substitute(expr)

  opts  <- .audit_opts(watch = watch, ignore = ignore, level = level,
                       profile = profile, keys = keys,
                       numeric_summary = numeric_summary,
                       continue_on_error = continue_on_error)
  trail <- audit_trail(name)
  .audit_init(trail, opts)
  .audit_baseline(env, trail, opts)

  stmts <- if (is.call(captured) && identical(captured[[1L]], as.name("{"))) {
    as.list(captured)[-1L]
  } else {
    list(captured)
  }
  for (st in stmts) .audit_eval_step(st, env, trail, opts)

  trail
}
