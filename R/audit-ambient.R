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
                        level = "metadata", keys = NULL,
                        numeric_summary = TRUE, continue_on_error = FALSE) {
  list(
    watch = watch, ignore = ignore, level = level,
    keys = keys, numeric_summary = numeric_summary,
    continue_on_error = continue_on_error,
    # Per-run salt: hashes are comparable within a run but not across runs.
    # Derived from the clock + PID rather than the RNG, so enabling auditing
    # never perturbs the user's `.Random.seed` (reproducibility is preserved).
    # Unsalted hashes of small categorical columns are dictionary-attackable,
    # so these are never presented as privacy-preserving.
    salt = paste0(format(Sys.time(), "%Y%m%d%H%M%OS6"), "-", Sys.getpid())
  )
}

#' Describe the hashing evidence captured at a given level, for serialisation.
#' @noRd
.audit_evidence <- function(level, hash) {
  if (identical(level, "metadata")) return(NULL)
  list(
    algorithm   = "xxhash (rlang::hash)",
    level       = level,
    sample      = if (identical(level, "sample_hash")) {
      "deterministic stride, up to 100 rows"
    } else {
      "all rows and columns"
    },
    salt_policy = "per-run (clock + PID); not privacy-preserving",
    hash        = hash
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

#' Lightweight fingerprint used for change detection. Records shape, types and
#' NA counts; copies rows only when a hash `level` above `"metadata"` is
#' requested.
#' @noRd
.audit_fingerprint <- function(df, level = "metadata", salt = NULL) {
  fp <- list(
    nrow      = nrow(df),
    ncol      = ncol(df),
    colnames  = names(df),
    coltypes  = vapply(df, function(x) class(x)[[1L]], character(1)),
    total_nas = sum(vapply(df, function(x) sum(is.na(x)), integer(1)))
  )
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

#' Assignment target(s), removals, the right-hand-side value expression, and
#' whether the assignment modifies an object in place (replacement form).
#' Handles `<-`/`=`/`<<-`, replacement forms ($<-, [<-, names<-, attr<-, ...),
#' `assign("x", ...)`, and `rm(a, b)`.
#' @noRd
.audit_assign_targets <- function(expr) {
  targets <- character(0)
  removed <- character(0)
  value   <- NULL
  inplace <- FALSE
  if (is.call(expr) && is.symbol(expr[[1L]])) {
    op <- as.character(expr[[1L]])
    if (op %in% c("<-", "=", "<<-")) {
      lhs  <- expr[[2L]]
      root <- .audit_root_symbol(lhs)
      if (!is.null(root)) targets <- root
      inplace <- is.call(lhs)        # f(x, ...) <- v modifies x in place
      value   <- expr[[3L]]
    } else if (op == "assign" && length(expr) >= 2L && is.character(expr[[2L]])) {
      targets <- expr[[2L]]
      if (length(expr) >= 3L) value <- expr[[3L]]
    } else if (op == "rm") {
      args <- as.list(expr)[-1L]
      removed <- unlist(lapply(args, function(a) {
        if (is.symbol(a)) as.character(a) else character(0)
      }))
    }
  }
  list(targets = targets, removed = removed %||% character(0),
       value = value, inplace = inplace)
}

# Arithmetic / comparison / logical operators and the formula operator. Their
# operands are values inside a data mask, never data-frame inputs, so we do not
# descend into them when collecting candidate parents.
.audit_operator_ops <- c(">", "<", ">=", "<=", "==", "!=", "+", "-", "*", "/",
                         "^", "%%", "%/%", "&", "|", "&&", "||", "!", ":", "~",
                         "%in%")

# Extraction / element-access operators. The object being extracted from is a
# candidate parent (its root), but the column name or index is not.
.audit_extract_ops <- c("$", "[", "[[", "@")

#' Collect candidate parent symbols from a right-hand-side value expression,
#' context-aware about tidy-eval data masks and element extraction:
#'
#' * Operators (`mpg > 20`, `x + 1`): skipped — operands are masked values.
#' * Extraction (`raw[i, ]`, `lookup$v`): only the root object is collected,
#'   not the column/index.
#' * Ordinary calls: positional (unnamed) arguments are data candidates; named
#'   arguments are treated as data-masked expressions (e.g. `z = mpg` in
#'   `mutate(raw, z = mpg)`) and skipped.
#' @noRd
.audit_value_symbols <- function(value_expr) {
  if (is.null(value_expr)) return(character(0))
  syms <- character(0)
  add  <- function(s) syms[[length(syms) + 1L]] <<- s
  walk <- function(e) {
    if (is.symbol(e)) { add(as.character(e)); return(invisible()) }
    if (!is.call(e)) return(invisible())
    fn_name <- if (is.symbol(e[[1L]])) as.character(e[[1L]]) else ""
    if (fn_name %in% .audit_operator_ops) return(invisible())
    if (fn_name %in% .audit_extract_ops) {
      if (length(e) >= 2L) walk(e[[2L]])   # root object only
      return(invisible())
    }
    args <- as.list(e)[-1L]
    nms  <- names(args)
    for (i in seq_along(args)) {
      if (!is.null(nms) && nzchar(nms[[i]])) next   # named arg: data-masked
      a <- args[[i]]
      if (is.symbol(a)) add(as.character(a))
      else if (is.call(a)) walk(a)
    }
    invisible()
  }
  walk(value_expr)
  unique(syms)
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

#' Resolve parent snapshot IDs from candidate RHS symbols, against the
#' PRE-evaluation registry (so self-overwrites link to the previous version).
#' Only symbols that were tracked data.frames before the statement ran count.
#' @noRd
.audit_resolve_parents <- function(value_syms, reg_before) {
  cand <- intersect(value_syms, names(reg_before))
  if (length(cand) == 0L) return(character(0))
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
    # Carry forward the last-known NA count so report paths stay numeric.
    total_nas       = if (!is.null(fp) && !is.null(fp$total_nas)) {
      as.integer(fp$total_nas)
    } else {
      NA_integer_
    },
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
    level               = lineage$level,
    evidence            = lineage$evidence
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
    parent_snapshot_ids = parent_ids, level = opts$level,
    evidence = .audit_evidence(opts$level, ev$fingerprint$hash)
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

  # Candidate parents: data.frames referenced in the RHS value expression only,
  # so the assignment target is never treated as its own parent unless the value
  # genuinely reads it (a self-overwrite).
  step_parents <- .audit_resolve_parents(.audit_value_symbols(tgt$value), reg_before)

  events <- list()
  for (nm in candidates) {
    in_env  <- exists(nm, envir = env, inherits = FALSE)
    val     <- if (in_env) get(nm, envir = env, inherits = FALSE) else NULL
    is_df   <- in_env && is.data.frame(val)
    tracked <- nm %in% names(reg_before)
    target  <- nm %in% tgt$targets
    removed <- nm %in% tgt$removed

    if (is_df) {
      fp <- .audit_fingerprint(val, opts$level, opts$salt)
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
    nm      <- ev$name
    self_id <- reg_before[[nm]]$snapshot_id   # NULL when the object is new
    if (ev$event %in% c("delete", "retire")) {
      parents <- if (!is.null(self_id)) self_id else character(0)
    } else {
      parents <- step_parents
      # In-place modification (replacement form, or a tracked df that changed
      # without being the rebind target) derives from its own previous version.
      inplace_mod <- (nm %in% tgt$targets && isTRUE(tgt$inplace)) ||
        (ev$event == "update" && !(nm %in% tgt$targets))
      if (inplace_mod && !is.null(self_id)) parents <- unique(c(parents, self_id))
    }
    .audit_record_event(ev, parents, reg_before, trail, opts,
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
    fp  <- .audit_fingerprint(val, opts$level, opts$salt)
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
#'   per-run salt. Salted hashes are *not* a privacy guarantee. The hashing
#'   policy (algorithm, sampling, salt) is recorded in each snapshot's
#'   `evidence` field.
#' @param keys Optional named list mapping object names to key column(s), used by
#'   the HTML report to flag primary-key status.
#' @param numeric_summary Logical; passed to the snapshot builder. If `FALSE`,
#'   skip numeric quantile summaries (the main cost control on wide data).
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
                         keys = NULL, numeric_summary = TRUE,
                         continue_on_error = FALSE) {
  level   <- match.arg(level)
  captured <- substitute(expr)

  opts  <- .audit_opts(watch = watch, ignore = ignore, level = level,
                       keys = keys, numeric_summary = numeric_summary,
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


# ── Exported: audit_source ───────────────────────────────────────────────────

#' Audit a Script File End to End
#'
#' The canonical script runner: parses an `.R` file, evaluates it one top-level
#' statement at a time, and records a versioned audit trail of every data.frame
#' created or changed — like [base::source()] but returning an [audit_trail()].
#' Because the evaluation loop is owned by tidyaudit, capture works in every
#' context (interactive, `source()`d, or `Rscript`), unlike [audit_start()].
#'
#' Capture granularity is **top-level statement lineage** (see [audit_record()]).
#'
#' @inheritParams audit_record
#' @param file Path to an `.R` script.
#' @param env Environment in which to evaluate the script. Defaults to a fresh
#'   child of the global environment so the script's objects do not clobber your
#'   workspace; pass `globalenv()` for `source()`-style behaviour.
#' @param echo Logical. If `TRUE`, echo each statement before evaluating it.
#'
#' @returns An [audit_trail()] populated with versioned, lineage-aware snapshots.
#'
#' @examples
#' tmp <- tempfile(fileext = ".R")
#' writeLines(c(
#'   "raw   <- dplyr::as_tibble(mtcars)",
#'   "clean <- dplyr::filter(raw, mpg > 20)"
#' ), tmp)
#' trail <- audit_source(tmp)
#' print(trail)
#'
#' @family audited execution
#' @seealso [audit_record()], [audit_start()]
#' @export
audit_source <- function(file, name = NULL,
                         env = new.env(parent = globalenv()),
                         watch = "data.frames", ignore = NULL,
                         level = c("metadata", "sample_hash", "column_hash", "full_hash"),
                         keys = NULL, numeric_summary = TRUE,
                         continue_on_error = FALSE, echo = FALSE) {
  level <- match.arg(level)

  if (!is.character(file) || length(file) != 1L || is.na(file)) {
    cli::cli_abort("{.arg file} must be a single non-missing character string.")
  }
  if (!file.exists(file)) {
    cli::cli_abort("File not found: {.path {file}}")
  }

  exprs   <- parse(file, keep.source = TRUE)
  srcrefs <- attr(exprs, "srcref")

  opts  <- .audit_opts(watch = watch, ignore = ignore, level = level,
                       keys = keys, numeric_summary = numeric_summary,
                       continue_on_error = continue_on_error)
  trail <- audit_trail(name %||% basename(file))
  .audit_init(trail, opts)
  .audit_baseline(env, trail, opts)

  for (i in seq_along(exprs)) {
    st <- exprs[[i]]
    if (!is.null(srcrefs) && length(srcrefs) >= i) {
      attr(st, "srcref") <- srcrefs[[i]]
    }
    if (isTRUE(echo)) cat(deparse(st), sep = "\n")
    .audit_eval_step(st, env, trail, opts)
  }

  trail
}


# ── Exported: audit_start / audit_stop ───────────────────────────────────────

#' Audit an Interactive Session
#'
#' Begins ambient capture in an **interactive session** (or a script run
#' directly with `Rscript file.R`): registers a top-level task callback that
#' records a snapshot after each statement you run, until [audit_stop()].
#'
#' This is a convenience wrapper, not the canonical script runner. Task
#' callbacks fire per top-level statement at the REPL, but R treats
#' `source("file.R")` as a **single** task — so running a script via `source()`
#' under `audit_start()` records only one combined step. **For scripts, use
#' [audit_source()].**
#'
#' The capture handler only *observes* completed statements; it never
#' re-evaluates them, so side effects are not duplicated. Capture errors are
#' swallowed so they can never break your REPL.
#'
#' @inheritParams audit_record
#' @param env Environment to watch. Defaults to the global environment.
#'
#' @returns [audit_start()] returns the new [audit_trail()] invisibly;
#'   [audit_stop()] returns the completed trail.
#'
#' @examples
#' \dontrun{
#' audit_start("session")
#' raw   <- dplyr::as_tibble(mtcars)
#' clean <- dplyr::filter(raw, mpg > 20)
#' trail <- audit_stop()
#' print(trail)
#' }
#'
#' @family audited execution
#' @seealso [audit_source()], [audit_record()]
#' @export
audit_start <- function(name = NULL, env = globalenv(),
                        watch = "data.frames", ignore = NULL,
                        level = c("metadata", "sample_hash", "column_hash", "full_hash"),
                        keys = NULL, numeric_summary = TRUE) {
  level <- match.arg(level)

  if (!is.null(the$active)) {
    cli::cli_abort(c(
      "An audit session is already active.",
      "i" = "Call {.fn audit_stop} before starting a new one."
    ))
  }

  opts  <- .audit_opts(watch = watch, ignore = ignore, level = level,
                       keys = keys, numeric_summary = numeric_summary)
  trail <- audit_trail(name)
  .audit_init(trail, opts)
  .audit_baseline(env, trail, opts)

  handler <- function(expr, value, ok, visible) {
    .audit_observe_step(expr, env, trail, opts)
    TRUE  # keep the callback registered
  }
  addTaskCallback(handler, name = "tidyaudit")
  the$active <- list(trail = trail, env = env, opts = opts, handler = handler)

  cli::cli_alert_success("Audit session started. Call {.fn audit_stop} to finish.")
  invisible(trail)
}

#' @rdname audit_start
#' @export
audit_stop <- function() {
  if (is.null(the$active)) {
    cli::cli_abort(c(
      "No active audit session.",
      "i" = "Call {.fn audit_start} first."
    ))
  }
  removeTaskCallback("tidyaudit")
  trail <- the$active$trail
  the$active <- NULL
  n <- length(trail$snapshots)
  cli::cli_alert_success("Audit session stopped ({n} snapshot{?s} recorded).")
  trail
}
