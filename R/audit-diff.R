#' Compare Two Audit Trail Snapshots
#'
#' Computes detailed differences between any two snapshots in an audit trail,
#' including row/column/NA deltas, columns added/removed, type changes,
#' per-column NA changes, and numeric distribution shifts.
#'
#' @param .trail An [audit_trail()] object.
#' @param from Label (character) or index (integer) of the first snapshot.
#' @param to Label (character) or index (integer) of the second snapshot.
#'
#' @return An `audit_diff` object (S3 list).
#'
#' @examples
#' trail <- audit_trail("example")
#' mtcars |>
#'   audit_tap(trail, "raw") |>
#'   dplyr::filter(mpg > 20) |>
#'   audit_tap(trail, "filtered")
#' audit_diff(trail, "raw", "filtered")
#'
#' @export
audit_diff <- function(.trail, from, to) {
  if (!inherits(.trail, "audit_trail")) {
    cli::cli_abort("{.arg .trail} must be an {.cls audit_trail} object.")
  }

  snap_from <- .resolve_snapshot(.trail, from)
  snap_to   <- .resolve_snapshot(.trail, to)

  diff_obj <- list(
    from_label     = snap_from$label,
    to_label       = snap_to$label,
    from_nrow      = snap_from$nrow,
    to_nrow        = snap_to$nrow,
    from_ncol      = snap_from$ncol,
    to_ncol        = snap_to$ncol,
    from_nas       = snap_from$total_nas,
    to_nas         = snap_to$total_nas,
    row_delta      = snap_to$nrow - snap_from$nrow,
    col_delta      = snap_to$ncol - snap_from$ncol,
    na_delta       = snap_to$total_nas - snap_from$total_nas,
    cols_added     = setdiff(snap_to$col_info$column, snap_from$col_info$column),
    cols_removed   = setdiff(snap_from$col_info$column, snap_to$col_info$column),
    type_changes   = .detect_type_changes(snap_from$col_info, snap_to$col_info),
    na_changes     = .compare_na_counts(snap_from$col_info, snap_to$col_info),
    numeric_shifts = .compare_numeric_summaries(snap_from, snap_to)
  )
  structure(diff_obj, class = c("audit_diff", "list"))
}

#' Resolve a Snapshot by Label or Index
#'
#' @param .trail An audit trail.
#' @param ref Character label or integer index.
#'
#' @return The matching `audit_snap` object.
#'
#' @noRd
.resolve_snapshot <- function(.trail, ref) {
  if (is.character(ref)) {
    idx <- match(ref, .trail$labels)
    if (is.na(idx)) {
      trail_name <- .trail$name
      cli::cli_abort("Label {.val {ref}} not found in trail {.val {trail_name}}.")
    }
    return(.trail$snapshots[[idx]])
  }

  if (is.numeric(ref)) {
    if (length(ref) != 1L || is.na(ref) || !is.finite(ref) || ref != trunc(ref)) {
      cli::cli_abort("{.arg from}/{.arg to} must be a whole number or character label, not {.val {ref}}.")
    }
    ref <- as.integer(ref)
    if (ref < 1L || ref > length(.trail$snapshots)) {
      n <- length(.trail$snapshots)
      cli::cli_abort("Index {.val {ref}} out of range (trail has {n} snapshots).")
    }
    return(.trail$snapshots[[ref]])
  }

  cli::cli_abort("{.arg from}/{.arg to} must be a character label or integer index.")
}

#' Compare NA Counts Between Two Snapshots
#'
#' @param from_col_info Column info from the source snapshot.
#' @param to_col_info Column info from the target snapshot.
#'
#' @return A data.frame of per-column NA changes for common columns, or `NULL`.
#'
#' @noRd
.compare_na_counts <- function(from_col_info, to_col_info) {
  common <- intersect(from_col_info$column, to_col_info$column)
  if (length(common) == 0L) return(NULL)

  from_nas <- setNames(from_col_info$n_na, from_col_info$column)[common]
  to_nas   <- setNames(to_col_info$n_na, to_col_info$column)[common]
  delta    <- to_nas - from_nas

  changed <- common[delta != 0L]
  if (length(changed) == 0L) return(NULL)

  data.frame(
    column  = changed,
    from_na = from_nas[changed],
    to_na   = to_nas[changed],
    delta   = delta[changed],
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

#' Compare Numeric Summaries Between Two Snapshots
#'
#' @param snap_from Source snapshot.
#' @param snap_to Target snapshot.
#'
#' @return A data.frame of numeric distribution shifts, or `NULL`.
#'
#' @noRd
.compare_numeric_summaries <- function(snap_from, snap_to) {
  ns_from <- snap_from$numeric_summary
  ns_to   <- snap_to$numeric_summary
  if (is.null(ns_from) || is.null(ns_to)) return(NULL)

  common <- intersect(ns_from$column, ns_to$column)
  if (length(common) == 0L) return(NULL)

  from_sub <- ns_from[ns_from$column %in% common, , drop = FALSE]
  to_sub   <- ns_to[ns_to$column %in% common, , drop = FALSE]

  # Align by column order
  from_sub <- from_sub[match(common, from_sub$column), , drop = FALSE]
  to_sub   <- to_sub[match(common, to_sub$column), , drop = FALSE]

  data.frame(
    column      = common,
    mean_before = from_sub$mean,
    mean_after  = to_sub$mean,
    mean_shift  = to_sub$mean - from_sub$mean,
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

#' @export
print.audit_diff <- function(x, ...) {
  cli::cli_h2("Audit Diff: {.val {x$from_label}} \u2192 {.val {x$to_label}}")

  # Dimension changes as aligned table
  delta_tbl <- data.frame(
    Metric = c("Rows", "Cols", "NAs"),
    Before = c(format(x$from_nrow, big.mark = ",", trim = TRUE),
               as.character(x$from_ncol),
               format(x$from_nas, big.mark = ",", trim = TRUE)),
    After  = c(format(x$to_nrow, big.mark = ",", trim = TRUE),
               as.character(x$to_ncol),
               format(x$to_nas, big.mark = ",", trim = TRUE)),
    Delta  = c(.format_delta(x$row_delta), .format_delta(x$col_delta), .format_delta(x$na_delta)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  .cli_table(delta_tbl, right_align = c("Before", "After", "Delta"))

  # Columns added/removed
  cli::cli_text("")
  if (length(x$cols_added) > 0L) {
    cli::cli_alert_success("Columns added: {.field {x$cols_added}}")
  }
  if (length(x$cols_removed) > 0L) {
    cli::cli_alert_danger("Columns removed: {.field {x$cols_removed}}")
  }
  if (length(x$cols_added) == 0L && length(x$cols_removed) == 0L) {
    cli::cli_alert_info("No columns added or removed")
  }

  # Type changes
  if (!is.null(x$type_changes)) {
    cli::cli_text("")
    cli::cli_text("{.strong Type changes:}")
    bullets <- setNames(
      vapply(seq_len(nrow(x$type_changes)), function(i) {
        tc <- x$type_changes[i, ]
        glue::glue("{tc$column}: {tc$from} \u2192 {tc$to}")
      }, character(1)),
      rep("*", nrow(x$type_changes))
    )
    cli::cli_bullets(bullets)
  }

  # NA changes
  if (!is.null(x$na_changes)) {
    cli::cli_text("")
    cli::cli_text("{.strong NA changes:}")
    bullets <- setNames(
      vapply(seq_len(nrow(x$na_changes)), function(i) {
        nc <- x$na_changes[i, ]
        delta_str <- .format_delta(nc$delta)
        paste0(nc$column, ": ", nc$from_na, " \u2192 ", nc$to_na, " (", delta_str, ")")
      }, character(1)),
      rep("*", nrow(x$na_changes))
    )
    cli::cli_bullets(bullets)
  }

  # Numeric shifts
  if (!is.null(x$numeric_shifts)) {
    cli::cli_text("")
    cli::cli_text("{.strong Numeric shifts (common columns):}")

    ns <- x$numeric_shifts
    shift_tbl <- data.frame(
      Column        = ns$column,
      `Mean before` = format(round(ns$mean_before, 2), big.mark = ",", trim = TRUE),
      `Mean after`  = format(round(ns$mean_after, 2), big.mark = ",", trim = TRUE),
      Shift         = .format_delta_numeric(ns$mean_shift),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    .cli_table(shift_tbl, right_align = c("Mean before", "Mean after", "Shift"), indent = 4L)
  }

  invisible(x)
}

#' Format an integer delta with sign prefix
#' @noRd
.format_delta <- function(delta) {
  if (delta > 0) {
    paste0("+", format(delta, big.mark = ",", trim = TRUE))
  } else if (delta < 0) {
    format(delta, big.mark = ",", trim = TRUE)
  } else {
    "="
  }
}

#' Format a numeric (double) delta with sign prefix
#' @noRd
.format_delta_numeric <- function(shifts) {
  vapply(shifts, function(d) {
    if (is.na(d)) return("NA")
    formatted <- format(round(d, 2), big.mark = ",", trim = TRUE)
    if (d > 0) paste0("+", formatted) else formatted
  }, character(1))
}
