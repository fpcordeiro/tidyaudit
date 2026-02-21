#' Record a Pipeline Snapshot
#'
#' Transparent pipe pass-through that captures a metadata snapshot and appends
#' it to an audit trail. Returns `.data` unchanged — the function's only purpose
#' is its side effect on `.trail`.
#'
#' @param .data A data.frame or tibble flowing through the pipe.
#' @param .trail An [audit_trail()] object.
#' @param label Optional character label for this snapshot. If `NULL`, an
#'   auto-generated label like `"step_1"` is used.
#' @param .fns Optional named list of diagnostic functions (or formula lambdas)
#'   to run on `.data`. Results are stored in the snapshot.
#'
#' @returns `.data`, unchanged, returned invisibly. The function is a
#'   transparent pass-through; its only effect is the side effect on `.trail`.
#'
#' @examples
#' trail <- audit_trail("example")
#' result <- mtcars |>
#'   audit_tap(trail, "raw") |>
#'   dplyr::filter(mpg > 20) |>
#'   audit_tap(trail, "filtered")
#' print(trail)
#'
#' @family audit trail
#' @export
audit_tap <- function(.data, .trail, label = NULL, .fns = NULL) {
  data_expr <- substitute(.data)

  # Validate arguments that don't require .data evaluation
  if (!inherits(.trail, "audit_trail")) {
    cli::cli_abort("{.arg .trail} must be an {.cls audit_trail} object.")
  }
  if (!is.null(label)) {
    if (!is.character(label) || length(label) != 1L || is.na(label)) {
      cli::cli_abort("{.arg label} must be a single character string or NULL.")
    }
  }
  if (!is.null(.fns) && !is.list(.fns)) {
    cli::cli_abort("{.arg .fns} must be a named list of functions or formulas.")
  }

  # Force .data evaluation — with nested |> pipes, inner audit_tap()
  # calls execute as a side effect of forcing .data. We must force before
  # computing index or checking labels so that the trail state reflects
  # all inner taps.
  force(.data)

  if (!is.data.frame(.data)) {
    cli::cli_abort("{.arg .data} must be a data.frame or tibble, not {.cls {class(.data)}}.")
  }

  # Auto-generate label if not provided
  if (is.null(label)) {
    label <- paste0("step_", length(.trail$snapshots) + 1L)
  }

  # Check label uniqueness
  if (label %in% .trail$labels) {
    trail_name <- .trail$name
    cli::cli_abort("Label {.val {label}} already exists in trail {.val {trail_name}}.")
  }

  # Build snapshot (index computed after forcing .data)
  index <- length(.trail$snapshots) + 1L
  snap <- .build_snapshot(.data, label = label, index = index)

  # Capture pipeline (best-effort)
  snap$pipeline <- tryCatch(.capture_pipeline(data_expr), error = function(e) NULL)

  # Run custom diagnostic functions if provided
  if (!is.null(.fns)) {
    # Auto-name unnamed entries so they appear in report output
    if (is.null(names(.fns))) {
      names(.fns) <- paste0("fn_", seq_along(.fns))
    } else {
      empty <- is.na(names(.fns)) | names(.fns) == ""
      if (any(empty)) {
        names(.fns)[empty] <- paste0("fn_", which(empty))
      }
    }
    snap$custom <- lapply(.fns, function(fn) {
      fn <- rlang::as_function(fn)
      fn(.data)
    })
  }

  # Smart change detection (compare to previous snapshot)
  if (index > 1L) {
    prev <- .trail$snapshots[[index - 1L]]
    snap$changes <- .detect_changes(prev, snap)
  }

  # Mutate trail (reference semantics — side effect)
  .trail$snapshots[[index]] <- snap
  .trail$labels <- c(.trail$labels, label)

  # Return data unchanged, invisibly (side-effect-only function)
  invisible(.data)
}

#' Detect Changes Between Two Consecutive Snapshots
#'
#' Computes row, column, and NA deltas between two snapshots.
#'
#' @param prev Previous `audit_snap`.
#' @param curr Current `audit_snap`.
#'
#' @returns A list of change metrics.
#'
#' @noRd
.detect_changes <- function(prev, curr) {
  list(
    row_delta    = curr$nrow - prev$nrow,
    col_delta    = curr$ncol - prev$ncol,
    na_delta     = curr$total_nas - prev$total_nas,
    cols_added   = setdiff(curr$col_info$column, prev$col_info$column),
    cols_removed = setdiff(prev$col_info$column, curr$col_info$column),
    type_changes = .detect_type_changes(prev$col_info, curr$col_info)
  )
}

#' Detect Column Type Changes Between Snapshots
#'
#' @param prev_col_info Column info data.frame from previous snapshot.
#' @param curr_col_info Column info data.frame from current snapshot.
#'
#' @returns A data.frame of type changes, or `NULL` if none.
#'
#' @noRd
.detect_type_changes <- function(prev_col_info, curr_col_info) {
  common <- intersect(prev_col_info$column, curr_col_info$column)
  if (length(common) == 0L) return(NULL)

  prev_types <- setNames(prev_col_info$type, prev_col_info$column)[common]
  curr_types <- setNames(curr_col_info$type, curr_col_info$column)[common]

  changed <- common[prev_types != curr_types]
  if (length(changed) == 0L) return(NULL)

  data.frame(
    column = changed,
    from   = prev_types[changed],
    to     = curr_types[changed],
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}
