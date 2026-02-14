#' Operation-Aware Join Taps
#'
#' Perform a dplyr join AND record enriched diagnostics in an audit trail.
#' These functions replace the pattern of wrapping a join with two
#' [audit_tap()] calls, capturing information that plain taps cannot:
#' match rates, relationship type, duplicate keys, and unmatched row counts.
#'
#' @param .data A data.frame or tibble (left table in the join).
#' @param y A data.frame or tibble (right table in the join).
#' @param ... Arguments passed to the corresponding `dplyr::*_join()` function,
#'   including `by`, `suffix`, `keep`, `multiple`, `unmatched`, etc. The `by`
#'   argument should be passed by name for enriched diagnostics.
#' @param .trail An [audit_trail()] object, or `NULL` (the default). When
#'   `NULL`, behavior depends on `.stat`: if `.stat` is also `NULL`, a plain
#'   dplyr join is performed; if `.stat` is provided, [validate_join()]
#'   diagnostics are printed before the join.
#' @param .label Optional character label for this snapshot. If `NULL`,
#'   auto-generated as `"left_join_1"` etc.
#' @param .stat Optional column name (string) for stat tracking, passed to
#'   [validate_join()].
#'
#' @details
#' Enriched diagnostics (match rates, relationship type, duplicate keys) require
#' equality joins — `by` as a character vector, named character vector, or
#' simple equality `join_by()` expression (e.g., `join_by(id)`,
#' `join_by(a == b)`). For non-equi `join_by()` expressions, the tap records
#' a basic snapshot without match-rate diagnostics.
#'
#' All dplyr join features (`join_by`, `multiple`, `unmatched`, `suffix`, etc.)
#' work unchanged via `...`.
#'
#' When `.trail` is `NULL`:
#' \itemize{
#'   \item `.stat` also `NULL`: plain dplyr join
#'   \item `.stat` provided: prints [validate_join()] diagnostics, then joins
#'   \item `.label` provided: warns that label is ignored
#' }
#'
#' @returns The joined data.frame or tibble (same as the corresponding
#'   `dplyr::*_join()`).
#'
#' @examples
#' orders <- data.frame(id = 1:4, amount = c(100, 200, 300, 400))
#' customers <- data.frame(id = c(2, 3, 5), name = c("A", "B", "C"))
#'
#' # With trail
#' trail <- audit_trail("join_example")
#' result <- orders |>
#'   audit_tap(trail, "raw") |>
#'   left_join_tap(customers, by = "id", .trail = trail, .label = "joined")
#' print(trail)
#'
#' # Without trail (plain join)
#' result2 <- left_join_tap(orders, customers, by = "id")
#'
#' @family operation taps
#' @name join_tap
NULL

#' @rdname join_tap
#' @export
left_join_tap <- function(.data, y, ..., .trail = NULL, .label = NULL,
                           .stat = NULL) {
  data_expr <- substitute(.data)
  y_name <- deparse(substitute(y))

  if (is.null(.trail)) {
    return(.join_tap_no_trail(.data, y, ..., .label = .label, .stat = .stat,
                               .join_fn = dplyr::left_join))
  }

  if (!inherits(.trail, "audit_trail")) {
    cli::cli_abort("{.arg .trail} must be an {.cls audit_trail} object.")
  }

  force(.data)

  .join_tap_impl(.data, y, ..., .trail = .trail, .label = .label,
                  .stat = .stat, .join_fn = dplyr::left_join,
                  .join_type = "left_join", .data_expr = data_expr,
                  .y_name = y_name)
}

#' @rdname join_tap
#' @export
right_join_tap <- function(.data, y, ..., .trail = NULL, .label = NULL,
                            .stat = NULL) {
  data_expr <- substitute(.data)
  y_name <- deparse(substitute(y))

  if (is.null(.trail)) {
    return(.join_tap_no_trail(.data, y, ..., .label = .label, .stat = .stat,
                               .join_fn = dplyr::right_join))
  }

  if (!inherits(.trail, "audit_trail")) {
    cli::cli_abort("{.arg .trail} must be an {.cls audit_trail} object.")
  }

  force(.data)

  .join_tap_impl(.data, y, ..., .trail = .trail, .label = .label,
                  .stat = .stat, .join_fn = dplyr::right_join,
                  .join_type = "right_join", .data_expr = data_expr,
                  .y_name = y_name)
}

#' @rdname join_tap
#' @export
inner_join_tap <- function(.data, y, ..., .trail = NULL, .label = NULL,
                            .stat = NULL) {
  data_expr <- substitute(.data)
  y_name <- deparse(substitute(y))

  if (is.null(.trail)) {
    return(.join_tap_no_trail(.data, y, ..., .label = .label, .stat = .stat,
                               .join_fn = dplyr::inner_join))
  }

  if (!inherits(.trail, "audit_trail")) {
    cli::cli_abort("{.arg .trail} must be an {.cls audit_trail} object.")
  }

  force(.data)

  .join_tap_impl(.data, y, ..., .trail = .trail, .label = .label,
                  .stat = .stat, .join_fn = dplyr::inner_join,
                  .join_type = "inner_join", .data_expr = data_expr,
                  .y_name = y_name)
}

#' @rdname join_tap
#' @export
full_join_tap <- function(.data, y, ..., .trail = NULL, .label = NULL,
                           .stat = NULL) {
  data_expr <- substitute(.data)
  y_name <- deparse(substitute(y))

  if (is.null(.trail)) {
    return(.join_tap_no_trail(.data, y, ..., .label = .label, .stat = .stat,
                               .join_fn = dplyr::full_join))
  }

  if (!inherits(.trail, "audit_trail")) {
    cli::cli_abort("{.arg .trail} must be an {.cls audit_trail} object.")
  }

  force(.data)

  .join_tap_impl(.data, y, ..., .trail = .trail, .label = .label,
                  .stat = .stat, .join_fn = dplyr::full_join,
                  .join_type = "full_join", .data_expr = data_expr,
                  .y_name = y_name)
}

#' @rdname join_tap
#' @export
anti_join_tap <- function(.data, y, ..., .trail = NULL, .label = NULL,
                           .stat = NULL) {
  data_expr <- substitute(.data)
  y_name <- deparse(substitute(y))

  if (is.null(.trail)) {
    return(.join_tap_no_trail(.data, y, ..., .label = .label, .stat = .stat,
                               .join_fn = dplyr::anti_join))
  }

  if (!inherits(.trail, "audit_trail")) {
    cli::cli_abort("{.arg .trail} must be an {.cls audit_trail} object.")
  }

  force(.data)

  .join_tap_impl(.data, y, ..., .trail = .trail, .label = .label,
                  .stat = .stat, .join_fn = dplyr::anti_join,
                  .join_type = "anti_join", .data_expr = data_expr,
                  .y_name = y_name)
}

#' @rdname join_tap
#' @export
semi_join_tap <- function(.data, y, ..., .trail = NULL, .label = NULL,
                           .stat = NULL) {
  data_expr <- substitute(.data)
  y_name <- deparse(substitute(y))

  if (is.null(.trail)) {
    return(.join_tap_no_trail(.data, y, ..., .label = .label, .stat = .stat,
                               .join_fn = dplyr::semi_join))
  }

  if (!inherits(.trail, "audit_trail")) {
    cli::cli_abort("{.arg .trail} must be an {.cls audit_trail} object.")
  }

  force(.data)

  .join_tap_impl(.data, y, ..., .trail = .trail, .label = .label,
                  .stat = .stat, .join_fn = dplyr::semi_join,
                  .join_type = "semi_join", .data_expr = data_expr,
                  .y_name = y_name)
}

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

#' Normalize `by` Into a Named Character Vector
#'
#' Converts `by` into a named character vector suitable for [validate_join()].
#' Returns `NULL` for non-equi join expressions or unrecognised types.
#'
#' @param by A character vector, named character vector, `dplyr_join_by`
#'   object, or `NULL`.
#'
#' @returns A named character vector, or `NULL`.
#'
#' @noRd
.normalize_by <- function(by) {
  if (is.null(by)) return(NULL)

  if (is.character(by)) return(by)

  if (inherits(by, "dplyr_join_by")) {
    return(tryCatch({
      if (all(by$condition == "==")) {
        setNames(by$y, by$x)
      } else {
        NULL
      }
    }, error = function(e) NULL))
  }

  NULL
}

#' Handle NULL-Trail Join Case
#'
#' When `.trail` is NULL, perform a plain join with optional validate_join
#' diagnostics.
#'
#' @noRd
.join_tap_no_trail <- function(.data, y, ..., .label, .stat, .join_fn) {
  if (!is.null(.label)) {
    cli::cli_warn("{.arg .label} is ignored when {.arg .trail} is NULL.")
  }
  if (!is.null(.stat)) {
    by_norm <- .normalize_by(list(...)$by)
    if (!is.null(by_norm)) {
      vj <- tryCatch(
        validate_join(.data, y, by = by_norm, stat = .stat),
        error = function(e) {
          err_msg <- conditionMessage(e)
          cli::cli_warn("validate_join() failed: {err_msg}")
          NULL
        }
      )
      if (!is.null(vj)) print(vj)
    }
  }
  .join_fn(.data, y, ...)
}

#' Shared Implementation for Join Taps
#'
#' Internal helper used by all six `*_join_tap()` functions. Handles
#' diagnostics, snapshot creation, and trail recording.
#'
#' @param .data Data.frame (already forced by the caller).
#' @param y Right table.
#' @param ... Forwarded dplyr join arguments.
#' @param .trail An audit_trail (validated by the caller).
#' @param .label Label or NULL.
#' @param .stat Stat column name or NULL.
#' @param .join_fn The dplyr join function.
#' @param .join_type Character name of the join type.
#' @param .data_expr Unevaluated expression for pipeline capture.
#' @param .y_name Deparsed name of `y`.
#'
#' @returns The join result.
#'
#' @noRd
.join_tap_impl <- function(.data, y, ..., .trail, .label, .stat,
                            .join_fn, .join_type, .data_expr, .y_name) {
  # Validate .data
  if (!is.data.frame(.data)) {
    cli::cli_abort(
      "{.arg .data} must be a data.frame or tibble, not {.cls {class(.data)}}."
    )
  }

  # Validate .label
  if (!is.null(.label)) {
    if (!is.character(.label) || length(.label) != 1L || is.na(.label)) {
      cli::cli_abort("{.arg .label} must be a single character string or NULL.")
    }
  }

  # Extract and normalize `by` for enriched diagnostics
  by_norm <- .normalize_by(list(...)$by)

  # Run validate_join if possible (best-effort)
  # Direct call preserves deparse(substitute()) inside validate_join
  vj <- NULL
  if (!is.null(by_norm)) {
    vj <- if (!is.null(.stat)) {
      tryCatch(validate_join(.data, y, by = by_norm, stat = .stat),
               error = function(e) NULL)
    } else {
      tryCatch(validate_join(.data, y, by = by_norm),
               error = function(e) NULL)
    }
  }

  # Perform the actual join
  result <- .join_fn(.data, y, ...)

  # Auto-generate label if not provided
  index <- length(.trail$snapshots) + 1L
  if (is.null(.label)) {
    .label <- paste0(.join_type, "_", index)
  }

  # Check label uniqueness
  if (.label %in% .trail$labels) {
    trail_name <- .trail$name
    label_val <- .label
    cli::cli_abort(
      "Label {.val {label_val}} already exists in trail {.val {trail_name}}."
    )
  }

  # Build snapshot of the join result
  snap <- .build_snapshot(result, label = .label, index = index)

  # Capture pipeline (best-effort)
  snap$pipeline <- tryCatch(.capture_pipeline(.data_expr), error = function(e) NULL)

  # Enrich the snapshot
  snap$type <- "join"

  if (!is.null(vj)) {
    snap$diagnostics <- list(
      join_type  = .join_type,
      relation   = vj$relation,
      match_rate = list(x = vj$counts$match_rate_x, y = vj$counts$match_rate_y),
      n_only_x   = vj$counts$n_only_x,
      n_only_y   = vj$counts$n_only_y,
      x_has_dups = vj$duplicates$x_has_dups,
      y_has_dups = vj$duplicates$y_has_dups,
      stat       = vj$stat,
      y_name     = .y_name
    )
  } else {
    snap$diagnostics <- list(
      join_type = .join_type,
      y_name    = .y_name
    )
  }

  # Compute changes vs previous snapshot
  if (index > 1L) {
    prev <- .trail$snapshots[[index - 1L]]
    snap$changes <- .detect_changes(prev, snap)
  }

  # Append to trail (reference semantics)
  .trail$snapshots[[index]] <- snap
  .trail$labels <- c(.trail$labels, .label)

  result
}
