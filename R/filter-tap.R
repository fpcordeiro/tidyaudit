#' Operation-Aware Filter Taps
#'
#' Perform a diagnostic filter AND record filter diagnostics in an audit trail.
#' `filter_tap()` keeps matching rows (like [dplyr::filter()]),
#' `filter_out_tap()` drops matching rows (the inverse).
#'
#' @param .data A data.frame or tibble.
#' @param ... Filter conditions, evaluated in the context of `.data` using
#'   tidy evaluation (same as [dplyr::filter()]).
#' @param .trail An [audit_trail()] object, or `NULL` (the default). When
#'   `NULL`, behavior depends on diagnostic arguments: if none are provided,
#'   a plain `dplyr::filter()` is performed; if `.stat`, `.warn_threshold`, or
#'   `.quiet = TRUE` is provided, delegates to [filter_keep()] or
#'   [filter_drop()].
#' @param .label Optional character label for this snapshot. If `NULL`,
#'   auto-generated as `"filter_1"` etc.
#' @param .stat An unquoted column or expression to total, e.g., `amount`,
#'   `price * qty`. Reports the stat amount dropped and its share of total.
#' @param .quiet Logical. If `TRUE`, suppress printing diagnostics (default
#'   `FALSE`).
#' @param .warn_threshold Numeric between 0 and 1. If set and the proportion of
#'   dropped rows exceeds this threshold, a warning is issued.
#'
#' @details
#' When `.trail` is `NULL`:
#' \itemize{
#'   \item No diagnostic args: plain `dplyr::filter()` / `dplyr::filter_out()`
#'   \item Diagnostic args provided: delegates to [filter_keep()] /
#'     [filter_drop()] (prints diagnostics but no trail recording)
#'   \item `.label` provided: warns that label is ignored
#' }
#'
#' @returns The filtered data.frame or tibble.
#'
#' @examples
#' df <- data.frame(id = 1:10, amount = 1:10 * 100, flag = rep(c(TRUE, FALSE), 5))
#'
#' # With trail
#' trail <- audit_trail("filter_example")
#' result <- df |>
#'   audit_tap(trail, "raw") |>
#'   filter_tap(amount > 300, .trail = trail, .label = "big_only")
#' print(trail)
#'
#' # Inverse: drop matching rows
#' trail2 <- audit_trail("filter_out_example")
#' result2 <- df |>
#'   audit_tap(trail2, "raw") |>
#'   filter_out_tap(flag == FALSE, .trail = trail2, .label = "flagged_only")
#' print(trail2)
#'
#' # Without trail (plain filter)
#' result3 <- filter_tap(df, amount > 300)
#'
#' @family operation taps
#' @name filter_tap
NULL

#' @rdname filter_tap
#' @export
filter_tap <- function(.data, ..., .trail = NULL, .label = NULL, .stat = NULL,
                        .quiet = FALSE, .warn_threshold = NULL) {
  data_expr <- substitute(.data)
  .validate_warn_threshold(.warn_threshold)
  stat_quo <- rlang::enquo(.stat)
  have_stat <- !rlang::quo_is_null(stat_quo)

  # NULL trail handling
  if (is.null(.trail)) {
    if (!is.null(.label)) {
      cli::cli_warn("{.arg .label} is ignored when {.arg .trail} is NULL.")
    }
    has_diag_args <- have_stat || !is.null(.warn_threshold) || .quiet
    if (has_diag_args) {
      return(filter_keep(.data, ..., .stat = !!stat_quo, .quiet = .quiet,
                          .warn_threshold = .warn_threshold))
    }
    return(dplyr::filter(.data, ...))
  }

  # Validate trail
  if (!inherits(.trail, "audit_trail")) {
    cli::cli_abort("{.arg .trail} must be an {.cls audit_trail} object.")
  }

  # Force .data before reading trail state
  force(.data)

  .filter_tap_impl(.data, rlang::enquos(...), .trail = .trail, .label = .label,
                    .stat_quo = stat_quo, .have_stat = have_stat,
                    .quiet = .quiet, .warn_threshold = .warn_threshold,
                    .filter_fn = dplyr::filter, .filter_type = "keep",
                    .label_prefix = "filter_", .tap_name = "filter_tap",
                    .action = "Dropped", .data_expr = data_expr)
}

#' @rdname filter_tap
#' @export
filter_out_tap <- function(.data, ..., .trail = NULL, .label = NULL,
                            .stat = NULL, .quiet = FALSE,
                            .warn_threshold = NULL) {
  data_expr <- substitute(.data)
  .validate_warn_threshold(.warn_threshold)
  stat_quo <- rlang::enquo(.stat)
  have_stat <- !rlang::quo_is_null(stat_quo)

  # NULL trail handling
  if (is.null(.trail)) {
    if (!is.null(.label)) {
      cli::cli_warn("{.arg .label} is ignored when {.arg .trail} is NULL.")
    }
    has_diag_args <- have_stat || !is.null(.warn_threshold) || .quiet
    if (has_diag_args) {
      return(filter_drop(.data, ..., .stat = !!stat_quo, .quiet = .quiet,
                          .warn_threshold = .warn_threshold))
    }
    return(dplyr::filter_out(.data, ...))
  }

  # Validate trail
  if (!inherits(.trail, "audit_trail")) {
    cli::cli_abort("{.arg .trail} must be an {.cls audit_trail} object.")
  }

  # Force .data before reading trail state
  force(.data)

  .filter_tap_impl(.data, rlang::enquos(...), .trail = .trail, .label = .label,
                    .stat_quo = stat_quo, .have_stat = have_stat,
                    .quiet = .quiet, .warn_threshold = .warn_threshold,
                    .filter_fn = dplyr::filter_out, .filter_type = "drop",
                    .label_prefix = "filter_out_", .tap_name = "filter_out_tap",
                    .action = "Removed", .data_expr = data_expr)
}

# ---------------------------------------------------------------------------
# Internal helper
# ---------------------------------------------------------------------------

#' Shared Implementation for Filter Taps
#'
#' Internal helper used by both `filter_tap()` and `filter_out_tap()`.
#' Handles diagnostics, snapshot creation, and trail recording.
#'
#' @param .data Data.frame (already forced by the caller).
#' @param dots Quosure list of filter conditions (from `rlang::enquos()`).
#' @param .trail An audit_trail (validated by the caller).
#' @param .label Label or NULL.
#' @param .stat_quo Quosure for the stat column.
#' @param .have_stat Logical indicating whether stat was provided.
#' @param .quiet Logical for suppressing output.
#' @param .warn_threshold Numeric threshold or NULL.
#' @param .filter_fn The dplyr filter function (`dplyr::filter` or
#'   `dplyr::filter_out`).
#' @param .filter_type Character: `"keep"` or `"drop"`.
#' @param .label_prefix Character prefix for auto-generated labels.
#' @param .tap_name Character name for diagnostic messages.
#' @param .action Character verb for diagnostic messages (`"Dropped"` or
#'   `"Removed"`).
#' @param .data_expr Unevaluated expression for pipeline capture.
#'
#' @returns The filtered result.
#'
#' @noRd
.filter_tap_impl <- function(.data, dots, .trail, .label, .stat_quo, .have_stat,
                              .quiet, .warn_threshold, .filter_fn, .filter_type,
                              .label_prefix, .tap_name, .action, .data_expr) {
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

  # Format expression label
  expr_lab <- paste(vapply(dots, rlang::as_label, character(1)), collapse = " & ")

  # Apply filter (guard against zero conditions)
  if (length(dots) == 0L) {
    result <- .data
  } else {
    result <- .filter_fn(.data, !!!dots)
  }

  # Compute diagnostics
  n_total <- nrow(.data)
  n_result <- nrow(result)
  n_dropped <- n_total - n_result
  pct_dropped <- if (n_total > 0L) 100 * n_dropped / n_total else 0

  # Count NAs in the filter condition (best-effort)
  n_na_in_filter <- tryCatch({
    if (length(dots) == 0L) {
      0L
    } else {
      mask_results <- lapply(dots, function(q) rlang::eval_tidy(q, data = .data))
      combined <- Reduce(`&`, mask_results)
      sum(is.na(combined))
    }
  }, error = function(e) NA_integer_)

  # Stat diagnostics
  stat_dropped <- NULL
  stat_total <- NULL
  stat_col <- NULL
  if (.have_stat) {
    stat_total <- rlang::eval_tidy(
      rlang::quo(sum(!!.stat_quo, na.rm = TRUE)), data = .data
    )
    stat_result <- rlang::eval_tidy(
      rlang::quo(sum(!!.stat_quo, na.rm = TRUE)), data = result
    )
    stat_dropped <- stat_total - stat_result
    stat_col <- rlang::as_label(.stat_quo)
  }

  # Build snapshot
  index <- length(.trail$snapshots) + 1L
  if (is.null(.label)) {
    .label <- paste0(.label_prefix, index)
  }
  if (.label %in% .trail$labels) {
    trail_name <- .trail$name
    label_val <- .label
    cli::cli_abort(
      "Label {.val {label_val}} already exists in trail {.val {trail_name}}."
    )
  }

  snap <- .build_snapshot(result, label = .label, index = index)
  snap$pipeline <- tryCatch(.capture_pipeline(.data_expr), error = function(e) NULL)

  # Enrich snapshot
  snap$type <- "filter"
  snap$diagnostics <- list(
    filter_type    = .filter_type,
    expr_label     = expr_lab,
    n_dropped      = n_dropped,
    n_total        = n_total,
    pct_dropped    = pct_dropped,
    stat_dropped   = stat_dropped,
    stat_total     = stat_total,
    stat_col       = stat_col,
    n_na_in_filter = n_na_in_filter
  )

  # Compute changes vs previous
  if (index > 1L) {
    prev <- .trail$snapshots[[index - 1L]]
    snap$changes <- .detect_changes(prev, snap)
  }

  # Print diagnostics (unless quiet)
  if (!.quiet) {
    tap_name_local <- .tap_name
    action_local <- .action
    cli::cli_alert_info(
      "{tap_name_local}: {expr_lab}"
    )
    cli::cli_text(
      "  {action_local} {format(n_dropped, big.mark = ',')} of {format(n_total, big.mark = ',')} rows ({sprintf('%.1f%%', pct_dropped)})"
    )
    if (.have_stat) {
      stat_col_local <- stat_col
      cli::cli_text(
        "  Stat {stat_col_local}: {tolower(action_local)} {format(stat_dropped, big.mark = ',', scientific = FALSE)} of {format(stat_total, big.mark = ',', scientific = FALSE)}"
      )
    }
  }

  # Warn if threshold exceeded
  if (!is.null(.warn_threshold)) {
    share_drop <- if (n_total > 0L) n_dropped / n_total else NA_real_
    if (!is.na(share_drop) && share_drop > .warn_threshold) {
      action_local <- .action
      cli::cli_warn(
        "{action_local} {sprintf('%.1f%%', 100 * share_drop)} of rows exceeds threshold ({sprintf('%.1f%%', 100 * .warn_threshold)})."
      )
    }
  }

  # Append to trail
  .trail$snapshots[[index]] <- snap
  .trail$labels <- c(.trail$labels, .label)

  result
}
