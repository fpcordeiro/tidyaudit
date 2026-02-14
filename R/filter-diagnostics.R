#' Filter Data with Diagnostic Statistics (Keep)
#'
#' Filters a data.frame or tibble while reporting statistics about dropped rows
#' and optionally the sum of a statistic column that was dropped. Keeps rows
#' where the conditions are TRUE (same as [dplyr::filter()]).
#'
#' @param .data A data.frame, tibble, or other object.
#' @param ... Arguments passed to methods.
#'
#' @returns The filtered data.frame or tibble.
#'
#' @family filter diagnostics
#' @export
filter_keep <- function(.data, ...) UseMethod("filter_keep")

#' @describeIn filter_keep Method for data.frame objects
#'
#' @param ... Filter conditions, evaluated in the context of `.data` using
#'   tidy evaluation (same as [dplyr::filter()]).
#' @param .stat An unquoted column or expression to total, e.g., `amount`,
#'   `price * qty`. Reports the amount dropped and its share of total.
#' @param .quiet Logical. If `TRUE`, suppress printing diagnostics.
#' @param .warn_threshold Numeric between 0 and 1. If set and the proportion of
#'   dropped rows exceeds this threshold, a warning is issued.
#'
#' @examples
#' df <- data.frame(
#'   id = 1:6,
#'   keep = c(TRUE, FALSE, TRUE, NA, TRUE, FALSE),
#'   sales = c(100, 50, 200, 25, NA, 75)
#' )
#' filter_keep(df, keep == TRUE)
#' filter_keep(df, keep == TRUE, .stat = sales)
#'
#' @export
filter_keep.data.frame <- function(.data, ..., .stat = NULL,
                                    .quiet = FALSE, .warn_threshold = NULL) {
  data_name <- deparse(substitute(.data))
  dots <- rlang::enquos(...)
  stat_quo <- rlang::enquo(.stat)
  have_stat <- !rlang::quo_is_null(stat_quo)

  expr_lab <- paste(vapply(dots, rlang::as_label, character(1)), collapse = ", ")

  # Evaluate filter: keep matching rows
  filtered <- dplyr::filter(.data, !!!dots)

  .print_filter_diagnostic(
    .data, filtered, data_name, expr_lab, stat_quo, have_stat,
    mode = "keep", quiet = .quiet, warn_threshold = .warn_threshold
  )

  filtered
}

#' Filter Data with Diagnostic Statistics (Drop)
#'
#' Filters a data.frame or tibble by DROPPING rows where the conditions are
#' TRUE, while reporting statistics about dropped rows and optionally the sum
#' of a statistic column that was dropped.
#'
#' @param .data A data.frame, tibble, or other object.
#' @param ... Arguments passed to methods.
#'
#' @returns The filtered data.frame or tibble.
#'
#' @family filter diagnostics
#' @export
filter_drop <- function(.data, ...) UseMethod("filter_drop")

#' @describeIn filter_drop Method for data.frame objects
#'
#' @param ... Filter conditions specifying rows to DROP, evaluated in the
#'   context of `.data` using tidy evaluation.
#' @param .stat An unquoted column or expression to total, e.g., `amount`,
#'   `price * qty`. Reports the amount dropped and its share of total.
#' @param .quiet Logical. If `TRUE`, suppress printing diagnostics.
#' @param .warn_threshold Numeric between 0 and 1. If set and the proportion of
#'   dropped rows exceeds this threshold, a warning is issued.
#'
#' @examples
#' df <- data.frame(
#'   id = 1:5,
#'   bad = c(FALSE, TRUE, FALSE, TRUE, FALSE),
#'   sales = 10:14
#' )
#' filter_drop(df, bad == TRUE)
#' filter_drop(df, bad == TRUE, .stat = sales)
#'
#' @export
filter_drop.data.frame <- function(.data, ..., .stat = NULL,
                                    .quiet = FALSE, .warn_threshold = NULL) {
  data_name <- deparse(substitute(.data))
  dots <- rlang::enquos(...)
  stat_quo <- rlang::enquo(.stat)
  have_stat <- !rlang::quo_is_null(stat_quo)

  expr_lab <- paste(vapply(dots, rlang::as_label, character(1)), collapse = ", ")

  # Evaluate filter: drop matching rows (keep the inverse)
  if (length(dots) == 0L) {
    filtered <- .data
  } else {
    filtered <- dplyr::filter_out(.data, !!!dots)
  }

  .print_filter_diagnostic(
    .data, filtered, data_name, expr_lab, stat_quo, have_stat,
    mode = "drop", quiet = .quiet, warn_threshold = .warn_threshold
  )

  filtered
}

#' Internal helper for filter diagnostic printing
#' @noRd
.print_filter_diagnostic <- function(.data, filtered, data_name, expr_lab,
                                      stat_quo, have_stat, mode, quiet,
                                      warn_threshold) {
  n_total <- nrow(.data)
  n_filtered <- nrow(filtered)
  n_drop <- n_total - n_filtered
  share_drop <- if (n_total > 0L) n_drop / n_total else NA_real_

  func_name <- if (mode == "keep") "filter_keep" else "filter_drop"

  if (have_stat) {
    total_val <- rlang::eval_tidy(
      rlang::quo(sum(!!stat_quo, na.rm = TRUE)), data = .data
    )
    filtered_val <- rlang::eval_tidy(
      rlang::quo(sum(!!stat_quo, na.rm = TRUE)), data = filtered
    )
    drop_val <- total_val - filtered_val
    share_val <- if (isTRUE(all.equal(total_val, 0))) NA_real_ else drop_val / total_val
    stat_lab <- rlang::as_label(stat_quo)
  }

  if (!quiet) {
    cli::cli_text("{func_name}({data_name}, {expr_lab})")
    cli::cli_text(
      "  Dropped {format(n_drop, big.mark = ',')} of {format(n_total, big.mark = ',')} rows ({sprintf('%.2f%%', 100 * share_drop)})."
    )
    if (have_stat) {
      cli::cli_text(
        "  Dropped {format(drop_val, big.mark = ',', scientific = FALSE)} of {format(total_val, big.mark = ',', scientific = FALSE)} for {stat_lab} ({sprintf('%.2f%%', 100 * share_val)})."
      )
    }
  }

  if (!is.null(warn_threshold) && !is.na(share_drop) && share_drop > warn_threshold) {
    cli::cli_warn(
      "Dropped {sprintf('%.1f%%', 100 * share_drop)} of rows exceeds threshold ({sprintf('%.1f%%', 100 * warn_threshold)})."
    )
  }
}
