#' Record a Tabulation Snapshot in a Pipeline
#'
#' Transparent pipe pass-through that runs [tab()] on the data and stores the
#' result as a custom diagnostic annotation in the audit trail snapshot.
#' Returns `.data` unchanged.
#'
#' @inheritParams tab
#' @param .trail An [audit_trail()] object.
#' @param .label Character label for this snapshot.
#' @param .numeric_summary Logical. If `FALSE`, skip numeric summary
#'   computation in the snapshot (default `TRUE`).
#' @param .cols_include Character vector of column names to include in the
#'   snapshot schema, or `NULL` (the default) to include all columns. Mutually
#'   exclusive with `.cols_exclude`.
#' @param .cols_exclude Character vector of column names to exclude from the
#'   snapshot schema, or `NULL` (the default). Mutually exclusive with
#'   `.cols_include`.
#'
#' @returns `.data`, unchanged, returned invisibly.
#'
#' @examples
#' trail <- audit_trail("example")
#' result <- mtcars |>
#'   tab_tap(cyl, .trail = trail, .label = "by_cyl") |>
#'   dplyr::filter(mpg > 20) |>
#'   tab_tap(cyl, .trail = trail, .label = "by_cyl_filtered")
#' print(trail)
#'
#' @family audit trail
#' @export
tab_tap <- function(.data, ..., .trail, .label = NULL, .wt = NULL,
                    .sort = c("value_asc", "value_desc", "freq_desc", "freq_asc"),
                    .cutoff = NULL, .na = c("include", "exclude", "only"),
                    .display = c("count", "row_pct", "col_pct", "total_pct"),
                    .numeric_summary = TRUE,
                    .cols_include = NULL, .cols_exclude = NULL) {
  data_expr <- substitute(.data)

  # Capture quosures before force() for consistency with other taps
  dots <- rlang::enquos(...)
  wt_quo <- rlang::enquo(.wt)

  # Validate arguments that don't require .data evaluation
  if (!inherits(.trail, "audit_trail")) {
    cli::cli_abort("{.arg .trail} must be an {.cls audit_trail} object.")
  }
  if (!is.null(.label)) {
    if (!is.character(.label) || length(.label) != 1L || is.na(.label)) {
      cli::cli_abort("{.arg .label} must be a single character string or NULL.")
    }
  }

  .sort <- match.arg(.sort)
  .na <- match.arg(.na)
  .display <- match.arg(.display)

  force(.data)

  if (!is.data.frame(.data)) {
    cli::cli_abort("{.arg .data} must be a data.frame or tibble, not {.cls {class(.data)}}.")
  }

  # Build tab result as a custom diagnostic
  tab_result <- tab(.data, !!!dots, .wt = !!wt_quo,
                    .sort = .sort, .cutoff = .cutoff, .na = .na,
                    .display = .display)

  # Build a descriptive name for the tab diagnostic
  var_names <- vapply(dots, rlang::as_label, character(1))
  tab_name <- paste0("tab(", paste(var_names, collapse = ", "), ")")

  # Delegate to audit_tap with the tab result as a custom function
  audit_tap(.data, .trail = .trail, .label = .label,
            .fns = stats::setNames(list(function(.d) tab_result), tab_name),
            .numeric_summary = .numeric_summary,
            .cols_include = .cols_include,
            .cols_exclude = .cols_exclude)

  invisible(.data)
}
