#' Generate an Audit Report
#'
#' Prints a full audit report for a trail, including the trail summary, all
#' diffs between consecutive snapshots, custom diagnostic results, and a final
#' data profile.
#'
#' @param .trail An [audit_trail()] object.
#' @param format Report format. Currently only `"console"` is supported.
#'   `"rmd"` is planned for a future version.
#' @param file Output file path (used only with `format = "rmd"`).
#'
#' @return `.trail`, invisibly.
#'
#' @examples
#' trail <- audit_trail("example")
#' mtcars |>
#'   audit_tap(trail, "raw") |>
#'   dplyr::filter(mpg > 20) |>
#'   audit_tap(trail, "filtered")
#' audit_report(trail)
#'
#' @export
audit_report <- function(.trail, format = c("console", "rmd"), file = NULL) {
  format <- match.arg(format)

  if (!inherits(.trail, "audit_trail")) {
    cli::cli_abort("{.arg .trail} must be an {.cls audit_trail} object.")
  }

  if (format == "rmd") {
    cli::cli_abort("Rmd report format is not yet implemented. Use {.val console}.")
  }

  .print_full_report(.trail)
  invisible(.trail)
}

#' Print Full Console Report
#'
#' @param .trail An audit trail.
#'
#' @noRd
.print_full_report <- function(.trail) {
  n_snaps <- length(.trail$snapshots)

  trail_name <- .trail$name
  cli::cli_rule(left = "Audit Report: {.val {trail_name}}")
  cli::cli_text("Created: {format(.trail$created_at, '%Y-%m-%d %H:%M:%S')}")
  cli::cli_text("Total snapshots: {n_snaps}")

  if (n_snaps == 0L) {
    cli::cli_text("")
    cli::cli_alert_info("No snapshots recorded.")
    cli::cli_rule()
    return(invisible(NULL))
  }

  # Trail summary (reuse print method)
  cli::cli_text("")
  print(.trail)

  # Detailed diffs between consecutive snapshots
  if (n_snaps > 1L) {
    cli::cli_text("")
    cli::cli_rule(left = "Detailed Diffs")

    for (i in seq(2L, n_snaps)) {
      diff_obj <- audit_diff(.trail, i - 1L, i)
      cli::cli_text("")
      print(diff_obj)
    }
  }

  # Custom diagnostics
  has_custom <- any(vapply(.trail$snapshots, function(s) !is.null(s$custom), logical(1)))
  if (has_custom) {
    cli::cli_text("")
    cli::cli_rule(left = "Custom Diagnostics")

    for (snap in .trail$snapshots) {
      if (!is.null(snap$custom)) {
        cli::cli_text("")
        cli::cli_text("{.strong {snap$label}:}")
        for (nm in names(snap$custom)) {
          val <- snap$custom[[nm]]
          cli::cli_text("  {nm}: {format(val)}")
        }
      }
    }
  }

  # Final snapshot profile
  cli::cli_text("")
  cli::cli_rule(left = "Final Snapshot Profile")
  last_snap <- .trail$snapshots[[n_snaps]]
  cli::cli_text("")
  cli::cli_text("{.strong {last_snap$label}} ({format(last_snap$nrow, big.mark = ',')} rows x {last_snap$ncol} cols)")

  # Column types summary
  col_info <- last_snap$col_info
  type_counts <- table(col_info$type)
  type_str <- paste(
    vapply(names(type_counts), function(t) glue::glue("{type_counts[t]} {t}"), character(1)),
    collapse = ", "
  )
  cli::cli_text("Column types: {type_str}")

  # NA summary
  if (last_snap$total_nas > 0L) {
    na_cols <- col_info[col_info$n_na > 0L, , drop = FALSE]
    pct <- round(100 * na_cols$n_na / last_snap$nrow, 1)
    na_tbl <- data.frame(
      Column = na_cols$column,
      NAs    = format(na_cols$n_na, big.mark = ",", trim = TRUE),
      `%`    = paste0(format(pct, nsmall = 1, trim = TRUE), "%"),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    cli::cli_text("{.strong Columns with NAs ({nrow(na_cols)}):}")
    .cli_table(na_tbl, right_align = c("NAs", "%"), indent = 4L)
  } else {
    cli::cli_alert_success("No missing values")
  }

  # Numeric summary
  if (!is.null(last_snap$numeric_summary)) {
    cli::cli_text("")
    cli::cli_text("{.strong Numeric summary:}")
    ns <- last_snap$numeric_summary
    num_tbl <- data.frame(
      Column = ns$column,
      Min    = format(round(ns$min, 2), big.mark = ",", trim = TRUE),
      Mean   = format(round(ns$mean, 2), big.mark = ",", trim = TRUE),
      Median = format(round(ns$median, 2), big.mark = ",", trim = TRUE),
      Max    = format(round(ns$max, 2), big.mark = ",", trim = TRUE),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    .cli_table(num_tbl, right_align = c("Min", "Mean", "Median", "Max"), indent = 4L)
  }

  cli::cli_text("")
  cli::cli_rule()
}
