#' Build a Metadata Snapshot
#'
#' Internal function that captures a metadata-only snapshot of the data at one
#' point in the pipeline. Does NOT store the data itself.
#'
#' @param .data A data.frame or tibble.
#' @param label Character label for this snapshot.
#' @param index Integer position in the trail.
#'
#' @returns An `audit_snap` object (S3 list).
#'
#' @noRd
.build_snapshot <- function(.data, label, index) {
  # Diagnostic aggregates on ungrouped data for deterministic results
  ungrouped <- dplyr::ungroup(.data)

  col_info <- data.frame(
    column = names(ungrouped),
    type   = vapply(ungrouped, function(x) class(x)[[1L]], character(1)),
    n_na   = vapply(ungrouped, function(x) sum(is.na(x)), integer(1)),
    stringsAsFactors = FALSE
  )

  # Numeric summaries
  numeric_cols <- col_info$column[col_info$type %in% c("numeric", "integer")]
  numeric_summary <- if (length(numeric_cols) > 0L) {
    summaries <- lapply(numeric_cols, function(col) {
      vals <- ungrouped[[col]]
      vals_clean <- vals[!is.na(vals)]
      if (length(vals_clean) == 0L) {
        data.frame(
          column = col, min = NA_real_, q25 = NA_real_, median = NA_real_,
          mean = NA_real_, q75 = NA_real_, max = NA_real_,
          stringsAsFactors = FALSE
        )
      } else {
        qs <- quantile(vals_clean, probs = c(0.25, 0.75), names = FALSE)
        data.frame(
          column = col,
          min    = min(vals_clean),
          q25    = qs[1L],
          median = median(vals_clean),
          mean   = mean(vals_clean),
          q75    = qs[2L],
          max    = max(vals_clean),
          stringsAsFactors = FALSE
        )
      }
    })
    do.call(rbind, summaries)
  } else {
    NULL
  }

  snap <- list(
    label           = label,
    index           = index,
    timestamp       = Sys.time(),
    type            = "tap",
    nrow            = as.integer(nrow(ungrouped)),
    ncol            = as.integer(ncol(ungrouped)),
    col_info        = col_info,
    total_nas       = as.integer(sum(col_info$n_na)),
    numeric_summary = numeric_summary,
    diagnostics     = NULL,
    pipeline        = NULL,
    changes         = NULL,
    custom          = NULL
  )
  structure(snap, class = c("audit_snap", "list"))
}

#' @rdname audit_trail
#' @export
print.audit_snap <- function(x, ...) {
  cli::cli_h2("Snapshot: {.val {x$label}} ({format(x$nrow, big.mark = ',')} rows x {x$ncol} cols)")
  cli::cli_text("Index: {x$index} | Type: {x$type} | Time: {format(x$timestamp, '%H:%M:%S')}")

  # Pipeline
  if (!is.null(x$pipeline) && length(x$pipeline) > 0L) {
    cli::cli_text("")
    cli::cli_text("{.strong Pipeline:}")
    n <- length(x$pipeline)
    lines <- character(n)
    for (i in seq_along(x$pipeline)) {
      step <- x$pipeline[[i]]
      if (i == 1L) {
        lines[i] <- paste0("  ", step, if (n > 1L) " |>" else "")
      } else if (i == n) {
        lines[i] <- paste0("    ", step)
      } else {
        lines[i] <- paste0("    ", step, " |>")
      }
    }
    cli::cli_verbatim(paste(lines, collapse = "\n"))
  }

  # Column info — grouped by type
  cli::cli_text("")
  cli::cli_text("{.strong Columns ({x$ncol}):}")
  col_info <- x$col_info
  types <- unique(col_info$type)
  for (tp in types) {
    cols_of_type <- col_info$column[col_info$type == tp]
    na_of_type <- col_info$n_na[col_info$type == tp]
    n_type <- length(cols_of_type)

    # Build column list with NA annotations
    col_labels <- vapply(seq_along(cols_of_type), function(j) {
      if (na_of_type[j] > 0L) {
        paste0(cols_of_type[j], " [", na_of_type[j], " NA]")
      } else {
        cols_of_type[j]
      }
    }, character(1))

    col_str <- paste(col_labels, collapse = ", ")
    cli::cli_text("  {.emph {tp}} ({n_type}): {col_str}")
  }

  # Total NAs
  if (x$total_nas > 0L) {
    cli::cli_text("")
    cli::cli_alert_warning("Total NAs: {format(x$total_nas, big.mark = ',')}")
  }

  # Changes from previous
  if (!is.null(x$changes)) {
    cli::cli_text("")
    cli::cli_text("{.strong Changes from previous:}")
    summary <- .format_change_summary(x$changes)
    cli::cli_text("  {summary}")
  }

  invisible(x)
}

#' Capture Pipeline Call Chain
#'
#' Reverse-engineers the pipe chain from an unevaluated expression captured via
#' `substitute(.data)`. Only works with the base R `|>` pipe.
#'
#' @param data_expr An unevaluated expression from `substitute(.data)`.
#' @param .max_steps Maximum number of steps to keep (default 5).
#'
#' @returns A character vector of pipeline steps, or `NULL` on failure.
#'
#' @noRd
.capture_pipeline <- function(data_expr, .max_steps = 5L) {
  steps <- .decompose_pipe(data_expr)
  if (length(steps) > .max_steps + 1L) {
    steps <- c("...", steps[(length(steps) - .max_steps + 1L):length(steps)])
  }
  steps
}

#' Decompose a Nested Pipe Expression
#'
#' Recursively walks nested call expressions to extract individual pipe steps.
#'
#' @param expr An unevaluated R expression.
#'
#' @returns A character vector of deparsed steps.
#'
#' @noRd
.decompose_pipe <- function(expr) {
  if (is.call(expr) && length(expr) >= 2L) {
    inner <- .decompose_pipe(expr[[2L]])
    step <- expr
    step[[2L]] <- quote(.)
    c(inner, deparse(step, width.cutoff = 500L))
  } else {
    deparse(expr)
  }
}
