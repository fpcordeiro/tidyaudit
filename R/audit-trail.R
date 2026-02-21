#' Create an Audit Trail
#'
#' Creates an audit trail object that captures metadata snapshots at each step
#' of a data pipeline. The trail uses environment-based reference semantics so
#' it can be modified in place inside pipes via [audit_tap()].
#'
#' @param name Optional name for the trail. If `NULL`, a timestamped name is
#'   generated automatically.
#'
#' @returns An `audit_trail` object (S3 class wrapping an environment).
#'
#' @examples
#' trail <- audit_trail("my_analysis")
#' print(trail)
#'
#' @family audit trail
#' @export
audit_trail <- function(name = NULL) {
  trail_env <- new.env(parent = emptyenv())
  trail_env$name <- name %||% format(Sys.time(), "trail_%Y%m%d_%H%M%S")
  trail_env$created_at <- Sys.time()
  trail_env$snapshots <- list()
  trail_env$labels <- character()
  structure(trail_env, class = c("audit_trail", "environment"))
}

#' @rdname audit_trail
#' @param x An object to print.
#' @param ... Additional arguments (currently unused).
#' @export
print.audit_trail <- function(x, ...) {
  n_snaps <- length(x$snapshots)

  cli::cli_h1("Audit Trail: {.val {x$name}}")
  cli::cli_text("Created: {format(x$created_at, '%Y-%m-%d %H:%M:%S')}")
  cli::cli_text("Snapshots: {n_snaps}")

  if (n_snaps == 0L) {
    cli::cli_text("")
    cli::cli_alert_info("No snapshots recorded yet. Use {.fn audit_tap} in a pipe to add snapshots.")
    return(invisible(x))
  }

  cli::cli_text("")

  # Build timeline table
  tbl <- data.frame(
    `#`    = as.character(seq_len(n_snaps)),
    Label  = vapply(x$snapshots, `[[`, character(1), "label"),
    Rows   = format(vapply(x$snapshots, `[[`, integer(1), "nrow"),
                    big.mark = ",", trim = TRUE),
    Cols   = as.character(vapply(x$snapshots, `[[`, integer(1), "ncol")),
    NAs    = format(vapply(x$snapshots, `[[`, integer(1), "total_nas"),
                    big.mark = ",", trim = TRUE),
    Type   = vapply(x$snapshots, .format_snap_type, character(1)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  .cli_table(tbl, right_align = c("Rows", "Cols", "NAs"))

  # Print change summaries
  if (n_snaps > 1L) {
    cli::cli_text("")
    cli::cli_text("{.strong Changes:}")
    for (i in seq(2L, n_snaps)) {
      snap <- x$snapshots[[i]]
      if (!is.null(snap$changes)) {
        prev_label <- x$snapshots[[i - 1L]]$label
        curr_label <- snap$label
        summary <- .format_change_summary(snap$changes)
        line <- glue::glue("  {prev_label} \u2192 {curr_label}: {summary}")
        cli::cli_verbatim(line)
      }
    }
  }

  invisible(x)
}

#' Print a formatted table with header separator via cli_verbatim
#'
#' @param tbl A data.frame of character values (already formatted for display).
#' @param right_align Character vector of column names to right-align. All other
#'   columns are left-aligned.
#' @param indent Number of leading spaces (default 2).
#'
#' @noRd
.cli_table <- function(tbl, right_align = character(), indent = 2L) {
  nms <- names(tbl)

  # Compute column widths (max of header and data)
  col_widths <- vapply(nms, function(nm) {
    data_width <- if (nrow(tbl) > 0L) max(nchar(tbl[[nm]]), na.rm = TRUE) else 0L
    max(nchar(nm), data_width)
  }, integer(1))

  pad <- strrep(" ", indent)
  gap <- "  "

  # Format one row
  format_row <- function(values) {
    cells <- vapply(seq_along(nms), function(j) {
      w <- col_widths[j]
      if (nms[j] %in% right_align) {
        formatC(values[j], width = w)
      } else {
        formatC(values[j], width = w, flag = "-")
      }
    }, character(1))
    paste0(pad, paste(cells, collapse = gap))
  }

  # Header
  header_line <- format_row(nms)

  # Separator: thin line matching each column width
  sep_cells <- vapply(seq_along(nms), function(j) {
    strrep("\u2500", col_widths[j])
  }, character(1))
  sep_line <- paste0(pad, paste(sep_cells, collapse = gap))

  # Data rows
  data_lines <- vapply(seq_len(nrow(tbl)), function(i) {
    format_row(as.character(tbl[i, ]))
  }, character(1))

  cli::cli_verbatim(paste(c(header_line, sep_line, data_lines), collapse = "\n"))
}

#' Format snapshot type for the timeline table
#' @noRd
.format_snap_type <- function(snap) {
  type <- snap$type %||% "tap"
  diag <- snap$diagnostics

  if (type == "join" && !is.null(diag)) {
    join_type <- diag$join_type %||% "join"
    relation <- diag$relation
    match_x <- diag$match_rate$x
    if (!is.null(match_x) && !is.na(match_x) && !is.null(relation)) {
      return(glue::glue("{join_type} ({relation}, {round(match_x, 1)}% matched)"))
    }
    if (!is.null(relation)) {
      return(glue::glue("{join_type} ({relation})"))
    }
    return(join_type)
  }

  if (type == "filter" && !is.null(diag)) {
    filter_type <- diag$filter_type %||% "keep"
    n_dropped <- diag$n_dropped %||% 0L
    pct_dropped <- diag$pct_dropped %||% 0
    action <- if (filter_type == "keep") "dropped" else "removed"
    return(glue::glue("filter ({action} {format(n_dropped, big.mark = ',')} rows, {round(pct_dropped, 1)}%)"))
  }

  type
}

#' Format a change summary for display
#' @noRd
.format_change_summary <- function(changes) {
  parts <- character()

  # Row delta
  rd <- changes$row_delta
  if (rd > 0) {
    parts <- c(parts, glue::glue("+{format(rd, big.mark = ',')} rows"))
  } else if (rd < 0) {
    parts <- c(parts, glue::glue("{format(rd, big.mark = ',')} rows"))
  } else {
    parts <- c(parts, "= rows")
  }

  # Col delta
  cd <- changes$col_delta
  if (cd > 0) {
    parts <- c(parts, glue::glue("+{cd} cols"))
  } else if (cd < 0) {
    parts <- c(parts, glue::glue("{cd} cols"))
  } else {
    parts <- c(parts, "= cols")
  }

  # NA delta
  nd <- changes$na_delta
  if (nd > 0) {
    parts <- c(parts, glue::glue("+{format(nd, big.mark = ',')} NAs"))
  } else if (nd < 0) {
    parts <- c(parts, glue::glue("{format(nd, big.mark = ',')} NAs"))
  } else {
    parts <- c(parts, "= NAs")
  }

  paste(parts, collapse = ", ")
}
