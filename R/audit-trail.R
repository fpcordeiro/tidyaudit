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
#' @param show_custom Logical. If `TRUE` (default), inline annotations (one
#'   indented line per custom function) are printed below each snapshot that has
#'   custom diagnostics. Set to `FALSE` to suppress them and display only the
#'   main timeline table.
#' @param ... Additional arguments (currently unused).
#' @export
print.audit_trail <- function(x, show_custom = TRUE, ...) {
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

  # Build per-row annotation lines for custom diagnostics
  row_annotations <- NULL
  if (show_custom) {
    row_annotations <- vector("list", n_snaps)
    for (i in seq_len(n_snaps)) {
      snap <- x$snapshots[[i]]
      if (!is.null(snap$custom) && length(snap$custom) > 0L) {
        row_annotations[[i]] <- vapply(
          names(snap$custom),
          function(nm) .format_custom_result(nm, snap$custom[[nm]]),
          character(1)
        )
      }
    }
  }

  .cli_table(tbl, right_align = c("Rows", "Cols", "NAs"), row_annotations = row_annotations)

  # Print change summaries as a table
  if (n_snaps > 1L) {
  change_rows <- lapply(seq(2L, n_snaps), function(i) {
    snap <- x$snapshots[[i]]
    if (is.null(snap$changes)) return(NULL)
    ch <- snap$changes
    list(
      From = x$snapshots[[i - 1L]]$label,
      To   = snap$label,
      Rows = .format_delta(ch$row_delta),
      Cols = .format_delta(ch$col_delta),
      NAs  = .format_delta(ch$na_delta)
    )
  })
  change_rows <- Filter(Negate(is.null), change_rows)
  }

  if (n_snaps > 1L && length(change_rows) > 0L) {
    cli::cli_text("")
    cli::cli_text("{.strong Changes:}")
    changes_tbl <- data.frame(
      From = vapply(change_rows, `[[`, character(1), "From"),
      To   = vapply(change_rows, `[[`, character(1), "To"),
      Rows = vapply(change_rows, `[[`, character(1), "Rows"),
      Cols = vapply(change_rows, `[[`, character(1), "Cols"),
      NAs  = vapply(change_rows, `[[`, character(1), "NAs"),
      stringsAsFactors = FALSE
    )
    .cli_table(changes_tbl, right_align = c("Rows", "Cols", "NAs"))
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
.cli_table <- function(tbl, right_align = character(), indent = 2L, row_annotations = NULL) {
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

  # Data rows — interleave annotation lines where present
  data_lines <- character(0)
  for (i in seq_len(nrow(tbl))) {
    data_lines <- c(data_lines, format_row(as.character(tbl[i, ])))
    if (!is.null(row_annotations) && !is.null(row_annotations[[i]])) {
      data_lines <- c(data_lines, row_annotations[[i]])
    }
  }

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

#' Format a numeric delta as a signed string for the changes table
#' @noRd
.format_delta <- function(d) {
  if (is.null(d) || is.na(d)) return("=")
  if (d > 0L) paste0("+", format(d, big.mark = ",", trim = TRUE))
  else if (d < 0L) format(d, big.mark = ",", trim = TRUE)
  else "="
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

#' Detect if x is a named collection of scalar values (Case 2 rendering)
#'
#' Returns TRUE for named atomic vectors and named lists whose every element is
#' a length-1 atomic. Returns FALSE for unnamed structures, data frames, and
#' anything with complex/nested elements.
#'
#' @param x Any R object.
#' @returns Logical scalar.
#' @noRd
.is_named_scalars <- function(x) {
  if (length(x) == 0L) return(FALSE)
  if (is.null(names(x)) || !all(nzchar(names(x)))) return(FALSE)
  if (is.data.frame(x)) return(FALSE)
  if (is.atomic(x) && !is.list(x)) return(TRUE)   # named atomic vector
  if (is.list(x)) {
    return(all(vapply(x, function(e) is.atomic(e) && length(e) == 1L, logical(1))))
  }
  FALSE
}

#' Format one custom diagnostic result for inline display
#'
#' @param name Character scalar: the function name as it appears in `snap$custom`.
#' @param result The value returned by the custom function.
#' @param indent Integer: number of leading spaces before the arrow (default 5).
#' @returns A single character string, e.g. `"     ↳ fn_name: key=val"`.
#' @noRd
.format_custom_result <- function(name, result, indent = 5L) {
  pad <- strrep(" ", indent)
  prefix <- paste0(pad, "\u21b3 ", name, ": ")

  # Case 1: single scalar
  if (is.atomic(result) && length(result) == 1L && !is.list(result)) {
    return(paste0(prefix, format(result)))
  }

  # Case 2: named atomic vector or named list of length-1 atomics
  if (.is_named_scalars(result)) {
    pairs <- paste(
      names(result),
      vapply(result, function(v) format(v[[1L]]), character(1)),  # [[1L]] works for both atomic elements and length-1 list elements
      sep = "="
    )
    value_str <- paste(pairs, collapse = ", ")
    if (nchar(value_str) > 60L) {
      value_str <- paste0(substr(value_str, 1L, 60L), "...")
    }
    return(paste0(prefix, value_str))
  }

  # Case 3: complex object
  paste0(prefix, "[complex \u2014 see audit_report()]")
}
