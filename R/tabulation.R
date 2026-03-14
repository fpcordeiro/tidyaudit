#' Tabulate Variables
#'
#' Produces one-way frequency tables or two-way crosstabulations. One variable
#' gives counts, percentages, and cumulative percentages; two variables give a
#' crosstabulation matrix with row/column totals.
#'
#' @param .data A data.frame or tibble.
#' @param ... One or two unquoted column names to tabulate. One variable
#'   produces a one-way frequency table; two produce a two-way crosstabulation.
#' @param .wt Optional unquoted column to use as frequency weights.
#'   When supplied, frequencies are weighted sums instead of row counts.
#' @param .sort How to order the rows (and columns in two-way tables).
#'   `"value_asc"` (default) sorts alphabetically (or by factor levels),
#'   `"value_desc"` sorts in reverse, `"freq_desc"` sorts by frequency
#'   descending, `"freq_asc"` sorts by frequency ascending.
#' @param .cutoff Controls how many values to display.
#'   An integer >= 1 keeps the top-N values by frequency.
#'   A number in (0, 1) keeps values that cumulatively account for that
#'   proportion of the total. Remaining values are grouped under `"(Other)"`.
#'   For two-way tables, the cutoff applies to the row variable only.
#' @param .na How to handle `NA` values. `"include"` (default) treats `NA` as a
#'   category, `"exclude"` drops `NA` rows before tabulation, `"only"` shows
#'   only `NA` rows.
#' @param .display Cell contents for two-way crosstabulations. One of
#'   `"count"` (default), `"row_pct"`, `"col_pct"`, or `"total_pct"`.
#'   Ignored for one-way tables.
#'
#' @returns An S3 object of class `tidyaudit_tab`. Use [as.data.frame()] to
#'   extract the underlying table.
#'
#' @examples
#' tab(mtcars, cyl)
#' tab(mtcars, cyl, .sort = "freq_desc")
#' tab(mtcars, cyl, gear)
#' tab(mtcars, cyl, gear, .display = "row_pct")
#' tab(mtcars, cyl, .wt = mpg)
#' tab(mtcars, cyl, .cutoff = 2)
#'
#' @family data quality
#' @export
tab <- function(.data, ..., .wt = NULL,
                .sort = c("value_asc", "value_desc", "freq_desc", "freq_asc"),
                .cutoff = NULL,
                .na = c("include", "exclude", "only"),
                .display = c("count", "row_pct", "col_pct", "total_pct")) {

  if (!is.data.frame(.data)) {
    cli::cli_abort("{.arg .data} must be a data.frame or tibble.")
  }

  .sort <- match.arg(.sort)
  .na <- match.arg(.na)
  .display <- match.arg(.display)

  # Validate .cutoff
  if (!is.null(.cutoff)) {
    if (!is.numeric(.cutoff) || length(.cutoff) != 1L || is.na(.cutoff) ||
        .cutoff <= 0) {
      cli::cli_abort(
        "{.arg .cutoff} must be a positive number (integer >= 1 for top-N, or a proportion in (0, 1))."
      )
    }
  }

  # Capture variable names via NSE
  dots <- rlang::enquos(...)
  n_vars <- length(dots)

  if (n_vars == 0L) {
    cli::cli_abort("At least one variable must be supplied to {.fn tab}.")
  }
  if (n_vars > 2L) {
    cli::cli_abort("{.fn tab} supports at most 2 variables, not {n_vars}.")
  }

  var_names <- unname(vapply(dots, rlang::as_label, character(1)))

  # Validate columns exist
  missing_cols <- setdiff(var_names, names(.data))
  if (length(missing_cols) > 0L) {
    cli::cli_abort("Column{?s} not found in {.arg .data}: {.field {missing_cols}}")
  }

  # Capture weight
  wt_quo <- rlang::enquo(.wt)
  have_wt <- !rlang::quo_is_null(wt_quo)
  wt_name <- if (have_wt) rlang::as_label(wt_quo) else NULL

  if (have_wt && !wt_name %in% names(.data)) {
    cli::cli_abort("Weight column {.field {wt_name}} not found in {.arg .data}.")
  }

  # Extract vectors
  n_obs <- nrow(.data)
  var1 <- .data[[var_names[1L]]]
  var2 <- if (n_vars == 2L) .data[[var_names[2L]]] else NULL
  wt_vec <- if (have_wt) .data[[wt_name]] else NULL

  # Count NAs: number of rows where any tabulation variable is NA
  has_any_na <- is.na(var1)
  if (!is.null(var2)) has_any_na <- has_any_na | is.na(var2)
  n_missing <- sum(has_any_na)

  # Handle NAs
  if (.na == "exclude") {
    keep <- !has_any_na
    var1 <- var1[keep]
    if (!is.null(var2)) var2 <- var2[keep]
    if (!is.null(wt_vec)) wt_vec <- wt_vec[keep]
  } else if (.na == "only") {
    keep <- has_any_na
    var1 <- var1[keep]
    if (!is.null(var2)) var2 <- var2[keep]
    if (!is.null(wt_vec)) wt_vec <- wt_vec[keep]
  }

  if (n_vars == 1L) {
    .tab_oneway(var1, wt_vec, var_names[1L], n_obs, n_missing,
                .na, .sort, .cutoff, have_wt, wt_name)
  } else {
    .tab_twoway(var1, var2, wt_vec, var_names, n_obs, n_missing,
                .na, .sort, .cutoff, .display, have_wt, wt_name)
  }
}

# -- Internal helpers ---------------------------------------------------------

#' One-way tabulation
#' @noRd
.tab_oneway <- function(var, wt, var_name, n_obs, n_missing,
                         na_mode, sort, cutoff, weighted, wt_name) {
  # Count NAs before removing them for table()
  n_na <- sum(is.na(var))
  include_na <- (na_mode %in% c("include", "only")) && n_na > 0L

  # Compute frequencies on non-NA values (preserving original type for sorting)
  var_nona <- var[!is.na(var)]
  wt_nona <- if (!is.null(wt)) wt[!is.na(var)] else NULL

  if (is.null(wt_nona)) {
    tbl_raw <- table(var_nona, useNA = "no")
    if (length(tbl_raw) == 0L) {
      freq <- data.frame(Value = character(), Freq = numeric(),
                         stringsAsFactors = FALSE)
    } else {
      freq <- as.data.frame(tbl_raw, stringsAsFactors = FALSE)
      names(freq) <- c("Value", "Freq")
      freq$Freq <- as.numeric(freq$Freq)
    }
  } else {
    # Weighted: exclude NA weights silently
    ok <- !is.na(wt_nona)
    freq_vals <- tapply(wt_nona[ok], var_nona[ok], sum)
    freq <- data.frame(
      Value = names(freq_vals),
      Freq = as.numeric(freq_vals),
      stringsAsFactors = FALSE
    )
    # Drop NaN from tapply on empty groups
    freq <- freq[!is.na(freq$Freq), , drop = FALSE]
  }

  # Append <NA> row if including NAs
  if (include_na) {
    na_freq <- if (is.null(wt)) {
      n_na
    } else {
      sum(wt[is.na(var)], na.rm = TRUE)
    }
    freq <- rbind(freq, data.frame(Value = "<NA>", Freq = na_freq,
                                    stringsAsFactors = FALSE))
  }

  # High-cardinality guard
  if (nrow(freq) > 50L && is.null(cutoff)) {
    cli::cli_warn(
      c("{.field {var_name}} has {nrow(freq)} unique values.",
        "i" = "Consider using {.arg .cutoff} to limit the display.")
    )
  }

  # Apply cutoff
  n_other <- 0L
  if (!is.null(cutoff)) {
    result <- .apply_cutoff(freq, cutoff)
    freq <- result$freq
    n_other <- result$n_other
  }

  # Sort
  freq <- .sort_freq(freq, sort, var)

  # Compute percentages
  total <- sum(freq$Freq)
  if (nrow(freq) > 0L) {
    freq$Percent <- if (total > 0) round(100 * freq$Freq / total, 1) else rep(0, nrow(freq))
    freq$Cum_Percent <- pmin(cumsum(freq$Percent), 100.0)
  } else {
    freq$Percent <- numeric()
    freq$Cum_Percent <- numeric()
  }

  # Add total row
  total_row <- data.frame(
    Value = "Total",
    Freq = total,
    Percent = 100.0,
    Cum_Percent = NA_real_,
    stringsAsFactors = FALSE
  )
  tbl <- rbind(freq, total_row)
  row.names(tbl) <- NULL

  structure(
    list(
      type        = "oneway",
      var_name    = var_name,
      n_obs       = n_obs,
      n_missing   = n_missing,
      na_mode     = na_mode,
      sort        = sort,
      cutoff      = cutoff,
      n_other     = n_other,
      weighted    = weighted,
      wt_name     = wt_name,
      table       = tbl
    ),
    class = c("tidyaudit_tab", "list")
  )
}

#' Two-way crosstabulation
#' @noRd
.tab_twoway <- function(var1, var2, wt, var_names, n_obs, n_missing,
                         na_mode, sort, cutoff, display, weighted, wt_name) {
  # Determine which rows have NAs in either variable
  na1 <- is.na(var1)
  na2 <- is.na(var2)
  include_na <- na_mode %in% c("include", "only")

  # Work with non-NA rows for the main table (preserves original types)
  complete <- !na1 & !na2
  v1_complete <- var1[complete]
  v2_complete <- var2[complete]
  wt_complete <- if (!is.null(wt)) wt[complete] else NULL

  # High-cardinality guard
  n_unique1 <- length(unique(v1_complete))
  n_unique2 <- length(unique(v2_complete))
  if ((n_unique1 > 50L || n_unique2 > 50L) && is.null(cutoff)) {
    cli::cli_warn(
      c("Variables have many unique values ({n_unique1} x {n_unique2}).",
        "i" = "Consider using {.arg .cutoff} to limit the display.")
    )
  }

  # Compute marginal frequencies for cutoff (applied to row variable only)
  n_other <- 0L
  if (!is.null(cutoff)) {
    if (is.null(wt_complete)) {
      marginal <- as.data.frame(table(v1_complete, useNA = "no"), stringsAsFactors = FALSE)
      names(marginal) <- c("Value", "Freq")
      marginal$Freq <- as.numeric(marginal$Freq)
    } else {
      ok <- !is.na(wt_complete)
      mfreq <- tapply(wt_complete[ok], v1_complete[ok], sum)
      marginal <- data.frame(
        Value = names(mfreq), Freq = as.numeric(mfreq),
        stringsAsFactors = FALSE
      )
      marginal <- marginal[!is.na(marginal$Freq), , drop = FALSE]
    }
    result <- .apply_cutoff(marginal, cutoff)
    kept_values <- result$freq$Value
    n_other <- result$n_other

    if (n_other > 0L) {
      v1_complete <- ifelse(
        as.character(v1_complete) %in% kept_values,
        as.character(v1_complete), "(Other)"
      )
    }
  }

  # Build cross-table from complete rows
  if (is.null(wt_complete)) {
    ct <- table(v1_complete, v2_complete, useNA = "no")
  } else {
    ok <- !is.na(wt_complete)
    ct <- tapply(wt_complete[ok], list(v1_complete[ok], v2_complete[ok]), sum)
    ct[is.na(ct)] <- 0
  }

  ct_mat <- as.matrix(ct)

  # Append <NA> row/column if including NAs and there are any
  if (include_na) {
    ct_mat <- .append_na_margins(ct_mat, var1, var2, wt, na1, na2)
  }

  # Sort rows
  row_sums <- rowSums(ct_mat)
  ct_mat <- .sort_matrix_rows(ct_mat, row_sums, sort, var1)

  # Sort columns
  col_sums <- colSums(ct_mat)
  ct_mat <- .sort_matrix_cols(ct_mat, col_sums, sort, var2)

  # Build output data.frame with totals
  row_totals <- rowSums(ct_mat)
  col_totals <- colSums(ct_mat)
  grand_total <- sum(ct_mat)

  # Format cells according to .display
  ct_display <- .format_crosstab(ct_mat, row_totals, col_totals,
                                  grand_total, display)

  # Assemble matrix data.frame
  mat_df <- data.frame(
    V1 = c(rownames(ct_mat), "Total"),
    stringsAsFactors = FALSE
  )
  names(mat_df)[1L] <- var_names[1L]

  for (j in seq_len(ncol(ct_display))) {
    col_total_cell <- .format_crosstab_total(col_totals[j])
    mat_df[[colnames(ct_display)[j]]] <- c(ct_display[, j], col_total_cell)
  }

  # Total column
  total_col <- c(
    vapply(seq_len(nrow(ct_mat)), function(i) {
      .format_crosstab_total(row_totals[i])
    }, character(1)),
    .format_number(grand_total)
  )
  mat_df[["Total"]] <- total_col

  structure(
    list(
      type        = "twoway",
      var1_name   = var_names[1L],
      var2_name   = var_names[2L],
      n_obs       = n_obs,
      n_missing   = n_missing,
      na_mode     = na_mode,
      sort        = sort,
      cutoff      = cutoff,
      n_other     = n_other,
      weighted    = weighted,
      wt_name     = wt_name,
      display     = display,
      matrix      = mat_df,
      counts      = ct_mat,
      row_totals  = row_totals,
      col_totals  = col_totals,
      grand_total = grand_total
    ),
    class = c("tidyaudit_tab", "list")
  )
}

#' Apply cutoff to a frequency table
#' @noRd
.apply_cutoff <- function(freq, cutoff) {
  # Sort by frequency descending for cutoff
  freq <- freq[order(-freq$Freq), , drop = FALSE]

  if (cutoff >= 1) {
    # Top-N
    n_keep <- min(as.integer(cutoff), nrow(freq))
  } else {
    # Cumulative proportion
    total <- sum(freq$Freq)
    cum_prop <- cumsum(freq$Freq) / total
    n_keep <- which(cum_prop >= cutoff)[1L]
    if (is.na(n_keep)) n_keep <- nrow(freq)
  }

  if (n_keep >= nrow(freq)) {
    return(list(freq = freq, n_other = 0L))
  }

  kept <- freq[seq_len(n_keep), , drop = FALSE]
  collapsed <- freq[seq(n_keep + 1L, nrow(freq)), , drop = FALSE]
  n_other <- nrow(collapsed)

  other_row <- data.frame(
    Value = "(Other)",
    Freq = sum(collapsed$Freq),
    stringsAsFactors = FALSE
  )
  freq_out <- rbind(kept, other_row)
  row.names(freq_out) <- NULL

  list(freq = freq_out, n_other = n_other)
}

#' Sort a frequency data.frame
#' @noRd
.sort_freq <- function(freq, sort, original_var) {
  # Keep (Other) and <NA> always last
  is_special <- freq$Value %in% c("(Other)", "<NA>")
  special_rows <- freq[is_special, , drop = FALSE]
  main <- freq[!is_special, , drop = FALSE]

  if (sort == "freq_desc") {
    main <- main[order(-main$Freq), , drop = FALSE]
  } else if (sort == "freq_asc") {
    main <- main[order(main$Freq), , drop = FALSE]
  } else if (sort == "value_asc") {
    main <- main[order(.sort_key(main$Value, original_var)), , drop = FALSE]
  } else if (sort == "value_desc") {
    main <- main[order(.sort_key(main$Value, original_var),
                        decreasing = TRUE), , drop = FALSE]
  }

  result <- rbind(main, special_rows)
  row.names(result) <- NULL
  result
}

#' Compute a sort key that respects the original variable type
#'
#' For factors, returns factor-level position. For numeric-origin columns,
#' returns numeric values so that "2" sorts before "10". Otherwise returns
#' the character values for alphabetical sorting.
#' @noRd
.sort_key <- function(labels, original_var) {
  if (is.factor(original_var)) {
    return(match(labels, levels(original_var)))
  }
  if (is.numeric(original_var)) {
    return(suppressWarnings(as.numeric(labels)))
  }
  labels
}

#' Sort matrix rows by criteria
#' @noRd
.sort_matrix_rows <- function(mat, row_sums, sort, original_var) {
  rn <- rownames(mat)
  key <- .sort_key(rn, original_var)
  idx <- switch(sort,
    "value_asc"  = order(key),
    "value_desc" = order(key, decreasing = TRUE),
    "freq_desc"  = order(-row_sums),
    "freq_asc"   = order(row_sums)
  )
  # Keep (Other) and <NA> last
  is_special <- rn %in% c("(Other)", "<NA>")
  if (any(is_special)) {
    idx <- c(idx[!is_special[idx]], which(is_special))
  }
  mat[idx, , drop = FALSE]
}

#' Sort matrix columns by criteria
#' @noRd
.sort_matrix_cols <- function(mat, col_sums, sort, original_var) {
  cn <- colnames(mat)
  key <- .sort_key(cn, original_var)
  idx <- switch(sort,
    "value_asc"  = order(key),
    "value_desc" = order(key, decreasing = TRUE),
    "freq_desc"  = order(-col_sums),
    "freq_asc"   = order(col_sums)
  )
  # Keep (Other) and <NA> last
  is_special <- cn %in% c("(Other)", "<NA>")
  if (any(is_special)) {
    idx <- c(idx[!is_special[idx]], which(is_special))
  }
  mat[, idx, drop = FALSE]
}

#' Append <NA> row and/or column to a crosstab matrix
#' @noRd
.append_na_margins <- function(ct_mat, var1, var2, wt, na1, na2) {
  has_na1 <- any(na1)
  has_na2 <- any(na2)

  if (!has_na1 && !has_na2) return(ct_mat)

  rn <- rownames(ct_mat)
  cn <- colnames(ct_mat)

  # <NA> column: for each non-NA var1 value, count rows where var2 is NA
  if (has_na2) {
    na_col <- vapply(rn, function(rv) {
      mask <- !na1 & as.character(var1) == rv & na2
      if (is.null(wt)) sum(mask) else sum(wt[mask], na.rm = TRUE)
    }, numeric(1))
    ct_mat <- cbind(ct_mat, `<NA>` = na_col)
  }

  # <NA> row: for each non-NA var2 value, count rows where var1 is NA
  if (has_na1) {
    na_row <- vapply(cn, function(cv) {
      mask <- na1 & !na2 & as.character(var2) == cv
      if (is.null(wt)) sum(mask) else sum(wt[mask], na.rm = TRUE)
    }, numeric(1))
    # Both NA corner cell
    if (has_na2) {
      both_na <- if (is.null(wt)) sum(na1 & na2) else sum(wt[na1 & na2], na.rm = TRUE)
      na_row <- c(na_row, both_na)
    }
    ct_mat <- rbind(ct_mat, `<NA>` = na_row)
  }

  ct_mat
}

#' Format crosstab cells according to display mode
#' @noRd
.format_crosstab <- function(ct_mat, row_totals, col_totals,
                              grand_total, display) {
  out <- matrix("", nrow = nrow(ct_mat), ncol = ncol(ct_mat))
  rownames(out) <- rownames(ct_mat)
  colnames(out) <- colnames(ct_mat)

  for (i in seq_len(nrow(ct_mat))) {
    for (j in seq_len(ncol(ct_mat))) {
      val <- ct_mat[i, j]
      out[i, j] <- switch(display,
        "count"     = .format_number(val),
        "row_pct"   = .format_pct_val(val, row_totals[i]),
        "col_pct"   = .format_pct_val(val, col_totals[j]),
        "total_pct" = .format_pct_val(val, grand_total)
      )
    }
  }
  out
}

#' Format a total cell in crosstab (always shows count regardless of display)
#' @noRd
.format_crosstab_total <- function(value) {
  .format_number(value)
}

#' Format a number for display
#' @noRd
.format_number <- function(x) {
  format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
}

#' Format a percentage value
#' @noRd
.format_pct_val <- function(num, denom) {
  if (denom == 0) return("0.0%")
  sprintf("%.1f%%", 100 * num / denom)
}

# -- Print method -------------------------------------------------------------

#' @rdname tab
#' @param x A `tidyaudit_tab` object.
#' @param ... Additional arguments (currently unused).
#' @export
print.tidyaudit_tab <- function(x, ...) {
  if (x$type == "oneway") {
    .print_tab_oneway(x)
  } else {
    .print_tab_twoway(x)
  }
  invisible(x)
}

#' Print one-way tabulation
#' @noRd
.print_tab_oneway <- function(x) {
  header <- if (x$weighted) {
    glue::glue("Tabulation: {x$var_name} (weighted by {x$wt_name})")
  } else {
    glue::glue("Tabulation: {x$var_name}")
  }
  cli::cli_h1(header)

  # Observation line
  na_note <- switch(x$na_mode,
    "include" = if (x$n_missing > 0L) {
      glue::glue(" ({format(x$n_missing, big.mark = ',')} missing \u2014 included as category)")
    } else {
      ""
    },
    "exclude" = if (x$n_missing > 0L) {
      glue::glue(" ({format(x$n_missing, big.mark = ',')} missing \u2014 excluded)")
    } else {
      ""
    },
    "only" = " (showing only missing rows)"
  )
  cli::cli_text("{format(x$n_obs, big.mark = ',')} observations{na_note}")
  cli::cli_text("")

  # Build display table (without the Total row for .cli_table, add separator)
  tbl <- x$table
  n_data <- nrow(tbl) - 1L  # exclude Total row

  if (n_data == 0L) {
    cli::cli_alert_info("No values to tabulate.")
    return(invisible(NULL))
  }

  data_rows <- tbl[seq_len(n_data), , drop = FALSE]
  total_row <- tbl[nrow(tbl), , drop = FALSE]

  # Format for display
  display_tbl <- data.frame(
    Value   = c(data_rows$Value, total_row$Value),
    Freq    = c(
      .format_number(data_rows$Freq),
      .format_number(total_row$Freq)
    ),
    Percent = c(
      sprintf("%.1f%%", data_rows$Percent),
      sprintf("%.1f%%", total_row$Percent)
    ),
    Cum.    = c(
      sprintf("%.1f%%", data_rows$Cum_Percent),
      ""
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  .cli_table_with_footer(display_tbl, n_data,
                          right_align = c("Freq", "Percent", "Cum."))

  # Cutoff footnote
  if (x$n_other > 0L) {
    cli::cli_text("")
    cli::cli_text(
      "  {.emph (Other) collapses {x$n_other} value{?s} below cutoff}"
    )
  }
}

#' Print two-way crosstabulation
#' @noRd
.print_tab_twoway <- function(x) {
  header <- if (x$weighted) {
    glue::glue("Crosstabulation: {x$var1_name} \u00d7 {x$var2_name} (weighted by {x$wt_name})")
  } else {
    glue::glue("Crosstabulation: {x$var1_name} \u00d7 {x$var2_name}")
  }
  cli::cli_h1(header)

  display_label <- switch(x$display,
    "count"     = "count",
    "row_pct"   = "row %",
    "col_pct"   = "column %",
    "total_pct" = "total %"
  )

  na_note <- switch(x$na_mode,
    "include" = if (x$n_missing > 0L) {
      glue::glue(" ({format(x$n_missing, big.mark = ',')} missing \u2014 included)")
    } else {
      ""
    },
    "exclude" = if (x$n_missing > 0L) {
      glue::glue(" ({format(x$n_missing, big.mark = ',')} missing \u2014 excluded)")
    } else {
      ""
    },
    "only" = " (showing only missing rows)"
  )

  cli::cli_text(
    "{format(x$n_obs, big.mark = ',')} observations{na_note} | Cell contents: {display_label}"
  )
  cli::cli_text("")

  mat <- x$matrix
  n_data <- nrow(mat) - 1L  # exclude Total row

  if (n_data == 0L) {
    cli::cli_alert_info("No values to tabulate.")
    return(invisible(NULL))
  }

  # All columns except the first (var1 name) are numeric-display
  right_cols <- names(mat)[-1L]
  .cli_table_with_footer(mat, n_data, right_align = right_cols)

  if (x$n_other > 0L) {
    cli::cli_text("")
    cli::cli_text(
      "  {.emph (Other) collapses {x$n_other} value{?s} below cutoff}"
    )
  }
}

#' Render a table with a separator before the last (Total) row
#'
#' @param tbl data.frame to display (last row is the footer/Total row).
#' @param n_data Number of data rows (everything except Total).
#' @param right_align Character vector of column names to right-align.
#' @noRd
.cli_table_with_footer <- function(tbl, n_data, right_align = character()) {
  nms <- names(tbl)

  # All values as character
  for (j in seq_along(tbl)) {
    tbl[[j]] <- as.character(tbl[[j]])
  }

  # Column widths
  col_widths <- vapply(nms, function(nm) {
    data_width <- if (nrow(tbl) > 0L) max(nchar(tbl[[nm]]), na.rm = TRUE) else 0L
    max(nchar(nm), data_width)
  }, integer(1))

  pad <- "  "
  gap <- "  "

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

  header_line <- format_row(nms)
  sep_cells <- vapply(seq_along(nms), function(j) {
    strrep("\u2500", col_widths[j])
  }, character(1))
  sep_line <- paste0(pad, paste(sep_cells, collapse = gap))

  data_lines <- vapply(seq_len(n_data), function(i) {
    format_row(as.character(tbl[i, ]))
  }, character(1))

  total_line <- format_row(as.character(tbl[nrow(tbl), ]))

  all_lines <- c(header_line, sep_line, data_lines, sep_line, total_line)
  cli::cli_verbatim(paste(all_lines, collapse = "\n"))
}

# -- as.data.frame method ----------------------------------------------------

#' @rdname tab
#' @param row.names Passed to [as.data.frame()]. Default `NULL`.
#' @param optional Passed to [as.data.frame()]. Default `FALSE`.
#' @export
as.data.frame.tidyaudit_tab <- function(x, row.names = NULL,
                                         optional = FALSE, ...) {
  if (x$type == "oneway") {
    # Return data rows only (no Total row)
    tbl <- x$table
    tbl[tbl$Value != "Total", , drop = FALSE]
  } else {
    # Return numeric counts matrix (not the formatted display strings)
    ct <- x$counts
    df <- data.frame(
      V1 = rownames(ct),
      stringsAsFactors = FALSE
    )
    names(df)[1L] <- x$var1_name
    for (j in seq_len(ncol(ct))) {
      df[[colnames(ct)[j]]] <- ct[, j]
    }
    row.names(df) <- NULL
    df
  }
}
