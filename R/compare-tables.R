#' Compare Two Tables
#'
#' Compares two data.frames or tibbles by examining column names, row counts,
#' key overlap, and numeric discrepancies. Useful for validating data processing
#' pipelines.
#'
#' @param x First data.frame or tibble to compare.
#' @param y Second data.frame or tibble to compare.
#' @param key_cols Character vector of column names to use as keys for matching
#'   rows. If `NULL` (default), automatically detects character, factor, and
#'   integer columns as keys.
#' @param tol Numeric tolerance for comparing numeric columns. Differences
#'   less than or equal to `tol` are considered equal. Defaults to
#'   [`.Machine$double.eps`][.Machine] (machine double-precision).
#' @param top_n Maximum number of row-level discrepancies to store **per numeric
#'   column**, and maximum unmatched keys to store. Defaults to `Inf` (all).
#'   Unmatched keys are stored in arbitrary order.
#'
#' @returns An S3 object of class `compare_tbl` containing:
#' \describe{
#'   \item{name_x, name_y}{Names of the compared objects}
#'   \item{common_columns}{Column names present in both tables}
#'   \item{only_x}{Column names only in x}
#'   \item{only_y}{Column names only in y}
#'   \item{type_mismatches}{Data.frame of columns with different types, or NULL}
#'   \item{nrow_x}{Number of rows in x}
#'   \item{nrow_y}{Number of rows in y}
#'   \item{key_summary}{Summary of key overlap, or NULL}
#'   \item{numeric_summary}{Data.frame of numeric discrepancy quantiles (with
#'     `n_over_tol` count), or NULL}
#'   \item{numeric_method}{How numeric columns were compared}
#'   \item{rows_matched}{Number of rows matched on keys}
#'   \item{tol}{The tolerance used}
#'   \item{top_n}{The top_n used}
#'   \item{discrepancies}{Data.frame of row-level numeric discrepancies
#'     exceeding `tol` (or where one side is `NA`), with key columns (or
#'     `row_index`), `column`, `value_x`, `value_y`, and `abs_diff`. Rows with
#'     `abs_diff = NA` indicate that one value is `NA`. NULL if none.}
#'   \item{only_x_keys}{Data.frame of key combinations only in x (up to
#'     `top_n` rows), or NULL}
#'   \item{only_y_keys}{Data.frame of key combinations only in y (up to
#'     `top_n` rows), or NULL}
#'   \item{match_summary}{List with `only_x`, `only_y`, `matched_no_disc`,
#'     `matched_with_disc`, `pct_no_disc`, `pct_with_disc`}
#' }
#'
#' @examples
#' x <- data.frame(id = 1:3, value = c(10.0, 20.0, 30.0))
#' y <- data.frame(id = 1:3, value = c(10.1, 20.0, 30.5))
#' compare_tables(x, y)
#'
#' # With tolerance — differences <= 0.15 are considered equal
#' compare_tables(x, y, tol = 0.15)
#'
#' @family join validation
#' @export
compare_tables <- function(x, y, key_cols = NULL, tol = .Machine$double.eps, top_n = Inf) {
  name_x <- deparse(substitute(x))
  name_y <- deparse(substitute(y))

  if (!is.data.frame(x)) {
    cli::cli_abort("{.arg x} must be a data.frame or tibble.")
  }
  if (!is.data.frame(y)) {
    cli::cli_abort("{.arg y} must be a data.frame or tibble.")
  }
  if (!is.numeric(tol) || length(tol) != 1L || is.na(tol) || tol < 0) {
    cli::cli_abort("{.arg tol} must be a single non-negative number.")
  }
  if (!is.numeric(top_n) || length(top_n) != 1L || is.na(top_n) || top_n <= 0) {
    cli::cli_abort("{.arg top_n} must be a single positive number or {.code Inf}.")
  }

  n1 <- nrow(x)
  n2 <- nrow(y)

  names1 <- names(x)
  names2 <- names(y)

  common_cols <- intersect(names1, names2)
  only_x <- setdiff(names1, names2)
  only_y <- setdiff(names2, names1)

  # Type mismatches in common columns
  type_mismatches <- NULL
  if (length(common_cols) > 0L) {
    type_list <- lapply(common_cols, function(nm) {
      type1 <- class(x[[nm]])[1L]
      type2 <- class(y[[nm]])[1L]
      if (type1 != type2) {
        data.frame(column = nm, type_x = type1, type_y = type2,
                   stringsAsFactors = FALSE)
      } else {
        NULL
      }
    })
    type_list <- Filter(Negate(is.null), type_list)
    if (length(type_list)) {
      type_mismatches <- do.call(rbind, type_list)
      row.names(type_mismatches) <- NULL
    }
  }

  if (length(common_cols) == 0L) {
    cli::cli_abort("No matching column names between {.arg x} and {.arg y}.")
  }

  # Determine keys
  mismatched_cols <- if (!is.null(type_mismatches)) type_mismatches$column else character(0L)
  auto_keys <- FALSE

  if (is.null(key_cols)) {
    key_cols <- common_cols[vapply(common_cols, function(nm) {
      if (nm %in% mismatched_cols) return(FALSE)
      is_key_type <- function(col) is.integer(col) || is.factor(col) || is.character(col)
      is_key_type(x[[nm]]) && is_key_type(y[[nm]])
    }, logical(1L))]
    auto_keys <- TRUE
  } else {
    miss1 <- setdiff(key_cols, names1)
    miss2 <- setdiff(key_cols, names2)
    if (length(miss1) > 0L || length(miss2) > 0L) {
      cli::cli_abort(c(
        "Some {.arg key_cols} not present in both tables.",
        if (length(miss1)) paste0("Missing from x: ", paste(miss1, collapse = ", ")),
        if (length(miss2)) paste0("Missing from y: ", paste(miss2, collapse = ", "))
      ))
    }
  }

  # Key overlap
  key_summary <- NULL
  only_x_keys_df <- NULL
  only_y_keys_df <- NULL
  if (length(key_cols) > 0L) {
    u1 <- dplyr::distinct(x[key_cols])
    u2 <- dplyr::distinct(y[key_cols])
    match_keys <- dplyr::intersect(u1, u2)
    only_x_keys_full <- dplyr::setdiff(u1, u2)
    only_y_keys_full <- dplyr::setdiff(u2, u1)

    key_summary <- list(
      keys = key_cols,
      auto = auto_keys,
      x_unique = nrow(u1),
      y_unique = nrow(u2),
      matches = nrow(match_keys),
      only_x = nrow(only_x_keys_full),
      only_y = nrow(only_y_keys_full)
    )

    if (nrow(only_x_keys_full) > 0L) {
      only_x_keys_df <- as.data.frame(head(only_x_keys_full, top_n),
                                       stringsAsFactors = FALSE)
    }
    if (nrow(only_y_keys_full) > 0L) {
      only_y_keys_df <- as.data.frame(head(only_y_keys_full, top_n),
                                       stringsAsFactors = FALSE)
    }
  }

  # Numeric discrepancies for common numeric columns
  num_cols <- common_cols[vapply(common_cols, function(nm) {
    is.numeric(x[[nm]]) && is.numeric(y[[nm]])
  }, logical(1L))]
  num_cols <- setdiff(num_cols, key_cols)

  num_summary <- NULL
  numeric_method <- NA_character_
  rows_matched <- NA_integer_
  discrepancies <- NULL
  match_summary <- NULL

  if (length(num_cols) > 0L) {
    if (length(key_cols) == 0L) {
      numeric_method <- "row_index"
      maxn <- min(n1, n2)
      idx <- seq_len(maxn)

      # Cache per-column diffs and NA-mismatch flags
      col_diffs <- lapply(num_cols, function(nm) {
        v1 <- x[[nm]][idx]
        v2 <- y[[nm]][idx]
        d <- abs(v1 - v2)
        na_mismatch <- is.na(v1) != is.na(v2)
        list(v1 = v1, v2 = v2, d = d, na_mismatch = na_mismatch)
      })
      names(col_diffs) <- num_cols

      num_list <- lapply(num_cols, function(nm) {
        cd <- col_diffs[[nm]]
        d_clean <- cd$d[!is.na(cd$d)]
        if (!length(d_clean)) return(NULL)
        n_over <- sum(d_clean > tol)
        qs <- quantile(d_clean, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE, names = FALSE)
        data.frame(column = nm, n = length(d_clean),
                   min = qs[1], q25 = qs[2], median = qs[3], q75 = qs[4], max = qs[5],
                   n_over_tol = n_over,
                   stringsAsFactors = FALSE)
      })
      num_list <- Filter(Negate(is.null), num_list)
      if (length(num_list)) {
        num_summary <- do.call(rbind, num_list)
        row.names(num_summary) <- NULL
      }

      # Row-level discrepancies (numeric diffs > tol OR NA-vs-value)
      disc_list <- lapply(num_cols, function(nm) {
        cd <- col_diffs[[nm]]
        keep <- which((!is.na(cd$d) & cd$d > tol) | cd$na_mismatch)
        if (!length(keep)) return(NULL)
        df <- data.frame(row_index = idx[keep], column = nm,
                         value_x = cd$v1[keep], value_y = cd$v2[keep],
                         abs_diff = cd$d[keep],
                         stringsAsFactors = FALSE)
        # Sort: finite diffs descending, then NA diffs at the end
        df <- df[order(is.na(df$abs_diff), -replace(df$abs_diff, is.na(df$abs_diff), 0)), ]
        head(df, top_n)
      })
      disc_list <- Filter(Negate(is.null), disc_list)
      if (length(disc_list)) {
        discrepancies <- do.call(rbind, disc_list)
        row.names(discrepancies) <- NULL
      }

      # Match summary for row-index mode
      # A row has a discrepancy if any column has diff > tol or NA-vs-value
      row_has_disc <- rep(FALSE, maxn)
      for (nm in num_cols) {
        cd <- col_diffs[[nm]]
        row_has_disc <- row_has_disc |
          (!is.na(cd$d) & cd$d > tol) | cd$na_mismatch
      }
      n_with_disc <- sum(row_has_disc)
      n_no_disc <- maxn - n_with_disc
      n_only_x <- if (n1 > maxn) n1 - maxn else 0L
      n_only_y <- if (n2 > maxn) n2 - maxn else 0L
      pct_no <- if (maxn > 0L) round(100 * n_no_disc / maxn, 1) else NA_real_
      pct_with <- if (maxn > 0L) round(100 * n_with_disc / maxn, 1) else NA_real_

      match_summary <- list(
        only_x = n_only_x,
        only_y = n_only_y,
        matched_no_disc = n_no_disc,
        matched_with_disc = n_with_disc,
        pct_no_disc = pct_no,
        pct_with_disc = pct_with
      )

      # Unmatched row indices for row-index mode
      if (n_only_x > 0L) {
        extra_idx <- seq.int(maxn + 1L, n1)
        only_x_keys_df <- data.frame(
          row_index = head(extra_idx, top_n),
          stringsAsFactors = FALSE
        )
      }
      if (n_only_y > 0L) {
        extra_idx <- seq.int(maxn + 1L, n2)
        only_y_keys_df <- data.frame(
          row_index = head(extra_idx, top_n),
          stringsAsFactors = FALSE
        )
      }
    } else {
      numeric_method <- "keys"
      # Warn if keys are non-unique — inner_join can cause cartesian expansion
      x_dup <- nrow(x) != nrow(dplyr::distinct(x[key_cols]))
      y_dup <- nrow(y) != nrow(dplyr::distinct(y[key_cols]))
      if (x_dup || y_dup) {
        dup_tbls <- c(if (x_dup) name_x, if (y_dup) name_y)
        cli::cli_warn(
          "Key columns are not unique in {dup_tbls}; numeric comparison may be inflated by cartesian row expansion."
        )
      }
      x_sub <- x[c(key_cols, num_cols)]
      y_sub <- y[c(key_cols, num_cols)]
      merged <- dplyr::inner_join(x_sub, y_sub, by = key_cols, suffix = c(".x", ".y"))
      rows_matched <- nrow(merged)

      # Cache per-column diffs and NA-mismatch flags
      col_diffs <- lapply(num_cols, function(nm) {
        v1 <- merged[[paste0(nm, ".x")]]
        v2 <- merged[[paste0(nm, ".y")]]
        d <- abs(v1 - v2)
        na_mismatch <- is.na(v1) != is.na(v2)
        list(v1 = v1, v2 = v2, d = d, na_mismatch = na_mismatch)
      })
      names(col_diffs) <- num_cols

      num_list <- lapply(num_cols, function(nm) {
        cd <- col_diffs[[nm]]
        d_clean <- cd$d[!is.na(cd$d)]
        if (!length(d_clean)) return(NULL)
        n_over <- sum(d_clean > tol)
        qs <- quantile(d_clean, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE, names = FALSE)
        data.frame(column = nm, n = length(d_clean),
                   min = qs[1], q25 = qs[2], median = qs[3], q75 = qs[4], max = qs[5],
                   n_over_tol = n_over,
                   stringsAsFactors = FALSE)
      })
      num_list <- Filter(Negate(is.null), num_list)
      if (length(num_list)) {
        num_summary <- do.call(rbind, num_list)
        row.names(num_summary) <- NULL
      }

      # Row-level discrepancies (numeric diffs > tol OR NA-vs-value)
      disc_list <- lapply(num_cols, function(nm) {
        cd <- col_diffs[[nm]]
        keep <- which((!is.na(cd$d) & cd$d > tol) | cd$na_mismatch)
        if (!length(keep)) return(NULL)
        key_df <- merged[keep, key_cols, drop = FALSE]
        df <- cbind(
          key_df,
          data.frame(column = nm,
                     value_x = cd$v1[keep], value_y = cd$v2[keep],
                     abs_diff = cd$d[keep],
                     stringsAsFactors = FALSE)
        )
        df <- df[order(is.na(df$abs_diff), -replace(df$abs_diff, is.na(df$abs_diff), 0)), ]
        head(df, top_n)
      })
      disc_list <- Filter(Negate(is.null), disc_list)
      if (length(disc_list)) {
        discrepancies <- do.call(rbind, disc_list)
        row.names(discrepancies) <- NULL
      }

      # Match summary for keys mode
      # A row has a discrepancy if any column has diff > tol or NA-vs-value
      row_has_disc <- rep(FALSE, rows_matched)
      for (nm in num_cols) {
        cd <- col_diffs[[nm]]
        row_has_disc <- row_has_disc |
          (!is.na(cd$d) & cd$d > tol) | cd$na_mismatch
      }
      n_with_disc <- sum(row_has_disc)
      n_no_disc <- rows_matched - n_with_disc
      pct_no <- if (rows_matched > 0L) round(100 * n_no_disc / rows_matched, 1) else NA_real_
      pct_with <- if (rows_matched > 0L) round(100 * n_with_disc / rows_matched, 1) else NA_real_

      match_summary <- list(
        only_x = key_summary$only_x,
        only_y = key_summary$only_y,
        matched_no_disc = n_no_disc,
        matched_with_disc = n_with_disc,
        pct_no_disc = pct_no,
        pct_with_disc = pct_with
      )
    }
  } else if (length(key_cols) > 0L) {
    # Keys exist but no numeric columns — match summary based on key overlap only
    match_summary <- list(
      only_x = key_summary$only_x,
      only_y = key_summary$only_y,
      matched_no_disc = key_summary$matches,
      matched_with_disc = 0L,
      pct_no_disc = if (key_summary$matches > 0L) 100 else NA_real_,
      pct_with_disc = if (key_summary$matches > 0L) 0 else NA_real_
    )
  }

  result <- list(
    name_x = name_x,
    name_y = name_y,
    common_columns = common_cols,
    only_x = only_x,
    only_y = only_y,
    type_mismatches = type_mismatches,
    nrow_x = n1,
    nrow_y = n2,
    key_summary = key_summary,
    numeric_summary = num_summary,
    numeric_method = numeric_method,
    rows_matched = rows_matched,
    tol = tol,
    top_n = top_n,
    discrepancies = discrepancies,
    only_x_keys = only_x_keys_df,
    only_y_keys = only_y_keys_df,
    match_summary = match_summary
  )
  structure(result, class = c("compare_tbl", "list"))
}

#' @rdname compare_tables
#' @param show_n Maximum number of rows to display for discrepancies and
#'   unmatched keys in the printed output. Defaults to `5`.
#' @param ... Additional arguments (currently unused).
#' @export
print.compare_tbl <- function(x, show_n = 5L, ...) {
  if (!is.numeric(show_n) || length(show_n) != 1L || is.na(show_n) || show_n < 0) {
    cli::cli_abort("{.arg show_n} must be a single non-negative integer.")
  }
  show_n <- as.integer(show_n)

  cli::cli_h1("Table Comparison: {x$name_x} vs {x$name_y}")
  sec <- 0L

  # Row counts
  sec <- sec + 1L
  cli::cli_text("{.strong {sec}. Row counts}")
  row_diff <- x$nrow_x - x$nrow_y
  cli::cli_text("  {x$name_x}: {format(x$nrow_x, big.mark = ',')} rows")
  cli::cli_text("  {x$name_y}: {format(x$nrow_y, big.mark = ',')} rows")
  diff_str <- .format_delta(row_diff)
  cli::cli_text("  Difference: {diff_str}")

  # Column names
  cli::cli_text("")
  sec <- sec + 1L
  cli::cli_text("{.strong {sec}. Column names}")
  cli::cli_text("  Matching columns: {length(x$common_columns)}")
  if (length(x$only_x) > 0L) {
    cli::cli_text("  Only in {x$name_x}: {length(x$only_x)} ({.field {x$only_x}})")
  } else {
    cli::cli_text("  Only in {x$name_x}: 0")
  }
  if (length(x$only_y) > 0L) {
    cli::cli_text("  Only in {x$name_y}: {length(x$only_y)} ({.field {x$only_y}})")
  } else {
    cli::cli_text("  Only in {x$name_y}: 0")
  }

  n_mismatches <- if (is.null(x$type_mismatches)) 0L else nrow(x$type_mismatches)
  cli::cli_text("  Type mismatches: {n_mismatches}")
  if (n_mismatches > 0L) {
    print(x$type_mismatches)
  }

  # Key summary
  cli::cli_text("")
  sec <- sec + 1L
  cli::cli_text("{.strong {sec}. Key columns}")
  ks <- x$key_summary
  if (is.null(ks)) {
    if (is.na(x$numeric_method) || x$numeric_method != "row_index") {
      cli::cli_text("  No common character/factor/integer columns found.")
    } else {
      cli::cli_text("  No key columns; comparing by row index.")
    }
  } else {
    auto_lbl <- if (ks$auto) " (auto-detected)" else ""
    cli::cli_text("  Key columns: {.field {ks$keys}}{auto_lbl}")
    cli::cli_text("  Distinct combos in {x$name_x}: {format(ks$x_unique, big.mark = ',')}")
    cli::cli_text("  Distinct combos in {x$name_y}: {format(ks$y_unique, big.mark = ',')}")
  }

  # Row matching (only shown when match_summary exists)
  ms <- x$match_summary
  if (!is.null(ms)) {
    cli::cli_text("")
    sec <- sec + 1L
    tol_val <- x$tol
    tol_lbl <- if (!is.null(tol_val) && tol_val > 0) paste0(" (tol = ", tol_val, ")") else ""
    cli::cli_text("{.strong {sec}. Row matching}{tol_lbl}")
    cli::cli_text("  Only in {x$name_x}: {format(ms$only_x, big.mark = ',')}")
    cli::cli_text("  Only in {x$name_y}: {format(ms$only_y, big.mark = ',')}")
    if (!is.na(ms$pct_no_disc)) {
      cli::cli_text("  Matched, no discrepancies: {format(ms$matched_no_disc, big.mark = ',')} ({ms$pct_no_disc}%)")
      cli::cli_text("  Matched, with discrepancies: {format(ms$matched_with_disc, big.mark = ',')} ({ms$pct_with_disc}%)")
    } else {
      cli::cli_text("  Matched, no discrepancies: {format(ms$matched_no_disc, big.mark = ',')}")
      cli::cli_text("  Matched, with discrepancies: {format(ms$matched_with_disc, big.mark = ',')}")
    }

    # Show unmatched keys
    if (!is.null(x$only_x_keys) && nrow(x$only_x_keys) > 0L) {
      cli::cli_text("")
      cli::cli_text("  Unmatched keys in {x$name_x}:")
      display_keys <- head(x$only_x_keys, show_n)
      .cli_table(display_keys, indent = 4L)
      remaining <- ms$only_x - nrow(display_keys)
      if (remaining > 0L) {
        cli::cli_text("    {.emph ... and {format(remaining, big.mark = ',')} more}")
      }
    }
    if (!is.null(x$only_y_keys) && nrow(x$only_y_keys) > 0L) {
      cli::cli_text("")
      cli::cli_text("  Unmatched keys in {x$name_y}:")
      display_keys <- head(x$only_y_keys, show_n)
      .cli_table(display_keys, indent = 4L)
      remaining <- ms$only_y - nrow(display_keys)
      if (remaining > 0L) {
        cli::cli_text("    {.emph ... and {format(remaining, big.mark = ',')} more}")
      }
    }
  }

  # Numeric discrepancies
  cli::cli_text("")
  sec <- sec + 1L
  cli::cli_text("{.strong {sec}. Numeric discrepancies (absolute differences)}")
  if (is.na(x$numeric_method)) {
    cli::cli_text("  No common numeric columns found.")
  } else {
    if (x$numeric_method == "row_index") {
      cli::cli_text("  Compared by row index.")
    } else {
      cli::cli_text("  Compared after merging on keys.")
      cli::cli_text("  Rows matched: {format(x$rows_matched, big.mark = ',')}")
    }

    if (!is.null(x$numeric_summary) && nrow(x$numeric_summary) > 0L) {
      ns <- x$numeric_summary
      display_tbl <- data.frame(
        Column     = ns$column,
        N          = format(ns$n, big.mark = ",", trim = TRUE),
        Min        = format(round(ns$min, 4), trim = TRUE),
        Q25        = format(round(ns$q25, 4), trim = TRUE),
        Median     = format(round(ns$median, 4), trim = TRUE),
        Q75        = format(round(ns$q75, 4), trim = TRUE),
        Max        = format(round(ns$max, 4), trim = TRUE),
        `>tol`     = format(ns$n_over_tol, big.mark = ",", trim = TRUE),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      cli::cli_text("")
      .cli_table(display_tbl, right_align = c("N", "Min", "Q25", "Median", "Q75", "Max", ">tol"), indent = 4L)
    }

    # Show top discrepancies
    if (!is.null(x$discrepancies) && nrow(x$discrepancies) > 0L) {
      cli::cli_text("")
      cli::cli_text("  Top discrepancies:")
      disc_show <- head(x$discrepancies, show_n)
      # Format numeric columns for display (handle NA values)
      disc_display <- disc_show
      disc_display$value_x <- ifelse(is.na(disc_display$value_x), "NA",
        format(round(disc_display$value_x, 4), trim = TRUE))
      disc_display$value_y <- ifelse(is.na(disc_display$value_y), "NA",
        format(round(disc_display$value_y, 4), trim = TRUE))
      disc_display$abs_diff <- ifelse(is.na(disc_display$abs_diff), "NA",
        format(round(disc_display$abs_diff, 4), trim = TRUE))
      # Determine which columns to right-align
      ralign <- intersect(c("row_index", "value_x", "value_y", "abs_diff"), names(disc_display))
      .cli_table(disc_display, right_align = ralign, indent = 4L)
      remaining <- nrow(x$discrepancies) - nrow(disc_show)
      if (remaining > 0L) {
        cli::cli_text("    {.emph ... and {format(remaining, big.mark = ',')} more}")
      }
    }
  }

  invisible(x)
}
