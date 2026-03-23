# Bind a list of data.frames, dropping NULLs. Returns NULL if nothing remains.
.rbind_compact <- function(xs) {
  xs <- Filter(Negate(is.null), xs)
  if (!length(xs)) return(NULL)
  out <- do.call(rbind, xs)
  row.names(out) <- NULL
  out
}

# Shared helper: compute numeric summary, discrepancies, and row-level disc count
# from pre-computed col_diffs. key_fn(keep_indices) returns a key data.frame.
.analyze_numeric_diffs <- function(col_diffs, num_cols, tol, top_n, key_fn) {
  # Numeric summary (quantiles + n_over_tol including NA-vs-value pairs)
  num_list <- lapply(num_cols, function(nm) {
    cd <- col_diffs[[nm]]
    d_clean <- cd$d[!is.na(cd$d)]
    if (!length(d_clean) && !any(cd$na_mismatch)) return(NULL)
    n_over <- sum(d_clean > tol) + sum(cd$na_mismatch)
    if (length(d_clean)) {
      qs <- quantile(d_clean, probs = c(0, 0.25, 0.5, 0.75, 1),
                     na.rm = TRUE, names = FALSE)
    } else {
      qs <- rep(NA_real_, 5L)
    }
    data.frame(column = nm, n = length(d_clean),
               min = qs[1], q25 = qs[2], median = qs[3], q75 = qs[4], max = qs[5],
               n_over_tol = n_over,
               stringsAsFactors = FALSE)
  })
  num_summary <- .rbind_compact(num_list)

  # Row-level discrepancies (numeric diffs > tol OR NA-vs-value)
  disc_list <- lapply(num_cols, function(nm) {
    cd <- col_diffs[[nm]]
    keep <- which((!is.na(cd$d) & cd$d > tol) | cd$na_mismatch)
    if (!length(keep)) return(NULL)
    # Relative difference: abs_diff / max(|value_x|, |value_y|), as proportion
    pct <- ifelse(
      is.na(cd$d[keep]), NA_real_,
      ifelse(cd$v1[keep] == 0 & cd$v2[keep] == 0, 0,
             cd$d[keep] / pmax(abs(cd$v1[keep]), abs(cd$v2[keep])))
    )
    df <- cbind(
      key_fn(keep),
      data.frame(column = nm,
                 value_x = cd$v1[keep], value_y = cd$v2[keep],
                 abs_diff = cd$d[keep], pct_diff = pct,
                 stringsAsFactors = FALSE)
    )
    # Sort: finite diffs descending, then NA diffs at the end
    df <- df[order(is.na(df$abs_diff), -replace(df$abs_diff, is.na(df$abs_diff), 0)), ]
    head(df, top_n)
  })
  discrepancies <- .rbind_compact(disc_list)

  list(
    num_summary = num_summary,
    discrepancies = discrepancies
  )
}

# Build a match_summary list from disc counts and unmatched counts
.build_match_summary <- function(n_with_disc, n_matched, n_only_x, n_only_y) {
  n_no_disc <- n_matched - n_with_disc
  pct_no <- if (n_matched > 0L) n_no_disc / n_matched else NA_real_
  pct_with <- if (n_matched > 0L) n_with_disc / n_matched else NA_real_
  list(
    only_x = n_only_x,
    only_y = n_only_y,
    matched_no_disc = n_no_disc,
    matched_with_disc = n_with_disc,
    pct_no_disc = pct_no,
    pct_with_disc = pct_with
  )
}

# Shared helper: compute categorical summary, discrepancies, and row-level disc count
# from pre-computed col_diffs_cat. key_fn(keep_indices) returns a key data.frame.
.analyze_categorical_diffs <- function(col_diffs_cat, cat_cols, top_n, key_fn) {
  # Categorical summary (n_compared, n_mismatched, pct, NA mismatches)
  cat_list <- lapply(cat_cols, function(nm) {
    cd <- col_diffs_cat[[nm]]
    n_compared <- sum(!is.na(cd$v1) | !is.na(cd$v2))
    n_mis <- sum(cd$mismatch)
    n_na <- sum(cd$na_mismatch)
    if (n_compared == 0L && n_na == 0L) return(NULL)
    data.frame(column = nm, n_compared = n_compared,
               n_mismatched = n_mis,
               pct_mismatched = if (n_compared > 0L) n_mis / n_compared else NA_real_,
               n_na_mismatch = n_na,
               stringsAsFactors = FALSE)
  })
  cat_summary <- .rbind_compact(cat_list)

  # Row-level categorical discrepancies
  disc_list <- lapply(cat_cols, function(nm) {
    cd <- col_diffs_cat[[nm]]
    keep <- which(cd$mismatch)
    if (!length(keep)) return(NULL)
    df <- cbind(
      key_fn(keep),
      data.frame(column = nm,
                 value_x = cd$v1[keep], value_y = cd$v2[keep],
                 stringsAsFactors = FALSE)
    )
    head(df, top_n)
  })
  cat_discrepancies <- .rbind_compact(disc_list)

  list(
    cat_summary = cat_summary,
    cat_discrepancies = cat_discrepancies
  )
}

#' Compare Two Tables
#'
#' Compares two data.frames or tibbles by examining column names, row counts,
#' key overlap, numeric discrepancies, and categorical discrepancies. Useful for
#' validating data processing pipelines.
#'
#' @param x First data.frame or tibble to compare.
#' @param y Second data.frame or tibble to compare.
#' @param key_cols Character vector of column names to use as keys for matching
#'   rows. If `NULL` (default), automatically detects character, factor, and
#'   integer columns as keys.
#' @param tol Numeric tolerance for comparing numeric columns. Differences
#'   less than or equal to `tol` are considered equal. Defaults to
#'   [`.Machine$double.eps`][.Machine] (machine double-precision).
#' @param top_n Maximum number of row-level discrepancies to store **per
#'   column** (numeric and categorical), and maximum unmatched keys to store.
#'   Defaults to `Inf` (all). Unmatched keys are stored in arbitrary order.
#' @param compare_cols Character vector of column names to compare. If `NULL`
#'   (default), all common non-key columns are compared. Mutually exclusive
#'   with `exclude_cols`.
#' @param exclude_cols Character vector of column names to exclude from
#'   comparison. If `NULL` (default), no columns are excluded. Mutually
#'   exclusive with `compare_cols`.
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
#'   \item{comparison_method}{How columns were compared (`"keys"`,
#'     `"row_index"`, or `NA`)}
#'   \item{rows_matched}{Number of rows matched on keys}
#'   \item{tol}{The tolerance used}
#'   \item{top_n}{The top_n used}
#'   \item{discrepancies}{Data.frame of row-level numeric discrepancies
#'     exceeding `tol` (or where one side is `NA`), with key columns (or
#'     `row_index`), `column`, `value_x`, `value_y`, `abs_diff`, and
#'     `pct_diff` (relative difference as a proportion). NULL if none.}
#'   \item{categorical_summary}{Data.frame with `column`, `n_compared`,
#'     `n_mismatched`, `pct_mismatched` (proportion, 0--1), `n_na_mismatch`,
#'     or NULL}
#'   \item{categorical_discrepancies}{Data.frame of row-level categorical
#'     discrepancies with key columns (or `row_index`), `column`, `value_x`,
#'     `value_y`. NULL if none.}
#'   \item{total_discrepancies}{Total number of cell-level discrepancies
#'     across all column types (not limited by `top_n`)}
#'   \item{only_x_keys}{Data.frame of key combinations only in x (up to
#'     `top_n` rows), or NULL}
#'   \item{only_y_keys}{Data.frame of key combinations only in y (up to
#'     `top_n` rows), or NULL}
#'   \item{match_summary}{List with `only_x`, `only_y`, `matched_no_disc`,
#'     `matched_with_disc`, `pct_no_disc` (proportion, 0--1),
#'     `pct_with_disc` (proportion, 0--1)}
#' }
#'
#' Use [as.data.frame()] to extract all discrepancies (numeric and categorical)
#' as a single tidy data.frame.
#'
#' @examples
#' x <- data.frame(id = 1:3, value = c(10.0, 20.0, 30.0))
#' y <- data.frame(id = 1:3, value = c(10.1, 20.0, 30.5))
#' compare_tables(x, y)
#'
#' # With tolerance — differences <= 0.15 are considered equal
#' compare_tables(x, y, tol = 0.15)
#'
#' # Categorical columns are also compared
#' a <- data.frame(id = 1:3, status = c("ok", "warn", "fail"),
#'                  stringsAsFactors = FALSE)
#' b <- data.frame(id = 1:3, status = c("ok", "warn", "error"),
#'                  stringsAsFactors = FALSE)
#' compare_tables(a, b)
#'
#' @family join validation
#' @export
compare_tables <- function(x, y, key_cols = NULL, tol = .Machine$double.eps,
                           top_n = Inf, compare_cols = NULL,
                           exclude_cols = NULL) {
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
  if (!is.null(compare_cols) && !is.null(exclude_cols)) {
    cli::cli_abort("{.arg compare_cols} and {.arg exclude_cols} cannot both be specified.")
  }
  if (!is.null(compare_cols) && (!is.character(compare_cols) || length(compare_cols) == 0L)) {
    cli::cli_abort("{.arg compare_cols} must be a character vector.")
  }
  if (!is.null(exclude_cols) && (!is.character(exclude_cols) || length(exclude_cols) == 0L)) {
    cli::cli_abort("{.arg exclude_cols} must be a character vector.")
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
    type_mismatches <- .rbind_compact(type_list)
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

  # Determine value columns to compare (non-key common columns)
  value_cols <- setdiff(common_cols, key_cols)
  if (!is.null(compare_cols)) {
    bad <- setdiff(compare_cols, value_cols)
    if (length(bad) > 0L) {
      cli::cli_abort(c(
        "Some {.arg compare_cols} not found among common non-key columns.",
        paste0("Not found: ", paste(bad, collapse = ", "))
      ))
    }
    value_cols <- intersect(value_cols, compare_cols)
  }
  if (!is.null(exclude_cols)) {
    unknown <- setdiff(exclude_cols, value_cols)
    if (length(unknown) > 0L) {
      cli::cli_warn(
        "Some {.arg exclude_cols} not found among common non-key columns and will be ignored: {.field {unknown}}."
      )
    }
    value_cols <- setdiff(value_cols, exclude_cols)
  }

  # Split value columns into numeric and categorical
  num_cols <- value_cols[vapply(value_cols, function(nm) {
    if (nm %in% mismatched_cols) return(FALSE)
    is.numeric(x[[nm]]) && is.numeric(y[[nm]])
  }, logical(1L))]

  cat_cols <- value_cols[vapply(value_cols, function(nm) {
    if (nm %in% mismatched_cols) return(FALSE)
    if (nm %in% num_cols) return(FALSE)
    TRUE
  }, logical(1L))]

  num_summary <- NULL
  cat_summary <- NULL
  comparison_method <- NA_character_
  rows_matched <- NA_integer_
  discrepancies <- NULL
  cat_discrepancies <- NULL
  match_summary <- NULL

  has_num <- length(num_cols) > 0L
  has_cat <- length(cat_cols) > 0L

  if (has_num || has_cat) {
    if (length(key_cols) == 0L) {
      comparison_method <- "row_index"
      maxn <- min(n1, n2)
      idx <- seq_len(maxn)

      key_fn <- function(keep) {
        data.frame(row_index = idx[keep], stringsAsFactors = FALSE)
      }

      # Numeric analysis
      if (has_num) {
        col_diffs <- lapply(num_cols, function(nm) {
          v1 <- x[[nm]][idx]
          v2 <- y[[nm]][idx]
          d <- abs(v1 - v2)
          na_mismatch <- is.na(v1) != is.na(v2)
          list(v1 = v1, v2 = v2, d = d, na_mismatch = na_mismatch)
        })
        names(col_diffs) <- num_cols
        analysis <- .analyze_numeric_diffs(col_diffs, num_cols, tol, top_n, key_fn)
        num_summary <- analysis$num_summary
        discrepancies <- analysis$discrepancies
      }

      # Categorical analysis
      if (has_cat) {
        col_diffs_cat <- lapply(cat_cols, function(nm) {
          v1 <- as.character(x[[nm]][idx])
          v2 <- as.character(y[[nm]][idx])
          na_mismatch <- is.na(v1) != is.na(v2)
          mismatch <- na_mismatch | (!is.na(v1) & !is.na(v2) & v1 != v2)
          list(v1 = v1, v2 = v2, mismatch = mismatch, na_mismatch = na_mismatch)
        })
        names(col_diffs_cat) <- cat_cols
        cat_analysis <- .analyze_categorical_diffs(col_diffs_cat, cat_cols, top_n, key_fn)
        cat_summary <- cat_analysis$cat_summary
        cat_discrepancies <- cat_analysis$cat_discrepancies
      }

      # Combined row-level disc count (reuse col_diffs / col_diffs_cat)
      row_has_any <- rep(FALSE, maxn)
      if (has_num) {
        for (nm in num_cols) {
          cd <- col_diffs[[nm]]
          row_has_any <- row_has_any |
            (!is.na(cd$d) & cd$d > tol) | cd$na_mismatch
        }
      }
      if (has_cat) {
        for (nm in cat_cols) {
          row_has_any <- row_has_any | col_diffs_cat[[nm]]$mismatch
        }
      }

      n_only_x <- n1 - maxn
      n_only_y <- n2 - maxn
      match_summary <- .build_match_summary(
        sum(row_has_any), maxn, n_only_x, n_only_y
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
      comparison_method <- "keys"
      # Warn if keys are non-unique — inner_join can cause cartesian expansion
      x_dup <- nrow(x) != nrow(u1)
      y_dup <- nrow(y) != nrow(u2)
      if (x_dup || y_dup) {
        dup_tbls <- c(if (x_dup) name_x, if (y_dup) name_y)
        cli::cli_warn(
          "Key columns are not unique in {dup_tbls}; comparison may be inflated by cartesian row expansion."
        )
      }
      all_value_cols <- union(num_cols, cat_cols)
      x_sub <- x[c(key_cols, all_value_cols)]
      y_sub <- y[c(key_cols, all_value_cols)]
      merged <- dplyr::inner_join(x_sub, y_sub, by = key_cols, suffix = c(".x", ".y"))
      rows_matched <- nrow(merged)

      key_fn <- function(keep) merged[keep, key_cols, drop = FALSE]

      # Numeric analysis
      if (has_num) {
        col_diffs <- lapply(num_cols, function(nm) {
          v1 <- merged[[paste0(nm, ".x")]]
          v2 <- merged[[paste0(nm, ".y")]]
          d <- abs(v1 - v2)
          na_mismatch <- is.na(v1) != is.na(v2)
          list(v1 = v1, v2 = v2, d = d, na_mismatch = na_mismatch)
        })
        names(col_diffs) <- num_cols
        analysis <- .analyze_numeric_diffs(col_diffs, num_cols, tol, top_n, key_fn)
        num_summary <- analysis$num_summary
        discrepancies <- analysis$discrepancies
      }

      # Categorical analysis
      if (has_cat) {
        col_diffs_cat <- lapply(cat_cols, function(nm) {
          v1 <- as.character(merged[[paste0(nm, ".x")]])
          v2 <- as.character(merged[[paste0(nm, ".y")]])
          na_mismatch <- is.na(v1) != is.na(v2)
          mismatch <- na_mismatch | (!is.na(v1) & !is.na(v2) & v1 != v2)
          list(v1 = v1, v2 = v2, mismatch = mismatch, na_mismatch = na_mismatch)
        })
        names(col_diffs_cat) <- cat_cols
        cat_analysis <- .analyze_categorical_diffs(col_diffs_cat, cat_cols, top_n, key_fn)
        cat_summary <- cat_analysis$cat_summary
        cat_discrepancies <- cat_analysis$cat_discrepancies
      }

      # Combined row-level disc count
      row_has_any <- rep(FALSE, rows_matched)
      if (has_num) {
        for (nm in num_cols) {
          cd <- col_diffs[[nm]]
          row_has_any <- row_has_any |
            (!is.na(cd$d) & cd$d > tol) | cd$na_mismatch
        }
      }
      if (has_cat) {
        for (nm in cat_cols) {
          row_has_any <- row_has_any | col_diffs_cat[[nm]]$mismatch
        }
      }

      match_summary <- .build_match_summary(
        sum(row_has_any), rows_matched,
        key_summary$only_x, key_summary$only_y
      )
    }
  } else if (length(key_cols) > 0L) {
    # Keys exist but no value columns to compare
    match_summary <- list(
      only_x = key_summary$only_x,
      only_y = key_summary$only_y,
      matched_no_disc = key_summary$matches,
      matched_with_disc = 0L,
      pct_no_disc = if (key_summary$matches > 0L) 1 else NA_real_,
      pct_with_disc = if (key_summary$matches > 0L) 0 else NA_real_
    )
  }

  # Total discrepancies (true count from summaries, not limited by top_n)
  n_num_total <- if (!is.null(num_summary)) sum(num_summary$n_over_tol) else 0L
  n_cat_total <- if (!is.null(cat_summary)) sum(cat_summary$n_mismatched) else 0L
  total_discrepancies <- n_num_total + n_cat_total

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
    comparison_method = comparison_method,
    rows_matched = rows_matched,
    tol = tol,
    top_n = top_n,
    discrepancies = discrepancies,
    categorical_summary = cat_summary,
    categorical_discrepancies = cat_discrepancies,
    total_discrepancies = total_discrepancies,
    only_x_keys = only_x_keys_df,
    only_y_keys = only_y_keys_df,
    match_summary = match_summary
  )
  structure(result, class = c("compare_tbl", "list"))
}

# Print "... and N more" with an optional note when top_n truncated storage
.print_remaining <- function(total, n_displayed, top_n) {
  remaining <- total - n_displayed
  if (remaining <= 0L) return(invisible())
  msg <- format(remaining, big.mark = ",")
  if (is.finite(top_n) && top_n < total) {
    cli::cli_text("    {.emph ... and {msg} more ({format(top_n, big.mark = ',')} stored)}")
  } else {
    cli::cli_text("    {.emph ... and {msg} more}")
  }
}

#' @rdname compare_tables
#' @param show_n Maximum number of rows to display for discrepancies and
#'   unmatched keys in the printed output. Defaults to `5L`.
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
    if (is.na(x$comparison_method) || x$comparison_method != "row_index") {
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
    tol_lbl <- if (!is.null(tol_val) && tol_val > .Machine$double.eps) {
      paste0(" (tol = ", format(tol_val, scientific = FALSE), ")")
    } else {
      ""
    }
    cli::cli_text("{.strong {sec}. Row matching}{tol_lbl}")
    cli::cli_text("  Only in {x$name_x}: {format(ms$only_x, big.mark = ',')}")
    cli::cli_text("  Only in {x$name_y}: {format(ms$only_y, big.mark = ',')}")
    if (!is.na(ms$pct_no_disc)) {
      pct_no_lbl <- round(ms$pct_no_disc * 100, 1)
      pct_with_lbl <- round(ms$pct_with_disc * 100, 1)
      cli::cli_text("  Matched, no discrepancies: {format(ms$matched_no_disc, big.mark = ',')} ({pct_no_lbl}%)")
      cli::cli_text("  Matched, with discrepancies: {format(ms$matched_with_disc, big.mark = ',')} ({pct_with_lbl}%)")
    } else {
      cli::cli_text("  Matched, no discrepancies: {format(ms$matched_no_disc, big.mark = ',')}")
      cli::cli_text("  Matched, with discrepancies: {format(ms$matched_with_disc, big.mark = ',')}")
    }

    # Total cell discrepancies
    td <- x$total_discrepancies
    if (!is.null(td) && td > 0L) {
      n_num_d <- if (!is.null(x$numeric_summary)) sum(x$numeric_summary$n_over_tol) else 0L
      n_cat_d <- if (!is.null(x$categorical_summary)) sum(x$categorical_summary$n_mismatched) else 0L
      cli::cli_text("  Total cell discrepancies: {format(td, big.mark = ',')} ({format(n_num_d, big.mark = ',')} numeric, {format(n_cat_d, big.mark = ',')} categorical)")
    }

    # Show unmatched keys
    if (!is.null(x$only_x_keys) && nrow(x$only_x_keys) > 0L) {
      cli::cli_text("")
      cli::cli_text("  Unmatched keys in {x$name_x}:")
      display_keys <- head(x$only_x_keys, show_n)
      .cli_table(display_keys, indent = 4L)
      .print_remaining(ms$only_x, nrow(display_keys), x$top_n)
    }
    if (!is.null(x$only_y_keys) && nrow(x$only_y_keys) > 0L) {
      cli::cli_text("")
      cli::cli_text("  Unmatched keys in {x$name_y}:")
      display_keys <- head(x$only_y_keys, show_n)
      .cli_table(display_keys, indent = 4L)
      .print_remaining(ms$only_y, nrow(display_keys), x$top_n)
    }
  }

  # Numeric discrepancies
  cli::cli_text("")
  sec <- sec + 1L
  cli::cli_text("{.strong {sec}. Numeric discrepancies (absolute differences)}")
  if (is.na(x$comparison_method)) {
    cli::cli_text("  No common numeric columns found.")
  } else {
    if (x$comparison_method == "row_index") {
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
      disc_display$pct_diff <- ifelse(is.na(disc_display$pct_diff), "NA",
        paste0(format(round(disc_display$pct_diff * 100, 1), trim = TRUE), "%"))
      # Determine which columns to right-align
      ralign <- intersect(c("row_index", "value_x", "value_y", "abs_diff", "pct_diff"), names(disc_display))
      .cli_table(disc_display, right_align = ralign, indent = 4L)
      remaining <- nrow(x$discrepancies) - nrow(disc_show)
      if (remaining > 0L) {
        cli::cli_text("    {.emph ... and {format(remaining, big.mark = ',')} more}")
      }
    }
  }

  # Categorical discrepancies
  cli::cli_text("")
  sec <- sec + 1L
  cli::cli_text("{.strong {sec}. Categorical discrepancies}")
  has_comparison <- !is.na(x$comparison_method)
  if (!has_comparison) {
    cli::cli_text("  No value columns compared.")
  } else if (is.null(x$categorical_summary) || nrow(x$categorical_summary) == 0L) {
    cli::cli_text("  No categorical discrepancies found.")
  } else {
    cs <- x$categorical_summary
    display_tbl <- data.frame(
      Column     = cs$column,
      N          = format(cs$n_compared, big.mark = ",", trim = TRUE),
      Mismatched = format(cs$n_mismatched, big.mark = ",", trim = TRUE),
      `%Mismatch` = ifelse(is.na(cs$pct_mismatched), "NA",
        paste0(format(round(cs$pct_mismatched * 100, 1), trim = TRUE), "%")),
      `NA mismatch` = format(cs$n_na_mismatch, big.mark = ",", trim = TRUE),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    cli::cli_text("")
    .cli_table(display_tbl, right_align = c("N", "Mismatched", "%Mismatch", "NA mismatch"), indent = 4L)

    n_total_na <- sum(cs$n_na_mismatch)
    if (n_total_na > 0L) {
      cli::cli_text("  {.emph Note: {format(n_total_na, big.mark = ',')} NA-vs-value mismatch(es) are included in the mismatch counts.}")
    }

    if (!is.null(x$categorical_discrepancies) && nrow(x$categorical_discrepancies) > 0L) {
      cli::cli_text("")
      cli::cli_text("  Top discrepancies:")
      disc_show <- head(x$categorical_discrepancies, show_n)
      disc_display <- disc_show
      disc_display$value_x <- ifelse(is.na(disc_display$value_x), "NA", disc_display$value_x)
      disc_display$value_y <- ifelse(is.na(disc_display$value_y), "NA", disc_display$value_y)
      ralign <- intersect(c("row_index"), names(disc_display))
      .cli_table(disc_display, right_align = ralign, indent = 4L)
      remaining <- nrow(x$categorical_discrepancies) - nrow(disc_show)
      if (remaining > 0L) {
        cli::cli_text("    {.emph ... and {format(remaining, big.mark = ',')} more}")
      }
    }
  }

  invisible(x)
}

# -- as.data.frame method ----------------------------------------------------

#' @rdname compare_tables
#' @param row.names Passed to [as.data.frame()]. Default `NULL`.
#' @param optional Passed to [as.data.frame()]. Default `FALSE`.
#' @export
as.data.frame.compare_tbl <- function(x, row.names = NULL, optional = FALSE, ...) {
  # Determine key column names from either source
  key_nms <- character(0L)
  if (!is.null(x$discrepancies) && nrow(x$discrepancies) > 0L) {
    key_nms <- setdiff(names(x$discrepancies),
                       c("column", "value_x", "value_y", "abs_diff", "pct_diff"))
  } else if (!is.null(x$categorical_discrepancies) && nrow(x$categorical_discrepancies) > 0L) {
    key_nms <- setdiff(names(x$categorical_discrepancies),
                       c("column", "value_x", "value_y"))
  }

  num_part <- NULL
  if (!is.null(x$discrepancies) && nrow(x$discrepancies) > 0L) {
    d <- x$discrepancies
    d$value_x <- as.character(d$value_x)
    d$value_y <- as.character(d$value_y)
    d$type <- "numeric"
    num_part <- d[c(key_nms, "column", "type", "value_x", "value_y", "abs_diff", "pct_diff")]
  }

  cat_part <- NULL
  if (!is.null(x$categorical_discrepancies) && nrow(x$categorical_discrepancies) > 0L) {
    d <- x$categorical_discrepancies
    d$type <- "categorical"
    d$abs_diff <- NA_real_
    d$pct_diff <- NA_real_
    cat_part <- d[c(key_nms, "column", "type", "value_x", "value_y", "abs_diff", "pct_diff")]
  }

  out <- .rbind_compact(list(num_part, cat_part))
  if (is.null(out)) {
    # Return empty data.frame with correctly typed columns
    out <- data.frame(
      column = character(0L), type = character(0L),
      value_x = character(0L), value_y = character(0L),
      abs_diff = numeric(0L), pct_diff = numeric(0L),
      stringsAsFactors = FALSE
    )
    if (length(key_nms)) {
      key_df <- as.data.frame(
        setNames(rep(list(character(0L)), length(key_nms)), key_nms),
        stringsAsFactors = FALSE
      )
      out <- cbind(key_df, out)
    }
  }
  row.names(out) <- row.names
  out
}
