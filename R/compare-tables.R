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
#'
#' @returns An S3 object of class `compare_tbl` containing:
#' \describe{
#'   \item{name1, name2}{Names of the compared objects}
#'   \item{common_columns}{Column names present in both tables}
#'   \item{only_x}{Column names only in x}
#'   \item{only_y}{Column names only in y}
#'   \item{type_mismatches}{Data.frame of columns with different types, or NULL}
#'   \item{nrow_x}{Number of rows in x}
#'   \item{nrow_y}{Number of rows in y}
#'   \item{key_summary}{Summary of key overlap, or NULL}
#'   \item{numeric_summary}{Data.frame of numeric discrepancies, or NULL}
#'   \item{numeric_method}{How numeric columns were compared}
#'   \item{rows_matched}{Number of rows matched on keys}
#' }
#'
#' @examples
#' x <- data.frame(id = 1:3, value = c(10.0, 20.0, 30.0))
#' y <- data.frame(id = 1:3, value = c(10.1, 20.0, 30.5))
#' compare_tables(x, y)
#'
#' @family join validation
#' @export
compare_tables <- function(x, y, key_cols = NULL) {
  name1 <- deparse(substitute(x))
  name2 <- deparse(substitute(y))

  if (!is.data.frame(x)) {
    cli::cli_abort("{.arg x} must be a data.frame or tibble.")
  }
  if (!is.data.frame(y)) {
    cli::cli_abort("{.arg y} must be a data.frame or tibble.")
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
  if (length(key_cols) > 0L) {
    u1 <- dplyr::distinct(x[key_cols])
    u2 <- dplyr::distinct(y[key_cols])
    match_keys <- dplyr::intersect(u1, u2)
    only_x_keys <- dplyr::setdiff(u1, u2)
    only_y_keys <- dplyr::setdiff(u2, u1)

    key_summary <- list(
      keys = key_cols,
      auto = auto_keys,
      x_unique = nrow(u1),
      y_unique = nrow(u2),
      matches = nrow(match_keys),
      only_x = nrow(only_x_keys),
      only_y = nrow(only_y_keys)
    )
  }

  # Numeric discrepancies for common numeric columns
  num_cols <- common_cols[vapply(common_cols, function(nm) {
    is.numeric(x[[nm]]) && is.numeric(y[[nm]])
  }, logical(1L))]
  num_cols <- setdiff(num_cols, key_cols)

  num_summary <- NULL
  numeric_method <- NA_character_
  rows_matched <- NA_integer_

  if (length(num_cols) > 0L) {
    if (length(key_cols) == 0L) {
      numeric_method <- "row_index"
      maxn <- min(n1, n2)
      idx <- seq_len(maxn)

      num_list <- lapply(num_cols, function(nm) {
        d <- abs(x[[nm]][idx] - y[[nm]][idx])
        d <- d[!is.na(d)]
        if (!length(d)) return(NULL)
        qs <- quantile(d, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE, names = FALSE)
        data.frame(column = nm, n = length(d),
                   min = qs[1], q25 = qs[2], median = qs[3], q75 = qs[4], max = qs[5],
                   stringsAsFactors = FALSE)
      })
      num_list <- Filter(Negate(is.null), num_list)
      if (length(num_list)) {
        num_summary <- do.call(rbind, num_list)
        row.names(num_summary) <- NULL
      }
    } else {
      numeric_method <- "keys"
      # Warn if keys are non-unique — inner_join can cause cartesian expansion
      x_dup <- nrow(x) != nrow(dplyr::distinct(x[key_cols]))
      y_dup <- nrow(y) != nrow(dplyr::distinct(y[key_cols]))
      if (x_dup || y_dup) {
        dup_tbls <- c(if (x_dup) name1, if (y_dup) name2)
        cli::cli_warn(
          "Key columns are not unique in {dup_tbls}; numeric comparison may be inflated by cartesian row expansion."
        )
      }
      x_sub <- x[c(key_cols, num_cols)]
      y_sub <- y[c(key_cols, num_cols)]
      merged <- dplyr::inner_join(x_sub, y_sub, by = key_cols, suffix = c(".x", ".y"))
      rows_matched <- nrow(merged)

      num_list <- lapply(num_cols, function(nm) {
        v1 <- merged[[paste0(nm, ".x")]]
        v2 <- merged[[paste0(nm, ".y")]]
        d <- abs(v1 - v2)
        d <- d[!is.na(d)]
        if (!length(d)) return(NULL)
        qs <- quantile(d, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE, names = FALSE)
        data.frame(column = nm, n = length(d),
                   min = qs[1], q25 = qs[2], median = qs[3], q75 = qs[4], max = qs[5],
                   stringsAsFactors = FALSE)
      })
      num_list <- Filter(Negate(is.null), num_list)
      if (length(num_list)) {
        num_summary <- do.call(rbind, num_list)
        row.names(num_summary) <- NULL
      }
    }
  }

  result <- list(
    name1 = name1,
    name2 = name2,
    common_columns = common_cols,
    only_x = only_x,
    only_y = only_y,
    type_mismatches = type_mismatches,
    nrow_x = n1,
    nrow_y = n2,
    key_summary = key_summary,
    numeric_summary = num_summary,
    numeric_method = numeric_method,
    rows_matched = rows_matched
  )
  structure(result, class = c("compare_tbl", "list"))
}

#' @rdname compare_tables
#' @param ... Additional arguments (currently unused).
#' @export
print.compare_tbl <- function(x, ...) {
  cli::cli_h1("Table Comparison: {x$name1} vs {x$name2}")

  # 1. Row counts
  cli::cli_text("{.strong 1. Row counts}")
  row_diff <- x$nrow_x - x$nrow_y
  cli::cli_text("  {x$name1}: {format(x$nrow_x, big.mark = ',')} rows")
  cli::cli_text("  {x$name2}: {format(x$nrow_y, big.mark = ',')} rows")
  diff_str <- .format_delta(row_diff)
  cli::cli_text("  Difference: {diff_str}")

  # 2. Column names
  cli::cli_text("")
  cli::cli_text("{.strong 2. Column names}")
  cli::cli_text("  Matching columns: {length(x$common_columns)}")
  if (length(x$only_x) > 0L) {
    cli::cli_text("  Only in {x$name1}: {length(x$only_x)} ({.field {x$only_x}})")
  } else {
    cli::cli_text("  Only in {x$name1}: 0")
  }
  if (length(x$only_y) > 0L) {
    cli::cli_text("  Only in {x$name2}: {length(x$only_y)} ({.field {x$only_y}})")
  } else {
    cli::cli_text("  Only in {x$name2}: 0")
  }

  n_mismatches <- if (is.null(x$type_mismatches)) 0L else nrow(x$type_mismatches)
  cli::cli_text("  Type mismatches: {n_mismatches}")
  if (n_mismatches > 0L) {
    print(x$type_mismatches)
  }

  # 3. Key summary
  cli::cli_text("")
  cli::cli_text("{.strong 3. Key columns}")
  ks <- x$key_summary
  if (is.null(ks)) {
    cli::cli_text("  No common character/factor/integer columns found.")
  } else {
    auto_lbl <- if (ks$auto) " (auto-detected)" else ""
    cli::cli_text("  Key columns: {.field {ks$keys}}{auto_lbl}")
    cli::cli_text("  Distinct combos in {x$name1}: {format(ks$x_unique, big.mark = ',')}")
    cli::cli_text("  Distinct combos in {x$name2}: {format(ks$y_unique, big.mark = ',')}")
    cli::cli_text("  Matching combos: {format(ks$matches, big.mark = ',')}")
    cli::cli_text("  Only in {x$name1}: {format(ks$only_x, big.mark = ',')}")
    cli::cli_text("  Only in {x$name2}: {format(ks$only_y, big.mark = ',')}")
  }

  # 4. Numeric discrepancies
  cli::cli_text("")
  cli::cli_text("{.strong 4. Numeric discrepancies (absolute differences)}")
  if (is.na(x$numeric_method)) {
    cli::cli_text("  No common numeric columns found.")
  } else if (x$numeric_method == "row_index") {
    cli::cli_text("  No key columns; comparing by row index.")
  } else {
    cli::cli_text("  Comparing after merging on keys.")
    cli::cli_text("  Rows matched: {format(x$rows_matched, big.mark = ',')}")
  }

  if (!is.null(x$numeric_summary) && nrow(x$numeric_summary) > 0L) {
    ns <- x$numeric_summary
    display_tbl <- data.frame(
      Column = ns$column,
      N      = format(ns$n, big.mark = ",", trim = TRUE),
      Min    = format(round(ns$min, 4), trim = TRUE),
      Q25    = format(round(ns$q25, 4), trim = TRUE),
      Median = format(round(ns$median, 4), trim = TRUE),
      Q75    = format(round(ns$q75, 4), trim = TRUE),
      Max    = format(round(ns$max, 4), trim = TRUE),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    cli::cli_text("")
    .cli_table(display_tbl, right_align = c("N", "Min", "Q25", "Median", "Q75", "Max"), indent = 4L)
  }

  invisible(x)
}
