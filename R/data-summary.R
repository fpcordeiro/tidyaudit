#' Summarize a Single Column
#'
#' Computes summary statistics for a vector. Handles numeric, character,
#' factor, logical, Date, and other types with appropriate statistics for each.
#'
#' @param x A vector to summarize.
#'
#' @returns A named character vector with summary statistics including:
#'   type, unique count, missing count, most frequent value (for non-numeric),
#'   mean, sd, min, quartiles (q25, q50, q75), max, and three example values.
#'
#' @examples
#' summarize_column(c(1, 2, 3, NA, 5))
#' summarize_column(c("a", "b", "a", "c"))
#'
#' @family data quality
#' @export
summarize_column <- function(x) {
  x_NAs <- is.na(x)
  x_no_NAs <- x[!x_NAs]
  n_valid <- length(x_no_NAs)
  n_uniq <- length(unique(x_no_NAs))

  if (is.numeric(x)) {
    c(
      type = "numeric",
      n_unique = as.character(n_uniq),
      missing = as.character(sum(x_NAs)),
      most_frequent = NA_character_,
      mean = if (n_valid) as.character(mean(x_no_NAs)) else NA_character_,
      sd = if (n_valid > 1L) as.character(stats::sd(x_no_NAs)) else NA_character_,
      min = if (n_valid) as.character(min(x_no_NAs)) else NA_character_,
      q25 = if (n_valid) as.character(quantile(x_no_NAs, 0.25, names = FALSE)) else NA_character_,
      q50 = if (n_valid) as.character(quantile(x_no_NAs, 0.5, names = FALSE)) else NA_character_,
      q75 = if (n_valid) as.character(quantile(x_no_NAs, 0.75, names = FALSE)) else NA_character_,
      max = if (n_valid) as.character(max(x_no_NAs)) else NA_character_,
      example1 = if (n_valid >= 1L) as.character(x_no_NAs[1]) else NA_character_,
      example2 = if (n_valid >= 2L) as.character(x_no_NAs[2]) else NA_character_,
      example3 = if (n_valid >= 3L) as.character(x_no_NAs[3]) else NA_character_
    )
  } else if (is.factor(x)) {
    x_chr <- as.character(x_no_NAs)
    c(
      type = "factor",
      n_unique = as.character(n_uniq),
      missing = as.character(sum(x_NAs)),
      most_frequent = .most_frequent(x),
      mean = NA_character_,
      sd = NA_character_,
      min = if (n_valid) min(x_chr) else NA_character_,
      q25 = NA_character_,
      q50 = NA_character_,
      q75 = NA_character_,
      max = if (n_valid) max(x_chr) else NA_character_,
      example1 = if (n_valid >= 1L) as.character(x_no_NAs[1]) else NA_character_,
      example2 = if (n_valid >= 2L) as.character(x_no_NAs[2]) else NA_character_,
      example3 = if (n_valid >= 3L) as.character(x_no_NAs[3]) else NA_character_
    )
  } else if (is.character(x)) {
    c(
      type = "character",
      n_unique = as.character(n_uniq),
      missing = as.character(sum(x_NAs)),
      most_frequent = .most_frequent(x),
      mean = NA_character_,
      sd = NA_character_,
      min = if (n_valid) min(x_no_NAs) else NA_character_,
      q25 = NA_character_,
      q50 = NA_character_,
      q75 = NA_character_,
      max = if (n_valid) max(x_no_NAs) else NA_character_,
      example1 = if (n_valid >= 1L) x_no_NAs[1] else NA_character_,
      example2 = if (n_valid >= 2L) x_no_NAs[2] else NA_character_,
      example3 = if (n_valid >= 3L) x_no_NAs[3] else NA_character_
    )
  } else if (is.logical(x)) {
    c(
      type = "logical",
      n_unique = as.character(n_uniq),
      missing = as.character(sum(x_NAs)),
      most_frequent = .most_frequent(x),
      mean = if (n_valid) as.character(mean(x_no_NAs)) else NA_character_,
      sd = if (n_valid > 1L) as.character(stats::sd(x_no_NAs)) else NA_character_,
      min = NA_character_,
      q25 = NA_character_,
      q50 = NA_character_,
      q75 = NA_character_,
      max = NA_character_,
      example1 = if (n_valid >= 1L) as.character(x_no_NAs[1]) else NA_character_,
      example2 = if (n_valid >= 2L) as.character(x_no_NAs[2]) else NA_character_,
      example3 = if (n_valid >= 3L) as.character(x_no_NAs[3]) else NA_character_
    )
  } else if ("Date" %in% class(x)) {
    c(
      type = "Date",
      n_unique = as.character(n_uniq),
      missing = as.character(sum(x_NAs)),
      most_frequent = .most_frequent(x),
      mean = NA_character_,
      sd = NA_character_,
      min = if (n_valid) as.character(min(x_no_NAs)) else NA_character_,
      q25 = NA_character_,
      q50 = NA_character_,
      q75 = NA_character_,
      max = if (n_valid) as.character(max(x_no_NAs)) else NA_character_,
      example1 = if (n_valid >= 1L) as.character(x_no_NAs[1]) else NA_character_,
      example2 = if (n_valid >= 2L) as.character(x_no_NAs[2]) else NA_character_,
      example3 = if (n_valid >= 3L) as.character(x_no_NAs[3]) else NA_character_
    )
  } else {
    c(
      type = class(x)[1L],
      n_unique = as.character(n_uniq),
      missing = as.character(sum(x_NAs)),
      most_frequent = .most_frequent(x),
      mean = NA_character_,
      sd = NA_character_,
      min = NA_character_,
      q25 = NA_character_,
      q50 = NA_character_,
      q75 = NA_character_,
      max = NA_character_,
      example1 = if (n_valid >= 1L) as.character(x_no_NAs[1]) else NA_character_,
      example2 = if (n_valid >= 2L) as.character(x_no_NAs[2]) else NA_character_,
      example3 = if (n_valid >= 3L) as.character(x_no_NAs[3]) else NA_character_
    )
  }
}

#' Find Most Frequent Value
#' @noRd
.most_frequent <- function(x) {
  x_no_na <- x[!is.na(x)]
  if (length(x_no_na) == 0L) return(NA_character_)
  tbl <- table(x_no_na)
  as.character(names(tbl)[which.max(tbl)])
}

# Statistic names returned by summarize_column()
.summary_stat_names <- c(
  "type", "n_unique", "missing", "most_frequent",
  "mean", "sd", "min", "q25", "q50", "q75", "max",
  "example1", "example2", "example3"
)

#' Generate Summary Table for a Data Frame
#'
#' Creates a comprehensive summary of all columns in a data.frame, including
#' type, missing values, descriptive statistics, and example values.
#'
#' @param .data A data.frame or tibble to summarize.
#' @param cols Optional character vector of column names to summarize. If
#'   `NULL` (the default), all columns are summarized.
#'
#' @returns A data.frame with one row per column containing summary statistics.
#'
#' @examples
#' df <- data.frame(
#'   id = 1:100,
#'   value = rnorm(100),
#'   category = sample(letters[1:5], 100, replace = TRUE)
#' )
#' get_summary_table(df)
#'
#' @family data quality
#' @export
get_summary_table <- function(.data, cols = NULL) {
  if (!is.data.frame(.data)) {
    cli::cli_abort("{.arg .data} must be a data.frame or tibble.")
  }

  if (!is.null(cols)) {
    miss <- setdiff(cols, names(.data))
    if (length(miss) > 0L) {
      cli::cli_abort("Column{?s} not found in {.arg .data}: {.field {miss}}")
    }
    .data <- .data[cols]
  }

  summaries <- lapply(.data, summarize_column)
  result <- data.frame(
    variable = names(summaries),
    do.call(rbind, summaries),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  result
}

#' Diagnose Missing Values
#'
#' Reports NA counts and percentages for each column in a data.frame, sorted
#' by missing percentage in descending order.
#'
#' @param .data A data.frame or tibble to diagnose.
#'
#' @returns An S3 object of class `diagnose_na` containing:
#' \describe{
#'   \item{table}{A data.frame with columns `variable`, `n_na`, `pct_na`, and
#'     `n_valid`, sorted by `pct_na` descending.}
#'   \item{n_cols}{Total number of columns in the input.}
#'   \item{n_with_na}{Number of columns that have at least one NA.}
#' }
#'
#' @examples
#' df <- data.frame(
#'   a = c(1, NA, 3),
#'   b = c(NA, NA, "x"),
#'   c = c(TRUE, FALSE, TRUE)
#' )
#' diagnose_nas(df)
#'
#' @family data quality
#' @export
diagnose_nas <- function(.data) {
  if (!is.data.frame(.data)) {
    cli::cli_abort("{.arg .data} must be a data.frame or tibble.")
  }

  n_rows <- nrow(.data)
  na_counts <- vapply(.data, function(col) sum(is.na(col)), integer(1L))

  tbl <- data.frame(
    variable = names(na_counts),
    n_na     = as.integer(na_counts),
    pct_na   = if (n_rows > 0L) round(100 * na_counts / n_rows, 1) else rep(0, length(na_counts)),
    n_valid  = as.integer(n_rows - na_counts),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  # Sort by pct_na descending
  tbl <- tbl[order(-tbl$pct_na), , drop = FALSE]
  row.names(tbl) <- NULL

  structure(
    list(
      table = tbl,
      n_cols = ncol(.data),
      n_with_na = sum(tbl$n_na > 0L)
    ),
    class = c("diagnose_na", "list")
  )
}

#' @rdname diagnose_nas
#' @param x An object to print.
#' @param ... Additional arguments (currently unused).
#' @export
print.diagnose_na <- function(x, ...) {
  cli::cli_h1("Missing Value Diagnosis")
  cli::cli_text("{x$n_with_na} of {x$n_cols} columns have missing values")

  if (x$n_with_na > 0L) {
    cols_with_na <- x$table[x$table$n_na > 0L, , drop = FALSE]
    display_tbl <- data.frame(
      Variable = cols_with_na$variable,
      `N NA`   = format(cols_with_na$n_na, big.mark = ",", trim = TRUE),
      `Pct NA` = sprintf("%.1f%%", cols_with_na$pct_na),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    cli::cli_text("")
    .cli_table(display_tbl, right_align = c("N NA", "Pct NA"))
  }

  invisible(x)
}
