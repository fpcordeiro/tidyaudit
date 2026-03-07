#' Audit a Vector Transformation
#'
#' Applies a transformation function to a vector and reports what changed.
#' Works with any vector type: character, numeric, Date/POSIXct, factor, or
#' logical. Diagnostics are adapted to the detected input type.
#'
#' @param x Vector to transform. Accepted types: character, numeric,
#'   Date, POSIXct, factor, or logical.
#' @param clean_fn A function applied to `x` that returns a vector of the
#'   same length, **or** a pre-computed vector of the same length (used
#'   directly as the transformation result).
#' @param name Optional name for the variable (used in output). If `NULL`,
#'   captures the variable name from the call.
#' @param .tolerance Numeric tolerance used for the "changed beyond tolerance"
#'   diagnostic (numeric type only). Defaults to `sqrt(.Machine$double.eps)`.
#'
#' @returns An S3 object of class `audit_transform` containing:
#' \describe{
#'   \item{name}{Name of the variable}
#'   \item{clean_fn_name}{Name of the transformation function, or
#'     `"<pre-computed>"` when a vector was supplied directly}
#'   \item{type_class}{Detected type: `"character"`, `"numeric"`,
#'     `"Date"`, `"POSIXct"`, `"factor"`, or `"logical"`}
#'   \item{n_total}{Total number of elements}
#'   \item{n_changed}{Count of values that changed (including NA status changes)}
#'   \item{n_unchanged}{Count of values that stayed the same}
#'   \item{n_na_before}{Count of NA values before transformation}
#'   \item{n_na_after}{Count of NA values after transformation}
#'   \item{pct_changed}{Percentage of total elements that changed}
#'   \item{change_examples}{Data frame with before/after pairs (up to 10)}
#'   \item{diagnostics}{Type-specific diagnostic list, or `NULL` for character}
#'   \item{cleaned}{The transformed vector, retaining its type}
#' }
#'
#' @examples
#' # Character
#' x <- c("  hello ", "WORLD", "  foo  ", NA)
#' result <- audit_transform(x, trimws)
#' result$cleaned
#'
#' # Numeric
#' prices <- c(10.5, 20.0, NA, 30.0)
#' audit_transform(prices, function(v) round(v))
#'
#' # Pre-computed result
#' audit_transform(prices, round(prices))
#'
#' @family data quality
#' @seealso [diagnose_strings()]
#' @export
audit_transform <- function(x, clean_fn, name = NULL,
                            .tolerance = sqrt(.Machine$double.eps)) {
  if (is.null(name)) {
    name <- deparse(substitute(x))
  }

  if (is.function(clean_fn)) {
    clean_fn_name <- deparse(substitute(clean_fn))
    cleaned <- clean_fn(x)
  } else {
    clean_fn_name <- "<pre-computed>"
    cleaned <- clean_fn
  }

  if (length(cleaned) != length(x)) {
    input_len <- length(x)
    output_len <- length(cleaned)
    cli::cli_abort(
      "{.arg clean_fn} must return a vector of the same length as the input ({input_len}), but returned length {output_len}."
    )
  }

  type_class <- if (inherits(x, c("Date", "POSIXct"))) {
    class(x)[1]
  } else if (is.numeric(x)) {
    "numeric"
  } else if (is.factor(x)) {
    "factor"
  } else if (is.logical(x)) {
    "logical"
  } else {
    "character"
  }

  n_total      <- length(x)
  is_na_before <- is.na(x)
  is_na_after  <- is.na(cleaned)
  n_na_before  <- sum(is_na_before)
  n_na_after   <- sum(is_na_after)

  # Changed: NA status changed, OR both non-NA and values differ.
  # For factors with different level sets, compare via character to avoid error.
  if (is.factor(x) || is.factor(cleaned)) {
    values_differ <- as.character(x) != as.character(cleaned)
  } else {
    values_differ <- x != cleaned
  }
  changed <- (is_na_before != is_na_after) |
    (!is_na_before & !is_na_after & values_differ)

  n_changed   <- sum(changed)
  n_unchanged <- n_total - n_changed
  pct_changed <- if (n_total > 0L) 100 * n_changed / n_total else 0

  if (n_changed > 0L) {
    changed_idx <- which(changed)
    sample_idx  <- changed_idx[seq_len(min(10L, length(changed_idx)))]
    change_examples <- data.frame(
      before = .for_examples(x[sample_idx]),
      after  = .for_examples(cleaned[sample_idx]),
      stringsAsFactors = FALSE
    )
  } else {
    change_examples <- data.frame(
      before = character(0),
      after  = character(0),
      stringsAsFactors = FALSE
    )
  }

  diagnostics <- .audit_transform_diagnostics(
    x, cleaned, type_class, .tolerance, changed
  )

  out <- list(
    name          = name,
    clean_fn_name = clean_fn_name,
    type_class    = type_class,
    n_total       = n_total,
    n_changed     = n_changed,
    n_unchanged   = n_unchanged,
    n_na_before   = n_na_before,
    n_na_after    = n_na_after,
    pct_changed   = pct_changed,
    change_examples = change_examples,
    diagnostics   = diagnostics,
    cleaned       = cleaned
  )
  structure(out, class = c("audit_transform", "list"))
}

# Convert a vector to character for display in change_examples.
# Date/POSIXct uses format() for ISO representation; everything else as.character().
#' @noRd
.for_examples <- function(v) {
  if (inherits(v, c("Date", "POSIXct"))) format(v) else as.character(v)
}

# Compute type-specific diagnostic list
#' @noRd
.audit_transform_diagnostics <- function(x, cleaned, type_class, .tolerance,
                                         changed) {
  switch(type_class,
    "numeric" = {
      delta <- abs(cleaned - x)  # NA where either operand is NA
      n_non_na_delta <- sum(!is.na(delta))
      n_beyond_tol   <- sum(delta > .tolerance, na.rm = TRUE)

      list(
        mean_before    = mean(x,       na.rm = TRUE),
        mean_after     = mean(cleaned, na.rm = TRUE),
        sd_before      = stats::sd(x,       na.rm = TRUE),
        sd_after       = stats::sd(cleaned, na.rm = TRUE),
        min_before     = .safe_stat(x,       min),
        min_after      = .safe_stat(cleaned, min),
        median_before  = .safe_stat(x,       stats::median),
        median_after   = .safe_stat(cleaned, stats::median),
        max_before     = .safe_stat(x,       max),
        max_after      = .safe_stat(cleaned, max),
        n_nan_before   = sum(is.nan(x)),
        n_nan_after    = sum(is.nan(cleaned)),
        n_inf_before   = sum(is.infinite(x)),
        n_inf_after    = sum(is.infinite(cleaned)),
        # na.rm = TRUE: NA-status changes (value→NA, NA→value) produce NA delta;
        # silently excluded so mean reflects only finite-to-finite changes.
        mean_abs_delta = if (any(changed)) mean(delta[changed], na.rm = TRUE) else 0,
        n_beyond_tol   = n_beyond_tol,
        pct_beyond_tol = if (n_non_na_delta > 0L) 100 * n_beyond_tol / n_non_na_delta else 0
      )
    },
    "Date" = ,
    "POSIXct" = {
      list(
        min_before        = .safe_stat(x,       min),
        min_after         = .safe_stat(cleaned, min),
        max_before        = .safe_stat(x,       max),
        max_after         = .safe_stat(cleaned, max),
        range_days_before = .date_range_days(x),
        range_days_after  = .date_range_days(cleaned)
      )
    },
    "factor" = {
      lvl_before <- levels(x)
      lvl_after  <- if (is.factor(cleaned)) {
        levels(cleaned)
      } else {
        sort(unique(as.character(cleaned[!is.na(cleaned)])))
      }
      list(
        levels_before       = lvl_before,
        levels_after        = lvl_after,
        levels_added        = setdiff(lvl_after,  lvl_before),
        levels_removed      = setdiff(lvl_before, lvl_after),
        level_counts_before = .factor_counts(x),
        level_counts_after  = .factor_counts(cleaned)
      )
    },
    "logical" = {
      list(
        n_true_before  = sum( x,       na.rm = TRUE),
        n_true_after   = sum( cleaned, na.rm = TRUE),
        n_false_before = sum(!x,       na.rm = TRUE),
        n_false_after  = sum(!cleaned, na.rm = TRUE)
      )
    },
    # character — no extra diagnostics needed
    NULL
  )
}

# min/max that returns NA instead of Inf/-Inf on all-NA input
#' @noRd
.safe_stat <- function(x, fn) {
  x_valid <- x[!is.na(x)]
  if (length(x_valid) == 0L) NA else fn(x_valid)
}

# Frequency table for a factor/vector as a data.frame with standard columns
#' @noRd
.factor_counts <- function(x) {
  tbl <- as.data.frame(table(x, useNA = "always"), stringsAsFactors = FALSE)
  names(tbl) <- c("Level", "N")
  tbl
}

# Date/POSIXct range in days; NA if fewer than two non-NA values
#' @noRd
.date_range_days <- function(x) {
  x_valid <- x[!is.na(x)]
  if (length(x_valid) < 2L) {
    return(NA_real_)
  }
  as.numeric(difftime(max(x_valid), min(x_valid), units = "days"))
}

#' @rdname audit_transform
#' @param ... Additional arguments (currently unused).
#' @export
print.audit_transform <- function(x, ...) {
  fmt_int <- function(z) format(z, big.mark = ",", scientific = FALSE, trim = TRUE)
  fmt_pct <- function(z) sprintf("%.1f%%", z)

  type_label <- x$type_class
  x_name     <- x$name
  cli::cli_h1("Transformation Audit [{type_label}]: {x_name}")

  fn_name <- x$clean_fn_name
  cli::cli_text("Function: {fn_name}")
  cli::cli_text("")

  # Common metrics table
  tbl <- data.frame(
    Metric = c("Total elements", "NA (before)", "NA (after)", "Changed",
               "Unchanged"),
    Value  = c(
      fmt_int(x$n_total),
      fmt_int(x$n_na_before),
      fmt_int(x$n_na_after),
      paste0(fmt_int(x$n_changed),
             " (", fmt_pct(x$pct_changed), " of total)"),
      fmt_int(x$n_unchanged)
    ),
    stringsAsFactors = FALSE
  )
  .cli_table(tbl, right_align = "Value")

  # Type-specific summary
  .print_transform_diagnostics(x, fmt_int, fmt_pct)

  # Change examples
  if (x$n_changed > 0L) {
    n_ex <- nrow(x$change_examples)
    cli::cli_text("")
    cli::cli_text(
      "Examples of changes (showing {n_ex} of {fmt_int(x$n_changed)}):"
    )
    print(x$change_examples, row.names = FALSE)
  }

  cli::cli_text("")
  cli::cli_text("Access cleaned vector with: {.code result$cleaned}")

  invisible(x)
}

# Dispatch type-specific summary section
#' @noRd
.print_transform_diagnostics <- function(x, fmt_int, fmt_pct) {
  d <- x$diagnostics
  if (is.null(d)) return(invisible(NULL))

  fmt_num <- function(z) {
    if (is.na(z)) "NA" else sprintf("%.4g", z)
  }

  tc <- x$type_class

  if (tc == "numeric") {
    cli::cli_text("")
    cli::cli_text("Numeric summary:")
    stats_tbl <- data.frame(
      Metric = c("Mean", "SD", "Min", "Median", "Max", "NaN", "Inf"),
      Before = c(
        fmt_num(d$mean_before), fmt_num(d$sd_before),
        fmt_num(d$min_before),  fmt_num(d$median_before),
        fmt_num(d$max_before),
        fmt_int(d$n_nan_before), fmt_int(d$n_inf_before)
      ),
      After  = c(
        fmt_num(d$mean_after), fmt_num(d$sd_after),
        fmt_num(d$min_after),  fmt_num(d$median_after),
        fmt_num(d$max_after),
        fmt_int(d$n_nan_after), fmt_int(d$n_inf_after)
      ),
      stringsAsFactors = FALSE
    )
    .cli_table(stats_tbl, right_align = c("Before", "After"))
    cli::cli_text("")
    mean_delta <- fmt_num(d$mean_abs_delta)
    cli::cli_text("Mean absolute delta: {mean_delta}")
    n_bt  <- fmt_int(d$n_beyond_tol)
    p_bt  <- fmt_pct(d$pct_beyond_tol)
    cli::cli_text("Changed beyond tolerance: {n_bt} ({p_bt})")

  } else if (tc %in% c("Date", "POSIXct")) {
    cli::cli_text("")
    cli::cli_text("Date range:")
    fmt_date <- function(z) if (is.na(z)) "NA" else format(z)
    fmt_days <- function(z) if (is.na(z)) "NA" else sprintf("%.1f", z)
    range_tbl <- data.frame(
      Metric = c("Min", "Max", "Span (days)"),
      Before = c(fmt_date(d$min_before), fmt_date(d$max_before),
                 fmt_days(d$range_days_before)),
      After  = c(fmt_date(d$min_after),  fmt_date(d$max_after),
                 fmt_days(d$range_days_after)),
      stringsAsFactors = FALSE
    )
    .cli_table(range_tbl, right_align = c("Before", "After"))

  } else if (tc == "factor") {
    cli::cli_text("")
    cli::cli_text("Factor levels:")
    if (length(d$levels_added) > 0L) {
      added <- paste(d$levels_added, collapse = ", ")
      cli::cli_bullets(c("*" = "Levels added:   {added}"))
    }
    if (length(d$levels_removed) > 0L) {
      removed <- paste(d$levels_removed, collapse = ", ")
      cli::cli_bullets(c("*" = "Levels removed: {removed}"))
    }
    if (length(d$levels_added) == 0L && length(d$levels_removed) == 0L) {
      cli::cli_text("No level changes.")
    }
    cli::cli_text("")
    cli::cli_text("Level counts (before | after):")
    cnt_b <- d$level_counts_before
    cnt_a <- d$level_counts_after
    cnt_b$Level[is.na(cnt_b$Level)] <- "<NA>"
    cnt_a$Level[is.na(cnt_a$Level)] <- "<NA>"
    names(cnt_b) <- c("Level", "Before")
    names(cnt_a) <- c("Level", "After")
    cnt <- merge(cnt_b, cnt_a, by = "Level", all = TRUE)
    cnt$Before[is.na(cnt$Before)] <- 0L
    cnt$After[is.na(cnt$After)]   <- 0L
    cnt$Before <- fmt_int(cnt$Before)
    cnt$After  <- fmt_int(cnt$After)
    .cli_table(cnt, right_align = c("Before", "After"))

  } else if (tc == "logical") {
    cli::cli_text("")
    cli::cli_text("Logical balance:")
    na_b <- x$n_na_before
    na_a <- x$n_na_after
    bal_tbl <- data.frame(
      Value  = c("TRUE", "FALSE", "NA"),
      Before = fmt_int(c(d$n_true_before, d$n_false_before, na_b)),
      After  = fmt_int(c(d$n_true_after,  d$n_false_after,  na_a)),
      stringsAsFactors = FALSE
    )
    .cli_table(bal_tbl, right_align = c("Before", "After"))
  }

  invisible(NULL)
}
