#' Diagnose String Column Quality
#'
#' Audits a character vector for common data quality issues including missing
#' values, empty strings, whitespace problems, non-ASCII characters, and case
#' inconsistencies. Requires the stringi package (in Suggests).
#'
#' @param x Character vector to diagnose.
#' @param name Optional name for the variable (used in output). If `NULL`,
#'   captures the variable name from the call.
#'
#' @returns An S3 object of class `diagnose_strings` containing:
#' \describe{
#'   \item{name}{Name of the variable}
#'   \item{n_total}{Total number of elements}
#'   \item{n_na}{Count of NA values}
#'   \item{n_empty}{Count of empty strings}
#'   \item{n_whitespace_only}{Count of whitespace-only strings}
#'   \item{n_leading_ws}{Count of strings with leading whitespace}
#'   \item{n_trailing_ws}{Count of strings with trailing whitespace}
#'   \item{n_non_ascii}{Count of strings with non-ASCII characters}
#'   \item{n_case_variants}{Number of unique values with case variants}
#'   \item{n_case_variant_groups}{Number of groups of case-insensitive duplicates}
#'   \item{case_variant_examples}{Data.frame with examples of case variants}
#' }
#'
#' @examples
#' firms <- c("Apple", "APPLE", "apple", "  Microsoft ", "Google", NA, "")
#' diagnose_strings(firms)
#'
#' @family data quality
#' @export
diagnose_strings <- function(x, name = NULL) {
  rlang::check_installed("stringi", reason = "for string diagnostics")

  if (is.null(name)) {
    name <- deparse(substitute(x))
  }

  x <- as.character(x)
  n_total <- length(x)
  n_na <- sum(is.na(x))

  non_na <- x[!is.na(x)]

  n_empty <- sum(non_na == "")
  n_whitespace_only <- sum(stringi::stri_detect_regex(non_na, "^\\s+$"))

  # Leading/trailing whitespace (excluding empty and whitespace-only)
  content_strings <- non_na[non_na != "" & !stringi::stri_detect_regex(non_na, "^\\s*$")]
  n_leading_ws <- sum(stringi::stri_detect_regex(content_strings, "^\\s"))
  n_trailing_ws <- sum(stringi::stri_detect_regex(content_strings, "\\s$"))

  n_non_ascii <- sum(stringi::stri_detect_regex(non_na, "[^\\x00-\\x7F]"))

  # Case variants
  if (length(content_strings) > 0L) {
    lower_versions <- stringi::stri_trans_tolower(content_strings)
    case_df <- data.frame(original = content_strings, lower = lower_versions,
                          stringsAsFactors = FALSE)

    # Group by lowercase and find groups with multiple distinct originals
    case_groups <- lapply(split(case_df$original, case_df$lower), function(vals) {
      uvals <- unique(vals)
      if (length(uvals) > 1L) {
        data.frame(
          lower = vals[1], # lowercase version via the split key
          n_variants = length(uvals),
          examples = paste(uvals[seq_len(min(3L, length(uvals)))], collapse = ", "),
          stringsAsFactors = FALSE
        )
      } else {
        NULL
      }
    })
    case_groups <- Filter(Negate(is.null), case_groups)

    if (length(case_groups) > 0L) {
      case_variant_examples <- do.call(rbind, case_groups)
      # Fix: the "lower" column should be the split key, not the original value
      case_variant_examples$lower <- names(case_groups)
      row.names(case_variant_examples) <- NULL
      n_case_variant_groups <- nrow(case_variant_examples)
      n_case_variants <- sum(case_variant_examples$n_variants)
    } else {
      n_case_variant_groups <- 0L
      n_case_variants <- 0L
      case_variant_examples <- data.frame(
        lower = character(0), n_variants = integer(0), examples = character(0),
        stringsAsFactors = FALSE
      )
    }
  } else {
    n_case_variant_groups <- 0L
    n_case_variants <- 0L
    case_variant_examples <- data.frame(
      lower = character(0), n_variants = integer(0), examples = character(0),
      stringsAsFactors = FALSE
    )
  }

  out <- list(
    name = name,
    n_total = n_total,
    n_na = n_na,
    n_empty = n_empty,
    n_whitespace_only = n_whitespace_only,
    n_leading_ws = n_leading_ws,
    n_trailing_ws = n_trailing_ws,
    n_non_ascii = n_non_ascii,
    n_case_variants = n_case_variants,
    n_case_variant_groups = n_case_variant_groups,
    case_variant_examples = case_variant_examples
  )
  structure(out, class = c("diagnose_strings", "list"))
}

#' @rdname diagnose_strings
#' @param ... Additional arguments (currently unused).
#' @export
print.diagnose_strings <- function(x, ...) {
  fmt_int <- function(z) format(z, big.mark = ",", scientific = FALSE, trim = TRUE)
  fmt_pct <- function(n, total) {
    if (total == 0L) return("0.0%")
    sprintf("%.1f%%", 100 * n / total)
  }

  cli::cli_h1("String Column Diagnosis: {x$name}")
  cli::cli_text("Total elements: {fmt_int(x$n_total)}")
  cli::cli_text("")

  cli::cli_text("{.strong Missing & Empty:}")
  cli::cli_bullets(c(
    "*" = "NA values: {fmt_int(x$n_na)} ({fmt_pct(x$n_na, x$n_total)})",
    "*" = "Empty strings: {fmt_int(x$n_empty)} ({fmt_pct(x$n_empty, x$n_total)})",
    "*" = "Whitespace-only: {fmt_int(x$n_whitespace_only)} ({fmt_pct(x$n_whitespace_only, x$n_total)})"
  ))

  cli::cli_text("")
  cli::cli_text("{.strong Whitespace Issues:}")
  cli::cli_bullets(c(
    "*" = "Leading whitespace: {fmt_int(x$n_leading_ws)}",
    "*" = "Trailing whitespace: {fmt_int(x$n_trailing_ws)}"
  ))

  cli::cli_text("")
  cli::cli_text("{.strong Encoding:}")
  cli::cli_bullets(c("*" = "Non-ASCII chars: {fmt_int(x$n_non_ascii)}"))

  cli::cli_text("")
  cli::cli_text("{.strong Case Inconsistencies:}")
  cli::cli_bullets(c(
    "*" = "Variant groups: {fmt_int(x$n_case_variant_groups)}",
    "*" = "Total variants: {fmt_int(x$n_case_variants)}"
  ))

  if (x$n_case_variant_groups > 0L) {
    cli::cli_text("")
    cli::cli_text("Case variant examples (up to 5 groups):")
    print(utils::head(x$case_variant_examples, 5L), row.names = FALSE)
  }

  invisible(x)
}
