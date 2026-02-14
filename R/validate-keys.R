#' Validate Primary Keys
#'
#' Tests whether a set of columns constitute primary keys of a data.frame,
#' i.e., whether they uniquely identify every row in the table.
#'
#' @param .data A data.frame or tibble.
#' @param keys Character vector of column names to test as primary keys.
#'
#' @returns An S3 object of class `validate_pk` containing:
#' \describe{
#'   \item{table_name}{Name of the input table from the original call}
#'   \item{keys}{Character vector of column names tested}
#'   \item{is_primary_key}{Logical: TRUE if keys uniquely identify all rows}
#'   \item{n_rows}{Total number of rows in the table}
#'   \item{n_unique_keys}{Number of distinct key combinations}
#'   \item{n_duplicate_keys}{Number of key combinations that appear more than once}
#'   \item{duplicate_keys}{A data.frame of duplicated key values with their counts}
#'   \item{has_numeric_keys}{Logical: TRUE if any key column is of type double}
#' }
#'
#' @examples
#' df <- data.frame(
#'   id = c(1L, 2L, 3L, 4L),
#'   group = c("A", "A", "B", "B"),
#'   value = c(10, 20, 30, 40)
#' )
#' validate_primary_keys(df, "id")
#' validate_primary_keys(df, "group")
#'
#' @family join validation
#' @export
validate_primary_keys <- function(.data, keys) {
  if (!is.data.frame(.data)) {
    cli::cli_abort("{.arg .data} must be a data.frame or tibble.")
  }

  table_name <- deparse(substitute(.data))

  if (!is.character(keys) || length(keys) == 0L) {
    cli::cli_abort("{.arg keys} must be a non-empty character vector.")
  }

  miss <- setdiff(keys, names(.data))
  if (length(miss) > 0L) {
    cli::cli_abort("Column{?s} not found in table: {.field {miss}}")
  }

  # Warn about numeric (double) keys
  numeric_keys <- keys[vapply(.data[keys], is.double, logical(1))]
  has_numeric_keys <- length(numeric_keys) > 0L
  if (has_numeric_keys) {
    cli::cli_warn(
      "Key column{?s} {.field {numeric_keys}} {?is/are} numeric (double). Consider using integer or character for exact matching."
    )
  }

  # Count occurrences of each key combination
  key_counts <- dplyr::count(.data, dplyr::across(dplyr::all_of(keys)), name = "n")
  n_rows <- nrow(.data)
  n_unique_keys <- nrow(key_counts)

  # Identify duplicates
  duplicate_keys <- dplyr::filter(key_counts, .data$n > 1L)
  n_duplicate_keys <- nrow(duplicate_keys)

  is_primary_key <- n_unique_keys == n_rows

  out <- list(
    table_name = table_name,
    keys = keys,
    is_primary_key = is_primary_key,
    n_rows = n_rows,
    n_unique_keys = n_unique_keys,
    n_duplicate_keys = n_duplicate_keys,
    duplicate_keys = duplicate_keys,
    has_numeric_keys = has_numeric_keys
  )
  structure(out, class = c("validate_pk", "list"))
}

#' @rdname validate_primary_keys
#' @param x An object to print.
#' @param ... Additional arguments (currently unused).
#' @export
print.validate_pk <- function(x, ...) {
  cli::cli_h1("Primary Key Validation")
  cli::cli_text("Table: {x$table_name}")
  cli::cli_text("Key column{?s}: {.field {x$keys}}")
  cli::cli_text("")

  tbl <- data.frame(
    Metric = c("Total rows", "Unique key combinations", "Duplicate key combos"),
    Value = c(
      format(x$n_rows, big.mark = ",", trim = TRUE),
      format(x$n_unique_keys, big.mark = ",", trim = TRUE),
      format(x$n_duplicate_keys, big.mark = ",", trim = TRUE)
    ),
    stringsAsFactors = FALSE
  )
  .cli_table(tbl, right_align = "Value")

  cli::cli_text("")
  if (x$is_primary_key) {
    cli::cli_alert_success("YES - Keys uniquely identify all rows.")
  } else {
    cli::cli_alert_danger("NO - Keys do NOT uniquely identify all rows.")
    if (x$n_duplicate_keys > 0L) {
      cli::cli_text("")
      cli::cli_text("{.strong Duplicate keys (showing up to 10):}")
      print(utils::head(x$duplicate_keys, 10L))
    }
  }

  if (x$has_numeric_keys) {
    cli::cli_text("")
    cli::cli_alert_warning("Numeric (double) key column(s) detected.")
  }

  invisible(x)
}

#' Validate Variable Relationship
#'
#' Determines the relationship between two variables in a data.frame:
#' one-to-one, one-to-many, many-to-one, or many-to-many.
#'
#' @param .data A data.frame or tibble.
#' @param var1 Character string: name of the first variable.
#' @param var2 Character string: name of the second variable.
#'
#' @returns An S3 object of class `validate_var_rel` containing:
#' \describe{
#'   \item{table_name}{Name of the input table}
#'   \item{var1, var2}{Names of the variables analyzed}
#'   \item{relation}{Character string: "one-to-one", "one-to-many",
#'     "many-to-one", or "many-to-many"}
#'   \item{var1_unique}{Number of distinct values in var1}
#'   \item{var2_unique}{Number of distinct values in var2}
#'   \item{n_combinations}{Number of unique (var1, var2) pairs}
#'   \item{var1_has_dups}{Does any var1 value map to multiple var2 values?}
#'   \item{var2_has_dups}{Does any var2 value map to multiple var1 values?}
#' }
#'
#' @details
#' Only accepts variables of type character, integer, or factor. Numeric
#' (double) variables are not allowed due to floating-point comparison issues.
#'
#' @examples
#' df <- data.frame(
#'   person_id = c(1L, 2L, 3L, 4L),
#'   department = c("Sales", "Sales", "Engineering", "Engineering"),
#'   country = c("US", "US", "US", "UK")
#' )
#' validate_var_relationship(df, "person_id", "department")
#'
#' @family join validation
#' @export
validate_var_relationship <- function(.data, var1, var2) {
  if (!is.data.frame(.data)) {
    cli::cli_abort("{.arg .data} must be a data.frame or tibble.")
  }

  table_name <- deparse(substitute(.data))

  if (!is.character(var1) || length(var1) != 1L) {
    cli::cli_abort("{.arg var1} must be a single character string.")
  }
  if (!is.character(var2) || length(var2) != 1L) {
    cli::cli_abort("{.arg var2} must be a single character string.")
  }

  if (!var1 %in% names(.data)) {
    cli::cli_abort("Variable {.field {var1}} not found in table.")
  }
  if (!var2 %in% names(.data)) {
    cli::cli_abort("Variable {.field {var2}} not found in table.")
  }

  # Check allowed types
  allowed_types <- c("character", "integer", "factor")
  var1_class <- class(.data[[var1]])[1L]
  var2_class <- class(.data[[var2]])[1L]

  if (!var1_class %in% allowed_types) {
    cli::cli_abort("Variable {.field {var1}} must be character, integer, or factor. Got: {.cls {var1_class}}")
  }
  if (!var2_class %in% allowed_types) {
    cli::cli_abort("Variable {.field {var2}} must be character, integer, or factor. Got: {.cls {var2_class}}")
  }

  # Unique combinations
  combos <- dplyr::distinct(.data, dplyr::across(dplyr::all_of(c(var1, var2))))
  n_combinations <- nrow(combos)

  var1_unique <- dplyr::n_distinct(.data[[var1]])
  var2_unique <- dplyr::n_distinct(.data[[var2]])

  # Check if var1 has duplicates (same var1 -> different var2)
  var1_mapping <- dplyr::count(combos, dplyr::across(dplyr::all_of(var1)), name = "n")
  var1_has_dups <- any(var1_mapping[["n"]] > 1L)

  # Check if var2 has duplicates (same var2 -> different var1)
  var2_mapping <- dplyr::count(combos, dplyr::across(dplyr::all_of(var2)), name = "n")
  var2_has_dups <- any(var2_mapping[["n"]] > 1L)

  # Determine relationship
  relation <- if (!var1_has_dups && !var2_has_dups) {
    "one-to-one"
  } else if (!var1_has_dups && var2_has_dups) {
    "many-to-one"
  } else if (var1_has_dups && !var2_has_dups) {
    "one-to-many"
  } else {
    "many-to-many"
  }

  out <- list(
    table_name = table_name,
    var1 = var1,
    var2 = var2,
    relation = relation,
    var1_unique = var1_unique,
    var2_unique = var2_unique,
    n_combinations = n_combinations,
    var1_has_dups = var1_has_dups,
    var2_has_dups = var2_has_dups
  )
  structure(out, class = c("validate_var_rel", "list"))
}

#' @rdname validate_var_relationship
#' @param x An object to print.
#' @param ... Additional arguments (currently unused).
#' @export
print.validate_var_rel <- function(x, ...) {
  cli::cli_h1("Variable Relationship Validation")
  cli::cli_text("Table: {x$table_name}")
  cli::cli_text("Variables: {.field {x$var1}} \u2194 {.field {x$var2}}")
  cli::cli_text("")

  tbl <- data.frame(
    Metric = c(
      paste("Unique values in", x$var1),
      paste("Unique values in", x$var2),
      paste0("Unique (", x$var1, ", ", x$var2, ") pairs")
    ),
    Value = c(
      format(x$var1_unique, big.mark = ",", trim = TRUE),
      format(x$var2_unique, big.mark = ",", trim = TRUE),
      format(x$n_combinations, big.mark = ",", trim = TRUE)
    ),
    stringsAsFactors = FALSE
  )
  .cli_table(tbl, right_align = "Value")

  cli::cli_text("")
  v1_dir <- if (x$var1_has_dups) "one-to-many" else "one-to-one"
  v2_dir <- if (x$var2_has_dups) "one-to-many" else "one-to-one"
  cli::cli_text("  {x$var1} \u2192 {x$var2}: {v1_dir}")
  cli::cli_text("  {x$var2} \u2192 {x$var1}: {v2_dir}")
  cli::cli_text("")
  cli::cli_text("Relationship: {.strong {toupper(x$relation)}}")

  invisible(x)
}
