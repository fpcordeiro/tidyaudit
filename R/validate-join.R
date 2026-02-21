#' Validate Join Operations Between Two Tables
#'
#' Analyzes a potential join between two data.frames or tibbles without
#' performing the full join. Reports relationship type (one-to-one,
#' one-to-many, etc.), match rates, duplicate keys, and unmatched rows.
#' Optionally tracks a numeric statistic column through the join to quantify
#' impact.
#'
#' @param x A data.frame or tibble (left table).
#' @param y A data.frame or tibble (right table).
#' @param by A character vector of column names to join on. Use a named vector
#'   `c("key_x" = "key_y")` when column names differ between tables. Unnamed
#'   elements are used for both tables.
#' @param stat Optional single column name (string) to track in both tables when
#'   the column name is the same. Ignored if `stat_x` or `stat_y` is provided.
#' @param stat_x Optional column name (string) for a numeric statistic in `x`.
#' @param stat_y Optional column name (string) for a numeric statistic in `y`.
#'
#' @returns An S3 object of class `validate_join` containing:
#' \describe{
#'   \item{x_name, y_name}{Names of the input tables from the original call}
#'   \item{by_x, by_y}{Key columns used for the join}
#'   \item{counts}{List with row counts, match rates, and overlap statistics}
#'   \item{stat}{When `stat`, `stat_x`, or `stat_y` is provided, a list with
#'     stat diagnostics per table. `NULL` when no stat is provided.}
#'   \item{duplicates}{List with duplicate key information for each table}
#'   \item{summary_table}{A data.frame summarizing the join diagnostics}
#'   \item{relation}{Character string describing the relationship}
#'   \item{keys_only_in_x}{Unmatched keys from x}
#'   \item{keys_only_in_y}{Unmatched keys from y}
#' }
#'
#' @examples
#' x <- data.frame(id = c(1L, 2L, 3L, 3L), value = c("a", "b", "c", "d"))
#' y <- data.frame(id = c(2L, 3L, 4L), score = c(10, 20, 30))
#' result <- validate_join(x, y, by = "id")
#' print(result)
#'
#' # Track a stat column with different names in each table
#' x2 <- data.frame(id = 1:3, sales = c(100, 200, 300))
#' y2 <- data.frame(id = 2:4, cost = c(10, 20, 30))
#' validate_join(x2, y2, by = "id", stat_x = "sales", stat_y = "cost")
#'
#' @family join validation
#' @export
validate_join <- function(x, y, by = NULL, stat = NULL, stat_x = NULL,
                          stat_y = NULL) {
  if (!is.data.frame(x)) {
    cli::cli_abort("{.arg x} must be a data.frame or tibble.")
  }
  if (!is.data.frame(y)) {
    cli::cli_abort("{.arg y} must be a data.frame or tibble.")
  }

  x_name <- deparse(substitute(x))
  y_name <- deparse(substitute(y))

  # Resolve by into by_x / by_y
  if (is.null(by)) {
    cli::cli_abort("{.arg by} must be provided.")
  }
  if (!is.character(by) || length(by) == 0L) {
    cli::cli_abort("{.arg by} must be a non-empty character vector.")
  }

  by_names <- names(by)
  if (is.null(by_names)) {
    by_x <- by
    by_y <- by
  } else {
    by_x <- ifelse(by_names == "", by, by_names)
    by_y <- unname(by)
  }

  # Column existence checks
  miss_x <- setdiff(by_x, names(x))
  if (length(miss_x) > 0L) {
    cli::cli_abort("{.arg x} is missing key column{?s}: {.field {miss_x}}")
  }
  miss_y <- setdiff(by_y, names(y))
  if (length(miss_y) > 0L) {
    cli::cli_abort("{.arg y} is missing key column{?s}: {.field {miss_y}}")
  }

  # Resolve stat columns: stat_x/stat_y take precedence over stat
  have_stat_x <- FALSE
  have_stat_y <- FALSE
  stat_col_x <- NULL
  stat_col_y <- NULL

  if (!is.null(stat_x) || !is.null(stat_y)) {
    if (!is.null(stat_x)) {
      if (!is.character(stat_x) || length(stat_x) != 1L) {
        cli::cli_abort("{.arg stat_x} must be a single column name string.")
      }
      stat_col_x <- stat_x
      have_stat_x <- TRUE
    }
    if (!is.null(stat_y)) {
      if (!is.character(stat_y) || length(stat_y) != 1L) {
        cli::cli_abort("{.arg stat_y} must be a single column name string.")
      }
      stat_col_y <- stat_y
      have_stat_y <- TRUE
    }
  } else if (!is.null(stat)) {
    if (!is.character(stat) || length(stat) != 1L) {
      cli::cli_abort("{.arg stat} must be a single column name string.")
    }
    stat_col_x <- stat_col_y <- stat
    have_stat_x <- have_stat_y <- TRUE
  }

  if (have_stat_x) {
    if (!stat_col_x %in% names(x)) {
      cli::cli_abort("Column {.field {stat_col_x}} not found in {.arg x}.")
    }
    if (!is.numeric(x[[stat_col_x]])) {
      cli::cli_abort("Column {.field {stat_col_x}} in {.arg x} is not numeric.")
    }
  }
  if (have_stat_y) {
    if (!stat_col_y %in% names(y)) {
      cli::cli_abort("Column {.field {stat_col_y}} not found in {.arg y}.")
    }
    if (!is.numeric(y[[stat_col_y]])) {
      cli::cli_abort("Column {.field {stat_col_y}} in {.arg y} is not numeric.")
    }
  }

  # Row counts
  x_rows <- nrow(x)
  y_rows <- nrow(y)

  # Aggregate to counts per key (no cartesian explosion)
  xc <- dplyr::count(x, dplyr::across(dplyr::all_of(by_x)), name = "N")
  yc <- dplyr::count(y, dplyr::across(dplyr::all_of(by_y)), name = "N")

  x_unique <- nrow(xc)
  y_unique <- nrow(yc)
  x_has_dups <- any(xc[["N"]] > 1L)
  y_has_dups <- any(yc[["N"]] > 1L)

  # Build join_by for merging aggregate counts
  merge_by <- setNames(by_y, by_x)
  comb <- dplyr::full_join(xc, yc, by = merge_by, suffix = c(".x", ".y"))

  Nx <- comb[["N.x"]]
  Ny <- comb[["N.y"]]
  if (is.null(Nx)) Nx <- integer(0)
  if (is.null(Ny)) Ny <- integer(0)
  Nx0 <- ifelse(is.na(Nx), 0L, Nx)
  Ny0 <- ifelse(is.na(Ny), 0L, Ny)

  # Merge metrics
  n_matched <- sum(as.double(Nx0) * as.double(Ny0))
  n_only_x <- sum(Nx0[Ny0 == 0L])
  n_only_y <- sum(Ny0[Nx0 == 0L])
  n_key_overlap <- sum(Nx0 > 0L & Ny0 > 0L)
  match_rate_x <- if (x_rows > 0L) 100 * ((x_rows - n_only_x) / x_rows) else NA_real_
  match_rate_y <- if (y_rows > 0L) 100 * ((y_rows - n_only_y) / y_rows) else NA_real_

  # Stat diagnostics
  stat_info <- NULL
  if (have_stat_x || have_stat_y) {
    stat_info <- list()

    # Identify matched keys in x
    matched_mask <- Nx0 > 0L & Ny0 > 0L
    matched_keys_df <- comb[matched_mask, by_x, drop = FALSE]

    if (have_stat_x) {
      stat_total_x <- sum(x[[stat_col_x]], na.rm = TRUE)
      stat_na_x <- sum(is.na(x[[stat_col_x]]))
      stat_matched_x <- if (nrow(matched_keys_df) > 0L) {
        matched_x <- dplyr::semi_join(x, matched_keys_df, by = setNames(by_x, by_x))
        sum(matched_x[[stat_col_x]], na.rm = TRUE)
      } else {
        0
      }
      stat_only_x <- stat_total_x - stat_matched_x
      stat_rate_x <- if (stat_total_x != 0) 100 * stat_matched_x / stat_total_x else NA_real_
      stat_info$stat_col_x <- stat_col_x
      stat_info$x <- list(
        total = stat_total_x, matched = stat_matched_x,
        only = stat_only_x, rate = stat_rate_x, n_na = stat_na_x
      )
    }

    if (have_stat_y) {
      stat_total_y <- sum(y[[stat_col_y]], na.rm = TRUE)
      stat_na_y <- sum(is.na(y[[stat_col_y]]))
      # Build matched keys for y (may need column renaming)
      if (!identical(by_x, by_y)) {
        matched_keys_y <- matched_keys_df
        names(matched_keys_y) <- by_y
      } else {
        matched_keys_y <- matched_keys_df
      }
      stat_matched_y <- if (nrow(matched_keys_y) > 0L) {
        matched_y <- dplyr::semi_join(y, matched_keys_y, by = setNames(by_y, by_y))
        sum(matched_y[[stat_col_y]], na.rm = TRUE)
      } else {
        0
      }
      stat_only_y <- stat_total_y - stat_matched_y
      stat_rate_y <- if (stat_total_y != 0) 100 * stat_matched_y / stat_total_y else NA_real_
      stat_info$stat_col_y <- stat_col_y
      stat_info$y <- list(
        total = stat_total_y, matched = stat_matched_y,
        only = stat_only_y, rate = stat_rate_y, n_na = stat_na_y
      )
    }
  }

  # Relationship classification
  relation <- if (n_key_overlap == 0L) {
    "no matches"
  } else if (!x_has_dups && !y_has_dups) {
    "one-to-one"
  } else if (!x_has_dups && y_has_dups) {
    "one-to-many"
  } else if (x_has_dups && !y_has_dups) {
    "many-to-one"
  } else {
    "many-to-many"
  }

  # Build summary table
  fmt_int <- function(z) format(z, big.mark = ",", scientific = FALSE, trim = TRUE)
  key_lbl_x <- paste(by_x, collapse = ", ")
  key_lbl_y <- paste(by_y, collapse = ", ")

  summary_table <- data.frame(
    Item = c(
      "Relationship",
      sprintf("Key(s) in %s   [%s]", x_name, key_lbl_x),
      sprintf("Key(s) in %s   [%s]", y_name, key_lbl_y),
      sprintf("Rows in %s", x_name),
      sprintf("Distinct key combos in %s", x_name),
      sprintf("Rows in %s", y_name),
      sprintf("Distinct key combos in %s", y_name),
      "Overlapping distinct key combos",
      "Matched row pairs (cartesian)",
      sprintf("Match rate from %s", x_name),
      sprintf("Match rate from %s", y_name),
      sprintf("Rows only in %s (no match in %s)", x_name, y_name),
      sprintf("Rows only in %s (no match in %s)", y_name, x_name)
    ),
    Value = c(
      relation,
      sprintf("(%d col%s)", length(by_x), ifelse(length(by_x) == 1L, "", "s")),
      sprintf("(%d col%s)", length(by_y), ifelse(length(by_y) == 1L, "", "s")),
      fmt_int(x_rows),
      fmt_int(x_unique),
      fmt_int(y_rows),
      fmt_int(y_unique),
      fmt_int(n_key_overlap),
      fmt_int(n_matched),
      if (is.na(match_rate_x)) "N/A" else sprintf("%.2f%%", match_rate_x),
      if (is.na(match_rate_y)) "N/A" else sprintf("%.2f%%", match_rate_y),
      fmt_int(n_only_x),
      fmt_int(n_only_y)
    ),
    stringsAsFactors = FALSE
  )

  # Duplicate key tables
  x_dupe_keys <- xc[xc$N > 1L, , drop = FALSE]
  y_dupe_keys <- yc[yc$N > 1L, , drop = FALSE]

  # Unmatched keys
  keys_only_in_x <- comb[is.na(Ny), , drop = FALSE]
  keys_only_in_y <- comb[is.na(Nx), , drop = FALSE]

  out <- list(
    x_name = x_name,
    y_name = y_name,
    by_x = by_x,
    by_y = by_y,
    counts = list(
      x_rows = x_rows, y_rows = y_rows,
      x_unique = x_unique, y_unique = y_unique,
      n_key_overlap = n_key_overlap,
      n_matched_pairs = n_matched,
      match_rate_x = match_rate_x,
      match_rate_y = match_rate_y,
      n_only_x = n_only_x,
      n_only_y = n_only_y
    ),
    stat = stat_info,
    duplicates = list(
      x_has_dups = x_has_dups, y_has_dups = y_has_dups,
      x_dupe_keys = x_dupe_keys,
      y_dupe_keys = y_dupe_keys
    ),
    summary_table = summary_table,
    relation = relation,
    keys_only_in_x = keys_only_in_x,
    keys_only_in_y = keys_only_in_y
  )
  structure(out, class = c("validate_join", "list"))
}

#' @rdname validate_join
#' @param ... Additional arguments (currently unused).
#' @export
print.validate_join <- function(x, ...) {
  cli::cli_h1("Join Validation: {x$x_name} \u2194 {x$y_name}")
  cli::cli_text("Keys in {x$x_name}: {.field {x$by_x}}")
  cli::cli_text("Keys in {x$y_name}: {.field {x$by_y}}")
  cli::cli_text("")

  # Summary table via .cli_table
  .cli_table(x$summary_table, right_align = "Value")

  # Stat diagnostics section
  if (!is.null(x$stat)) {
    cli::cli_text("")
    cli::cli_rule("Stat diagnostics")

    if (!is.null(x$stat$x)) {
      sx <- x$stat$x
      scx <- x$stat$stat_col_x
      cli::cli_text("")
      cli::cli_text("{.strong {scx}} in {x$x_name}:")
      cli::cli_bullets(c(
        "*" = "Total: {format(sx$total, big.mark = ',', trim = TRUE)}",
        "*" = "Matched: {format(sx$matched, big.mark = ',', trim = TRUE)}  ({if (is.na(sx$rate)) 'N/A' else sprintf('%.2f%%', sx$rate)})",
        "*" = "Unmatched: {format(sx$only, big.mark = ',', trim = TRUE)}  ({if (is.na(sx$rate)) 'N/A' else sprintf('%.2f%%', 100 - sx$rate)})"
      ))
    }

    if (!is.null(x$stat$y)) {
      sy <- x$stat$y
      scy <- x$stat$stat_col_y
      cli::cli_text("")
      cli::cli_text("{.strong {scy}} in {x$y_name}:")
      cli::cli_bullets(c(
        "*" = "Total: {format(sy$total, big.mark = ',', trim = TRUE)}",
        "*" = "Matched: {format(sy$matched, big.mark = ',', trim = TRUE)}  ({if (is.na(sy$rate)) 'N/A' else sprintf('%.2f%%', sy$rate)})",
        "*" = "Unmatched: {format(sy$only, big.mark = ',', trim = TRUE)}  ({if (is.na(sy$rate)) 'N/A' else sprintf('%.2f%%', 100 - sy$rate)})"
      ))
    }
  }

  # Duplicates footer
  cli::cli_text("")
  du <- x$duplicates
  x_dup <- if (du$x_has_dups) "yes" else "no"
  y_dup <- if (du$y_has_dups) "yes" else "no"
  cli::cli_text("Duplicates: {x$x_name}={x_dup}  {x$y_name}={y_dup}")

  invisible(x)
}

#' @rdname validate_join
#' @param object A `validate_join` object to summarize.
#' @export
summary.validate_join <- function(object, ...) {
  print(object)
  invisible(object$summary_table)
}
