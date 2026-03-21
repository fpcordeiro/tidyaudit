# ── Helpers: serialisation ──────────────────────────────────────────────────

#' @noRd
.fmt_posixct <- function(x) {
  if (is.null(x) || (length(x) == 1L && is.na(x))) return(NA_character_)
  format(x, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

#' @noRd
.parse_posixct <- function(x) {
  if (is.null(x) || (length(x) == 1L && is.na(x))) {
    return(as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC"))
  }
  as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

#' Convert a data.frame to a list of named rows
#' @noRd
.df_to_rows <- function(df) {
  if (is.null(df)) return(NULL)
  lapply(seq_len(nrow(df)), function(i) as.list(df[i, , drop = FALSE]))
}

#' Reconstruct a data.frame from a list of named rows
#'
#' @param rows   List of named lists (one per row). NULL or length-0 returns NULL.
#' @param cols   Character vector of expected column names.
#' @param int_cols Character vector of columns to coerce to integer.
#' @param dbl_cols Character vector of columns to coerce to double.
#' @noRd
.rows_to_df <- function(rows, cols, int_cols = character(), dbl_cols = character()) {
  if (is.null(rows) || length(rows) == 0L) return(NULL)
  out <- data.frame(
    lapply(cols, function(col) {
      vals <- lapply(rows, function(r) {
        v <- r[[col]]
        if (is.null(v)) NA else v
      })
      vals <- unlist(vals)
      if (col %in% int_cols) as.integer(vals)
      else if (col %in% dbl_cols) as.double(vals)
      else vals
    }),
    stringsAsFactors = FALSE
  )
  names(out) <- cols
  out
}

#' @noRd
.changes_to_list <- function(ch) {
  if (is.null(ch)) return(NULL)
  ch$type_changes <- .df_to_rows(ch$type_changes)
  ch
}

#' @noRd
.list_to_changes <- function(ch) {
  if (is.null(ch)) return(NULL)
  # Coerce integer fields that jsonlite inflates to double
  ch$row_delta <- as.integer(ch$row_delta)
  ch$col_delta <- as.integer(ch$col_delta)
  ch$na_delta  <- as.integer(ch$na_delta)
  # Coerce empty lists back to character(0)
  ch$cols_added   <- unlist(ch$cols_added)   %||% character(0)
  ch$cols_removed <- unlist(ch$cols_removed) %||% character(0)
  # Reconstruct type_changes as data.frame
  if (!is.null(ch$type_changes) && is.list(ch$type_changes) &&
      !is.data.frame(ch$type_changes)) {
    ch$type_changes <- .rows_to_df(
      ch$type_changes,
      cols = c("column", "from", "to")
    )
  }
  ch
}


# ── Helpers: RDS form ────────────────────────────────────────────────────────

#' @noRd
.trail_to_rds_form <- function(.trail) {
  list(
    name      = .trail$name,
    created_at = .trail$created_at,
    labels    = .trail$labels,
    snapshots = lapply(.trail$snapshots, unclass)
  )
}

#' @noRd
.rds_form_to_trail <- function(lst) {
  trail <- audit_trail(lst$name)
  trail$created_at <- lst$created_at
  trail$labels     <- lst$labels
  trail$snapshots  <- lapply(lst$snapshots, function(snap) {
    structure(snap, class = c("audit_snap", "list"))
  })
  trail
}


# ── Helpers: JSON form ────────────────────────────────────────────────────────

#' @noRd
.detect_format <- function(file) {
  ext <- tolower(tools::file_ext(file))
  if (ext == "rds")  return("rds")
  if (ext == "json") return("json")
  cli::cli_abort(
    "Cannot infer file format from extension {.val {ext}}. Supply {.arg format} explicitly."
  )
}

#' @noRd
.list_to_trail <- function(lst) {
  trail <- audit_trail(lst$name)
  trail$created_at <- .parse_posixct(lst$created_at)

  snap_labels <- names(lst$snapshots) %||% character(0)
  trail$labels <- snap_labels

  trail$snapshots <- lapply(seq_along(lst$snapshots), function(i) {
    s <- lst$snapshots[[i]]

    schema <- .rows_to_df(
      s$schema,
      cols     = c("column", "type", "n_na"),
      int_cols = "n_na"
    ) %||% data.frame(column = character(), type = character(),
                      n_na = integer(), stringsAsFactors = FALSE)

    numeric_summary <- .rows_to_df(
      s$numeric_summary,
      cols     = c("column", "min", "q25", "median", "mean", "q75", "max"),
      dbl_cols = c("min", "q25", "median", "mean", "q75", "max")
    )

    snap <- list(
      label           = s$label,
      index           = as.integer(s$index),
      timestamp       = .parse_posixct(s$timestamp),
      type            = s$type,
      nrow            = as.integer(s$nrow),
      ncol            = as.integer(s$ncol),
      all_columns     = unlist(s$all_columns) %||% character(0),
      schema          = schema,
      total_nas       = as.integer(s$total_nas),
      numeric_summary = numeric_summary,
      diagnostics     = s$diagnostics,
      pipeline        = unlist(s$pipeline),
      changes         = .list_to_changes(s$changes),
      custom          = s$custom,
      controls        = s$controls
    )
    structure(snap, class = c("audit_snap", "list"))
  })

  trail
}


# ── Exported: trail_to_list ──────────────────────────────────────────────────

#' Convert an Audit Trail to a Plain List
#'
#' Converts an [audit_trail()] object to a plain R list suitable for
#' serialisation with [jsonlite::toJSON()]. All POSIXct timestamps are
#' converted to ISO 8601 character strings and data.frames are converted
#' to lists of named rows for JSON compatibility.
#'
#' @param .trail An [audit_trail()] object.
#'
#' @returns A named list with elements `name`, `created_at` (ISO 8601 string),
#'   `n_snapshots`, and `snapshots` (a named list keyed by snapshot label).
#'
#' @examples
#' trail <- audit_trail("example")
#' mtcars |> audit_tap(trail, "raw")
#' lst <- trail_to_list(trail)
#' str(lst, max.level = 2)
#'
#' @family audit export
#' @export
trail_to_list <- function(.trail) {
  if (!inherits(.trail, "audit_trail")) {
    cli::cli_abort("{.arg .trail} must be an {.cls audit_trail} object.")
  }

  snap_list <- lapply(.trail$snapshots, function(snap) {
    list(
      label           = snap$label,
      index           = snap$index,
      timestamp       = .fmt_posixct(snap$timestamp),
      type            = snap$type,
      nrow            = snap$nrow,
      ncol            = snap$ncol,
      all_columns     = snap$all_columns,
      total_nas       = snap$total_nas,
      schema          = .df_to_rows(snap$schema),
      numeric_summary = .df_to_rows(snap$numeric_summary),
      diagnostics     = snap$diagnostics,
      pipeline        = snap$pipeline,
      changes         = .changes_to_list(snap$changes),
      custom          = snap$custom,
      controls        = snap$controls
    )
  })

  if (length(snap_list) > 0L) {
    names(snap_list) <- .trail$labels
  }

  list(
    name        = .trail$name,
    created_at  = .fmt_posixct(.trail$created_at),
    n_snapshots = length(.trail$snapshots),
    snapshots   = snap_list
  )
}


# ── Exported: trail_to_df ─────────────────────────────────────────────────────

#' Convert an Audit Trail to a Data Frame
#'
#' Returns a plain `data.frame` with one row per snapshot. Nested fields
#' (`all_columns`, `schema`, `numeric_summary`, `changes`, `diagnostics`,
#' `custom`, `pipeline`, `controls`) become list-columns. Trail metadata is
#' stored as attributes on the result.
#'
#' @param .trail An [audit_trail()] object.
#'
#' @returns A `data.frame` with columns `index`, `label`, `type`, `timestamp`,
#'   `nrow`, `ncol`, `total_nas`, `all_columns`, `schema`, `numeric_summary`,
#'   `changes`, `diagnostics`, `custom`, `pipeline`, and `controls`. Trail
#'   `name` and `created_at` are stored as attributes `"trail_name"` and
#'   `"created_at"`.
#'
#' @examples
#' trail <- audit_trail("example")
#' mtcars |> audit_tap(trail, "raw")
#' dplyr::filter(mtcars, mpg > 20) |> audit_tap(trail, "filtered")
#' df <- trail_to_df(trail)
#' print(df)
#' attr(df, "trail_name")
#'
#' @family audit export
#' @export
trail_to_df <- function(.trail) {
  if (!inherits(.trail, "audit_trail")) {
    cli::cli_abort("{.arg .trail} must be an {.cls audit_trail} object.")
  }

  n <- length(.trail$snapshots)

  if (n == 0L) {
    result <- data.frame(
      index           = integer(),
      label           = character(),
      type            = character(),
      timestamp       = as.POSIXct(character()),
      nrow            = integer(),
      ncol            = integer(),
      total_nas       = integer(),
      all_columns     = I(list()),
      schema          = I(list()),
      numeric_summary = I(list()),
      changes         = I(list()),
      diagnostics     = I(list()),
      custom          = I(list()),
      pipeline        = I(list()),
      controls        = I(list()),
      stringsAsFactors = FALSE
    )
  } else {
    ts_vec <- do.call(c, lapply(.trail$snapshots, `[[`, "timestamp"))

    result <- data.frame(
      index           = vapply(.trail$snapshots, `[[`, integer(1L), "index"),
      label           = vapply(.trail$snapshots, `[[`, character(1L), "label"),
      type            = vapply(.trail$snapshots, `[[`, character(1L), "type"),
      timestamp       = ts_vec,
      nrow            = vapply(.trail$snapshots, `[[`, integer(1L), "nrow"),
      ncol            = vapply(.trail$snapshots, `[[`, integer(1L), "ncol"),
      total_nas       = vapply(.trail$snapshots, `[[`, integer(1L), "total_nas"),
      all_columns     = I(lapply(.trail$snapshots, `[[`, "all_columns")),
      schema          = I(lapply(.trail$snapshots, `[[`, "schema")),
      numeric_summary = I(lapply(.trail$snapshots, `[[`, "numeric_summary")),
      changes         = I(lapply(.trail$snapshots, `[[`, "changes")),
      diagnostics     = I(lapply(.trail$snapshots, `[[`, "diagnostics")),
      custom          = I(lapply(.trail$snapshots, `[[`, "custom")),
      pipeline        = I(lapply(.trail$snapshots, `[[`, "pipeline")),
      controls        = I(lapply(.trail$snapshots, `[[`, "controls")),
      stringsAsFactors = FALSE
    )
  }

  attr(result, "trail_name") <- .trail$name
  attr(result, "created_at") <- .trail$created_at
  result
}


# ── Exported: write_trail ─────────────────────────────────────────────────────

#' Write an Audit Trail to a File
#'
#' Saves an [audit_trail()] to disk as either an RDS file (default) or a
#' JSON file. The RDS format preserves all R types and can be restored
#' perfectly with [read_trail()]. The JSON format produces a human-readable
#' representation suitable for archiving or interoperability with other tools.
#'
#' @param .trail An [audit_trail()] object.
#' @param file   Path to the output file. A `.rds` extension is conventional
#'   for `format = "rds"`; `.json` for `format = "json"`.
#' @param format One of `"rds"` (default) or `"json"`. The JSON format
#'   requires the \pkg{jsonlite} package to be installed.
#'
#' @returns `.trail`, invisibly.
#'
#' @note Custom diagnostic results (the `custom` field, populated via `.fns`
#'   in [audit_tap()]) are serialised on a best-effort basis for JSON output.
#'   Complex R objects such as environments or functions cannot be represented
#'   in JSON and will cause an error.
#'
#' @examples
#' trail <- audit_trail("example")
#' mtcars |> audit_tap(trail, "raw")
#' tmp <- tempfile(fileext = ".rds")
#' write_trail(trail, tmp)
#' restored <- read_trail(tmp)
#'
#' @family audit export
#' @seealso [read_trail()], [trail_to_list()]
#' @export
write_trail <- function(.trail, file, format = c("rds", "json")) {
  format <- match.arg(format)

  if (!inherits(.trail, "audit_trail")) {
    cli::cli_abort("{.arg .trail} must be an {.cls audit_trail} object.")
  }
  if (!is.character(file) || length(file) != 1L || is.na(file)) {
    cli::cli_abort("{.arg file} must be a single non-missing character string.")
  }

  if (format == "rds") {
    saveRDS(.trail_to_rds_form(.trail), file = file)
  } else {
    pkg <- "jsonlite"
    if (!requireNamespace(pkg, quietly = TRUE)) {
      cli::cli_abort(
        c(
          "Package {.pkg {pkg}} is required for JSON export.",
          "i" = "Install it with {.code install.packages(\"{pkg}\")}."
        )
      )
    }
    lst      <- trail_to_list(.trail)
    json_txt <- jsonlite::toJSON(lst, auto_unbox = TRUE, pretty = TRUE, null = "null")
    writeLines(as.character(json_txt), con = file)
  }

  invisible(.trail)
}


# ── Exported: read_trail ─────────────────────────────────────────────────────

#' Read an Audit Trail from a File
#'
#' Restores an [audit_trail()] previously saved with [write_trail()]. The
#' file format is detected automatically from the file extension (`.rds` for
#' RDS, `.json` for JSON), or can be specified explicitly via `format`.
#'
#' @param file   Path to an RDS or JSON file created by [write_trail()].
#' @param format One of `"rds"`, `"json"`, or `NULL` (default). When `NULL`,
#'   the format is inferred from the file extension.
#'
#' @returns A reconstructed [audit_trail()] object with all S3 classes
#'   restored.
#'
#' @examples
#' trail <- audit_trail("example")
#' mtcars |> audit_tap(trail, "raw")
#' tmp <- tempfile(fileext = ".rds")
#' write_trail(trail, tmp)
#' restored <- read_trail(tmp)
#' print(restored)
#'
#' @family audit export
#' @seealso [write_trail()]
#' @export
read_trail <- function(file, format = NULL) {
  if (!is.character(file) || length(file) != 1L || is.na(file)) {
    cli::cli_abort("{.arg file} must be a single non-missing character string.")
  }
  if (!file.exists(file)) {
    cli::cli_abort("File not found: {.path {file}}")
  }

  if (is.null(format)) {
    format <- .detect_format(file)
  } else {
    format <- match.arg(format, c("rds", "json"))
  }

  if (format == "rds") {
    lst <- readRDS(file)
    .rds_form_to_trail(lst)
  } else {
    pkg <- "jsonlite"
    if (!requireNamespace(pkg, quietly = TRUE)) {
      cli::cli_abort(
        c(
          "Package {.pkg {pkg}} is required for JSON import.",
          "i" = "Install it with {.code install.packages(\"{pkg}\")}."
        )
      )
    }
    lst <- jsonlite::fromJSON(file, simplifyVector = FALSE)
    .list_to_trail(lst)
  }
}


# ── Exported: audit_export ──────────────────────────────────────────────────

#' Export an Audit Trail as a Self-Contained HTML File
#'
#' Produces a standalone HTML file that visualises the audit trail as an
#' interactive pipeline flow diagram. The file is completely self-contained
#' — no server, internet connection, or R installation is required to view
#' it. Open it in any browser.
#'
#' The trail is serialised via [trail_to_list()] and embedded as JSON inside
#' an HTML template with inline CSS and vanilla JavaScript. The visualisation
#' features:
#' \itemize{
#'   \item Horizontal pipeline flow diagram with colour-coded nodes per
#'         operation type (snapshot, join, filter).
#'   \item Edges annotated with key deltas (match rate, drop \%, columns
#'         added).
#'   \item Clickable nodes expanding to show column schema, operation
#'         diagnostics, and custom \code{.fns} results.
#'   \item Clickable edges showing the full diff between adjacent snapshots.
#'   \item Light / dark theme toggle.
#'   \item Collapsible JSON export panel.
#' }
#'
#' @param .trail An [audit_trail()] object.
#' @param file   Path to the output \code{.html} file. If \code{NULL}
#'   (the default), writes to a temporary file and opens it in the default
#'   browser via [utils::browseURL()].
#'
#' @returns The file path (character), invisibly.
#'
#' @examples
#' \donttest{
#' trail <- audit_trail("demo")
#' mtcars |> audit_tap(trail, "raw")
#' dplyr::filter(mtcars, mpg > 20) |> audit_tap(trail, "filtered")
#' audit_export(trail, tempfile(fileext = ".html"))
#' }
#'
#' @family audit export
#' @seealso [trail_to_list()], [write_trail()]
#' @export
audit_export <- function(.trail, file = NULL) {
  if (!inherits(.trail, "audit_trail")) {
    cli::cli_abort("{.arg .trail} must be an {.cls audit_trail} object.")
  }

  pkg <- "jsonlite"
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cli::cli_abort(
      c(
        "Package {.pkg {pkg}} is required for HTML export.",
        "i" = "Install it with {.code install.packages(\"{pkg}\")}."
      )
    )
  }

  if (!is.null(file)) {
    if (!is.character(file) || length(file) != 1L || is.na(file)) {
      cli::cli_abort("{.arg file} must be a single non-missing character string.")
    }
  }

  # Serialise trail to JSON
  lst      <- trail_to_list(.trail)
  json_txt <- jsonlite::toJSON(lst, auto_unbox = TRUE, pretty = FALSE,
                               null = "null")
  json_txt <- as.character(json_txt)

  # Prevent inline JSON from breaking the <script> tag
  json_txt <- gsub("</script>", "<\\/script>", json_txt, fixed = TRUE)

  # Read template
  template_path <- system.file("templates", "audit_trail.html",
                               package = "tidyaudit")
  if (template_path == "") {
    cli::cli_abort("HTML template not found. Is the package installed correctly?")
  }
  template <- paste(readLines(template_path, warn = FALSE), collapse = "\n")

  # Inject JSON into template
  html <- sub("/*__TRAIL_DATA__*/null", json_txt, template, fixed = TRUE)

  # Write output
  if (is.null(file)) {
    file <- tempfile(fileext = ".html")
    writeLines(html, con = file)
    if (interactive()) utils::browseURL(file)
  } else {
    writeLines(html, con = file)
  }

  invisible(file)
}
