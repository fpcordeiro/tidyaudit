# Audit a Script File End to End

The canonical script runner: parses an `.R` file, evaluates it one
top-level statement at a time, and records a versioned audit trail of
every data.frame created or changed — like
[`base::source()`](https://rdrr.io/r/base/source.html) but returning an
[`audit_trail()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_trail.md).
Because the evaluation loop is owned by tidyaudit, capture works in
every context (interactive,
[`source()`](https://rdrr.io/r/base/source.html)d, or `Rscript`), unlike
[`audit_start()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_start.md).

## Usage

``` r
audit_source(
  file,
  name = NULL,
  env = new.env(parent = globalenv()),
  watch = "data.frames",
  ignore = NULL,
  level = c("metadata", "sample_hash", "column_hash", "full_hash"),
  keys = NULL,
  numeric_summary = TRUE,
  continue_on_error = FALSE,
  echo = FALSE
)
```

## Arguments

- file:

  Path to an `.R` script.

- name:

  Optional trail name. If `NULL`, a timestamped name is generated.

- env:

  Environment in which to evaluate the script. Defaults to a fresh child
  of the global environment so the script's objects do not clobber your
  workspace; pass
  [`globalenv()`](https://rdrr.io/r/base/environment.html) for
  [`source()`](https://rdrr.io/r/base/source.html)-style behaviour.

- watch:

  Either `"data.frames"` (the default — track every data.frame) or a
  character vector of object names to restrict tracking to.

- ignore:

  Optional character vector of regular expressions; objects whose names
  match any pattern are skipped (e.g. scratch variables).

- level:

  Evidence level: `"metadata"` (default, privacy-safe) detects
  shape/type/NA changes only; `"sample_hash"`, `"column_hash"`, and
  `"full_hash"` additionally detect value-only changes by hashing data
  with a per-run salt. Salted hashes are *not* a privacy guarantee. The
  hashing policy (algorithm, sampling, salt) is recorded in each
  snapshot's `evidence` field.

- keys:

  Optional named list mapping object names to key column(s), used by the
  HTML report to flag primary-key status.

- numeric_summary:

  Logical; passed to the snapshot builder. If `FALSE`, skip numeric
  quantile summaries (the main cost control on wide data).

- continue_on_error:

  Logical. If `FALSE` (default), an error in a statement is recorded and
  then re-thrown so the block aborts like normal R evaluation. If
  `TRUE`, the error is recorded and evaluation continues.

- echo:

  Logical. If `TRUE`, echo each statement before evaluating it.

## Value

An
[`audit_trail()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_trail.md)
populated with versioned, lineage-aware snapshots.

## Details

Capture granularity is **top-level statement lineage** (see
[`audit_record()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_record.md)).

## See also

[`audit_record()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_record.md),
[`audit_start()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_start.md)

Other audited execution:
[`audit_record()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_record.md),
[`audit_start()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_start.md)

## Examples

``` r
tmp <- tempfile(fileext = ".R")
writeLines(c(
  "raw   <- dplyr::as_tibble(mtcars)",
  "clean <- dplyr::filter(raw, mpg > 20)"
), tmp)
trail <- audit_source(tmp)
print(trail)
#> 
#> ── Audit Trail: "file1a8f2616bd5f.R" ───────────────────────────────────────────
#> Created: 2026-06-26 22:14:02
#> Snapshots: 2
#> 
#>   #  Label  Rows  Cols  NAs  Type
#>   ─  ─────  ────  ────  ───  ────
#>   1  raw      32    11    0  tap 
#>   2  clean    14    11    0  tap 
```
