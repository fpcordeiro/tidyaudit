# Record Data-Frame Lineage for a Block of Code

Evaluates a block of top-level statements and records a versioned audit
trail of every data.frame created or changed along the way — without
per-step taps. Capture granularity is **top-level statement lineage**: a
multi-verb pipe assigned in one statement is a single step, and a loop
yields one snapshot after it. For intra-pipeline detail, use the
explicit taps
([`audit_tap()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_tap.md),
`*_join_tap()`,
[`filter_tap()`](https://fpcordeiro.github.io/tidyaudit/reference/filter_tap.md)),
which compose inside an audited run.

## Usage

``` r
audit_record(
  expr,
  name = NULL,
  env = parent.frame(),
  watch = "data.frames",
  ignore = NULL,
  level = c("metadata", "sample_hash", "column_hash", "full_hash"),
  keys = NULL,
  numeric_summary = TRUE,
  continue_on_error = FALSE
)
```

## Arguments

- expr:

  A braced block of statements, e.g. `{ x <- ...; y <- ... }`. Captured
  unevaluated and evaluated statement by statement in `env`.

- name:

  Optional trail name. If `NULL`, a timestamped name is generated.

- env:

  Environment in which to evaluate the block. Defaults to the caller's
  environment.

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

## Value

An
[`audit_trail()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_trail.md)
populated with versioned, lineage-aware snapshots.

## Details

Capture is metadata-only by default (shape, types, NA counts); raw rows
never enter the trail unless a hash `level` above `"metadata"` is
requested.

## See also

[`audit_source()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_source.md),
[`audit_start()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_start.md)

Other audited execution:
[`audit_source()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_source.md),
[`audit_start()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_start.md)

## Examples

``` r
trail <- audit_record({
  raw    <- dplyr::as_tibble(mtcars)
  clean  <- dplyr::filter(raw, mpg > 20)
  joined <- dplyr::left_join(clean,
                             data.frame(cyl = c(4, 6, 8)), by = "cyl")
})
print(trail)
#> 
#> ── Audit Trail: "trail_20260626_221401" ────────────────────────────────────────
#> Created: 2026-06-26 22:14:01
#> Snapshots: 3
#> 
#>   #  Label   Rows  Cols  NAs  Type
#>   ─  ──────  ────  ────  ───  ────
#>   1  raw       32    11    0  tap 
#>   2  clean     14    11    0  tap 
#>   3  joined    14    11    0  tap 
```
