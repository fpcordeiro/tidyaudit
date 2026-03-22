# Record a Pipeline Snapshot

Transparent pipe pass-through that captures a metadata snapshot and
appends it to an audit trail. Returns `.data` unchanged — the function's
only purpose is its side effect on `.trail`.

## Usage

``` r
audit_tap(
  .data,
  .trail,
  .label = NULL,
  .fns = NULL,
  .numeric_summary = TRUE,
  .cols_include = NULL,
  .cols_exclude = NULL
)
```

## Arguments

- .data:

  A data.frame or tibble flowing through the pipe.

- .trail:

  An
  [`audit_trail()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_trail.md)
  object.

- .label:

  Optional character label for this snapshot. If `NULL`, an
  auto-generated label like `"step_1"` is used.

- .fns:

  Optional named list of diagnostic functions (or formula lambdas) to
  run on `.data`. Results are stored in the snapshot.

- .numeric_summary:

  Logical. If `FALSE`, skip numeric summary computation in the snapshot
  (default `TRUE`).

- .cols_include:

  Character vector of column names to include in the snapshot schema, or
  `NULL` (the default) to include all columns. Mutually exclusive with
  `.cols_exclude`.

- .cols_exclude:

  Character vector of column names to exclude from the snapshot schema,
  or `NULL` (the default). Mutually exclusive with `.cols_include`.

## Value

`.data`, unchanged, returned invisibly. The function is a transparent
pass-through; its only effect is the side effect on `.trail`.

## See also

Other audit trail:
[`audit_diff()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_diff.md),
[`audit_report()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_report.md),
[`print.audit_snap()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_trail.md),
[`tab_tap()`](https://fpcordeiro.github.io/tidyaudit/reference/tab_tap.md)

## Examples

``` r
trail <- audit_trail("example")
result <- mtcars |>
  audit_tap(trail, "raw") |>
  dplyr::filter(mpg > 20) |>
  audit_tap(trail, "filtered")
print(trail)
#> 
#> ── Audit Trail: "example" ──────────────────────────────────────────────────────
#> Created: 2026-03-22 18:31:38
#> Snapshots: 2
#> 
#>   #  Label     Rows  Cols  NAs  Type
#>   ─  ────────  ────  ────  ───  ────
#>   1  raw         32    11    0  tap 
#>   2  filtered    14    11    0  tap 
#> 
#> Changes:
#>   From  To        Rows  Cols  NAs
#>   ────  ────────  ────  ────  ───
#>   raw   filtered   -18     =    =
```
