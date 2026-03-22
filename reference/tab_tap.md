# Record a Tabulation Snapshot in a Pipeline

Transparent pipe pass-through that runs
[`tab()`](https://fpcordeiro.github.io/tidyaudit/reference/tab.md) on
the data and stores the result as a custom diagnostic annotation in the
audit trail snapshot. Returns `.data` unchanged.

## Usage

``` r
tab_tap(
  .data,
  ...,
  .trail,
  .label = NULL,
  .wt = NULL,
  .sort = c("value_asc", "value_desc", "freq_desc", "freq_asc"),
  .cutoff = NULL,
  .na = c("include", "exclude", "only"),
  .display = c("count", "row_pct", "col_pct", "total_pct"),
  .numeric_summary = TRUE,
  .cols_include = NULL,
  .cols_exclude = NULL
)
```

## Arguments

- .data:

  A data.frame or tibble.

- ...:

  Additional arguments (currently unused).

- .trail:

  An
  [`audit_trail()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_trail.md)
  object.

- .label:

  Character label for this snapshot.

- .wt:

  Optional unquoted column to use as frequency weights. When supplied,
  frequencies are weighted sums instead of row counts.

- .sort:

  How to order the rows (and columns in two-way tables). `"value_asc"`
  (default) sorts alphabetically (or by factor levels), `"value_desc"`
  sorts in reverse, `"freq_desc"` sorts by frequency descending,
  `"freq_asc"` sorts by frequency ascending.

- .cutoff:

  Controls how many values to display. An integer \>= 1 keeps the top-N
  values by frequency. A number in (0, 1) keeps values that cumulatively
  account for that proportion of the total. Remaining values are grouped
  under `"(Other)"`. For two-way tables, the cutoff applies to the row
  variable only.

- .na:

  How to handle `NA` values. `"include"` (default) treats `NA` as a
  category, `"exclude"` drops `NA` rows before tabulation, `"only"`
  shows only `NA` rows.

- .display:

  Cell contents for two-way crosstabulations. One of `"count"`
  (default), `"row_pct"`, `"col_pct"`, or `"total_pct"`. Ignored for
  one-way tables.

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

`.data`, unchanged, returned invisibly.

## See also

Other audit trail:
[`audit_diff()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_diff.md),
[`audit_report()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_report.md),
[`audit_tap()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_tap.md),
[`print.audit_snap()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_trail.md)

## Examples

``` r
trail <- audit_trail("example")
result <- mtcars |>
  tab_tap(cyl, .trail = trail, .label = "by_cyl") |>
  dplyr::filter(mpg > 20) |>
  tab_tap(cyl, .trail = trail, .label = "by_cyl_filtered")
print(trail)
#> 
#> ── Audit Trail: "example" ──────────────────────────────────────────────────────
#> Created: 2026-03-22 18:31:41
#> Snapshots: 2
#> 
#>   #  Label            Rows  Cols  NAs  Type
#>   ─  ───────────────  ────  ────  ───  ────
#>   1  by_cyl             32    11    0  tap 
#>      ↳ tab(cyl): [complex — see audit_report()]
#>   2  by_cyl_filtered    14    11    0  tap 
#>      ↳ tab(cyl): [complex — see audit_report()]
#> 
#> Changes:
#>   From    To               Rows  Cols  NAs
#>   ──────  ───────────────  ────  ────  ───
#>   by_cyl  by_cyl_filtered   -18     =    =
```
