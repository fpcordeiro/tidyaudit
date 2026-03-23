# Operation-Aware Join Taps

Performs a dplyr join AND records enriched diagnostics in an audit
trail. These functions replace the pattern of wrapping a join with two
[`audit_tap()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_tap.md)
calls, capturing information that plain taps cannot: match rates,
relationship type, duplicate keys, and unmatched row counts.

## Usage

``` r
left_join_tap(
  .data,
  y,
  ...,
  .trail = NULL,
  .label = NULL,
  .stat = NULL,
  .numeric_summary = TRUE,
  .cols_include = NULL,
  .cols_exclude = NULL
)

right_join_tap(
  .data,
  y,
  ...,
  .trail = NULL,
  .label = NULL,
  .stat = NULL,
  .numeric_summary = TRUE,
  .cols_include = NULL,
  .cols_exclude = NULL
)

inner_join_tap(
  .data,
  y,
  ...,
  .trail = NULL,
  .label = NULL,
  .stat = NULL,
  .numeric_summary = TRUE,
  .cols_include = NULL,
  .cols_exclude = NULL
)

full_join_tap(
  .data,
  y,
  ...,
  .trail = NULL,
  .label = NULL,
  .stat = NULL,
  .numeric_summary = TRUE,
  .cols_include = NULL,
  .cols_exclude = NULL
)

anti_join_tap(
  .data,
  y,
  ...,
  .trail = NULL,
  .label = NULL,
  .stat = NULL,
  .numeric_summary = TRUE,
  .cols_include = NULL,
  .cols_exclude = NULL
)

semi_join_tap(
  .data,
  y,
  ...,
  .trail = NULL,
  .label = NULL,
  .stat = NULL,
  .numeric_summary = TRUE,
  .cols_include = NULL,
  .cols_exclude = NULL
)
```

## Arguments

- .data:

  A data.frame or tibble (left table in the join).

- y:

  A data.frame or tibble (right table in the join).

- ...:

  Arguments passed to the corresponding `dplyr::*_join()` function,
  including `by`, `suffix`, `keep`, `multiple`, `unmatched`, etc. The
  `by` argument should be passed by name for enriched diagnostics.

- .trail:

  An
  [`audit_trail()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_trail.md)
  object, or `NULL` (the default). When `NULL`, behavior depends on
  `.stat`: if `.stat` is also `NULL`, a plain dplyr join is performed;
  if `.stat` is provided,
  [`validate_join()`](https://fpcordeiro.github.io/tidyaudit/reference/validate_join.md)
  diagnostics are printed before the join.

- .label:

  Optional character label for this snapshot. If `NULL`, auto-generated
  as `"left_join_1"` etc.

- .stat:

  An unquoted column name for stat tracking, e.g., `amount`. Passed to
  [`validate_join()`](https://fpcordeiro.github.io/tidyaudit/reference/validate_join.md).

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

The joined data.frame or tibble (same as the corresponding
`dplyr::*_join()`).

## Details

Enriched diagnostics (match rates, relationship type, duplicate keys)
require equality joins — `by` as a character vector, named character
vector, or simple equality
[`join_by()`](https://dplyr.tidyverse.org/reference/join_by.html)
expression (e.g., `join_by(id)`, `join_by(a == b)`). For non-equi
[`join_by()`](https://dplyr.tidyverse.org/reference/join_by.html)
expressions, the tap records a basic snapshot without match-rate
diagnostics.

All dplyr join features (`join_by`, `multiple`, `unmatched`, `suffix`,
etc.) work unchanged via `...`.

When `.trail` is `NULL`:

- `.stat` also `NULL`: plain dplyr join

- `.stat` provided: prints
  [`validate_join()`](https://fpcordeiro.github.io/tidyaudit/reference/validate_join.md)
  diagnostics, then joins

- `.label` provided: warns that label is ignored

## See also

Other operation taps:
[`filter_tap()`](https://fpcordeiro.github.io/tidyaudit/reference/filter_tap.md)

## Examples

``` r
orders <- data.frame(id = 1:4, amount = c(100, 200, 300, 400))
customers <- data.frame(id = c(2, 3, 5), name = c("A", "B", "C"))

# With trail
trail <- audit_trail("join_example")
result <- orders |>
  audit_tap(trail, "raw") |>
  left_join_tap(customers, by = "id", .trail = trail, .label = "joined")
print(trail)
#> 
#> ── Audit Trail: "join_example" ─────────────────────────────────────────────────
#> Created: 2026-03-23 11:48:35
#> Snapshots: 2
#> 
#>   #  Label   Rows  Cols  NAs  Type                               
#>   ─  ──────  ────  ────  ───  ───────────────────────────────────
#>   1  raw        4     2    0  tap                                
#>   2  joined     4     3    2  left_join (one-to-one, 50% matched)
#> 
#> Changes:
#>   From  To      Rows  Cols  NAs
#>   ────  ──────  ────  ────  ───
#>   raw   joined     =    +1   +2

# Without trail (plain join)
result2 <- left_join_tap(orders, customers, by = "id")
```
