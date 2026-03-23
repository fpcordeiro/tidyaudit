# Operation-Aware Filter Taps

Performs a diagnostic filter AND records filter diagnostics in an audit
trail. `filter_tap()` keeps matching rows (like
[`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)),
`filter_out_tap()` drops matching rows (the inverse).

## Usage

``` r
filter_tap(
  .data,
  ...,
  .trail = NULL,
  .label = NULL,
  .stat = NULL,
  .quiet = FALSE,
  .warn_threshold = NULL,
  .numeric_summary = TRUE,
  .cols_include = NULL,
  .cols_exclude = NULL
)

filter_out_tap(
  .data,
  ...,
  .trail = NULL,
  .label = NULL,
  .stat = NULL,
  .quiet = FALSE,
  .warn_threshold = NULL,
  .numeric_summary = TRUE,
  .cols_include = NULL,
  .cols_exclude = NULL
)
```

## Arguments

- .data:

  A data.frame or tibble.

- ...:

  Filter conditions, evaluated in the context of `.data` using tidy
  evaluation (same as
  [`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)).

- .trail:

  An
  [`audit_trail()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_trail.md)
  object, or `NULL` (the default). When `NULL`, behavior depends on
  diagnostic arguments: if none are provided, a plain
  [`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)
  is performed; if `.stat`, `.warn_threshold`, or `.quiet = TRUE` is
  provided, delegates to
  [`filter_keep()`](https://fpcordeiro.github.io/tidyaudit/reference/filter_keep.md)
  or
  [`filter_drop()`](https://fpcordeiro.github.io/tidyaudit/reference/filter_drop.md).

- .label:

  Optional character label for this snapshot. If `NULL`, auto-generated
  as `"filter_1"` etc.

- .stat:

  An unquoted column or expression to total, e.g., `amount`,
  `price * qty`. Reports the stat amount dropped and its share of the
  total.

- .quiet:

  Logical. If `TRUE`, suppress printing diagnostics (default `FALSE`).

- .warn_threshold:

  Numeric between 0 and 1. If set and the proportion of dropped rows
  exceeds this threshold, a warning is issued.

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

The filtered data.frame or tibble.

## Details

When `.trail` is `NULL`:

- No diagnostic args: plain
  [`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)
  /
  [`dplyr::filter_out()`](https://dplyr.tidyverse.org/reference/filter.html)

- Diagnostic args provided: delegates to
  [`filter_keep()`](https://fpcordeiro.github.io/tidyaudit/reference/filter_keep.md)
  /
  [`filter_drop()`](https://fpcordeiro.github.io/tidyaudit/reference/filter_drop.md)
  (prints diagnostics but no trail recording)

- `.label` provided: warns that label is ignored

## See also

Other operation taps:
[`join_tap`](https://fpcordeiro.github.io/tidyaudit/reference/join_tap.md)

## Examples

``` r
df <- data.frame(id = 1:10, amount = 1:10 * 100, flag = rep(c(TRUE, FALSE), 5))

# With trail
trail <- audit_trail("filter_example")
result <- df |>
  audit_tap(trail, "raw") |>
  filter_tap(amount > 300, .trail = trail, .label = "big_only")
#> ℹ filter_tap: amount > 300
#> Dropped 3 of 10 rows (30.0%)
print(trail)
#> 
#> ── Audit Trail: "filter_example" ───────────────────────────────────────────────
#> Created: 2026-03-23 13:32:16
#> Snapshots: 2
#> 
#>   #  Label     Rows  Cols  NAs  Type                        
#>   ─  ────────  ────  ────  ───  ────────────────────────────
#>   1  raw         10     3    0  tap                         
#>   2  big_only     7     3    0  filter (dropped 3 rows, 30%)
#> 
#> Changes:
#>   From  To        Rows  Cols  NAs
#>   ────  ────────  ────  ────  ───
#>   raw   big_only    -3     =    =

# Inverse: drop matching rows
trail2 <- audit_trail("filter_out_example")
result2 <- df |>
  audit_tap(trail2, "raw") |>
  filter_out_tap(flag == FALSE, .trail = trail2, .label = "flagged_only")
#> ℹ filter_out_tap: flag == FALSE
#> Removed 5 of 10 rows (50.0%)
print(trail2)
#> 
#> ── Audit Trail: "filter_out_example" ───────────────────────────────────────────
#> Created: 2026-03-23 13:32:16
#> Snapshots: 2
#> 
#>   #  Label         Rows  Cols  NAs  Type                        
#>   ─  ────────────  ────  ────  ───  ────────────────────────────
#>   1  raw             10     3    0  tap                         
#>   2  flagged_only     5     3    0  filter (removed 5 rows, 50%)
#> 
#> Changes:
#>   From  To            Rows  Cols  NAs
#>   ────  ────────────  ────  ────  ───
#>   raw   flagged_only    -5     =    =

# Without trail (plain filter)
result3 <- filter_tap(df, amount > 300)
```
