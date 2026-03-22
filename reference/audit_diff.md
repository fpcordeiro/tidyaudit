# Compare Two Audit Trail Snapshots

Computes detailed differences between any two snapshots in an audit
trail, including row/column/NA deltas, columns added/removed, type
changes, per-column NA changes, and numeric distribution shifts.

## Usage

``` r
audit_diff(.trail, from, to)

# S3 method for class 'audit_diff'
print(x, ...)
```

## Arguments

- .trail:

  An
  [`audit_trail()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_trail.md)
  object.

- from:

  Label (character) or index (integer) of the first snapshot.

- to:

  Label (character) or index (integer) of the second snapshot.

- x:

  An `audit_diff` object to print.

- ...:

  Additional arguments (currently unused).

## Value

An `audit_diff` object (S3 list).

## See also

Other audit trail:
[`audit_report()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_report.md),
[`audit_tap()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_tap.md),
[`print.audit_snap()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_trail.md),
[`tab_tap()`](https://fpcordeiro.github.io/tidyaudit/reference/tab_tap.md)

## Examples

``` r
trail <- audit_trail("example")
mtcars |>
  audit_tap(trail, "raw") |>
  dplyr::filter(mpg > 20) |>
  audit_tap(trail, "filtered")
audit_diff(trail, "raw", "filtered")
#> 
#> ── Audit Diff: "raw" → "filtered" ──
#> 
#>   Metric  Before  After  Delta
#>   ──────  ──────  ─────  ─────
#>   Rows        32     14    -18
#>   Cols        11     11      =
#>   NAs          0      0      =
#> 
#> ℹ No columns added or removed
#> 
#> Numeric shifts (common columns):
#>     Column  Mean before  Mean after    Shift
#>     ──────  ───────────  ──────────  ───────
#>     mpg           20.09       25.48    +5.39
#>     cyl            6.19        4.43    -1.76
#>     disp         230.72      123.89  -106.83
#>     hp           146.69       88.50   -58.19
#>     drat           3.60        3.98    +0.38
#>     wt             3.22        2.42     -0.8
#>     qsec          17.85       18.82    +0.97
#>     vs             0.44        0.79    +0.35
#>     am             0.41        0.71    +0.31
#>     gear           3.69        4.00    +0.31
#>     carb           2.81        1.86    -0.96
```
