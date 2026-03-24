# Generate an Audit Report

Prints a full audit report for a trail, including the trail summary, all
diffs between consecutive snapshots, custom diagnostic results, and a
final data profile.

## Usage

``` r
audit_report(.trail, format = "console")
```

## Arguments

- .trail:

  An
  [`audit_trail()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_trail.md)
  object.

- format:

  Report format. Currently only `"console"` is supported.

## Value

`.trail`, invisibly.

## See also

Other audit trail:
[`audit_diff()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_diff.md),
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
audit_report(trail)
#> ── Audit Report: "example" ─────────────────────────────────────────────────────
#> Created: 2026-03-24 12:14:29
#> Total snapshots: 2
#> 
#> 
#> ── Audit Trail: "example" ──────────────────────────────────────────────────────
#> Created: 2026-03-24 12:14:29
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
#> 
#> ── Detailed Diffs ──────────────────────────────────────────────────────────────
#> 
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
#> 
#> ── Final Snapshot Profile ──────────────────────────────────────────────────────
#> 
#> filtered (14 rows x 11 cols)
#> Column types: 11 numeric
#> ✔ No missing values
#> 
#> Numeric summary:
#>     Column    Min    Mean  Median     Max
#>     ──────  ─────  ──────  ──────  ──────
#>     mpg     21.00   25.48   23.60   33.90
#>     cyl      4.00    4.43    4.00    6.00
#>     disp    71.10  123.89  120.20  258.00
#>     hp      52.00   88.50   94.00  113.00
#>     drat     3.08    3.98    3.91    4.93
#>     wt       1.51    2.42    2.39    3.21
#>     qsec    16.46   18.82   18.75   22.90
#>     vs       0.00    0.79    1.00    1.00
#>     am       0.00    0.71    1.00    1.00
#>     gear     3.00    4.00    4.00    5.00
#>     carb     1.00    1.86    2.00    4.00
#> 
#> ────────────────────────────────────────────────────────────────────────────────
```
