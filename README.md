# tidyaudit

<!-- badges: start -->
[![R-CMD-check](https://github.com/fpcordeiro/tidyaudit/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/fpcordeiro/tidyaudit/actions/workflows/R-CMD-check.yaml)
[![License: LGPL-3](https://img.shields.io/badge/license-LGPL--3-blue.svg)](LICENSE.md)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)
<!-- badges: end -->


Pipeline audit trails and data diagnostics for the tidyverse.

tidyaudit captures **metadata-only snapshots** at each step of a dplyr pipeline,
building a structured audit report without storing the data itself.
Operation-aware taps enrich snapshots with join match rates, filter drop
statistics, and more. The package combines diagnostic tools for **interactive** development and **production**-oriented tools.

## Installation

Install the development version from GitHub:

```r
# install.packages("pak")
pak::pak("fpcordeiro/tidyaudit")
```

## Quick example

```r
library(tidyaudit)
library(dplyr)

orders    <- data.frame(id = 1:100, amount = runif(100, 10, 500), region_id = sample(1:5, 100, TRUE))
regions   <- data.frame(region_id = 1:4, name = c("North", "South", "East", "West"))

trail <- audit_trail("order_pipeline")

result <- orders |>
  audit_tap(trail, "raw") |>
  left_join_tap(regions, by = "region_id", .trail = trail, .label = "with_region") |>
  filter_tap(amount > 100, .trail = trail, .label = "high_value", .stat = amount)
#> ℹ filter_tap: amount > 100
#> Dropped 14 of 100 rows (14.0%)
#> Stat amount: dropped 713.4317 of 26,831.99

print(trail)

#> ── Audit Trail: "order_pipeline" ───────────────────────────────────────────
#> Created: 2026-02-15 07:27:19
#> Snapshots: 3
#> 
#>   #  Label        Rows  Cols  NAs  Type                                
#>   ─  ───────────  ────  ────  ───  ────────────────────────────────────
#>   1  raw           100     3    0  tap                                 
#>   2  with_region   100     4   19  left_join (many-to-one, 81% matched)
#>   3  high_value     86     4   17  filter (dropped 14 rows, 14%)       
#> 
#> Changes:
#>   raw → with_region: = rows, +1 cols, +19 NAs
#>   with_region → high_value: -14 rows, = cols, -2 NAs

audit_diff(trail, "raw", "high_value")
#> 
#> ── Audit Diff: "raw" → "high_value" ──
#> 
#>   Metric  Before  After  Delta
#>   ──────  ──────  ─────  ─────
#>   Rows       100     86    -14
#>   Cols         3      4     +1
#>   NAs          0     17    +17
#> 
#> ✔ Columns added: name
#> 
#> Numeric shifts (common columns):
#>     Column     Mean before  Mean after   Shift
#>     ─────────  ───────────  ──────────  ──────
#>     id               50.50       49.42   -1.08
#>     amount          268.32      303.70  +35.38
#>     region_id         3.00        3.02   +0.02
```

## Features

**Audit trail system** — the core innovation:

- `audit_trail()` / `audit_tap()` — build snapshot timelines inside pipes
- `left_join_tap()`, `filter_tap()`, and friends — operation-aware taps with
  enriched diagnostics (match rates, drop statistics)
- `audit_diff()` — detailed before/after comparison of any two snapshots
- `audit_report()` — full pipeline report in one call

**Diagnostic functions** — tidyverse ports from
[dtaudit](https://github.com/fpcordeiro/dtaudit):

- `validate_join()` — analyze joins without performing them
- `validate_primary_keys()` / `validate_var_relationship()` — key validation
- `compare_tables()` — column, row, and numeric comparison
- `filter_keep()` / `filter_drop()` — filter with diagnostic output
- `diagnose_nas()` / `summarize_column()` / `get_summary_table()` — data quality
- `diagnose_strings()` / `audit_clean()` — string quality auditing

See `vignette("tidyaudit")` for the audit trail walkthrough and
`vignette("diagnostics")` for the diagnostic functions guide.

## Relationship to dtaudit

tidyaudit is a tidyverse-native sibling to
[dtaudit](https://github.com/fpcordeiro/dtaudit) (a data.table-based package on
CRAN). The two packages share design vocabulary and S3 class naming conventions
but no code or dependencies.

## License

LGPL (>= 3)
