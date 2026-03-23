# tidyaudit

Pipeline audit trails and data diagnostics for `tidyverse` workflows.

tidyaudit captures **metadata-only snapshots** at each step of a dplyr
pipeline, building a structured audit report without storing the data
itself. Operation-aware taps enrich snapshots with join match rates,
filter drop statistics, and more. The package combines diagnostic tools
for **interactive** development and **production**-oriented tools for
data quality.

## Installation

``` r
# Install from CRAN
install.packages("tidyaudit")

# Install development version using pak
pak::pak("fpcordeiro/tidyaudit")
```

## Quick Example

``` r
library(tidyaudit)
library(dplyr)
set.seed(123)

orders  <- data.frame(id = 1:100, amount = runif(100, 10, 500), region_id = sample(1:5, 100, TRUE))
regions <- data.frame(region_id = 1:4, name = c("North", "South", "East", "West"))

trail <- audit_trail("order_pipeline")

result <- orders |>
  audit_tap(trail, "raw") |>
  left_join_tap(regions, by = "region_id", .trail = trail, .label = "with_region") |>
  filter_tap(amount > 100, .trail = trail, .label = "high_value", .stat = amount)
#> ℹ filter_tap: amount > 100
#> Dropped 18 of 100 rows (18.0%)
#> Stat amount: dropped 1,062.191 of 25,429.39

print(trail)
#> ── Audit Trail: "order_pipeline" ─────────────────────────────────────────────────────────────────────
#> Created: 2026-02-21 14:36:35
#> Snapshots: 3
#> 
#>   #  Label        Rows  Cols  NAs  Type                                
#>   ─  ───────────  ────  ────  ───  ────────────────────────────────────
#>   1  raw           100     3    0  tap                                 
#>   2  with_region   100     4   23  left_join (many-to-one, 77% matched)
#>   3  high_value     82     4   20  filter (dropped 18 rows, 18%)       
#> 
#> Changes:
#>   raw → with_region: = rows, +1 cols, +23 NAs
#>   with_region → high_value: -18 rows, = cols, -3 NAs

audit_diff(trail, "raw", "high_value")
#> ── Audit Diff: "raw" → "high_value" ──
#> 
#>   Metric  Before  After  Delta
#>   ──────  ──────  ─────  ─────
#>   Rows       100     82    -18
#>   Cols         3      4     +1
#>   NAs          0     20    +20
#> 
#> ✔ Columns added: name
#> 
#> Numeric shifts (common columns):
#>     Column     Mean before  Mean after   Shift
#>     ─────────  ───────────  ──────────  ──────
#>     id               50.50       49.66   -0.84
#>     amount          254.29      297.16  +42.87
#>     region_id         3.08        3.05   -0.03
```

### Export as HTML

Generate a self-contained HTML visualization of any trail — one file you
can email, attach to a report, or drop into a compliance folder:

``` r
audit_export(trail, "order_pipeline.html")
```

The output is an interactive pipeline flow diagram with clickable nodes
and edges, light/dark theme toggle, and embedded JSON export — no server
or internet required.

![audit_export demo](reference/figures/audit_export_readme.gif)

audit_export demo

## Features

**Audit trail system** — the core innovation:

- [`audit_trail()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_trail.md)
  /
  [`audit_tap()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_tap.md)
  — build snapshot timelines inside pipes
- [`left_join_tap()`](https://fpcordeiro.github.io/tidyaudit/reference/join_tap.md),
  [`filter_tap()`](https://fpcordeiro.github.io/tidyaudit/reference/filter_tap.md),
  and friends — operation-aware taps with enriched diagnostics (match
  rates, drop statistics)
- [`audit_diff()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_diff.md)
  — detailed before/after comparison of any two snapshots
- [`audit_report()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_report.md)
  — full pipeline report in one call
- [`audit_export()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_export.md)
  — self-contained HTML trail visualization (interactive flow diagram,
  no server required)
- [`write_trail()`](https://fpcordeiro.github.io/tidyaudit/reference/write_trail.md)
  /
  [`read_trail()`](https://fpcordeiro.github.io/tidyaudit/reference/read_trail.md)
  — serialize trails to RDS or JSON for CI pipelines and dashboards
- [`tab_tap()`](https://fpcordeiro.github.io/tidyaudit/reference/tab_tap.md)
  — tabulation snapshots within pipelines (one-way and two-way frequency
  tables)
- Snapshot controls (`.numeric_summary`, `.cols_include`,
  `.cols_exclude`) — fine-tune what each tap captures

**Diagnostic functions** — tidyverse ports from
[dtaudit](https://github.com/fpcordeiro/dtaudit):

- [`validate_join()`](https://fpcordeiro.github.io/tidyaudit/reference/validate_join.md)
  — analyze joins without performing them
- [`validate_primary_keys()`](https://fpcordeiro.github.io/tidyaudit/reference/validate_primary_keys.md)
  /
  [`validate_var_relationship()`](https://fpcordeiro.github.io/tidyaudit/reference/validate_var_relationship.md)
  — key validation
- [`compare_tables()`](https://fpcordeiro.github.io/tidyaudit/reference/compare_tables.md)
  — column, row, and numeric comparison
- [`filter_keep()`](https://fpcordeiro.github.io/tidyaudit/reference/filter_keep.md)
  /
  [`filter_drop()`](https://fpcordeiro.github.io/tidyaudit/reference/filter_drop.md)
  — filter with diagnostic output
- [`diagnose_nas()`](https://fpcordeiro.github.io/tidyaudit/reference/diagnose_nas.md)
  /
  [`summarize_column()`](https://fpcordeiro.github.io/tidyaudit/reference/summarize_column.md)
  /
  [`get_summary_table()`](https://fpcordeiro.github.io/tidyaudit/reference/get_summary_table.md)
  — data quality
- [`diagnose_strings()`](https://fpcordeiro.github.io/tidyaudit/reference/diagnose_strings.md)
  /
  [`audit_transform()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_transform.md)
  — string quality auditing and transformation
- [`tab()`](https://fpcordeiro.github.io/tidyaudit/reference/tab.md) —
  frequency tables and crosstabulations with sorting, cutoffs, and
  weighting

See the [audit trail
walkthrough](https://fpcordeiro.github.io/tidyaudit/articles/tidyaudit.html)
and [diagnostic functions
guide](https://fpcordeiro.github.io/tidyaudit/articles/diagnostics.html)
for detailed documentation.

## Relationship to dtaudit

tidyaudit is a tidyverse-native sibling to
[dtaudit](https://github.com/fpcordeiro/dtaudit) (a data.table-based
package on CRAN). The two packages share design vocabulary and S3 class
naming conventions but no code or dependencies.

## License

LGPL (\>= 3)
