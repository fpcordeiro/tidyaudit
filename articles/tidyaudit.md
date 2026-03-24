# Audit Trail Walkthrough

``` r
library(tidyaudit)
library(dplyr)
```

## Tracking what happens in your pipeline

Every filter, join, and transformation in a data pipeline encodes an
assumption about the data. How many rows did that filter actually drop?
Did the join introduce NAs or duplicates? Which records did not match?
How much revenue was lost at each step?

These questions matter. Even when these answers are clear in the moment,
they become hard to recall during code review, debugging, or when
someone inherits your pipeline months later. An audit trail keeps that
record for you.

tidyaudit captures **metadata-only snapshots** at each step of a pipe —
row counts, column counts, NA totals, numeric summaries — without
storing the data itself. The trail is a lightweight, structured record
of your pipeline’s behavior that you can print, diff, export, and share.

## Your first trail

Create a trail object and insert
[`audit_tap()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_tap.md)
calls into your pipeline. Each tap records a snapshot and passes the
data through unchanged — three taps, three snapshots, one timeline.

``` r
# Sample data
orders <- data.frame(
  id       = 1:20,
  customer = rep(c("Alice", "Bob", "Carol", "Dan", "Eve"), 4),
  amount   = c(150, 200, 50, 300, 75, 120, 400, 90, 250, 60,
               180, 210, 45, 320, 85, 130, 380, 95, 270, 55),
  status   = rep(c("complete", "pending", "complete", "cancelled", "complete"), 4)
)

trail <- audit_trail("order_pipeline")

result <- orders |>
  audit_tap(trail, "raw") |>
  filter(status == "complete") |>
  audit_tap(trail, "complete_only") |>
  mutate(tax = amount * 0.1) |>
  audit_tap(trail, "with_tax")
```

Print the trail to see the full timeline:

``` r
print(trail)
#> 
#> ── Audit Trail: "order_pipeline" ───────────────────────────────────────────────
#> Created: 2026-03-24 11:34:28
#> Snapshots: 3
#> 
#>   #  Label          Rows  Cols  NAs  Type
#>   ─  ─────────────  ────  ────  ───  ────
#>   1  raw              20     4    0  tap 
#>   2  complete_only    12     4    0  tap 
#>   3  with_tax         12     5    0  tap
#> 
#> Changes:
#>   From           To             Rows  Cols  NAs
#>   ─────────────  ─────────────  ────  ────  ───
#>   raw            complete_only    -8     =    =
#>   complete_only  with_tax          =    +1    =
```

The timeline shows row counts, column counts, NA totals, and change
summaries between consecutive steps. Notice how the filter reduced 20
rows to 12, and the mutate added a column — all captured automatically.

## Operation-aware taps

Plain
[`audit_tap()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_tap.md)
records what the data looks like at a given point, but it can’t tell you
*why* it changed. Operation-aware taps solve this — they perform the
dplyr operation AND record enriched diagnostics in a single step.

### Join taps

Joins are where data quality problems hide. A left join can silently
introduce NAs, inflate row counts, or produce unexpected many-to-many
relationships. Replace
[`dplyr::left_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
with
[`left_join_tap()`](https://fpcordeiro.github.io/tidyaudit/reference/join_tap.md)
to capture match rates, relationship type, and duplicate key information
automatically:

``` r
customers <- data.frame(
  customer = c("Alice", "Bob", "Carol", "Dan"),
  region   = c("East", "West", "East", "North")
)

trail2 <- audit_trail("join_pipeline")

result2 <- orders |>
  audit_tap(trail2, "raw") |>
  left_join_tap(customers, by = "customer",
                .trail = trail2, .label = "with_region")

print(trail2)
#> 
#> ── Audit Trail: "join_pipeline" ────────────────────────────────────────────────
#> Created: 2026-03-24 11:34:28
#> Snapshots: 2
#> 
#>   #  Label        Rows  Cols  NAs  Type                                
#>   ─  ───────────  ────  ────  ───  ────────────────────────────────────
#>   1  raw            20     4    0  tap                                 
#>   2  with_region    20     5    4  left_join (many-to-one, 80% matched)
#> 
#> Changes:
#>   From  To           Rows  Cols  NAs
#>   ────  ───────────  ────  ────  ───
#>   raw   with_region     =    +1   +4
```

The `Type` column now shows the join type, relationship, and match rate
— all without leaving the pipe. All six dplyr join types are supported:
[`left_join_tap()`](https://fpcordeiro.github.io/tidyaudit/reference/join_tap.md),
[`right_join_tap()`](https://fpcordeiro.github.io/tidyaudit/reference/join_tap.md),
[`inner_join_tap()`](https://fpcordeiro.github.io/tidyaudit/reference/join_tap.md),
[`full_join_tap()`](https://fpcordeiro.github.io/tidyaudit/reference/join_tap.md),
[`anti_join_tap()`](https://fpcordeiro.github.io/tidyaudit/reference/join_tap.md),
[`semi_join_tap()`](https://fpcordeiro.github.io/tidyaudit/reference/join_tap.md).

### Filter taps

Filters are invisible by default — you remove rows and never know what
you lost.
[`filter_tap()`](https://fpcordeiro.github.io/tidyaudit/reference/filter_tap.md)
keeps matching rows (like
[`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html))
while recording exactly how many rows were dropped:

``` r
trail3 <- audit_trail("filter_pipeline")

result3 <- orders |>
  audit_tap(trail3, "raw") |>
  filter_tap(status == "complete",
             .trail = trail3, .label = "complete_only") |>
  filter_tap(amount > 100,
             .trail = trail3, .label = "high_value",
             .stat = amount)
#> ℹ filter_tap: status == "complete"
#> Dropped 8 of 20 rows (40.0%)
#> ℹ filter_tap: amount > 100
#> Dropped 8 of 12 rows (66.7%)
#> Stat amount: dropped 555 of 1,135

print(trail3)
#> 
#> ── Audit Trail: "filter_pipeline" ──────────────────────────────────────────────
#> Created: 2026-03-24 11:34:28
#> Snapshots: 3
#> 
#>   #  Label          Rows  Cols  NAs  Type                          
#>   ─  ─────────────  ────  ────  ───  ──────────────────────────────
#>   1  raw              20     4    0  tap                           
#>   2  complete_only    12     4    0  filter (dropped 8 rows, 40%)  
#>   3  high_value        4     4    0  filter (dropped 8 rows, 66.7%)
#> 
#> Changes:
#>   From           To             Rows  Cols  NAs
#>   ─────────────  ─────────────  ────  ────  ───
#>   raw            complete_only    -8     =    =
#>   complete_only  high_value       -8     =    =
```

The `.stat` argument is the feature that makes filter taps indispensable
for financial and business pipelines: it tracks how much of a numeric
column was removed at each step. You can see not just that you dropped 4
rows, but that those rows represented a specific dollar amount.

[`filter_out_tap()`](https://fpcordeiro.github.io/tidyaudit/reference/filter_tap.md)
works the same way but drops matching rows (the inverse).

## Comparing snapshots

[`audit_diff()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_diff.md)
gives you a detailed before/after comparison between any two snapshots
in the trail — not just adjacent ones:

``` r
audit_diff(trail3, "raw", "high_value")
#> 
#> ── Audit Diff: "raw" → "high_value" ──
#> 
#>   Metric  Before  After  Delta
#>   ──────  ──────  ─────  ─────
#>   Rows        20      4    -16
#>   Cols         4      4      =
#>   NAs          0      0      =
#> 
#> ℹ No columns added or removed
#> 
#> Numeric shifts (common columns):
#>     Column  Mean before  Mean after   Shift
#>     ──────  ───────────  ──────────  ──────
#>     id            10.50         8.5      -2
#>     amount       173.25       145.0  -28.25
```

This shows row/column/NA deltas, columns added or removed, and numeric
distribution shifts across the common columns.

## Full audit report

[`audit_report()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_report.md)
prints the complete trail summary plus all consecutive diffs in one call
— the full story of what your pipeline did:

``` r
audit_report(trail3)
#> ── Audit Report: "filter_pipeline" ─────────────────────────────────────────────
#> Created: 2026-03-24 11:34:28
#> Total snapshots: 3
#> 
#> ── Audit Trail: "filter_pipeline" ──────────────────────────────────────────────
#> Created: 2026-03-24 11:34:28
#> Snapshots: 3
#> 
#>   #  Label          Rows  Cols  NAs  Type                          
#>   ─  ─────────────  ────  ────  ───  ──────────────────────────────
#>   1  raw              20     4    0  tap                           
#>   2  complete_only    12     4    0  filter (dropped 8 rows, 40%)  
#>   3  high_value        4     4    0  filter (dropped 8 rows, 66.7%)
#> 
#> Changes:
#>   From           To             Rows  Cols  NAs
#>   ─────────────  ─────────────  ────  ────  ───
#>   raw            complete_only    -8     =    =
#>   complete_only  high_value       -8     =    =
#> 
#> ── Detailed Diffs ──────────────────────────────────────────────────────────────
#> 
#> ── Audit Diff: "raw" → "complete_only" ──
#> 
#>   Metric  Before  After  Delta
#>   ──────  ──────  ─────  ─────
#>   Rows        20     12     -8
#>   Cols         4      4      =
#>   NAs          0      0      =
#> 
#> ℹ No columns added or removed
#> 
#> Numeric shifts (common columns):
#>     Column  Mean before  Mean after   Shift
#>     ──────  ───────────  ──────────  ──────
#>     id            10.50       10.50       0
#>     amount       173.25       94.58  -78.67
#> 
#> ── Audit Diff: "complete_only" → "high_value" ──
#> 
#>   Metric  Before  After  Delta
#>   ──────  ──────  ─────  ─────
#>   Rows        12      4     -8
#>   Cols         4      4      =
#>   NAs          0      0      =
#> 
#> ℹ No columns added or removed
#> 
#> Numeric shifts (common columns):
#>     Column  Mean before  Mean after   Shift
#>     ──────  ───────────  ──────────  ──────
#>     id            10.50         8.5      -2
#>     amount        94.58       145.0  +50.42
#> 
#> ── Final Snapshot Profile ──────────────────────────────────────────────────────
#> 
#> high_value (4 rows x 4 cols)
#> Column types: 2 character, 1 integer, 1 numeric
#> ✔ No missing values
#> 
#> Numeric summary:
#>     Column  Min   Mean  Median  Max
#>     ──────  ───  ─────  ──────  ───
#>     id        1    8.5     8.5   16
#>     amount  120  145.0   140.0  180
#> 
#> ────────────────────────────────────────────────────────────────────────────────
```

## Domain-specific diagnostics

The built-in metrics cover structure and shape (rows, columns, NAs,
numeric summaries). But your domain has its own questions: how many
valid records remain? What’s the total revenue at this stage? Is a
business rule still satisfied?

Pass a named list of functions via `.fns` to compute anything
domain-specific at any tap. Each function receives the data and its
return value is stored in the snapshot:

``` r
trail4 <- audit_trail("custom_example")

result4 <- orders |>
  audit_tap(trail4, "raw", .fns = list(
    n_complete   = ~sum(.$status == "complete"),          # scalar
    amount_stats = ~c(mean = mean(.$amount),              # named vector
                      max  = max(.$amount))
  )) |>
  filter(status == "complete") |>
  audit_tap(trail4, "complete_only", .fns = list(
    n_complete   = ~sum(.$status == "complete"),
    amount_stats = ~c(mean = mean(.$amount),
                      max  = max(.$amount))
  ))
```

Custom results appear as inline annotations directly below each snapshot
row:

``` r
print(trail4)
#> 
#> ── Audit Trail: "custom_example" ───────────────────────────────────────────────
#> Created: 2026-03-24 11:34:29
#> Snapshots: 2
#> 
#>   #  Label          Rows  Cols  NAs  Type
#>   ─  ─────────────  ────  ────  ───  ────
#>   1  raw              20     4    0  tap 
#>      ↳ n_complete: 12
#>      ↳ amount_stats: mean=173.25, max=400
#>   2  complete_only    12     4    0  tap 
#>      ↳ n_complete: 12
#>      ↳ amount_stats: mean=94.58333, max=180
#> 
#> Changes:
#>   From  To             Rows  Cols  NAs
#>   ────  ─────────────  ────  ────  ───
#>   raw   complete_only    -8     =    =
```

The rendering rules are:

- **Scalar** return value: `fn_name: value`
- **Named vector or named list of scalars**: `fn_name: key=val, key=val`
  (truncated at 60 characters)
- **Complex object** (data frame, unnamed vector, nested list):
  `fn_name: [complex -- see audit_report()]`

To suppress annotations and display only the main table:

``` r
print(trail4, show_custom = FALSE)
#> 
#> ── Audit Trail: "custom_example" ───────────────────────────────────────────────
#> Created: 2026-03-24 11:34:29
#> Snapshots: 2
#> 
#>   #  Label          Rows  Cols  NAs  Type
#>   ─  ─────────────  ────  ────  ───  ────
#>   1  raw              20     4    0  tap 
#>   2  complete_only    12     4    0  tap
#> 
#> Changes:
#>   From  To             Rows  Cols  NAs
#>   ────  ─────────────  ────  ────  ───
#>   raw   complete_only    -8     =    =
```

## Snapshot controls

On wide datasets, computing numeric summaries for every column is
unnecessary and slows the pipeline down. Three parameters on all tap
functions let you control what gets captured:

- `.numeric_summary = FALSE` — skip quantile computation entirely
- `.cols_include` — character vector of column names to include in the
  snapshot’s schema (mutually exclusive with `.cols_exclude`)
- `.cols_exclude` — character vector of column names to exclude

Core invariants — `nrow`, `ncol`, and `total_nas` — are always recorded
regardless of these settings.

``` r
wide_data <- cbind(orders, matrix(rnorm(20 * 50), nrow = 20))

trail_ctrl <- audit_trail("snapshot_controls")

wide_data |>
  audit_tap(trail_ctrl, "full_snapshot") |>
  audit_tap(trail_ctrl, "minimal",
            .numeric_summary = FALSE,
            .cols_include = c("id", "amount", "status"))

print(trail_ctrl)
#> 
#> ── Audit Trail: "snapshot_controls" ────────────────────────────────────────────
#> Created: 2026-03-24 11:34:29
#> Snapshots: 2
#> 
#>   #  Label          Rows  Cols  NAs  Type
#>   ─  ─────────────  ────  ────  ───  ────
#>   1  full_snapshot    20    54    0  tap 
#>   2  minimal          20    54    0  tap
#> 
#> Changes:
#>   From           To       Rows  Cols  NAs
#>   ─────────────  ───────  ────  ────  ───
#>   full_snapshot  minimal     =     =    =
```

The “minimal” snapshot still knows the data has 54 columns and 20 rows,
but its schema only describes the three columns you asked for and
contains no numeric summaries.

## Tabulation in pipelines

[`tab()`](https://fpcordeiro.github.io/tidyaudit/reference/tab.md)
produces one-way frequency tables or two-way crosstabulations:

``` r
tab(orders, status)
#> 
#> ── Tabulation: status ──────────────────────────────────────────────────────────
#> 20 observations
#> 
#>   Value      Freq  Percent    Cum.
#>   ─────────  ────  ───────  ──────
#>   cancelled     4    20.0%   20.0%
#>   complete     12    60.0%   80.0%
#>   pending       4    20.0%  100.0%
#>   ─────────  ────  ───────  ──────
#>   Total        20   100.0%
tab(orders, status, customer)
#> 
#> ── Crosstabulation: status × customer ──────────────────────────────────────────
#> 20 observations | Cell contents: count
#> 
#>   status     Alice  Bob  Carol  Dan  Eve  Total
#>   ─────────  ─────  ───  ─────  ───  ───  ─────
#>   cancelled      0    0      0    4    0      4
#>   complete       4    0      4    0    4     12
#>   pending        0    4      0    0    0      4
#>   ─────────  ─────  ───  ─────  ───  ───  ─────
#>   Total          4    4      4    4    4     20
```

[`tab_tap()`](https://fpcordeiro.github.io/tidyaudit/reference/tab_tap.md)
embeds a tabulation inside a pipeline as a custom diagnostic annotation.
It runs
[`tab()`](https://fpcordeiro.github.io/tidyaudit/reference/tab.md) on
the data, stores the result in the snapshot, and returns the data
unchanged — useful for tracking how categorical distributions shift
across pipeline steps:

``` r
trail_tab <- audit_trail("tab_pipeline")

result_tab <- orders |>
  tab_tap(status, .trail = trail_tab, .label = "status_dist") |>
  filter(status == "complete") |>
  tab_tap(customer, .trail = trail_tab, .label = "customer_dist",
          .sort = "freq_desc")

print(trail_tab)
#> 
#> ── Audit Trail: "tab_pipeline" ─────────────────────────────────────────────────
#> Created: 2026-03-24 11:34:29
#> Snapshots: 2
#> 
#>   #  Label          Rows  Cols  NAs  Type
#>   ─  ─────────────  ────  ────  ───  ────
#>   1  status_dist      20     4    0  tap 
#>      ↳ tab(status): [complex — see audit_report()]
#>   2  customer_dist    12     4    0  tap 
#>      ↳ tab(customer): [complex — see audit_report()]
#> 
#> Changes:
#>   From         To             Rows  Cols  NAs
#>   ───────────  ─────────────  ────  ────  ───
#>   status_dist  customer_dist    -8     =    =
```

## Standalone mode

All tap functions work without a trail. When `.trail = NULL` (the
default):

- **No diagnostic args**: behaves like the plain dplyr function
- **With `.stat` or `.warn_threshold`**: runs diagnostics and prints
  results without recording to a trail

This makes it easy to add quick diagnostics to any pipeline without
setting up a full trail:

``` r
# Plain filter -- no diagnostics
orders |> filter_tap(amount > 100) |> nrow()
#> [1] 12

# Diagnostics without a trail
orders |> filter_tap(amount > 100, .stat = amount) |> invisible()
#> filter_keep(.data, amount > 100)
#> Dropped 8 of 20 rows (40.00%).
#> Dropped 555 of 3,465 for amount (16.02%).
```

## Exporting and sharing trails

Trails live in-memory as environment-based S3 objects. To share them
with colleagues, CI systems, or documentation, convert them to portable
formats.

### Converting to R objects

``` r
# As a plain R list (suitable for jsonlite::toJSON())
trail_list <- trail_to_list(trail3)
str(trail_list, max.level = 2)
#> List of 4
#>  $ name       : chr "filter_pipeline"
#>  $ created_at : chr "2026-03-24T11:34:28Z"
#>  $ n_snapshots: int 3
#>  $ snapshots  :List of 3
#>   ..$ raw          :List of 15
#>   ..$ complete_only:List of 15
#>   ..$ high_value   :List of 15

# As a data.frame (one row per snapshot)
trail_df <- trail_to_df(trail3)
print(trail_df)
#>   index         label   type           timestamp nrow ncol total_nas
#> 1     1           raw    tap 2026-03-24 11:34:28   20    4         0
#> 2     2 complete_only filter 2026-03-24 11:34:28   12    4         0
#> 3     3    high_value filter 2026-03-24 11:34:28    4    4         0
#>    all_columns       schema numeric_summary      changes  diagnostics custom
#> 1 id, cust.... c("id", ....    c("id", ....                                 
#> 2 id, cust.... c("id", ....    c("id", .... -8, 0, 0.... keep, st....       
#> 3 id, cust.... c("id", ....    c("id", .... -8, 0, 0.... keep, am....       
#>       pipeline controls
#> 1       orders         
#> 2 orders, ....         
#> 3 orders, ....
```

### Saving and loading trails

RDS format preserves all R types and round-trips perfectly:

``` r
tmp_rds <- tempfile(fileext = ".rds")
write_trail(trail3, tmp_rds)
restored <- read_trail(tmp_rds)
print(restored)
#> 
#> ── Audit Trail: "filter_pipeline" ──────────────────────────────────────────────
#> Created: 2026-03-24 11:34:28
#> Snapshots: 3
#> 
#>   #  Label          Rows  Cols  NAs  Type                          
#>   ─  ─────────────  ────  ────  ───  ──────────────────────────────
#>   1  raw              20     4    0  tap                           
#>   2  complete_only    12     4    0  filter (dropped 8 rows, 40%)  
#>   3  high_value        4     4    0  filter (dropped 8 rows, 66.7%)
#> 
#> Changes:
#>   From           To             Rows  Cols  NAs
#>   ─────────────  ─────────────  ────  ────  ───
#>   raw            complete_only    -8     =    =
#>   complete_only  high_value       -8     =    =
```

JSON format is available for interoperability with other tools (requires
jsonlite):

``` r
tmp_json <- tempfile(fileext = ".json")
write_trail(trail3, tmp_json, format = "json")
```

### HTML visualization

[`audit_export()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_export.md)
produces a self-contained HTML file — one file you can email, embed in
documentation, or drop into a compliance folder:

``` r
audit_export(trail3, tempfile(fileext = ".html"))
```

The HTML page renders an interactive pipeline flow diagram with
light/dark theme toggle. Nodes are clickable and expand to show column
schema and diagnostics. Edges show the full diff between adjacent
snapshots. No server or internet connection required.
