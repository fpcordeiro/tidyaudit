# Diagnostic Functions Guide

``` r
library(tidyaudit)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
```

tidyaudit includes tidyverse ports of the diagnostic functions from
[dtaudit](https://github.com/fpcordeiro/dtaudit). These functions help
you understand joins, validate keys, compare tables, diagnose missing
values and string quality, and filter with full visibility.

## Join diagnostics

[`validate_join()`](https://fpcordeiro.github.io/tidyaudit/reference/validate_join.md)
analyzes a potential join **without performing it**, reporting match
rates, relationship type, duplicate keys, and unmatched rows.

``` r
orders <- data.frame(
  id     = c(1L, 2L, 3L, 3L, 4L, 5L),
  amount = c(100, 200, 150, 175, 300, 50)
)
customers <- data.frame(
  id   = c(2L, 3L, 6L),
  name = c("Alice", "Bob", "Carol")
)

validate_join(orders, customers, by = "id")
#> 
#> ── Join Validation: orders ↔ customers ─────────────────────────────────────────
#> Keys in orders: id
#> Keys in customers: id
#> 
#>   Item                                               Value
#>   ───────────────────────────────────────────  ───────────
#>   Relationship                                 many-to-one
#>   Key(s) in orders   [id]                          (1 col)
#>   Key(s) in customers   [id]                       (1 col)
#>   Rows in orders                                         6
#>   Distinct key combos in orders                          5
#>   Rows in customers                                      3
#>   Distinct key combos in customers                       3
#>   Overlapping distinct key combos                        2
#>   Matched row pairs (cartesian)                          3
#>   Match rate from orders                            50.00%
#>   Match rate from customers                         66.67%
#>   Rows only in orders (no match in customers)            3
#>   Rows only in customers (no match in orders)            1
#> 
#> Duplicates: orders=yes customers=no
```

### Different key names

When the key columns have different names, use a named vector:

``` r
products <- data.frame(prod_id = 1:3, price = c(10, 20, 30))
sales    <- data.frame(item_id = c(1L, 1L, 2L), qty = c(5, 3, 7))

validate_join(products, sales, by = c("prod_id" = "item_id"))
#> 
#> ── Join Validation: products ↔ sales ───────────────────────────────────────────
#> Keys in products: prod_id
#> Keys in sales: item_id
#> 
#>   Item                                             Value
#>   ─────────────────────────────────────────  ───────────
#>   Relationship                               one-to-many
#>   Key(s) in products   [prod_id]                 (1 col)
#>   Key(s) in sales   [item_id]                    (1 col)
#>   Rows in products                                     3
#>   Distinct key combos in products                      3
#>   Rows in sales                                        3
#>   Distinct key combos in sales                         2
#>   Overlapping distinct key combos                      2
#>   Matched row pairs (cartesian)                        3
#>   Match rate from products                        66.67%
#>   Match rate from sales                          100.00%
#>   Rows only in products (no match in sales)            1
#>   Rows only in sales (no match in products)            0
#> 
#> Duplicates: products=no sales=yes
```

### Stat tracking

Track the impact on a numeric column with `stat` (same column name in
both tables) or `stat_x`/`stat_y` (different column names):

``` r
x <- data.frame(id = 1:4, revenue = c(100, 200, 300, 400))
y <- data.frame(id = c(2L, 3L, 5L), cost = c(10, 20, 30))

validate_join(x, y, by = "id", stat_x = "revenue", stat_y = "cost")
#> 
#> ── Join Validation: x ↔ y ──────────────────────────────────────────────────────
#> Keys in x: id
#> Keys in y: id
#> 
#>   Item                                  Value
#>   ───────────────────────────────  ──────────
#>   Relationship                     one-to-one
#>   Key(s) in x   [id]                  (1 col)
#>   Key(s) in y   [id]                  (1 col)
#>   Rows in x                                 4
#>   Distinct key combos in x                  4
#>   Rows in y                                 3
#>   Distinct key combos in y                  3
#>   Overlapping distinct key combos           2
#>   Matched row pairs (cartesian)             2
#>   Match rate from x                    50.00%
#>   Match rate from y                    66.67%
#>   Rows only in x (no match in y)            2
#>   Rows only in y (no match in x)            1
#> 
#> ── Stat diagnostics ────────────────────────────────────────────────────────────
#> 
#> revenue in x:
#> • Total: 1,000
#> • Matched: 500 (50.00%)
#> • Unmatched: 500 (50.00%)
#> 
#> cost in y:
#> • Total: 60
#> • Matched: 30 (50.00%)
#> • Unmatched: 30 (50.00%)
#> 
#> Duplicates: x=no y=no
```

## Key validation

### Primary keys

[`validate_primary_keys()`](https://fpcordeiro.github.io/tidyaudit/reference/validate_primary_keys.md)
tests whether a set of columns uniquely identify every row:

``` r
df <- data.frame(
  id    = c(1L, 2L, 3L, 3L, 4L),
  group = c("A", "A", "B", "C", "A"),
  value = c(10, 20, 30, 40, 50)
)

# Single column — not unique
validate_primary_keys(df, "id")
#> 
#> ── Primary Key Validation ──────────────────────────────────────────────────────
#> Table: df
#> Key column: id
#> 
#>   Metric                   Value
#>   ───────────────────────  ─────
#>   Total rows                   5
#>   Unique key combinations      4
#>   Duplicate key combos         1
#> 
#> ✖ NO - Keys do NOT uniquely identify all rows.
#> 
#> Duplicate keys (showing up to 10):
#>   id n
#> 1  3 2

# Composite key — unique
validate_primary_keys(df, c("id", "group"))
#> 
#> ── Primary Key Validation ──────────────────────────────────────────────────────
#> Table: df
#> Key columns: id and group
#> 
#>   Metric                   Value
#>   ───────────────────────  ─────
#>   Total rows                   5
#>   Unique key combinations      5
#>   Duplicate key combos         0
#> 
#> ✔ YES - Keys uniquely identify all rows.
```

### Variable relationships

[`validate_var_relationship()`](https://fpcordeiro.github.io/tidyaudit/reference/validate_var_relationship.md)
determines the relationship between two columns:

``` r
df2 <- data.frame(
  dept    = c("Sales", "Sales", "Engineering", "Engineering"),
  manager = c("Ann", "Ann", "Bob", "Bob")
)
validate_var_relationship(df2, "dept", "manager")
#> 
#> ── Variable Relationship Validation ────────────────────────────────────────────
#> Table: df2
#> Variables: dept ↔ manager
#> 
#>   Metric                        Value
#>   ────────────────────────────  ─────
#>   Unique values in dept             2
#>   Unique values in manager          2
#>   Unique (dept, manager) pairs      2
#> 
#> dept → manager: one-to-one
#> manager → dept: one-to-one
#> 
#> Relationship: ONE-TO-ONE
```

## Table comparison

[`compare_tables()`](https://fpcordeiro.github.io/tidyaudit/reference/compare_tables.md)
compares two data.frames by examining columns, row counts, key overlap,
and numeric discrepancies:

``` r
before <- data.frame(id = 1:5, value = c(10.0, 20.0, 30.0, 40.0, 50.0))
after  <- data.frame(id = 1:5, value = c(10.0, 22.5, 30.0, 40.0, 55.0))

compare_tables(before, after)
#> 
#> ── Table Comparison: before vs after ───────────────────────────────────────────
#> 1. Row counts
#> before: 5 rows
#> after: 5 rows
#> Difference: =
#> 
#> 2. Column names
#> Matching columns: 2
#> Only in before: 0
#> Only in after: 0
#> Type mismatches: 0
#> 
#> 3. Key columns
#> Key columns: id (auto-detected)
#> Distinct combos in before: 5
#> Distinct combos in after: 5
#> 
#> 4. Row matching
#> Only in before: 0
#> Only in after: 0
#> Matched, no discrepancies: 3 (60%)
#> Matched, with discrepancies: 2 (40%)
#> Total cell discrepancies: 2 (2 numeric, 0 categorical)
#> 
#> 5. Numeric discrepancies (absolute differences)
#> Compared after merging on keys.
#> Rows matched: 5
#> 
#>     Column  N  Min  Q25  Median  Q75  Max  >tol
#>     ──────  ─  ───  ───  ──────  ───  ───  ────
#>     value   5    0    0       0  2.5    5     2
#> 
#> Top discrepancies:
#>     id  column  value_x  value_y  abs_diff  pct_diff
#>     ──  ──────  ───────  ───────  ────────  ────────
#>     5   value        50     55.0       5.0      9.1%
#>     2   value        20     22.5       2.5     11.1%
#> 
#> 6. Categorical discrepancies
#> No categorical discrepancies found.
```

## Filter diagnostics

[`filter_keep()`](https://fpcordeiro.github.io/tidyaudit/reference/filter_keep.md)
and
[`filter_drop()`](https://fpcordeiro.github.io/tidyaudit/reference/filter_drop.md)
filter data while printing diagnostics about what was removed.

### filter_keep

Keeps rows where the condition is `TRUE` (same as
[`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)):

``` r
sales <- data.frame(
  id     = 1:10,
  amount = c(500, 25, 1200, 80, 3000, 15, 750, 40, 2000, 60),
  status = rep(c("valid", "suspect"), 5)
)

result <- filter_keep(sales, amount > 100, .stat = amount)
#> filter_keep(sales, amount > 100)
#> Dropped 5 of 10 rows (50.00%).
#> Dropped 220 of 7,670 for amount (2.87%).
```

### filter_drop

Drops rows where the condition is `TRUE` (the inverse):

``` r
result2 <- filter_drop(sales, status == "suspect", .stat = amount)
#> filter_drop(sales, status == "suspect")
#> Dropped 5 of 10 rows (50.00%).
#> Dropped 220 of 7,670 for amount (2.87%).
```

### Warning thresholds

Set `.warn_threshold` to get a warning when too many rows are dropped:

``` r
filter_keep(sales, amount > 1000, .stat = amount, .warn_threshold = 0.5)
#> filter_keep(sales, amount > 1000)
#> Dropped 7 of 10 rows (70.00%).
#> Dropped 1,470 of 7,670 for amount (19.17%).
#> Warning: Dropped 70.0% of rows exceeds threshold (50.0%).
#>   id amount status
#> 1  3   1200  valid
#> 2  5   3000  valid
#> 3  9   2000  valid
```

## Data quality

### Missing value diagnosis

[`diagnose_nas()`](https://fpcordeiro.github.io/tidyaudit/reference/diagnose_nas.md)
reports NA counts and percentages for every column:

``` r
messy <- data.frame(
  id    = 1:6,
  name  = c("A", NA, "C", "D", NA, "F"),
  score = c(10, 20, NA, NA, 50, NA),
  grade = c("A", "B", "C", NA, "A", "B")
)

diagnose_nas(messy)
#> 
#> ── Missing Value Diagnosis ─────────────────────────────────────────────────────
#> 3 of 4 columns have missing values
#> 
#>   Variable  N NA  Pct NA
#>   ────────  ────  ──────
#>   score        3   50.0%
#>   name         2   33.3%
#>   grade        1   16.7%
```

### Column summaries

[`summarize_column()`](https://fpcordeiro.github.io/tidyaudit/reference/summarize_column.md)
gives type-appropriate statistics for a single vector:

``` r
summarize_column(c(1, 2, 3, NA, 5, 10, 100))
#>                type            n_unique             missing       missing_share 
#>           "numeric"                 "6"                 "1" "0.142857142857143" 
#>       most_frequent                mean                  sd                 min 
#>                  NA  "20.1666666666667"  "39.2398606861271"                 "1" 
#>                 q25                 q50                 q75                 max 
#>              "2.25"                 "4"              "8.75"               "100" 
#>            example1            example2            example3 
#>                 "1"                 "2"                 "3"
summarize_column(c("apple", "banana", "apple", "cherry", NA))
#>          type      n_unique       missing missing_share most_frequent 
#>   "character"           "3"           "1"         "0.2"       "apple" 
#>          mean            sd           min           q25           q50 
#>            NA            NA       "apple"            NA            NA 
#>           q75           max      example1      example2      example3 
#>            NA      "cherry"       "apple"      "banana"       "apple"
```

[`get_summary_table()`](https://fpcordeiro.github.io/tidyaudit/reference/get_summary_table.md)
applies this to all columns (or selected ones):

``` r
get_summary_table(messy)
#>   variable      type n_unique missing     missing_share most_frequent
#> 1       id   numeric        6       0                 0          <NA>
#> 2     name character        4       2 0.333333333333333             A
#> 3    score   numeric        3       3               0.5          <NA>
#> 4    grade character        3       1 0.166666666666667             A
#>               mean               sd min  q25  q50  q75 max example1 example2
#> 1              3.5 1.87082869338697   1 2.25  3.5 4.75   6        1        2
#> 2             <NA>             <NA>   A <NA> <NA> <NA>   F        A        C
#> 3 26.6666666666667 20.8166599946613  10   15   20   35  50       10       20
#> 4             <NA>             <NA>   A <NA> <NA> <NA>   C        A        B
#>   example3
#> 1        3
#> 2        D
#> 3       50
#> 4        C
```

## Frequency tables

[`tab()`](https://fpcordeiro.github.io/tidyaudit/reference/tab.md)
produces one-way frequency tables or two-way crosstabulations with
counts, percentages, and cumulative percentages.

### One-way tables

``` r
tab(mtcars, cyl)
#> 
#> ── Tabulation: cyl ─────────────────────────────────────────────────────────────
#> 32 observations
#> 
#>   Value  Freq  Percent    Cum.
#>   ─────  ────  ───────  ──────
#>   4        11    34.4%   34.4%
#>   6         7    21.9%   56.3%
#>   8        14    43.8%  100.0%
#>   ─────  ────  ───────  ──────
#>   Total    32   100.0%
```

### Sorting and cutoffs

``` r
# Sort by frequency
tab(mtcars, carb, .sort = "freq_desc")
#> 
#> ── Tabulation: carb ────────────────────────────────────────────────────────────
#> 32 observations
#> 
#>   Value  Freq  Percent   Cum.
#>   ─────  ────  ───────  ─────
#>   2        10    31.2%  31.2%
#>   4        10    31.2%  62.4%
#>   1         7    21.9%  84.3%
#>   3         3     9.4%  93.7%
#>   6         1     3.1%  96.8%
#>   8         1     3.1%  99.9%
#>   ─────  ────  ───────  ─────
#>   Total    32   100.0%

# Keep only top-2 values, collapse rest into (Other)
tab(mtcars, carb, .cutoff = 2)
#> 
#> ── Tabulation: carb ────────────────────────────────────────────────────────────
#> 32 observations
#> 
#>   Value    Freq  Percent   Cum.
#>   ───────  ────  ───────  ─────
#>   2          10    31.2%  31.2%
#>   4          10    31.2%  62.4%
#>   (Other)    12    37.5%  99.9%
#>   ───────  ────  ───────  ─────
#>   Total      32   100.0%
#> 
#> (Other) collapses 4 values below cutoff
```

### Two-way crosstabulations

``` r
tab(mtcars, cyl, gear)
#> 
#> ── Crosstabulation: cyl × gear ─────────────────────────────────────────────────
#> 32 observations | Cell contents: count
#> 
#>   cyl     3   4  5  Total
#>   ─────  ──  ──  ─  ─────
#>   4       1   8  2     11
#>   6       2   4  1      7
#>   8      12   0  2     14
#>   ─────  ──  ──  ─  ─────
#>   Total  15  12  5     32

# Show row percentages instead of counts
tab(mtcars, cyl, gear, .display = "row_pct")
#> 
#> ── Crosstabulation: cyl × gear ─────────────────────────────────────────────────
#> 32 observations | Cell contents: row %
#> 
#>   cyl        3      4      5  Total
#>   ─────  ─────  ─────  ─────  ─────
#>   4       9.1%  72.7%  18.2%     11
#>   6      28.6%  57.1%  14.3%      7
#>   8      85.7%   0.0%  14.3%     14
#>   ─────  ─────  ─────  ─────  ─────
#>   Total     15     12      5     32
```

### Weighted tabulation

``` r
tab(mtcars, cyl, .wt = mpg)
#> 
#> ── Tabulation: cyl (weighted by mpg) ───────────────────────────────────────────
#> 32 observations
#> 
#>   Value   Freq  Percent    Cum.
#>   ─────  ─────  ───────  ──────
#>   4      293.3    45.6%   45.6%
#>   6      138.2    21.5%   67.1%
#>   8      211.4    32.9%  100.0%
#>   ─────  ─────  ───────  ──────
#>   Total  642.9   100.0%
```

## String cleaning

These functions require the **stringi** package (listed in Suggests).

### diagnose_strings

[`diagnose_strings()`](https://fpcordeiro.github.io/tidyaudit/reference/diagnose_strings.md)
audits a character vector for common quality issues:

``` r
firms <- c("Apple", "APPLE", "apple", "  Microsoft ", "Google", NA, "")
diagnose_strings(firms)
#> 
#> ── String Column Diagnosis: firms ──────────────────────────────────────────────
#> Total elements: 7
#> 
#> Missing & Empty:
#> • NA values: 1 (14.3%)
#> • Empty strings: 1 (14.3%)
#> • Whitespace-only: 0 (0.0%)
#> 
#> Whitespace Issues:
#> • Leading whitespace: 1
#> • Trailing whitespace: 1
#> 
#> Encoding:
#> • Non-ASCII chars: 0
#> 
#> Case Inconsistencies:
#> • Variant groups: 1
#> • Total variants: 3
#> 
#> Case variant examples (up to 5 groups):
#>  lower n_variants            examples
#>  apple          3 Apple, APPLE, apple
```

### audit_transform

[`audit_transform()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_transform.md)
shows exactly what a transformation function changes. It automatically
detects the vector type and computes type-appropriate diagnostics:

- **Numeric**: mean/min/max shift, proportion changed
- **Date/POSIXct**: range change, proportion shifted
- **Factor**: level remapping (gained/lost levels)
- **Logical**: TRUE/FALSE/NA balance shift
- **Character**: string-level before/after comparison

#### Character vectors

``` r
audit_transform(firms, trimws)
#> 
#> ── Transformation Audit [character]: firms ─────────────────────────────────────
#> Function: trimws
#> 
#>   Metric                       Value
#>   ──────────────  ──────────────────
#>   Total elements                   7
#>   NA (before)                      1
#>   NA (after)                       1
#>   Changed         1 (14.3% of total)
#>   Unchanged                        6
#> 
#> Examples of changes (showing 1 of 1):
#>        before     after
#>    Microsoft  Microsoft
#> 
#> Access cleaned vector with: `result$cleaned`
audit_transform(firms, tolower)
#> 
#> ── Transformation Audit [character]: firms ─────────────────────────────────────
#> Function: tolower
#> 
#>   Metric                       Value
#>   ──────────────  ──────────────────
#>   Total elements                   7
#>   NA (before)                      1
#>   NA (after)                       1
#>   Changed         4 (57.1% of total)
#>   Unchanged                        3
#> 
#> Examples of changes (showing 4 of 4):
#>        before        after
#>         Apple        apple
#>         APPLE        apple
#>    Microsoft    microsoft 
#>        Google       google
#> 
#> Access cleaned vector with: `result$cleaned`
```

#### Numeric vectors

``` r
prices <- c(10.456, 20.789, 30.123, NA, 50.999)
audit_transform(prices, round)
#> 
#> ── Transformation Audit [numeric]: prices ──────────────────────────────────────
#> Function: round
#> 
#>   Metric                       Value
#>   ──────────────  ──────────────────
#>   Total elements                   5
#>   NA (before)                      1
#>   NA (after)                       1
#>   Changed         4 (80.0% of total)
#>   Unchanged                        1
#> 
#> Numeric summary:
#>   Metric  Before  After
#>   ──────  ──────  ─────
#>   Mean     28.09     28
#>   SD       17.26  17.38
#>   Min      10.46     10
#>   Median   25.46   25.5
#>   Max         51     51
#>   NaN          0      0
#>   Inf          0      0
#> 
#> Mean absolute delta: 0.1977
#> Changed beyond tolerance: 4 (100.0%)
#> 
#> Examples of changes (showing 4 of 4):
#>  before after
#>  10.456    10
#>  20.789    21
#>  30.123    30
#>  50.999    51
#> 
#> Access cleaned vector with: `result$cleaned`
```

#### Date vectors

``` r
dates <- as.Date(c("2024-01-15", "2024-06-30", "2024-12-01", NA))
audit_transform(dates, function(d) d + 30)
#> 
#> ── Transformation Audit [Date]: dates ──────────────────────────────────────────
#> Function: function(d) d + 30
#> 
#>   Metric                       Value
#>   ──────────────  ──────────────────
#>   Total elements                   4
#>   NA (before)                      1
#>   NA (after)                       1
#>   Changed         3 (75.0% of total)
#>   Unchanged                        1
#> 
#> Date range:
#>   Metric           Before       After
#>   ───────────  ──────────  ──────────
#>   Min          2024-01-15  2024-02-14
#>   Max          2024-12-01  2024-12-31
#>   Span (days)       321.0       321.0
#> 
#> Examples of changes (showing 3 of 3):
#>      before      after
#>  2024-01-15 2024-02-14
#>  2024-06-30 2024-07-30
#>  2024-12-01 2024-12-31
#> 
#> Access cleaned vector with: `result$cleaned`
```

#### Factor vectors

``` r
sizes <- factor(c("S", "M", "L", "XL", "XXL", "S", "M"))
audit_transform(sizes, function(f) {
  levels(f)[levels(f) %in% c("XL", "XXL")] <- "XL+"
  f
})
#> 
#> ── Transformation Audit [factor]: sizes ────────────────────────────────────────
#> Function: function(f) {, levels(f)[levels(f) %in% c("XL", "XXL")] <- "XL+", f,
#> and }
#> 
#>   Metric                       Value
#>   ──────────────  ──────────────────
#>   Total elements                   7
#>   NA (before)                      0
#>   NA (after)                       0
#>   Changed         2 (28.6% of total)
#>   Unchanged                        5
#> 
#> Factor levels:
#> • Levels added: XL+
#> • Levels removed: XL, XXL
#> 
#> Level counts (before | after):
#>   Level  Before  After
#>   ─────  ──────  ─────
#>   <NA>        0      0
#>   L           1      1
#>   M           2      2
#>   S           2      2
#>   XL          1      0
#>   XL+         0      2
#>   XXL         1      0
#> 
#> Examples of changes (showing 2 of 2):
#>  before after
#>      XL   XL+
#>     XXL   XL+
#> 
#> Access cleaned vector with: `result$cleaned`
```

#### Logical vectors

``` r
flags <- c(TRUE, FALSE, TRUE, NA, FALSE)
audit_transform(flags, function(x) !x)
#> 
#> ── Transformation Audit [logical]: flags ───────────────────────────────────────
#> Function: function(x) !x
#> 
#>   Metric                       Value
#>   ──────────────  ──────────────────
#>   Total elements                   5
#>   NA (before)                      1
#>   NA (after)                       1
#>   Changed         4 (80.0% of total)
#>   Unchanged                        1
#> 
#> Logical balance:
#>   Value  Before  After
#>   ─────  ──────  ─────
#>   TRUE        2      2
#>   FALSE       2      2
#>   NA          1      1
#> 
#> Examples of changes (showing 4 of 4):
#>  before after
#>    TRUE FALSE
#>   FALSE  TRUE
#>    TRUE FALSE
#>   FALSE  TRUE
#> 
#> Access cleaned vector with: `result$cleaned`
```
