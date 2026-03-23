# Compare Two Tables

Compares two data.frames or tibbles by examining column names, row
counts, key overlap, numeric discrepancies, and categorical
discrepancies. Useful for validating data processing pipelines.

## Usage

``` r
compare_tables(
  x,
  y,
  key_cols = NULL,
  tol = .Machine$double.eps,
  top_n = Inf,
  compare_cols = NULL,
  exclude_cols = NULL
)

# S3 method for class 'compare_tbl'
print(x, show_n = 5L, ...)

# S3 method for class 'compare_tbl'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)
```

## Arguments

- x:

  First data.frame or tibble to compare.

- y:

  Second data.frame or tibble to compare.

- key_cols:

  Character vector of column names to use as keys for matching rows. If
  `NULL` (default), automatically detects character, factor, and integer
  columns as keys.

- tol:

  Numeric tolerance for comparing numeric columns. Differences less than
  or equal to `tol` are considered equal. Defaults to
  [`.Machine$double.eps`](https://rdrr.io/r/base/zMachine.html) (machine
  double-precision).

- top_n:

  Maximum number of row-level discrepancies to store **per column**
  (numeric and categorical), and maximum unmatched keys to store.
  Defaults to `Inf` (all). Unmatched keys are stored in arbitrary order.

- compare_cols:

  Character vector of column names to compare. If `NULL` (default), all
  common non-key columns are compared. Mutually exclusive with
  `exclude_cols`.

- exclude_cols:

  Character vector of column names to exclude from comparison. If `NULL`
  (default), no columns are excluded. Mutually exclusive with
  `compare_cols`.

- show_n:

  Maximum number of rows to display for discrepancies and unmatched keys
  in the printed output. Defaults to `5L`.

- ...:

  Additional arguments (currently unused).

- row.names:

  Passed to
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html).
  Default `NULL`.

- optional:

  Passed to
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html).
  Default `FALSE`.

## Value

An S3 object of class `compare_tbl` containing:

- name_x, name_y:

  Names of the compared objects

- common_columns:

  Column names present in both tables

- only_x:

  Column names only in x

- only_y:

  Column names only in y

- type_mismatches:

  Data.frame of columns with different types, or NULL

- nrow_x:

  Number of rows in x

- nrow_y:

  Number of rows in y

- key_summary:

  Summary of key overlap, or NULL

- numeric_summary:

  Data.frame of numeric discrepancy quantiles (with `n_over_tol` count),
  or NULL

- comparison_method:

  How columns were compared (`"keys"`, `"row_index"`, or `NA`)

- rows_matched:

  Number of rows matched on keys

- tol:

  The tolerance used

- top_n:

  The top_n used

- discrepancies:

  Data.frame of row-level numeric discrepancies exceeding `tol` (or
  where one side is `NA`), with key columns (or `row_index`), `column`,
  `value_x`, `value_y`, `abs_diff`, and `pct_diff` (relative difference
  as a proportion). NULL if none.

- categorical_summary:

  Data.frame with `column`, `n_compared`, `n_mismatched`,
  `pct_mismatched` (proportion, 0–1), `n_na_mismatch`, or NULL

- categorical_discrepancies:

  Data.frame of row-level categorical discrepancies with key columns (or
  `row_index`), `column`, `value_x`, `value_y`. NULL if none.

- total_discrepancies:

  Total number of cell-level discrepancies across all column types (not
  limited by `top_n`)

- only_x_keys:

  Data.frame of key combinations only in x (up to `top_n` rows), or NULL

- only_y_keys:

  Data.frame of key combinations only in y (up to `top_n` rows), or NULL

- match_summary:

  List with `only_x`, `only_y`, `matched_no_disc`, `matched_with_disc`,
  `pct_no_disc` (proportion, 0–1), `pct_with_disc` (proportion, 0–1)

Use [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) to
extract all discrepancies (numeric and categorical) as a single tidy
data.frame.

## See also

Other join validation:
[`validate_join()`](https://fpcordeiro.github.io/tidyaudit/reference/validate_join.md),
[`validate_primary_keys()`](https://fpcordeiro.github.io/tidyaudit/reference/validate_primary_keys.md),
[`validate_var_relationship()`](https://fpcordeiro.github.io/tidyaudit/reference/validate_var_relationship.md)

## Examples

``` r
x <- data.frame(id = 1:3, value = c(10.0, 20.0, 30.0))
y <- data.frame(id = 1:3, value = c(10.1, 20.0, 30.5))
compare_tables(x, y)
#> 
#> ── Table Comparison: x vs y ────────────────────────────────────────────────────
#> 1. Row counts
#> x: 3 rows
#> y: 3 rows
#> Difference: =
#> 
#> 2. Column names
#> Matching columns: 2
#> Only in x: 0
#> Only in y: 0
#> Type mismatches: 0
#> 
#> 3. Key columns
#> Key columns: id (auto-detected)
#> Distinct combos in x: 3
#> Distinct combos in y: 3
#> 
#> 4. Row matching
#> Only in x: 0
#> Only in y: 0
#> Matched, no discrepancies: 1 (33.3%)
#> Matched, with discrepancies: 2 (66.7%)
#> Total cell discrepancies: 2 (2 numeric, 0 categorical)
#> 
#> 5. Numeric discrepancies (absolute differences)
#> Compared after merging on keys.
#> Rows matched: 3
#> 
#>     Column  N  Min   Q25  Median  Q75  Max  >tol
#>     ──────  ─  ───  ────  ──────  ───  ───  ────
#>     value   3    0  0.05     0.1  0.3  0.5     2
#> 
#> Top discrepancies:
#>     id  column  value_x  value_y  abs_diff  pct_diff
#>     ──  ──────  ───────  ───────  ────────  ────────
#>     3   value        30     30.5       0.5      1.6%
#>     1   value        10     10.1       0.1      1.0%
#> 
#> 6. Categorical discrepancies
#> No categorical discrepancies found.

# With tolerance — differences <= 0.15 are considered equal
compare_tables(x, y, tol = 0.15)
#> 
#> ── Table Comparison: x vs y ────────────────────────────────────────────────────
#> 1. Row counts
#> x: 3 rows
#> y: 3 rows
#> Difference: =
#> 
#> 2. Column names
#> Matching columns: 2
#> Only in x: 0
#> Only in y: 0
#> Type mismatches: 0
#> 
#> 3. Key columns
#> Key columns: id (auto-detected)
#> Distinct combos in x: 3
#> Distinct combos in y: 3
#> 
#> 4. Row matching (tol = 0.15)
#> Only in x: 0
#> Only in y: 0
#> Matched, no discrepancies: 2 (66.7%)
#> Matched, with discrepancies: 1 (33.3%)
#> Total cell discrepancies: 1 (1 numeric, 0 categorical)
#> 
#> 5. Numeric discrepancies (absolute differences)
#> Compared after merging on keys.
#> Rows matched: 3
#> 
#>     Column  N  Min   Q25  Median  Q75  Max  >tol
#>     ──────  ─  ───  ────  ──────  ───  ───  ────
#>     value   3    0  0.05     0.1  0.3  0.5     1
#> 
#> Top discrepancies:
#>     id  column  value_x  value_y  abs_diff  pct_diff
#>     ──  ──────  ───────  ───────  ────────  ────────
#>     3   value        30     30.5       0.5      1.6%
#> 
#> 6. Categorical discrepancies
#> No categorical discrepancies found.

# Categorical columns are also compared
a <- data.frame(id = 1:3, status = c("ok", "warn", "fail"),
                 stringsAsFactors = FALSE)
b <- data.frame(id = 1:3, status = c("ok", "warn", "error"),
                 stringsAsFactors = FALSE)
compare_tables(a, b)
#> 
#> ── Table Comparison: a vs b ────────────────────────────────────────────────────
#> 1. Row counts
#> a: 3 rows
#> b: 3 rows
#> Difference: =
#> 
#> 2. Column names
#> Matching columns: 2
#> Only in a: 0
#> Only in b: 0
#> Type mismatches: 0
#> 
#> 3. Key columns
#> Key columns: id and status (auto-detected)
#> Distinct combos in a: 3
#> Distinct combos in b: 3
#> 
#> 4. Row matching
#> Only in a: 1
#> Only in b: 1
#> Matched, no discrepancies: 2 (100%)
#> Matched, with discrepancies: 0 (0%)
#> 
#> Unmatched keys in a:
#>     id  status
#>     ──  ──────
#>     3   fail  
#> 
#> Unmatched keys in b:
#>     id  status
#>     ──  ──────
#>     3   error 
#> 
#> 5. Numeric discrepancies (absolute differences)
#> No common numeric columns found.
#> 
#> 6. Categorical discrepancies
#> No value columns compared.
```
