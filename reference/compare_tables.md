# Compare Two Tables

Compares two data.frames or tibbles by examining column names, row
counts, key overlap, and numeric discrepancies. Useful for validating
data processing pipelines.

## Usage

``` r
compare_tables(x, y, key_cols = NULL, tol = .Machine$double.eps, top_n = Inf)

# S3 method for class 'compare_tbl'
print(x, show_n = 5L, ...)
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

  Maximum number of row-level discrepancies to store **per numeric
  column**, and maximum unmatched keys to store. Defaults to `Inf`
  (all). Unmatched keys are stored in arbitrary order.

- show_n:

  Maximum number of rows to display for discrepancies and unmatched keys
  in the printed output. Defaults to `5L`.

- ...:

  Additional arguments (currently unused).

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

- numeric_method:

  How numeric columns were compared

- rows_matched:

  Number of rows matched on keys

- tol:

  The tolerance used

- top_n:

  The top_n used

- discrepancies:

  Data.frame of row-level numeric discrepancies exceeding `tol` (or
  where one side is `NA`), with key columns (or `row_index`), `column`,
  `value_x`, `value_y`, and `abs_diff`. Rows with `abs_diff = NA`
  indicate that one value is `NA`. NULL if none.

- only_x_keys:

  Data.frame of key combinations only in x (up to `top_n` rows), or NULL

- only_y_keys:

  Data.frame of key combinations only in y (up to `top_n` rows), or NULL

- match_summary:

  List with `only_x`, `only_y`, `matched_no_disc`, `matched_with_disc`,
  `pct_no_disc`, `pct_with_disc`

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
#>     id  column  value_x  value_y  abs_diff
#>     ──  ──────  ───────  ───────  ────────
#>     3   value        30     30.5       0.5
#>     1   value        10     10.1       0.1

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
#>     id  column  value_x  value_y  abs_diff
#>     ──  ──────  ───────  ───────  ────────
#>     3   value        30     30.5       0.5
```
