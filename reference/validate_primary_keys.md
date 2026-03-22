# Validate Primary Keys

Tests whether a set of columns constitute primary keys of a data.frame,
i.e., whether they uniquely identify every row in the table.

## Usage

``` r
validate_primary_keys(.data, keys)

# S3 method for class 'validate_pk'
print(x, ...)
```

## Arguments

- .data:

  A data.frame or tibble.

- keys:

  Character vector of column names to test as primary keys.

- x:

  An object to print.

- ...:

  Additional arguments (currently unused).

## Value

An S3 object of class `validate_pk` containing:

- table_name:

  Name of the input table from the original call

- keys:

  Character vector of column names tested

- is_primary_key:

  Logical: TRUE if keys uniquely identify all rows AND no key column
  contains NA values

- n_rows:

  Total number of rows in the table

- n_unique_keys:

  Number of distinct key combinations

- n_duplicate_keys:

  Number of key combinations that appear more than once

- duplicate_keys:

  A data.frame of duplicated key values with their counts

- has_numeric_keys:

  Logical: TRUE if any key column is of type double

- has_na_keys:

  Logical: TRUE if any key column contains NA values

- na_in_keys:

  Named logical vector indicating which key columns contain NAs

## See also

Other join validation:
[`compare_tables()`](https://fpcordeiro.github.io/tidyaudit/reference/compare_tables.md),
[`validate_join()`](https://fpcordeiro.github.io/tidyaudit/reference/validate_join.md),
[`validate_var_relationship()`](https://fpcordeiro.github.io/tidyaudit/reference/validate_var_relationship.md)

## Examples

``` r
df <- data.frame(
  id = c(1L, 2L, 3L, 4L),
  group = c("A", "A", "B", "B"),
  value = c(10, 20, 30, 40)
)
validate_primary_keys(df, "id")
#> 
#> ── Primary Key Validation ──────────────────────────────────────────────────────
#> Table: df
#> Key column: id
#> 
#>   Metric                   Value
#>   ───────────────────────  ─────
#>   Total rows                   4
#>   Unique key combinations      4
#>   Duplicate key combos         0
#> 
#> ✔ YES - Keys uniquely identify all rows.
validate_primary_keys(df, "group")
#> 
#> ── Primary Key Validation ──────────────────────────────────────────────────────
#> Table: df
#> Key column: group
#> 
#>   Metric                   Value
#>   ───────────────────────  ─────
#>   Total rows                   4
#>   Unique key combinations      2
#>   Duplicate key combos         2
#> 
#> ✖ NO - Keys do NOT uniquely identify all rows.
#> 
#> Duplicate keys (showing up to 10):
#>   group n
#> 1     A 2
#> 2     B 2
```
