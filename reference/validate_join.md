# Validate Join Operations Between Two Tables

Analyzes a potential join between two data.frames or tibbles without
performing the full join. Reports relationship type (one-to-one,
one-to-many, etc.), match rates, duplicate keys, and unmatched rows.
Optionally tracks a numeric statistic column through the join to
quantify impact.

## Usage

``` r
validate_join(x, y, by = NULL, stat = NULL, stat_x = NULL, stat_y = NULL)

# S3 method for class 'validate_join'
print(x, ...)

# S3 method for class 'validate_join'
summary(object, ...)
```

## Arguments

- x:

  A data.frame or tibble (left table).

- y:

  A data.frame or tibble (right table).

- by:

  A character vector of column names to join on. Use a named vector
  `c("key_x" = "key_y")` when column names differ between tables.
  Unnamed elements are used for both tables.

- stat:

  Optional single column name (string) to track in both tables when the
  column name is the same. Ignored if `stat_x` or `stat_y` is provided.

- stat_x:

  Optional column name (string) for a numeric statistic in `x`.

- stat_y:

  Optional column name (string) for a numeric statistic in `y`.

- ...:

  Additional arguments (currently unused).

- object:

  A `validate_join` object to summarize.

## Value

An S3 object of class `validate_join` containing:

- x_name, y_name:

  Names of the input tables from the original call

- by_x, by_y:

  Key columns used for the join

- counts:

  List with row counts, match rates, and overlap statistics

- stat:

  When `stat`, `stat_x`, or `stat_y` is provided, a list with stat
  diagnostics per table. `NULL` when no stat is provided.

- duplicates:

  List with duplicate key information for each table

- summary_table:

  A data.frame summarizing the join diagnostics

- relation:

  Character string describing the relationship

- keys_only_in_x:

  Unmatched keys from x

- keys_only_in_y:

  Unmatched keys from y

## See also

Other join validation:
[`compare_tables()`](https://fpcordeiro.github.io/tidyaudit/reference/compare_tables.md),
[`validate_primary_keys()`](https://fpcordeiro.github.io/tidyaudit/reference/validate_primary_keys.md),
[`validate_var_relationship()`](https://fpcordeiro.github.io/tidyaudit/reference/validate_var_relationship.md)

## Examples

``` r
x <- data.frame(id = c(1L, 2L, 3L, 3L), value = c("a", "b", "c", "d"))
y <- data.frame(id = c(2L, 3L, 4L), score = c(10, 20, 30))
result <- validate_join(x, y, by = "id")
print(result)
#> 
#> ── Join Validation: x ↔ y ──────────────────────────────────────────────────────
#> Keys in x: id
#> Keys in y: id
#> 
#>   Item                                   Value
#>   ───────────────────────────────  ───────────
#>   Relationship                     many-to-one
#>   Key(s) in x   [id]                   (1 col)
#>   Key(s) in y   [id]                   (1 col)
#>   Rows in x                                  4
#>   Distinct key combos in x                   3
#>   Rows in y                                  3
#>   Distinct key combos in y                   3
#>   Overlapping distinct key combos            2
#>   Matched row pairs (cartesian)              3
#>   Match rate from x                     75.00%
#>   Match rate from y                     66.67%
#>   Rows only in x (no match in y)             1
#>   Rows only in y (no match in x)             1
#> 
#> Duplicates: x=yes y=no

# Track a stat column with different names in each table
x2 <- data.frame(id = 1:3, sales = c(100, 200, 300))
y2 <- data.frame(id = 2:4, cost = c(10, 20, 30))
validate_join(x2, y2, by = "id", stat_x = "sales", stat_y = "cost")
#> 
#> ── Join Validation: x2 ↔ y2 ────────────────────────────────────────────────────
#> Keys in x2: id
#> Keys in y2: id
#> 
#>   Item                                   Value
#>   ────────────────────────────────  ──────────
#>   Relationship                      one-to-one
#>   Key(s) in x2   [id]                  (1 col)
#>   Key(s) in y2   [id]                  (1 col)
#>   Rows in x2                                 3
#>   Distinct key combos in x2                  3
#>   Rows in y2                                 3
#>   Distinct key combos in y2                  3
#>   Overlapping distinct key combos            2
#>   Matched row pairs (cartesian)              2
#>   Match rate from x2                    66.67%
#>   Match rate from y2                    66.67%
#>   Rows only in x2 (no match in y2)           1
#>   Rows only in y2 (no match in x2)           1
#> 
#> ── Stat diagnostics ────────────────────────────────────────────────────────────
#> 
#> sales in x2:
#> • Total: 600
#> • Matched: 500 (83.33%)
#> • Unmatched: 100 (16.67%)
#> 
#> cost in y2:
#> • Total: 60
#> • Matched: 30 (50.00%)
#> • Unmatched: 30 (50.00%)
#> 
#> Duplicates: x2=no y2=no
```
