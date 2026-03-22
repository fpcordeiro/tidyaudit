# Validate Variable Relationship

Determines the relationship between two variables in a data.frame:
one-to-one, one-to-many, many-to-one, or many-to-many.

## Usage

``` r
validate_var_relationship(.data, var1, var2)

# S3 method for class 'validate_var_rel'
print(x, ...)
```

## Arguments

- .data:

  A data.frame or tibble.

- var1:

  Character string: name of the first variable.

- var2:

  Character string: name of the second variable.

- x:

  An object to print.

- ...:

  Additional arguments (currently unused).

## Value

An S3 object of class `validate_var_rel` containing:

- table_name:

  Name of the input table

- var1, var2:

  Names of the variables analyzed

- relation:

  Character string: "one-to-one", "one-to-many", "many-to-one", or
  "many-to-many"

- var1_unique:

  Number of distinct values in var1

- var2_unique:

  Number of distinct values in var2

- n_combinations:

  Number of unique (var1, var2) pairs

- var1_has_dups:

  Does any var1 value map to multiple var2 values?

- var2_has_dups:

  Does any var2 value map to multiple var1 values?

## Details

Only accepts variables of type character, integer, or factor. Numeric
(double) variables are not allowed due to floating-point comparison
issues.

## See also

Other join validation:
[`compare_tables()`](https://fpcordeiro.github.io/tidyaudit/reference/compare_tables.md),
[`validate_join()`](https://fpcordeiro.github.io/tidyaudit/reference/validate_join.md),
[`validate_primary_keys()`](https://fpcordeiro.github.io/tidyaudit/reference/validate_primary_keys.md)

## Examples

``` r
df <- data.frame(
  person_id = c(1L, 2L, 3L, 4L),
  department = c("Sales", "Sales", "Engineering", "Engineering"),
  country = c("US", "US", "US", "UK")
)
validate_var_relationship(df, "person_id", "department")
#> 
#> ── Variable Relationship Validation ────────────────────────────────────────────
#> Table: df
#> Variables: person_id ↔ department
#> 
#>   Metric                                Value
#>   ────────────────────────────────────  ─────
#>   Unique values in person_id                4
#>   Unique values in department               2
#>   Unique (person_id, department) pairs      4
#> 
#> person_id → department: one-to-one
#> department → person_id: one-to-many
#> 
#> Relationship: MANY-TO-ONE
```
