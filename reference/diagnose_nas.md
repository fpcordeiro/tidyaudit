# Diagnose Missing Values

Reports NA counts and percentages for each column in a data.frame,
sorted by missing percentage in descending order.

## Usage

``` r
diagnose_nas(.data)

# S3 method for class 'diagnose_na'
print(x, ...)
```

## Arguments

- .data:

  A data.frame or tibble to diagnose.

- x:

  An object to print.

- ...:

  Additional arguments (currently unused).

## Value

An S3 object of class `diagnose_na` containing:

- table:

  A data.frame with columns `variable`, `n_na`, `pct_na`, and `n_valid`,
  sorted by `pct_na` descending.

- n_cols:

  Total number of columns in the input.

- n_with_na:

  Number of columns that have at least one NA.

## See also

Other data quality:
[`audit_transform()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_transform.md),
[`diagnose_strings()`](https://fpcordeiro.github.io/tidyaudit/reference/diagnose_strings.md),
[`get_summary_table()`](https://fpcordeiro.github.io/tidyaudit/reference/get_summary_table.md),
[`summarize_column()`](https://fpcordeiro.github.io/tidyaudit/reference/summarize_column.md),
[`tab()`](https://fpcordeiro.github.io/tidyaudit/reference/tab.md)

## Examples

``` r
df <- data.frame(
  a = c(1, NA, 3),
  b = c(NA, NA, "x"),
  c = c(TRUE, FALSE, TRUE)
)
diagnose_nas(df)
#> 
#> ── Missing Value Diagnosis ─────────────────────────────────────────────────────
#> 2 of 3 columns have missing values
#> 
#>   Variable  N NA  Pct NA
#>   ────────  ────  ──────
#>   b            2   66.7%
#>   a            1   33.3%
```
