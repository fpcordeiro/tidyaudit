# Diagnose String Column Quality

Audits a character vector for common data quality issues including
missing values, empty strings, whitespace problems, non-ASCII
characters, and case inconsistencies. Requires the stringi package (in
Suggests).

## Usage

``` r
diagnose_strings(x, name = NULL)

# S3 method for class 'diagnose_strings'
print(x, ...)
```

## Arguments

- x:

  Character vector to diagnose.

- name:

  Optional name for the variable (used in output). If `NULL`, captures
  the variable name from the call.

- ...:

  Additional arguments (currently unused).

## Value

An S3 object of class `diagnose_strings` containing:

- name:

  Name of the variable

- n_total:

  Total number of elements

- n_na:

  Count of NA values

- n_empty:

  Count of empty strings

- n_whitespace_only:

  Count of whitespace-only strings

- n_leading_ws:

  Count of strings with leading whitespace

- n_trailing_ws:

  Count of strings with trailing whitespace

- n_non_ascii:

  Count of strings with non-ASCII characters

- n_case_variants:

  Number of unique values with case variants

- n_case_variant_groups:

  Number of groups of case-insensitive duplicates

- case_variant_examples:

  Data.frame with examples of case variants

## See also

Other data quality:
[`audit_transform()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_transform.md),
[`diagnose_nas()`](https://fpcordeiro.github.io/tidyaudit/reference/diagnose_nas.md),
[`get_summary_table()`](https://fpcordeiro.github.io/tidyaudit/reference/get_summary_table.md),
[`summarize_column()`](https://fpcordeiro.github.io/tidyaudit/reference/summarize_column.md),
[`tab()`](https://fpcordeiro.github.io/tidyaudit/reference/tab.md)

## Examples

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
