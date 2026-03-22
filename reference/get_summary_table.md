# Generate Summary Table for a Data Frame

Creates a comprehensive summary of all columns in a data.frame,
including type, missing values, descriptive statistics, and example
values.

## Usage

``` r
get_summary_table(.data, cols = NULL)
```

## Arguments

- .data:

  A data.frame or tibble to summarize.

- cols:

  Optional character vector of column names to summarize. If `NULL` (the
  default), all columns are summarized.

## Value

A data.frame with one row per column containing summary statistics.

## See also

Other data quality:
[`audit_transform()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_transform.md),
[`diagnose_nas()`](https://fpcordeiro.github.io/tidyaudit/reference/diagnose_nas.md),
[`diagnose_strings()`](https://fpcordeiro.github.io/tidyaudit/reference/diagnose_strings.md),
[`summarize_column()`](https://fpcordeiro.github.io/tidyaudit/reference/summarize_column.md),
[`tab()`](https://fpcordeiro.github.io/tidyaudit/reference/tab.md)

## Examples

``` r
df <- data.frame(
  id = 1:100,
  value = rnorm(100),
  category = sample(letters[1:5], 100, replace = TRUE)
)
get_summary_table(df)
#>   variable      type n_unique missing missing_share most_frequent
#> 1       id   numeric      100       0             0          <NA>
#> 2    value   numeric      100       0             0          <NA>
#> 3 category character        5       0             0             b
#>                 mean               sd              min                q25
#> 1               50.5  29.011491975882                1              25.75
#> 2 0.0707892042614083 1.05090514930022 -2.6123343328843 -0.356076186540761
#> 3               <NA>             <NA>                a               <NA>
#>                  q50               q75              max          example1
#> 1               50.5             75.25              100                 1
#> 2 0.0910364665832006 0.623410051570413 2.75541757533686 -1.40004351672175
#> 3               <NA>              <NA>                e                 d
#>           example2          example3
#> 1                2                 3
#> 2 0.25531705484526 -2.43726361121953
#> 3                d                 c
```
