# Filter Data with Diagnostic Statistics (Keep)

Filters a data.frame or tibble while reporting statistics about dropped
rows and optionally the sum of a statistic column that was dropped.
Keeps rows where the conditions are TRUE (same as
[`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)).

## Usage

``` r
filter_keep(.data, ...)

# S3 method for class 'data.frame'
filter_keep(.data, ..., .stat = NULL, .quiet = FALSE, .warn_threshold = NULL)
```

## Arguments

- .data:

  A data.frame, tibble, or other object.

- ...:

  Filter conditions, evaluated in the context of `.data` using tidy
  evaluation (same as
  [`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)).

- .stat:

  An unquoted column or expression to total, e.g., `amount`,
  `price * qty`. Reports the amount dropped and its share of the total.

- .quiet:

  Logical. If `TRUE`, suppress printing diagnostics.

- .warn_threshold:

  Numeric between 0 and 1. If set and the proportion of dropped rows
  exceeds this threshold, a warning is issued.

## Value

The filtered data.frame or tibble.

## Methods (by class)

- `filter_keep(data.frame)`: Method for data.frame objects

## See also

Other filter diagnostics:
[`filter_drop()`](https://fpcordeiro.github.io/tidyaudit/reference/filter_drop.md)

## Examples

``` r
df <- data.frame(
  id = 1:6,
  keep = c(TRUE, FALSE, TRUE, NA, TRUE, FALSE),
  sales = c(100, 50, 200, 25, NA, 75)
)
filter_keep(df, keep == TRUE)
#> filter_keep(df, keep == TRUE)
#> Dropped 3 of 6 rows (50.00%).
#>   id keep sales
#> 1  1 TRUE   100
#> 2  3 TRUE   200
#> 3  5 TRUE    NA
filter_keep(df, keep == TRUE, .stat = sales)
#> filter_keep(df, keep == TRUE)
#> Dropped 3 of 6 rows (50.00%).
#> Dropped 150 of 450 for sales (33.33%).
#>   id keep sales
#> 1  1 TRUE   100
#> 2  3 TRUE   200
#> 3  5 TRUE    NA
```
