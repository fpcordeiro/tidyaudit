# Filter Data with Diagnostic Statistics (Drop)

Filters a data.frame or tibble by DROPPING rows where the conditions are
TRUE, while reporting statistics about dropped rows and optionally the
sum of a statistic column that was dropped.

## Usage

``` r
filter_drop(.data, ...)

# S3 method for class 'data.frame'
filter_drop(.data, ..., .stat = NULL, .quiet = FALSE, .warn_threshold = NULL)
```

## Arguments

- .data:

  A data.frame, tibble, or other object.

- ...:

  Filter conditions specifying rows to DROP, evaluated in the context of
  `.data` using tidy evaluation.

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

- `filter_drop(data.frame)`: Method for data.frame objects

## See also

Other filter diagnostics:
[`filter_keep()`](https://fpcordeiro.github.io/tidyaudit/reference/filter_keep.md)

## Examples

``` r
df <- data.frame(
  id = 1:5,
  bad = c(FALSE, TRUE, FALSE, TRUE, FALSE),
  sales = 10:14
)
filter_drop(df, bad == TRUE)
#> filter_drop(df, bad == TRUE)
#> Dropped 2 of 5 rows (40.00%).
#>   id   bad sales
#> 1  1 FALSE    10
#> 2  3 FALSE    12
#> 3  5 FALSE    14
filter_drop(df, bad == TRUE, .stat = sales)
#> filter_drop(df, bad == TRUE)
#> Dropped 2 of 5 rows (40.00%).
#> Dropped 24 of 60 for sales (40.00%).
#>   id   bad sales
#> 1  1 FALSE    10
#> 2  3 FALSE    12
#> 3  5 FALSE    14
```
