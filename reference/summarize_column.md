# Summarize a Single Column

Computes summary statistics for a vector. Handles numeric, character,
factor, logical, Date, and other types with appropriate statistics for
each.

## Usage

``` r
summarize_column(x)
```

## Arguments

- x:

  A vector to summarize.

## Value

A named character vector with summary statistics including: type, unique
count, missing count, missing share (proportion from 0 to 1), most
frequent value (for non-numeric), mean, sd, min, quartiles (q25, q50,
q75), max, and three example values.

## See also

Other data quality:
[`audit_transform()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_transform.md),
[`diagnose_nas()`](https://fpcordeiro.github.io/tidyaudit/reference/diagnose_nas.md),
[`diagnose_strings()`](https://fpcordeiro.github.io/tidyaudit/reference/diagnose_strings.md),
[`get_summary_table()`](https://fpcordeiro.github.io/tidyaudit/reference/get_summary_table.md),
[`tab()`](https://fpcordeiro.github.io/tidyaudit/reference/tab.md)

## Examples

``` r
summarize_column(c(1, 2, 3, NA, 5))
#>               type           n_unique            missing      missing_share 
#>          "numeric"                "4"                "1"              "0.2" 
#>      most_frequent               mean                 sd                min 
#>                 NA             "2.75" "1.70782512765993"                "1" 
#>                q25                q50                q75                max 
#>             "1.75"              "2.5"              "3.5"                "5" 
#>           example1           example2           example3 
#>                "1"                "2"                "3" 
summarize_column(c("a", "b", "a", "c"))
#>          type      n_unique       missing missing_share most_frequent 
#>   "character"           "3"           "0"           "0"           "a" 
#>          mean            sd           min           q25           q50 
#>            NA            NA           "a"            NA            NA 
#>           q75           max      example1      example2      example3 
#>            NA           "c"           "a"           "b"           "a" 
```
