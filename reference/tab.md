# Tabulate Variables

Produces one-way frequency tables or two-way crosstabulations. One
variable gives counts, percentages, and cumulative percentages; two
variables give a crosstabulation matrix with row/column totals.

## Usage

``` r
tab(
  .data,
  ...,
  .wt = NULL,
  .sort = c("value_asc", "value_desc", "freq_desc", "freq_asc"),
  .cutoff = NULL,
  .na = c("include", "exclude", "only"),
  .display = c("count", "row_pct", "col_pct", "total_pct")
)

# S3 method for class 'tidyaudit_tab'
print(x, ...)

# S3 method for class 'tidyaudit_tab'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)
```

## Arguments

- .data:

  A data.frame or tibble.

- ...:

  Additional arguments (currently unused).

- .wt:

  Optional unquoted column to use as frequency weights. When supplied,
  frequencies are weighted sums instead of row counts.

- .sort:

  How to order the rows (and columns in two-way tables). `"value_asc"`
  (default) sorts alphabetically (or by factor levels), `"value_desc"`
  sorts in reverse, `"freq_desc"` sorts by frequency descending,
  `"freq_asc"` sorts by frequency ascending.

- .cutoff:

  Controls how many values to display. An integer \>= 1 keeps the top-N
  values by frequency. A number in (0, 1) keeps values that cumulatively
  account for that proportion of the total. Remaining values are grouped
  under `"(Other)"`. For two-way tables, the cutoff applies to the row
  variable only.

- .na:

  How to handle `NA` values. `"include"` (default) treats `NA` as a
  category, `"exclude"` drops `NA` rows before tabulation, `"only"`
  shows only `NA` rows.

- .display:

  Cell contents for two-way crosstabulations. One of `"count"`
  (default), `"row_pct"`, `"col_pct"`, or `"total_pct"`. Ignored for
  one-way tables.

- x:

  A `tidyaudit_tab` object.

- row.names:

  Passed to
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html).
  Default `NULL`.

- optional:

  Passed to
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html).
  Default `FALSE`.

## Value

An S3 object of class `tidyaudit_tab`. Use
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) to
extract the underlying table.

## See also

Other data quality:
[`audit_transform()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_transform.md),
[`diagnose_nas()`](https://fpcordeiro.github.io/tidyaudit/reference/diagnose_nas.md),
[`diagnose_strings()`](https://fpcordeiro.github.io/tidyaudit/reference/diagnose_strings.md),
[`get_summary_table()`](https://fpcordeiro.github.io/tidyaudit/reference/get_summary_table.md),
[`summarize_column()`](https://fpcordeiro.github.io/tidyaudit/reference/summarize_column.md)

## Examples

``` r
tab(mtcars, cyl)
#> 
#> ── Tabulation: cyl ─────────────────────────────────────────────────────────────
#> 32 observations
#> 
#>   Value  Freq  Percent    Cum.
#>   ─────  ────  ───────  ──────
#>   4        11    34.4%   34.4%
#>   6         7    21.9%   56.3%
#>   8        14    43.8%  100.0%
#>   ─────  ────  ───────  ──────
#>   Total    32   100.0%        
tab(mtcars, cyl, .sort = "freq_desc")
#> 
#> ── Tabulation: cyl ─────────────────────────────────────────────────────────────
#> 32 observations
#> 
#>   Value  Freq  Percent    Cum.
#>   ─────  ────  ───────  ──────
#>   8        14    43.8%   43.8%
#>   4        11    34.4%   78.2%
#>   6         7    21.9%  100.0%
#>   ─────  ────  ───────  ──────
#>   Total    32   100.0%        
tab(mtcars, cyl, gear)
#> 
#> ── Crosstabulation: cyl × gear ─────────────────────────────────────────────────
#> 32 observations | Cell contents: count
#> 
#>   cyl     3   4  5  Total
#>   ─────  ──  ──  ─  ─────
#>   4       1   8  2     11
#>   6       2   4  1      7
#>   8      12   0  2     14
#>   ─────  ──  ──  ─  ─────
#>   Total  15  12  5     32
tab(mtcars, cyl, gear, .display = "row_pct")
#> 
#> ── Crosstabulation: cyl × gear ─────────────────────────────────────────────────
#> 32 observations | Cell contents: row %
#> 
#>   cyl        3      4      5  Total
#>   ─────  ─────  ─────  ─────  ─────
#>   4       9.1%  72.7%  18.2%     11
#>   6      28.6%  57.1%  14.3%      7
#>   8      85.7%   0.0%  14.3%     14
#>   ─────  ─────  ─────  ─────  ─────
#>   Total     15     12      5     32
tab(mtcars, cyl, .wt = mpg)
#> 
#> ── Tabulation: cyl (weighted by mpg) ───────────────────────────────────────────
#> 32 observations
#> 
#>   Value   Freq  Percent    Cum.
#>   ─────  ─────  ───────  ──────
#>   4      293.3    45.6%   45.6%
#>   6      138.2    21.5%   67.1%
#>   8      211.4    32.9%  100.0%
#>   ─────  ─────  ───────  ──────
#>   Total  642.9   100.0%        
tab(mtcars, cyl, .cutoff = 2)
#> 
#> ── Tabulation: cyl ─────────────────────────────────────────────────────────────
#> 32 observations
#> 
#>   Value    Freq  Percent    Cum.
#>   ───────  ────  ───────  ──────
#>   4          11    34.4%   34.4%
#>   8          14    43.8%   78.2%
#>   (Other)     7    21.9%  100.0%
#>   ───────  ────  ───────  ──────
#>   Total      32   100.0%        
#> 
#> (Other) collapses 1 value below cutoff
```
