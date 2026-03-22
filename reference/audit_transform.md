# Audit a Vector Transformation

Applies a transformation function to a vector and reports what changed.
Works with any vector type: character, numeric, Date/POSIXct, factor, or
logical. Diagnostics are adapted to the detected input type.

## Usage

``` r
audit_transform(
  x,
  clean_fn,
  name = NULL,
  .tolerance = sqrt(.Machine$double.eps)
)

# S3 method for class 'audit_transform'
print(x, ...)
```

## Arguments

- x:

  Vector to transform. Accepted types: character, numeric, Date,
  POSIXct, factor, or logical.

- clean_fn:

  A function applied to `x` that returns a vector of the same length,
  **or** a pre-computed vector of the same length (used directly as the
  transformation result).

- name:

  Optional name for the variable (used in output). If `NULL`, captures
  the variable name from the call.

- .tolerance:

  Numeric tolerance used for the "changed beyond tolerance" diagnostic
  (numeric type only). Defaults to `sqrt(.Machine$double.eps)`.

- ...:

  Additional arguments (currently unused).

## Value

An S3 object of class `audit_transform` containing:

- name:

  Name of the variable

- clean_fn_name:

  Name of the transformation function, or `"<pre-computed>"` when a
  vector was supplied directly

- type_class:

  Detected type: `"character"`, `"numeric"`, `"Date"`, `"POSIXct"`,
  `"factor"`, or `"logical"`

- n_total:

  Total number of elements

- n_changed:

  Count of values that changed (including NA status changes)

- n_unchanged:

  Count of values that stayed the same

- n_na_before:

  Count of NA values before transformation

- n_na_after:

  Count of NA values after transformation

- pct_changed:

  Percentage of total elements that changed

- change_examples:

  Data frame with before/after pairs (up to 10)

- diagnostics:

  Type-specific diagnostic list, or `NULL` for character

- cleaned:

  The transformed vector, retaining its type

## See also

[`diagnose_strings()`](https://fpcordeiro.github.io/tidyaudit/reference/diagnose_strings.md)

Other data quality:
[`diagnose_nas()`](https://fpcordeiro.github.io/tidyaudit/reference/diagnose_nas.md),
[`diagnose_strings()`](https://fpcordeiro.github.io/tidyaudit/reference/diagnose_strings.md),
[`get_summary_table()`](https://fpcordeiro.github.io/tidyaudit/reference/get_summary_table.md),
[`summarize_column()`](https://fpcordeiro.github.io/tidyaudit/reference/summarize_column.md),
[`tab()`](https://fpcordeiro.github.io/tidyaudit/reference/tab.md)

## Examples

``` r
# Character
x <- c("  hello ", "WORLD", "  foo  ", NA)
result <- audit_transform(x, trimws)
result$cleaned
#> [1] "hello" "WORLD" "foo"   NA     

# Numeric
prices <- c(10.5, 20.0, NA, 30.0)
audit_transform(prices, function(v) round(v))
#> 
#> ── Transformation Audit [numeric]: prices ──────────────────────────────────────
#> Function: function(v) round(v)
#> 
#>   Metric                       Value
#>   ──────────────  ──────────────────
#>   Total elements                   4
#>   NA (before)                      1
#>   NA (after)                       1
#>   Changed         1 (25.0% of total)
#>   Unchanged                        3
#> 
#> Numeric summary:
#>   Metric  Before  After
#>   ──────  ──────  ─────
#>   Mean     20.17     20
#>   SD       9.751     10
#>   Min       10.5     10
#>   Median      20     20
#>   Max         30     30
#>   NaN          0      0
#>   Inf          0      0
#> 
#> Mean absolute delta: 0.5
#> Changed beyond tolerance: 1 (33.3%)
#> 
#> Examples of changes (showing 1 of 1):
#>  before after
#>    10.5    10
#> 
#> Access cleaned vector with: `result$cleaned`

# Pre-computed result
audit_transform(prices, round(prices))
#> 
#> ── Transformation Audit [numeric]: prices ──────────────────────────────────────
#> Function: <pre-computed>
#> 
#>   Metric                       Value
#>   ──────────────  ──────────────────
#>   Total elements                   4
#>   NA (before)                      1
#>   NA (after)                       1
#>   Changed         1 (25.0% of total)
#>   Unchanged                        3
#> 
#> Numeric summary:
#>   Metric  Before  After
#>   ──────  ──────  ─────
#>   Mean     20.17     20
#>   SD       9.751     10
#>   Min       10.5     10
#>   Median      20     20
#>   Max         30     30
#>   NaN          0      0
#>   Inf          0      0
#> 
#> Mean absolute delta: 0.5
#> Changed beyond tolerance: 1 (33.3%)
#> 
#> Examples of changes (showing 1 of 1):
#>  before after
#>    10.5    10
#> 
#> Access cleaned vector with: `result$cleaned`
```
