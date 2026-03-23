# Convert an Audit Trail to a Data Frame

Returns a plain `data.frame` with one row per snapshot. Nested fields
(`all_columns`, `schema`, `numeric_summary`, `changes`, `diagnostics`,
`custom`, `pipeline`, `controls`) become list-columns. Trail metadata is
stored as attributes on the result.

## Usage

``` r
trail_to_df(.trail)
```

## Arguments

- .trail:

  An
  [`audit_trail()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_trail.md)
  object.

## Value

A `data.frame` with columns `index`, `label`, `type`, `timestamp`,
`nrow`, `ncol`, `total_nas`, `all_columns`, `schema`, `numeric_summary`,
`changes`, `diagnostics`, `custom`, `pipeline`, and `controls`. Trail
`name` and `created_at` are stored as attributes `"trail_name"` and
`"created_at"`.

## See also

Other audit export:
[`audit_export()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_export.md),
[`read_trail()`](https://fpcordeiro.github.io/tidyaudit/reference/read_trail.md),
[`trail_to_list()`](https://fpcordeiro.github.io/tidyaudit/reference/trail_to_list.md),
[`write_trail()`](https://fpcordeiro.github.io/tidyaudit/reference/write_trail.md)

## Examples

``` r
trail <- audit_trail("example")
mtcars |> audit_tap(trail, "raw")
dplyr::filter(mtcars, mpg > 20) |> audit_tap(trail, "filtered")
df <- trail_to_df(trail)
print(df)
#>   index    label type           timestamp nrow ncol total_nas  all_columns
#> 1     1      raw  tap 2026-03-23 13:32:18   32   11         0 mpg, cyl....
#> 2     2 filtered  tap 2026-03-23 13:32:18   14   11         0 mpg, cyl....
#>         schema numeric_summary      changes diagnostics custom     pipeline
#> 1 c("mpg",....    c("mpg",....                                       mtcars
#> 2 c("mpg",....    c("mpg",.... -18, 0, ....                    mtcars, ....
#>   controls
#> 1         
#> 2         
attr(df, "trail_name")
#> [1] "example"
```
