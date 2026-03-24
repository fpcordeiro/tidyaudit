# Convert an Audit Trail to a Plain List

Converts an
[`audit_trail()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_trail.md)
object to a plain R list suitable for serialisation with
[`jsonlite::toJSON()`](https://jeroen.r-universe.dev/jsonlite/reference/fromJSON.html).
All POSIXct timestamps are converted to ISO 8601 character strings and
data.frames are converted to lists of named rows for JSON compatibility.

## Usage

``` r
trail_to_list(.trail)
```

## Arguments

- .trail:

  An
  [`audit_trail()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_trail.md)
  object.

## Value

A named list with elements `name`, `created_at` (ISO 8601 string),
`n_snapshots`, and `snapshots` (a named list keyed by snapshot label).

## See also

Other audit export:
[`audit_export()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_export.md),
[`read_trail()`](https://fpcordeiro.github.io/tidyaudit/reference/read_trail.md),
[`trail_to_df()`](https://fpcordeiro.github.io/tidyaudit/reference/trail_to_df.md),
[`write_trail()`](https://fpcordeiro.github.io/tidyaudit/reference/write_trail.md)

## Examples

``` r
trail <- audit_trail("example")
mtcars |> audit_tap(trail, "raw")
lst <- trail_to_list(trail)
str(lst, max.level = 2)
#> List of 4
#>  $ name       : chr "example"
#>  $ created_at : chr "2026-03-24T12:12:43Z"
#>  $ n_snapshots: int 1
#>  $ snapshots  :List of 1
#>   ..$ raw:List of 15
```
