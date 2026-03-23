# Read an Audit Trail from a File

Restores an
[`audit_trail()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_trail.md)
previously saved with
[`write_trail()`](https://fpcordeiro.github.io/tidyaudit/reference/write_trail.md).
The file format is detected automatically from the file extension
(`.rds` for RDS, `.json` for JSON), or can be specified explicitly via
`format`.

## Usage

``` r
read_trail(file, format = NULL)
```

## Arguments

- file:

  Path to an RDS or JSON file created by
  [`write_trail()`](https://fpcordeiro.github.io/tidyaudit/reference/write_trail.md).

- format:

  One of `"rds"`, `"json"`, or `NULL` (default). When `NULL`, the format
  is inferred from the file extension.

## Value

A reconstructed
[`audit_trail()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_trail.md)
object with all S3 classes restored.

## See also

[`write_trail()`](https://fpcordeiro.github.io/tidyaudit/reference/write_trail.md)

Other audit export:
[`audit_export()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_export.md),
[`trail_to_df()`](https://fpcordeiro.github.io/tidyaudit/reference/trail_to_df.md),
[`trail_to_list()`](https://fpcordeiro.github.io/tidyaudit/reference/trail_to_list.md),
[`write_trail()`](https://fpcordeiro.github.io/tidyaudit/reference/write_trail.md)

## Examples

``` r
trail <- audit_trail("example")
mtcars |> audit_tap(trail, "raw")
tmp <- tempfile(fileext = ".rds")
write_trail(trail, tmp)
restored <- read_trail(tmp)
print(restored)
#> 
#> ── Audit Trail: "example" ──────────────────────────────────────────────────────
#> Created: 2026-03-23 13:32:17
#> Snapshots: 1
#> 
#>   #  Label  Rows  Cols  NAs  Type
#>   ─  ─────  ────  ────  ───  ────
#>   1  raw      32    11    0  tap 
```
