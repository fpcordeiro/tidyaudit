# Write an Audit Trail to a File

Saves an
[`audit_trail()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_trail.md)
to disk as either an RDS file (default) or a JSON file. The RDS format
preserves all R types and can be restored perfectly with
[`read_trail()`](https://fpcordeiro.github.io/tidyaudit/reference/read_trail.md).
The JSON format produces a human-readable representation suitable for
archiving or interoperability with other tools.

## Usage

``` r
write_trail(.trail, file, format = c("rds", "json"))
```

## Arguments

- .trail:

  An
  [`audit_trail()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_trail.md)
  object.

- file:

  Path to the output file. A `.rds` extension is conventional for
  `format = "rds"`; `.json` for `format = "json"`.

- format:

  One of `"rds"` (default) or `"json"`. The JSON format requires the
  jsonlite package to be installed.

## Value

`.trail`, invisibly.

## Note

Custom diagnostic results (the `custom` field, populated via `.fns` in
[`audit_tap()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_tap.md))
are serialised on a best-effort basis for JSON output. Complex R objects
such as environments or functions cannot be represented in JSON and will
cause an error.

## See also

[`read_trail()`](https://fpcordeiro.github.io/tidyaudit/reference/read_trail.md),
[`trail_to_list()`](https://fpcordeiro.github.io/tidyaudit/reference/trail_to_list.md)

Other audit export:
[`audit_export()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_export.md),
[`read_trail()`](https://fpcordeiro.github.io/tidyaudit/reference/read_trail.md),
[`trail_to_df()`](https://fpcordeiro.github.io/tidyaudit/reference/trail_to_df.md),
[`trail_to_list()`](https://fpcordeiro.github.io/tidyaudit/reference/trail_to_list.md)

## Examples

``` r
trail <- audit_trail("example")
mtcars |> audit_tap(trail, "raw")
tmp <- tempfile(fileext = ".rds")
write_trail(trail, tmp)
restored <- read_trail(tmp)
```
