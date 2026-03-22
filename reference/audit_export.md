# Export an Audit Trail as a Self-Contained HTML File

Produces a standalone HTML file that visualises the audit trail as an
interactive pipeline flow diagram. The file is completely self-contained
— no server, internet connection, or R installation is required to view
it. Open it in any browser.

## Usage

``` r
audit_export(.trail, file = NULL)
```

## Arguments

- .trail:

  An
  [`audit_trail()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_trail.md)
  object.

- file:

  Path to the output `.html` file. If `NULL` (the default), writes to a
  temporary file and opens it in the default browser via
  [`utils::browseURL()`](https://rdrr.io/r/utils/browseURL.html).

## Value

The file path (character), invisibly.

## Details

The trail is serialised via
[`trail_to_list()`](https://fpcordeiro.github.io/tidyaudit/reference/trail_to_list.md)
and embedded as JSON inside an HTML template with inline CSS and vanilla
JavaScript. The visualisation features:

- Horizontal pipeline flow diagram with colour-coded nodes per operation
  type (snapshot, join, filter).

- Edges annotated with key deltas (match rate, drop \\ added).

- Clickable nodes expanding to show column schema, operation
  diagnostics, and custom `.fns` results.

- Clickable edges showing the full diff between adjacent snapshots.

- Light / dark theme toggle.

- Collapsible JSON export panel.

## See also

[`trail_to_list()`](https://fpcordeiro.github.io/tidyaudit/reference/trail_to_list.md),
[`write_trail()`](https://fpcordeiro.github.io/tidyaudit/reference/write_trail.md)

Other audit export:
[`read_trail()`](https://fpcordeiro.github.io/tidyaudit/reference/read_trail.md),
[`trail_to_df()`](https://fpcordeiro.github.io/tidyaudit/reference/trail_to_df.md),
[`trail_to_list()`](https://fpcordeiro.github.io/tidyaudit/reference/trail_to_list.md),
[`write_trail()`](https://fpcordeiro.github.io/tidyaudit/reference/write_trail.md)

## Examples

``` r
# \donttest{
trail <- audit_trail("demo")
mtcars |> audit_tap(trail, "raw")
dplyr::filter(mtcars, mpg > 20) |> audit_tap(trail, "filtered")
audit_export(trail, tempfile(fileext = ".html"))
# }
```
