# Create an Audit Trail

Creates an audit trail object that captures metadata snapshots at each
step of a data pipeline. The trail uses environment-based reference
semantics so it can be modified in place inside pipes via
[`audit_tap()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_tap.md).

## Usage

``` r
# S3 method for class 'audit_snap'
print(x, ...)

audit_trail(name = NULL)

# S3 method for class 'audit_trail'
print(x, show_custom = TRUE, ...)
```

## Arguments

- x:

  An object to print.

- ...:

  Additional arguments (currently unused).

- name:

  Optional name for the trail. If `NULL`, a timestamped name is
  generated automatically.

- show_custom:

  Logical. If `TRUE` (default), inline annotations (one indented line
  per custom function) are printed below each snapshot that has custom
  diagnostics. Set to `FALSE` to suppress them and display only the main
  timeline table.

## Value

An `audit_trail` object (S3 class wrapping an environment).

## See also

Other audit trail:
[`audit_diff()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_diff.md),
[`audit_report()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_report.md),
[`audit_tap()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_tap.md),
[`tab_tap()`](https://fpcordeiro.github.io/tidyaudit/reference/tab_tap.md)

## Examples

``` r
trail <- audit_trail("my_analysis")
print(trail)
#> 
#> ── Audit Trail: "my_analysis" ──────────────────────────────────────────────────
#> Created: 2026-03-23 13:32:14
#> Snapshots: 0
#> 
#> ℹ No snapshots recorded yet. Use `audit_tap()` in a pipe to add snapshots.
```
