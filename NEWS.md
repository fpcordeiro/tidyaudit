# tidyaudit 0.2.0

### New features

* `audit_export()` — self-contained HTML trail visualization (interactive
  pipeline flow diagram, light/dark theme, clickable nodes/edges). No server,
  no Shiny required.
* Serialization functions: `trail_to_list()`, `trail_to_df()`, `write_trail()`,
  `read_trail()` for machine-readable trail export (RDS and JSON formats).
* `tab()` / `tab_tap()` — frequency tables and crosstabulations within
  pipelines. One-way and two-way tables with `.sort`, `.cutoff`, `.wt`, `.na`,
  `.display` options.
* Snapshot controls: `.numeric_summary`, `.cols_include` / `.cols_exclude`
  parameters on all taps to limit snapshot scope on wide datasets.
* `print.audit_trail()` improvements: tabular "Changes" block with
  `From`/`To`/`Rows`/`Cols`/`NAs` columns and `+N`/`-N`/`=` deltas; inline
  custom diagnostic annotations (`.fns` results) below each snapshot row with
  `show_custom` parameter.

### Breaking changes

* `audit_tap()`: parameter `label` renamed to `.label` for API consistency with
  other taps (no deprecation — early-stage project).
* Join taps: `.stat` now accepts bare column names (NSE via
  `enquo()`/`as_label()`), matching filter tap behavior. Quoted strings still
  work.
* Internal snapshot field `col_info` renamed to `schema` (internal only, no
  user-facing API change).

### Improvements

* `audit_transform()` is now generic across all vector types (numeric,
  Date/POSIXct, factor, logical, character) with type-appropriate diagnostics.
* `audit_report()`: removed unimplemented `format = "rmd"` option (deferred to
  a future version).

# tidyaudit 0.1.0

* Initial CRAN submission.
