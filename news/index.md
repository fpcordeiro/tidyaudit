# Changelog

## tidyaudit 0.2.0

#### New features

- [`audit_export()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_export.md)
  — self-contained HTML trail visualization (interactive pipeline flow
  diagram, light/dark theme, clickable nodes/edges). No server, no Shiny
  required.
- Serialization functions:
  [`trail_to_list()`](https://fpcordeiro.github.io/tidyaudit/reference/trail_to_list.md),
  [`trail_to_df()`](https://fpcordeiro.github.io/tidyaudit/reference/trail_to_df.md),
  [`write_trail()`](https://fpcordeiro.github.io/tidyaudit/reference/write_trail.md),
  [`read_trail()`](https://fpcordeiro.github.io/tidyaudit/reference/read_trail.md)
  for machine-readable trail export (RDS and JSON formats).
- [`tab()`](https://fpcordeiro.github.io/tidyaudit/reference/tab.md) /
  [`tab_tap()`](https://fpcordeiro.github.io/tidyaudit/reference/tab_tap.md)
  — frequency tables and crosstabulations within pipelines. One-way and
  two-way tables with `.sort`, `.cutoff`, `.wt`, `.na`, `.display`
  options.
- Snapshot controls: `.numeric_summary`, `.cols_include` /
  `.cols_exclude` parameters on all taps to limit snapshot scope on wide
  datasets.
- [`print.audit_trail()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_trail.md)
  improvements: tabular “Changes” block with
  `From`/`To`/`Rows`/`Cols`/`NAs` columns and `+N`/`-N`/`=` deltas;
  inline custom diagnostic annotations (`.fns` results) below each
  snapshot row with `show_custom` parameter.

#### Breaking changes

- [`audit_tap()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_tap.md):
  parameter `label` renamed to `.label` for API consistency with other
  taps (no deprecation — early-stage project).
- Join taps: `.stat` now accepts bare column names (NSE via
  [`enquo()`](https://rlang.r-lib.org/reference/enquo.html)/[`as_label()`](https://rlang.r-lib.org/reference/as_label.html)),
  matching filter tap behavior. Quoted strings still work.
- Internal snapshot field `col_info` renamed to `schema` (internal only,
  no user-facing API change).

#### Improvements

- [`audit_transform()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_transform.md)
  is now generic across all vector types (numeric, Date/POSIXct, factor,
  logical, character) with type-appropriate diagnostics.
- [`audit_report()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_report.md):
  removed unimplemented `format = "rmd"` option (deferred to a future
  version).

## tidyaudit 0.1.0

CRAN release: 2026-02-27

- Initial CRAN submission.
