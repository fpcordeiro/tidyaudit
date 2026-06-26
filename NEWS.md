# tidyaudit 0.3.0

### New features

* **Audited execution** — capture data-frame lineage across a whole script or
  session without per-step taps:
  * `audit_source()` runs an `.R` file like `source()` but returns an audit
    trail; it owns the evaluation loop, so capture works interactively, under
    `source()`, and under `Rscript`.
  * `audit_record({ ... })` audits an inline block.
  * `audit_start()` / `audit_stop()` provide interactive-session convenience via
    a top-level task callback. (Use `audit_source()` for scripts: `source()` is
    a single top-level task.)
* Capture granularity is top-level statement lineage. Snapshots carry
  versioned-lineage fields (binding-stream `object_id`, `version`, `event`,
  `source`, `srcref`, `parent_snapshot_ids`, `level`); parents resolve from the
  pre-evaluation state and only to data.frames. Lifecycle events
  (`create`/`update`/`delete`/`retire`/`unchanged_assignment`) are tracked.
* Capture is metadata-only by default; `level` (`metadata`, `sample_hash`,
  `column_hash`, `full_hash`) opts into per-run-salted content hashing to detect
  value-only changes.
* `audit_export()` renders audited trails as a tabular report (run summary, step
  timeline, objects & versions, warnings & errors) plus a per-object lineage
  graph. Classic tap trails still render the linear flow.

# tidyaudit 0.2.1

### Bug fixes

* `audit_transform()` for factor inputs no longer errors under R-devel
  (r89994+). The internal frequency-table builder previously routed counts
  through `as.data.frame.table()`, which now rejects `NA` in row names and
  failed for any factor (since the `useNA = "always"` bucket carries an
  `NA` name). The table is now constructed directly. No user-visible
  output change.

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
