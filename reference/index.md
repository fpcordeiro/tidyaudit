# Package index

## Audit Trail

Create and inspect pipeline audit trails

- [`print(`*`<audit_snap>`*`)`](https://fpcordeiro.github.io/tidyaudit/reference/audit_trail.md)
  [`audit_trail()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_trail.md)
  [`print(`*`<audit_trail>`*`)`](https://fpcordeiro.github.io/tidyaudit/reference/audit_trail.md)
  : Create an Audit Trail
- [`audit_tap()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_tap.md)
  : Record a Pipeline Snapshot
- [`audit_diff()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_diff.md)
  [`print(`*`<audit_diff>`*`)`](https://fpcordeiro.github.io/tidyaudit/reference/audit_diff.md)
  : Compare Two Audit Trail Snapshots
- [`audit_report()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_report.md)
  : Generate an Audit Report

## Operation-Aware Taps

Pipeline taps with enriched diagnostics

- [`left_join_tap()`](https://fpcordeiro.github.io/tidyaudit/reference/join_tap.md)
  [`right_join_tap()`](https://fpcordeiro.github.io/tidyaudit/reference/join_tap.md)
  [`inner_join_tap()`](https://fpcordeiro.github.io/tidyaudit/reference/join_tap.md)
  [`full_join_tap()`](https://fpcordeiro.github.io/tidyaudit/reference/join_tap.md)
  [`anti_join_tap()`](https://fpcordeiro.github.io/tidyaudit/reference/join_tap.md)
  [`semi_join_tap()`](https://fpcordeiro.github.io/tidyaudit/reference/join_tap.md)
  : Operation-Aware Join Taps
- [`filter_tap()`](https://fpcordeiro.github.io/tidyaudit/reference/filter_tap.md)
  [`filter_out_tap()`](https://fpcordeiro.github.io/tidyaudit/reference/filter_tap.md)
  : Operation-Aware Filter Taps
- [`tab_tap()`](https://fpcordeiro.github.io/tidyaudit/reference/tab_tap.md)
  : Record a Tabulation Snapshot in a Pipeline

## Export & Serialization

Save, load, and visualize trails

- [`audit_export()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_export.md)
  : Export an Audit Trail as a Self-Contained HTML File
- [`trail_to_list()`](https://fpcordeiro.github.io/tidyaudit/reference/trail_to_list.md)
  : Convert an Audit Trail to a Plain List
- [`trail_to_df()`](https://fpcordeiro.github.io/tidyaudit/reference/trail_to_df.md)
  : Convert an Audit Trail to a Data Frame
- [`write_trail()`](https://fpcordeiro.github.io/tidyaudit/reference/write_trail.md)
  : Write an Audit Trail to a File
- [`read_trail()`](https://fpcordeiro.github.io/tidyaudit/reference/read_trail.md)
  : Read an Audit Trail from a File

## Diagnostic Functions

Analyze joins, keys, and table differences

- [`validate_join()`](https://fpcordeiro.github.io/tidyaudit/reference/validate_join.md)
  [`print(`*`<validate_join>`*`)`](https://fpcordeiro.github.io/tidyaudit/reference/validate_join.md)
  [`summary(`*`<validate_join>`*`)`](https://fpcordeiro.github.io/tidyaudit/reference/validate_join.md)
  : Validate Join Operations Between Two Tables
- [`validate_primary_keys()`](https://fpcordeiro.github.io/tidyaudit/reference/validate_primary_keys.md)
  [`print(`*`<validate_pk>`*`)`](https://fpcordeiro.github.io/tidyaudit/reference/validate_primary_keys.md)
  : Validate Primary Keys
- [`validate_var_relationship()`](https://fpcordeiro.github.io/tidyaudit/reference/validate_var_relationship.md)
  [`print(`*`<validate_var_rel>`*`)`](https://fpcordeiro.github.io/tidyaudit/reference/validate_var_relationship.md)
  : Validate Variable Relationship
- [`compare_tables()`](https://fpcordeiro.github.io/tidyaudit/reference/compare_tables.md)
  [`print(`*`<compare_tbl>`*`)`](https://fpcordeiro.github.io/tidyaudit/reference/compare_tables.md)
  : Compare Two Tables

## Data Quality

Filter diagnostics, missing values, string cleaning, and tabulation

- [`filter_keep()`](https://fpcordeiro.github.io/tidyaudit/reference/filter_keep.md)
  : Filter Data with Diagnostic Statistics (Keep)
- [`filter_drop()`](https://fpcordeiro.github.io/tidyaudit/reference/filter_drop.md)
  : Filter Data with Diagnostic Statistics (Drop)
- [`diagnose_nas()`](https://fpcordeiro.github.io/tidyaudit/reference/diagnose_nas.md)
  [`print(`*`<diagnose_na>`*`)`](https://fpcordeiro.github.io/tidyaudit/reference/diagnose_nas.md)
  : Diagnose Missing Values
- [`summarize_column()`](https://fpcordeiro.github.io/tidyaudit/reference/summarize_column.md)
  : Summarize a Single Column
- [`get_summary_table()`](https://fpcordeiro.github.io/tidyaudit/reference/get_summary_table.md)
  : Generate Summary Table for a Data Frame
- [`diagnose_strings()`](https://fpcordeiro.github.io/tidyaudit/reference/diagnose_strings.md)
  [`print(`*`<diagnose_strings>`*`)`](https://fpcordeiro.github.io/tidyaudit/reference/diagnose_strings.md)
  : Diagnose String Column Quality
- [`audit_transform()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_transform.md)
  [`print(`*`<audit_transform>`*`)`](https://fpcordeiro.github.io/tidyaudit/reference/audit_transform.md)
  : Audit a Vector Transformation
- [`tab()`](https://fpcordeiro.github.io/tidyaudit/reference/tab.md)
  [`print(`*`<tidyaudit_tab>`*`)`](https://fpcordeiro.github.io/tidyaudit/reference/tab.md)
  [`as.data.frame(`*`<tidyaudit_tab>`*`)`](https://fpcordeiro.github.io/tidyaudit/reference/tab.md)
  : Tabulate Variables
