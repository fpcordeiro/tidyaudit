# Audit an Interactive Session

Begins ambient capture in an **interactive session** (or a script run
directly with `Rscript file.R`): registers a top-level task callback
that records a snapshot after each statement you run, until
`audit_stop()`.

## Usage

``` r
audit_start(
  name = NULL,
  env = globalenv(),
  watch = "data.frames",
  ignore = NULL,
  level = c("metadata", "sample_hash", "column_hash", "full_hash"),
  keys = NULL,
  numeric_summary = TRUE
)

audit_stop()
```

## Arguments

- name:

  Optional trail name. If `NULL`, a timestamped name is generated.

- env:

  Environment to watch. Defaults to the global environment.

- watch:

  Either `"data.frames"` (the default — track every data.frame) or a
  character vector of object names to restrict tracking to.

- ignore:

  Optional character vector of regular expressions; objects whose names
  match any pattern are skipped (e.g. scratch variables).

- level:

  Evidence level: `"metadata"` (default, privacy-safe) detects
  shape/type/NA changes only; `"sample_hash"`, `"column_hash"`, and
  `"full_hash"` additionally detect value-only changes by hashing data
  with a per-run salt. Salted hashes are *not* a privacy guarantee. The
  hashing policy (algorithm, sampling, salt) is recorded in each
  snapshot's `evidence` field.

- keys:

  Optional named list mapping object names to key column(s), used by the
  HTML report to flag primary-key status.

- numeric_summary:

  Logical; passed to the snapshot builder. If `FALSE`, skip numeric
  quantile summaries (the main cost control on wide data).

## Value

`audit_start()` returns the new
[`audit_trail()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_trail.md)
invisibly; `audit_stop()` returns the completed trail.

## Details

This is a convenience wrapper, not the canonical script runner. Task
callbacks fire per top-level statement at the REPL, but R treats
`source("file.R")` as a **single** task — so running a script via
[`source()`](https://rdrr.io/r/base/source.html) under `audit_start()`
records only one combined step. **For scripts, use
[`audit_source()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_source.md).**

The capture handler only *observes* completed statements; it never
re-evaluates them, so side effects are not duplicated. Capture errors
are swallowed so they can never break your REPL.

## See also

[`audit_source()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_source.md),
[`audit_record()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_record.md)

Other audited execution:
[`audit_record()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_record.md),
[`audit_source()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_source.md)

## Examples

``` r
if (FALSE) { # \dontrun{
audit_start("session")
raw   <- dplyr::as_tibble(mtcars)
clean <- dplyr::filter(raw, mpg > 20)
trail <- audit_stop()
print(trail)
} # }
```
