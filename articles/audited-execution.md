# Audited Execution

``` r

library(tidyaudit)
library(dplyr)
```

## Capturing lineage without taps

The audit trail layer
([`audit_tap()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_tap.md)
and the operation-aware taps) records exactly the steps you mark.
**Audited execution** flips that around: write one line at the top, run
your wrangling, and tidyaudit records the lineage of *every* data.frame
you create or change — no per-step taps required.

``` r

trail <- audit_record({
  raw    <- as_tibble(mtcars)
  clean  <- filter(raw, mpg > 20)
  lookup <- data.frame(cyl = c(4, 6, 8), label = c("small", "mid", "big"))
  joined <- left_join(clean, lookup, by = "cyl")
})

trail
#> 
#> ── Audit Trail: "trail_20260626_221410" ────────────────────────────────────────
#> Created: 2026-06-26 22:14:10
#> Snapshots: 4
#> 
#>   #  Label   Rows  Cols  NAs  Type
#>   ─  ──────  ────  ────  ───  ────
#>   1  raw       32    11    0  tap 
#>   2  clean     14    11    0  tap 
#>   3  lookup     3     2    0  tap 
#>   4  joined    14    12    0  tap
```

Each top-level statement becomes a versioned snapshot, tagged with the
line of code that produced it and the parent data.frames it derived
from. The join is where lineage earns its keep: `joined` converges from
*both* tables it was built from, while `raw` — the entry point — has no
parent.

``` r

ids <- vapply(trail$snapshots, function(s) s$snapshot_id, character(1))
obj <- vapply(trail$snapshots, function(s) s$object_name, character(1))
data.frame(
  object  = obj,
  parents = vapply(trail$snapshots, function(s) {
    paste(obj[match(unlist(s$parent_snapshot_ids), ids)], collapse = ", ")
  }, character(1))
)
#>   object       parents
#> 1    raw              
#> 2  clean           raw
#> 3 lookup              
#> 4 joined clean, lookup
```

Capture is metadata-only by default: shape, types, and NA counts, never
the rows.

## Three ways in

| Function | Use it for |
|----|----|
| `audit_source("script.R")` | The canonical runner. Works in every context — interactive, [`source()`](https://rdrr.io/r/base/source.html)d, or `Rscript`. |
| `audit_record({ ... })` | Auditing an inline block, as above. |
| [`audit_start()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_start.md) / [`audit_stop()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_start.md) | Interactive-session convenience: a line at the top of a console session and one at the bottom. |

[`audit_source()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_source.md)
is the right tool for a whole script:

``` r

script <- tempfile(fileext = ".R")
writeLines(c(
  "raw   <- dplyr::as_tibble(mtcars)",
  "clean <- dplyr::filter(raw, mpg > 20)",
  "agg   <- dplyr::summarise(clean, n = dplyr::n(), mean_mpg = mean(mpg))"
), script)

strail <- audit_source(script)
strail
#> 
#> ── Audit Trail: "file1d007d86e5a0.R" ───────────────────────────────────────────
#> Created: 2026-06-26 22:14:10
#> Snapshots: 3
#> 
#>   #  Label  Rows  Cols  NAs  Type
#>   ─  ─────  ────  ────  ───  ────
#>   1  raw      32    11    0  tap 
#>   2  clean    14    11    0  tap 
#>   3  agg       1     2    0  tap
```

> **Note on
> [`audit_start()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_start.md).**
> It registers a top-level task callback, which fires per statement at
> the console. R treats `source("file.R")` as a *single* task, so a
> script run via [`source()`](https://rdrr.io/r/base/source.html) under
> [`audit_start()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_start.md)
> records only one combined step. For scripts, use
> [`audit_source()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_source.md).

## What a snapshot knows

Every snapshot carries versioned-lineage fields. Names are for display;
IDs are for identity, so reassignments and self-overwrites stay
unambiguous.

``` r

df <- trail_to_df(trail)
df[, c("object_name", "version", "event", "nrow", "ncol")]
#>   object_name version  event nrow ncol
#> 1         raw       1 create   32   11
#> 2       clean       1 create   14   11
#> 3      lookup       1 create    3    2
#> 4      joined       1 create   14   12
```

The identity fields ride along on every row as list-columns. Unpack them
to see the `snapshot_id` that names each version and the
`parent_snapshot_ids` that wire the lineage together — the same IDs the
join above resolved back to names:

``` r

data.frame(
  object_name = unlist(df$object_name),
  snapshot_id = unlist(df$snapshot_id),
  parents     = vapply(df$parent_snapshot_ids,
                       function(p) paste(p, collapse = ", "), character(1))
)
#>   object_name snapshot_id parents
#> 1         raw          s1        
#> 2       clean          s2      s1
#> 3      lookup          s3        
#> 4      joined          s4  s2, s3
```

Parents are resolved from the state *before* each statement runs, so a
self-overwrite links to the previous version:

``` r

trail2 <- audit_record({
  x <- data.frame(a = 1:5)
  x <- mutate(x, b = a * 2)   # parent is the previous `x`, not the new one
})
vapply(trail2$snapshots, function(s) {
  paste0(s$object_name, " v", s$version, " <- ",
         paste(s$parent_snapshot_ids, collapse = ", "))
}, character(1))
#> [1] "clean v1 <- "  "df v1 <- "     "joined v1 <- " "lookup v1 <- "
#> [5] "raw v1 <- "    "x v1 <- "      "x v2 <- s6"
```

## Lifecycle events

Assignments, in-place edits, and deletions all produce events: `create`,
`update`, `delete` (via [`rm()`](https://rdrr.io/r/base/rm.html)),
`retire` (a binding that stops being a data.frame), and
`unchanged_assignment` (assigned, but no detectable change at the
current evidence level).

## Detecting value-only changes

At the default `level = "metadata"`, a change that does not alter shape,
types, or NA counts is reported as `unchanged_assignment`. To detect
value-only edits, raise the evidence level — tidyaudit then hashes
column contents with a per-run salt:

``` r

audit_record({
  x <- data.frame(a = c(1, 2, 3))
  x <- mutate(x, a = a * 10)   # same shape; value-only change
}, level = "column_hash")$snapshots[[2]]$event
#> [1] "create"
```

When a hash level is used, each snapshot records an `evidence` entry
describing the algorithm, sampling, and salt policy alongside the hash
itself, so the trail documents exactly how it was produced:

``` r

s <- audit_record({
  x <- data.frame(a = c(1, 2, 3))
}, level = "column_hash")$snapshots[[1]]
str(s$evidence)
#> List of 5
#>  $ algorithm  : chr "xxhash (rlang::hash)"
#>  $ level      : chr "column_hash"
#>  $ sample     : chr "all rows and columns"
#>  $ salt_policy: chr "per-run (clock + PID); not privacy-preserving"
#>  $ hash       : chr "1a9284a114b95db2a3b7a68a66743a57"
```

Hashes use a per-run salt and are **not** a privacy guarantee — unsalted
hashes of small categorical columns can be reversed by dictionary
attack. Keep the default `"metadata"` level when the trail may be
shared.

## Lineage resolution limits

Parents are inferred statically from the call you wrote, not from
runtime values. For data-mask verbs
([`filter()`](https://dplyr.tidyverse.org/reference/filter.html),
[`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html),
[`summarise()`](https://dplyr.tidyverse.org/reference/summarise.html),
[`select()`](https://dplyr.tidyverse.org/reference/select.html), and the
like) only the primary data argument is treated as a parent; the masked
expressions are not scanned. That is deliberate — it keeps a column
reference such as `filter(df, mpg > 20)` from inventing a parent when a
stray object named `mpg` happens to exist. The trade-off is that a
genuine cross-data-frame reference *inside* a masked argument,
e.g. `mutate(df, new = other_df$x)`, will not record `other_df` as a
parent. If you want that edge captured in the lineage, lift the
reference into its own statement (`v <- other_df$x`) before using it.

## Exporting the report

[`audit_export()`](https://fpcordeiro.github.io/tidyaudit/reference/audit_export.md)
renders an audited trail as a self-contained HTML file: a tabular report
(run summary, step timeline, objects and versions, warnings and errors)
followed by a per-object lineage graph where joins converge from two
parents.

``` r

audit_export(trail, "lineage.html")
```
