## R CMD check results

0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Fernando Cordeiro <fernandolpcordeiro@gmail.com>'

  Days since last update: <fill in at submission time>

## Reason for resubmission

This patch release fixes ERRORs reported in the CRAN r-devel checks for
tidyaudit 0.2.0 on r-devel-linux-x86_64-debian-gcc,
r-devel-linux-x86_64-fedora-clang, and r-devel-windows-x86_64.

R-devel (r89994+) tightened `as.data.frame.table()` so the count column,
routed through `as.data.frame.integer()`, no longer accepts `NA` in
names. `audit_transform()` for factor inputs uses `table(., useNA = "always")`
internally and tripped this check. The internal helper now constructs the
data frame directly, sidestepping `as.data.frame.table()`. No user-visible
output change. Release builds remain unaffected.

## Test environments

* macOS (latest), R release
* Windows (latest), R release
* Ubuntu (latest), R devel, release, oldrel-1
