# tidyaudit: Pipeline Audit Trails and Data Diagnostics for 'tidyverse' Workflows

Provides pipeline audit trails and data diagnostics for 'tidyverse'
workflows. The audit trail system captures lightweight metadata
snapshots at each step of a pipeline, building a structured record
without storing the data itself. Operation-aware taps enrich snapshots
with join match rates and filter drop statistics. Trails can be
serialized to 'JSON' or 'RDS' and exported as self-contained 'HTML'
visualizations. Also includes diagnostic functions for interactive data
analysis including frequency tables, string quality auditing, and data
comparison.

## See also

Useful links:

- <https://fpcordeiro.github.io/tidyaudit/>

- <https://github.com/fpcordeiro/tidyaudit>

- Report bugs at <https://github.com/fpcordeiro/tidyaudit/issues>

## Author

**Maintainer**: Fernando Cordeiro <fernandolpcordeiro@gmail.com>
\[copyright holder\]
