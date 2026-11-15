# ddmlite

> **Under development.** Version 0.0.0.9000 is a skeleton: the interface, the
> documentation and the input validation exist, but most estimation functions
> are not implemented yet and signal a condition of class
> `ddl_error_not_implemented`. Do not use it for analysis.

`ddmlite`: completeness of death registration (death distribution methods, with and without net migration, by area, with paired draws).

This package is an independent implementation. It is not affiliated with, endorsed by, or supported by the Institute for Health Metrics and Evaluation (IHME), and it does not contain IHME code or data.

It is not a successor of the 'DDM' or 'DDMethods' packages. Child mortality from surveys is supplied by the user (for instance from 'DHS.rates', 'demogsurv' or 'SUMMER'); this package does not read surveys.

## What works today

Shared infrastructure of the family. The code is the same in the three
packages and is internal, so attaching two of them masks nothing; each package
exports it under its own prefix:

- `ddl_draws_new()`, `ddl_draws_long()`, `ddl_draws_wide()`,
  `ddl_summarise()`, `ddl_draws_generate()`: a container of draws (cells x
  draws matrix plus a table of cells), its conversions and its summary (mean,
  2.5th and 97.5th percentiles). Containers can be passed from one package of
  the family to another.
- `ddl_draws_generate()` draws cell by cell under a random state derived from
  the key of the cell, so that draw `k` of a cell is the same whatever the
  order of the cells, the split across processes, or the cells added later.
- `ddl_age_groups()`: age groups identified by exact start and end, including
  the six groups under 5 years of the Global Burden of Disease (GBD) study.
- `ddl_country()`: a small, fully invented country with planted truth.

Functions of this package that are implemented:

- `ddl_cap()`
- `ddl_combine()`
- `ddl_simulate()`

Skeletons (documented, validated, not implemented): `ddl_interval()`, `ddl_deaths_in_interval()`, `ddl_ggb()`, `ddl_seg()`, `ddl_ggbseg()`, `ddl_cohort_balance()`, `ddl_estimate()`, `ddl_draws()`, `ddl_child_completeness()`, `ddl_smooth_series()`, `ddl_area_mismatch()`.

## Installation

Not on CRAN. From a local clone of the repository:

```r
install.packages("path/to/demography-lite/ddmlite", repos = NULL, type = "source")
```

## Example

```r
library(ddmlite)
sim <- ddl_country(seed = 2024, n_areas = 2)
sim$truth$completeness

cells <- data.frame(location_id = 1:3)
x <- ddl_draws_generate(cells, n_draws = 100, seed = 2024, stream = "example",
                        generator = function(n, cell) stats::rnorm(n),
                        measure = "example")
ddl_summarise(x)
```

## Licence

MIT. Authors and copyright holder: TODO (placeholders until the repository
owner sets them).
