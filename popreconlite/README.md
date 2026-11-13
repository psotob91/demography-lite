# popreconlite

> **Under development.** Version 0.0.0.9000 is a skeleton: the interface, the
> documentation and the input validation exist, but most estimation functions
> are not implemented yet and signal a condition of class
> `prl_error_not_implemented`. Do not use it for analysis.

`popreconlite`: population by cohort components between censuses (single year of age, annual steps, both sexes, areas with internal migration, paired draws).

This package is an independent implementation. It is not affiliated with, endorsed by, or supported by the Institute for Health Metrics and Evaluation (IHME), and it does not contain IHME code or data.

It is not a successor of the 'popReconstruct' package. This package belongs to the second release wave: it will be completed after the other two.

## What works today

Shared infrastructure of the family (identical in the three packages):

- `draws_new()`, `as_draws_long()`, `as_draws_wide()`, `draws_summarise()`,
  `draws_generate()`: a container of draws (cells x draws matrix plus a table
  of cells) and its summary (mean, 2.5th and 97.5th percentiles).
- `cell_seed()`, `with_cell_seed()`: per-cell seeds, so that draw `k` of a cell
  is the same whatever the order of the cells or the split across processes.
- `age_groups()`: age groups identified by exact start and end, including the
  six groups under 5 years of the Global Burden of Disease (GBD) study.
- `simulate_country()`: a small, fully invented country with planted truth.

Functions of this package that are implemented:

- `prl_simulate()`

Skeletons (documented, validated, not implemented): `prl_interval()`, `prl_deaths_by_cohort()`, `prl_graduate()`, `prl_rogers_castro()`, `prl_fit_rogers_castro()`, `prl_project()`, `prl_births()`, `prl_spec()`, `prl_fit()`, `prl_draw()`, `prl_check_linearisation()`, `prl_rake()`, `prl_holdout()`.

## Installation

Not on CRAN. From a local clone of the repository:

```r
install.packages("path/to/demography-lite/popreconlite", repos = NULL, type = "source")
```

## Example

```r
library(popreconlite)
sim <- simulate_country(seed = 2024, n_areas = 2)
sim$truth$completeness

cells <- data.frame(location_id = 1:3)
x <- draws_generate(cells, n_draws = 100, seed = 2024, stream = "example",
                    generator = function(n, cell) stats::rnorm(n),
                    measure = "example")
draws_summarise(x)
```

## Licence

MIT. Authors and copyright holder: TODO (placeholders until the repository
owner sets them).
