# demography-lite

> **Under development, not released.** This repository is a skeleton. The
> shared infrastructure is implemented and tested; most estimation functions
> are documented interfaces that signal "not implemented yet". Do not use it
> for analysis.

A family of three small R packages for the demographic side of a burden of
disease study, written as independent, simplified implementations of methods
described for the Global Burden of Disease (GBD) study:

| Package | Prefix | What it is for | Status |
|---|---|---|---|
| [`ddmlite`](ddmlite/) | `ddl_` | completeness of death registration: death distribution methods with and without net migration, by area, with paired draws | 3 of its 14 completeness functions implemented, plus the 7 shared wrappers |
| [`popreconlite`](popreconlite/) | `prl_` | population between censuses by cohort components: single year of age, annual steps, both sexes, areas linked by internal migration | none of its 13 reconstruction functions implemented yet, only the 7 shared wrappers; second release wave |
| [`envelopelite`](envelopelite/) | `evl_` | thin linkage package: all-cause deaths by draw, scaling of areas to their total, data contract towards packages that estimate causes of death | none of its 8 linkage functions implemented yet, only the 7 shared wrappers |

These packages are independent implementations. They are **not affiliated
with, endorsed by, or supported by the Institute for Health Metrics and
Evaluation (IHME)**, and they do not contain IHME code or data. They are not
successors of the 'DDM', 'DDMethods' or 'popReconstruct' packages.

## What is implemented

Only what the three packages share, plus a few trivial functions. The shared
code is internal and each package exports it under its own prefix (`ddl_`,
`prl_`, `evl_`), so attaching two packages of the family masks nothing:

- **Draws container** (`<prefix>_draws_new()`, `<prefix>_draws_long()`,
  `<prefix>_draws_wide()`, `<prefix>_draws_generate()`): a table of cells and
  a cells x draws matrix. Draw `k` is the same simulated "world" in every
  product, and containers pass from one package of the family to another.
- **Summary of draws** (`<prefix>_summarise()`): mean, 2.5th and 97.5th
  percentiles.
- **Random state per cell** (internal `cell_seed()`, `with_cell_seed()`,
  reached through `<prefix>_draws_generate()`): draw `k` of a cell is the same
  whatever the order of the cells, the split across processes or the cells
  added later, because the state is derived from the labels of the cell. It is
  the six-word state of the L'Ecuyer-CMRG generator, about 2^192 values. No
  function ever fixes a seed; the caller always supplies it.
- **Age groups** (`<prefix>_age_groups()`): identified by exact start and end,
  with the six GBD groups under 5 years (0-6 days, 7-27 days, 1-5 months,
  6-11 months, 12-23 months, 2-4 years).
- **Input validators** with classed errors (`<prefix>_error_input`,
  `demolite_error_input`) and classed warnings (`<prefix>_warning_*`).
- **Simulated country with planted truth** (`<prefix>_country()`): small and
  fully invented; population at two censuses, births, migration, true deaths,
  a completeness planted in each area and the deaths registered.
- Trivial package functions: `ddl_cap()`, `ddl_combine()`, `ddl_simulate()`.

Everything else is a **skeleton**: signature, full documentation (including
the sections "GBD equivalent" and "Method"), input validation, and a body
that signals a condition of class `<prefix>_error_not_implemented`.

## Layout

```
standalone/         single source of the shared code and of its tests
ddmlite/            package (R/import-standalone-*.R and R/import-prefixed-*.R
                    are copies)
popreconlite/       package
envelopelite/       package
tools/              sync of the copies and the controls
renv.lock           development tools (documentation, tests, checks)
AI-USAGE.md         what was done by an AI tool and what a human still has to verify
```

The shared code lives in `standalone/` and is **copied** into each package,
in the style of `usethis::use_standalone()`: no fourth package, no
dependency between the three. Never edit a copy; edit the source and run
`Rscript tools/sync-standalone.R`. If the copies ever need to diverge, the
shared code becomes a small base package instead.

The shared code is internal, so each package also carries the public wrappers
that give it a usable interface of its own. They come from one template,
`standalone/prefixed-shared.R`, which the same tool copies into every package
replacing the token `PKGPREFIX` plus an underscore by the prefix of the
package. That file is also a copy: do not edit it either.

## Development

The packages import only 'data.table' and 'stats'. The development tools
('roxygen2', 'testthat', 'devtools', 'pkgdown', 'rcmdcheck') are pinned in
`renv.lock`:

```r
renv::restore()
```

```sh
Rscript tools/sync-standalone.R       # copy the shared files into the packages
Rscript tools/run-controls.R          # the four controls and their own tests
Rscript tools/check-packages.R        # roxygen2 + R CMD check of the three packages
Rscript tools/check-packages.R --pkgdown   # also build the local sites
```

Controls (each one prints how many files or functions it looked at, and each
one is tested against a broken case in `tools/tests/`):

| Control | Fails when |
|---|---|
| `tools/sync-standalone.R --check` | a copy of the shared code, or a copy of the template of the public wrappers, differs from what its source gives |
| `tools/check-clean.R` | anything from the national study in which these packages were first developed appears in the repository (place names, institutions, data paths, internal codes); `--history` scans every commit |
| `tools/check-cran-text.R` | a frequent reason for rejection by CRAN is detectable from text (`T`/`F`, fixed seeds, console output, global state, more than two cores, `\dontrun`, ...) |
| `tools/check-docs.R` | an exported function lacks a title, an argument, its return value, an example, or the sections "GBD equivalent" and "Method" |

## To do before making this repository public

These are decisions of the repository owner, not of the tools:

- the owner's ORCID, if it is to appear in `Authors@R`. Everything else is
  set: author, maintainer and copyright holder are Percy Soto-Becerra, with
  the e-mail address already public on his GitHub profile;
- the first `git push` to <https://github.com/psotob91/demography-lite>: the
  repository exists and is empty, and **nothing has been published yet**;
- whether the three pkgdown sites go on GitHub Pages, and then the `url:`
  field of each `_pkgdown.yml`, which is still empty;
- cite, in `ddl_cap()`, the passage of the GBD 2023 demographics appendix that
  states the rule of 95%;
- human review of everything listed in [`AI-USAGE.md`](AI-USAGE.md), where
  three of the seven decisions taken by the tool are now settled.

## Licence

MIT (see [`LICENSE.md`](LICENSE.md)); copyright holder: Percy Soto-Becerra.
