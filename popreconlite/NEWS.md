# popreconlite 0.0.0.9000

Development version, not released.

* Package skeleton: every public function has its signature, its documentation
  and its input validation.
* Shared infrastructure implemented and tested, and exported under the prefix
  of this package since 14 November 2026 (decision of the repository owner):
  draws container (`prl_draws_new()`, `prl_draws_long()`, `prl_draws_wide()`,
  `prl_summarise()`, `prl_draws_generate()`), age groups
  (`prl_age_groups()`) and a simulated country with planted truth
  (`prl_country()`). The code behind them is internal and identical in the
  three packages, so attaching two of them no longer masks nine names and no
  longer makes the order of loading decide which print method is registered;
  the print methods of the shared classes are registered as S3 methods.
* The random state of a cell is now the six words of the L'Ecuyer-CMRG
  generator (about 2^192 states) instead of a single 32-bit seed. It is still
  derived from the labels of the cell and from nothing else, so adding a year
  of data does not move the draws of the cells already computed, but two cells
  of a grid of 168,000 no longer collide with probability 96%: the probability
  is now about 2 x 10^-48, and no collision appeared on a simulated grid of
  200,000 cells. **Every draw of this version differs from the previous
  one**, the simulated country included.
* `prl_simulate()` is replaced by `prl_country()`, the name that every package
  of the family uses for the simulated country.
* Skeletons that signal a condition of class `prl_error_not_implemented`:
  `prl_interval()`, `prl_deaths_by_cohort()`, `prl_graduate()`, `prl_rogers_castro()`, `prl_fit_rogers_castro()`, `prl_project()`, `prl_births()`, `prl_spec()`, `prl_fit()`, `prl_draw()`, `prl_check_linearisation()`, `prl_rake()`, `prl_holdout()`.
