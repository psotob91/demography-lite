# ddmlite 0.0.0.9000

Development version, not released.

* Package skeleton: every public function has its signature, its documentation
  and its input validation.
* Shared infrastructure implemented and tested, and exported under the prefix
  of this package since 14 November 2026 (decision of the repository owner):
  draws container (`ddl_draws_new()`, `ddl_draws_long()`, `ddl_draws_wide()`,
  `ddl_summarise()`, `ddl_draws_generate()`), age groups
  (`ddl_age_groups()`) and a simulated country with planted truth
  (`ddl_country()`). The code behind them is internal and identical in the
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
* `ddl_cap()` offers only the published rule (completeness of 0.95 or more
  becomes 1) and adds no transition of its own; its help page gives the size
  of the jump (deaths multiplied by 1.053 at 0.9499 and untouched at 0.9501)
  and it now warns, with a condition of class `ddl_warning_threshold`, when
  some input value falls in [0.90, 0.95), where the threshold decides the
  result. What it returns does not change.
* Implemented: `ddl_cap()`, `ddl_combine()`, `ddl_simulate()`.
* Skeletons that signal a condition of class `ddl_error_not_implemented`:
  `ddl_interval()`, `ddl_deaths_in_interval()`, `ddl_ggb()`, `ddl_seg()`, `ddl_ggbseg()`, `ddl_cohort_balance()`, `ddl_estimate()`, `ddl_draws()`, `ddl_child_completeness()`, `ddl_smooth_series()`, `ddl_area_mismatch()`.
