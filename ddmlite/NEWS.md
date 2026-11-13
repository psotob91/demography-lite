# ddmlite 0.0.0.9000

Development version, not released.

* Package skeleton: every public function has its signature, its documentation
  and its input validation.
* Shared infrastructure implemented and tested: draws container
  (`draws_new()`, `as_draws_long()`, `as_draws_wide()`, `draws_summarise()`,
  `draws_generate()`), per-cell seeds (`cell_seed()`, `with_cell_seed()`), age
  groups (`age_groups()`) and a simulated country with planted truth
  (`simulate_country()`).
* Implemented: `ddl_cap()`, `ddl_combine()`, `ddl_simulate()`.
* Skeletons that signal a condition of class `ddl_error_not_implemented`:
  `ddl_interval()`, `ddl_deaths_in_interval()`, `ddl_ggb()`, `ddl_seg()`, `ddl_ggbseg()`, `ddl_cohort_balance()`, `ddl_estimate()`, `ddl_draws()`, `ddl_child_completeness()`, `ddl_smooth_series()`, `ddl_area_mismatch()`.
