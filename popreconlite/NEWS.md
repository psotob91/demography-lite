# popreconlite 0.0.0.9000

Development version, not released.

* Package skeleton: every public function has its signature, its documentation
  and its input validation.
* Shared infrastructure implemented and tested: draws container
  (`draws_new()`, `as_draws_long()`, `as_draws_wide()`, `draws_summarise()`,
  `draws_generate()`), per-cell seeds (`cell_seed()`, `with_cell_seed()`), age
  groups (`age_groups()`) and a simulated country with planted truth
  (`simulate_country()`).
* Implemented: `prl_simulate()`.
* Skeletons that signal a condition of class `prl_error_not_implemented`:
  `prl_interval()`, `prl_deaths_by_cohort()`, `prl_graduate()`, `prl_rogers_castro()`, `prl_fit_rogers_castro()`, `prl_project()`, `prl_births()`, `prl_spec()`, `prl_fit()`, `prl_draw()`, `prl_check_linearisation()`, `prl_rake()`, `prl_holdout()`.
