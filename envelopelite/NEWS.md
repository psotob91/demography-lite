# envelopelite 0.0.0.9000

Development version, not released.

* Package skeleton: every public function has its signature, its documentation
  and its input validation.
* Shared infrastructure implemented and tested: draws container
  (`draws_new()`, `as_draws_long()`, `as_draws_wide()`, `draws_summarise()`,
  `draws_generate()`), per-cell seeds (`cell_seed()`, `with_cell_seed()`), age
  groups (`age_groups()`) and a simulated country with planted truth
  (`simulate_country()`).
* Implemented: `evl_summarise()`.
* Skeletons that signal a condition of class `evl_error_not_implemented`:
  `evl_envelope()`, `evl_mx()`, `evl_lifetable()`, `evl_close_open_age()`, `evl_aggregate()`, `evl_abridge()`, `evl_rake()`, `evl_rake_mismatch()`.
