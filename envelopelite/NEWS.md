# envelopelite 0.0.0.9000

Development version, not released.

* Package skeleton: every public function has its signature, its documentation
  and its input validation.
* Shared infrastructure implemented and tested, and exported under the prefix
  of this package since 14 November 2026 (decision of the repository owner):
  draws container (`evl_draws_new()`, `evl_draws_long()`, `evl_draws_wide()`,
  `evl_summarise()`, `evl_draws_generate()`), age groups
  (`evl_age_groups()`) and a simulated country with planted truth
  (`evl_country()`). The code behind them is internal and identical in the
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
* `evl_summarise()` keeps its name and its behaviour; it is now the wrapper of
  the shared summary that every package of the family exports, so its first
  argument is called `x`.
* Skeletons that signal a condition of class `evl_error_not_implemented`:
  `evl_envelope()`, `evl_mx()`, `evl_lifetable()`, `evl_close_open_age()`, `evl_aggregate()`, `evl_abridge()`, `evl_rake()`, `evl_rake_mismatch()`.
