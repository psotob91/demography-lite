#' Specify what the reconstruction estimates
#'
#' Declares which components are estimated (base population, net migration)
#' and with which prior information, so that every variant of the adjustment
#' is explicit.
#'
#' **Status: skeleton.** In this development version the function validates
#' its inputs and then signals a condition of class
#' `prl_error_not_implemented`.
#'
#' @param estimate Character vector. Components to estimate: any of
#'   `"base_population"` and `"migration"`.
#' @param priors Named list. Prior information for the estimated components.
#'   Its elements will be fixed when the function is implemented.
#'
#' @return When implemented, an object of class `prl_spec` describing the
#'   variant of the adjustment.
#'
#' @section GBD equivalent:
#' **Own.** It makes explicit the choices that the population model of the
#' Global Burden of Disease (GBD) study fixes internally (GBD 2023
#' demographics appendix, section 4.4.2, p. 45).
#'
#' @section Method:
#' No estimation is involved: the function only records the choices.
#'
#' @examples
#' # Skeleton: valid inputs reach the "not implemented" condition.
#' tryCatch(
#'   prl_spec(estimate = "migration"),
#'   prl_error_not_implemented = function(e) conditionMessage(e)
#' )
#'
#' @family population
#' @export
prl_spec <- function(estimate = c("base_population", "migration"),
                     priors = list()) {
  .check_choice(estimate, "estimate", c("base_population", "migration"),
                several = TRUE)
  if (!is.list(priors)) {
    .dl_abort("`priors` must be a list.")
  }
  .dl_not_implemented("prl_spec")
}

#' Fit the reconstruction to the censuses
#'
#' Adjusts the components declared in the specification so that the projected
#' population reproduces the censuses.
#'
#' **Status: skeleton.** In this development version the function validates
#' its inputs and then signals a condition of class
#' `prl_error_not_implemented`.
#'
#' @param inputs Named list with the inputs of one area (see
#'   [prl_project()]), including the censuses to fit.
#' @param spec An object of class `prl_spec` created with [prl_spec()].
#'
#' @return When implemented, an object of class `prl_fit`: a list with the
#'   mode of the estimated components, the residuals at each census, the
#'   variance chosen for the census errors and convergence information.
#'
#' @section GBD equivalent:
#' **Adapts** the population model of the Global Burden of Disease (GBD)
#' study, a modified version of the Bayesian population reconstruction of
#' Wheldon and colleagues that estimates migration (GBD 2023 demographics
#' appendix, section 4.4.2, p. 45). **Differs** in the estimator: the mode is
#' found by Gauss-Newton iterations instead of sampling a full Bayesian
#' posterior; this is a simplification, and its accuracy is what
#' [prl_check_linearisation()] measures.
#'
#' @section Method:
#' Penalised least squares on the scale of log population: residuals between
#' projected and enumerated populations at each census, plus penalties that
#' pull the estimated components towards their priors. The objective, the
#' choice of the error variance and the stopping rule will be written here
#' when the function is implemented.
#'
#' @references
#' Wheldon MC, Raftery AE, Clark SJ, Gerland P (2013). Reconstructing past
#' populations with uncertainty from fragmentary data. *Journal of the
#' American Statistical Association*, 108, 96-110.
#' \doi{10.1080/01621459.2012.737729}
#'
#' Wheldon MC, Raftery AE, Clark SJ, Gerland P (2015). Bayesian reconstruction
#' of two-sex populations by age. *Journal of the Royal Statistical Society:
#' Series A*, 178, 977-1007. \doi{10.1111/rssa.12104}
#'
#' @examples
#' sim <- prl_country(seed = 1, n_areas = 1, stochastic = FALSE)
#' # Skeleton: `spec` objects cannot be created yet, so the call stops at
#' # input validation.
#' tryCatch(
#'   prl_fit(inputs = list(census = sim$census), spec = list()),
#'   prl_error_input = function(e) conditionMessage(e)
#' )
#'
#' @family population
#' @export
prl_fit <- function(inputs, spec) {
  .prl_check_inputs(inputs)
  if (!inherits(spec, "prl_spec")) {
    .dl_abort("`spec` must be an object created with `prl_spec()`.")
  }
  .dl_not_implemented("prl_fit")
}

#' One draw of population and migration around the fit
#'
#' Produces one draw of the reconstructed population and of net migration,
#' given the fit and the inputs of that draw (for instance draw `k` of the
#' deaths), so that draw `k` of the population belongs to the same "world" as
#' draw `k` of its inputs.
#'
#' **Status: skeleton.** In this development version the function validates
#' its inputs and then signals a condition of class
#' `prl_error_not_implemented`.
#'
#' @param fit An object of class `prl_fit` created with [prl_fit()].
#' @param inputs Named list with the inputs of this draw (see
#'   [prl_project()]).
#' @param draw Whole number. Index of the draw, from 1.
#' @param seed Whole number. The caller's seed; the package never chooses one.
#'
#' @return When implemented, a list with the population and the net migration
#'   of the draw, in the layout of [prl_project()].
#'
#' @section GBD equivalent:
#' **Own.** The Global Burden of Disease (GBD) study obtains draws from the
#' posterior of its model; here draws are generated around the mode.
#'
#' @section Method:
#' A linear (one-step) update of the mode for the inputs of the draw, plus a
#' random deviation from the approximate posterior, generated under the
#' per-cell seed (see [cell_seed()]). Details will be documented when the
#' function is implemented.
#'
#' @examples
#' # Skeleton: `fit` objects cannot be created yet, so the call stops at
#' # input validation.
#' tryCatch(
#'   prl_draw(fit = list(), inputs = list(deaths = 1), draw = 1, seed = 1),
#'   prl_error_input = function(e) conditionMessage(e)
#' )
#'
#' @family population
#' @export
prl_draw <- function(fit, inputs, draw, seed) {
  if (!inherits(fit, "prl_fit")) {
    .dl_abort("`fit` must be an object created with `prl_fit()`.")
  }
  .prl_check_inputs(inputs)
  .check_count(draw, "draw")
  .check_seed(seed)
  .dl_not_implemented("prl_draw")
}

#' Check the linear approximation used for the draws
#'
#' Compares, for a few draws, the one-step update used by [prl_draw()] with a
#' full re-fit, to measure the error of the approximation.
#'
#' **Status: skeleton.** In this development version the function validates
#' its inputs and then signals a condition of class
#' `prl_error_not_implemented`.
#'
#' @param fit An object of class `prl_fit` created with [prl_fit()].
#' @param n Whole number. Number of draws to re-fit in full.
#' @param seed Whole number. The caller's seed; the package never chooses one.
#'
#' @return When implemented, a data.table with one row per draw checked and
#'   the largest relative difference in population between the one-step
#'   update and the full fit.
#'
#' @section GBD equivalent:
#' **Own.** It exists because the estimator differs from the full Bayesian
#' model of the Global Burden of Disease (GBD) study (see [prl_fit()]).
#'
#' @section Method:
#' For each of `n` draws: full fit with the inputs of the draw, one-step
#' update from the central fit, and the maximum over ages, sexes and years of
#' the relative difference between both.
#'
#' @examples
#' # Skeleton: `fit` objects cannot be created yet, so the call stops at
#' # input validation.
#' tryCatch(
#'   prl_check_linearisation(fit = list(), n = 5, seed = 1),
#'   prl_error_input = function(e) conditionMessage(e)
#' )
#'
#' @family population
#' @export
prl_check_linearisation <- function(fit, n, seed) {
  if (!inherits(fit, "prl_fit")) {
    .dl_abort("`fit` must be an object created with `prl_fit()`.")
  }
  .check_count(n, "n")
  .check_seed(seed)
  .dl_not_implemented("prl_check_linearisation")
}
