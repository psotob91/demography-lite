#' Scale the areas to the national population
#'
#' Brings the populations of the areas to the national total, draw by draw,
#' and adjusts net migration so that internal migration adds up to zero over
#' areas. The mismatch that the adjustment removes is returned, not hidden.
#'
#' **Status: skeleton.** In this development version the function validates
#' its inputs and then signals a condition of class
#' `prl_error_not_implemented`.
#'
#' @param areas A `demolite_draws` container (see [draws_new()]) with the
#'   population of each area by year, sex and age (persons).
#' @param national A `demolite_draws` container with the national population
#'   for the same years, sexes and ages and the same number of draws.
#'
#' @return When implemented, a list with `population` (a `demolite_draws`
#'   container with the scaled areas) and `mismatch` (a `demolite_draws`
#'   container with the sum over areas divided by the national value before
#'   scaling; 1 means agreement).
#'
#' @section GBD equivalent:
#' **Adapts** the scaling of subnational estimates to the national one in the
#' Global Burden of Disease (GBD) study (GBD 2023 demographics appendix,
#' section 2.5.1). **Differs** in that the pre-scaling mismatch is published
#' and that internal migration is constrained to add up to zero.
#'
#' @section Method:
#' Multiplicative scaling within each stratum and draw:
#' \deqn{P^{*}_{j,k} = P_{j,k}\, \frac{P_{0,k}}{\sum_{j} P_{j,k}}}
#' with \eqn{j} the areas and \eqn{0} the national total. How the change is
#' passed on to migration will be documented when the function is
#' implemented.
#'
#' @references
#' Deming WE, Stephan FF (1940). On a least squares adjustment of a sampled
#' frequency table when the expected marginal totals are known. *Annals of
#' Mathematical Statistics*, 11, 427-444. \doi{10.1214/aoms/1177731829}
#'
#' @examples
#' areas <- draws_new(data.frame(location_id = 2:3, year_id = 2005L),
#'                    matrix(c(100, 210, 104, 205), nrow = 2),
#'                    measure = "population", unit = "persons")
#' national <- draws_new(data.frame(location_id = 1L, year_id = 2005L),
#'                       matrix(c(318, 300), nrow = 1),
#'                       measure = "population", unit = "persons")
#' # Skeleton: valid inputs reach the "not implemented" condition.
#' tryCatch(
#'   prl_rake(areas, national),
#'   prl_error_not_implemented = function(e) conditionMessage(e)
#' )
#'
#' @family population
#' @export
prl_rake <- function(areas, national) {
  .check_draws(areas, "areas")
  .check_draws(national, "national")
  if (ncol(areas$value) != ncol(national$value)) {
    .dl_abort(c("`areas` and `national` must have the same number of draws; ",
                "they have ", ncol(areas$value), " and ", ncol(national$value),
                "."))
  }
  .dl_not_implemented("prl_rake")
}

#' Out-of-sample check with a held-out census
#'
#' Fits the reconstruction without one census, predicts the population at the
#' date of that census and reports how often the uncertainty interval covers
#' the enumerated value.
#'
#' **Status: skeleton.** In this development version the function validates
#' its inputs and then signals a condition of class
#' `prl_error_not_implemented`.
#'
#' @param inputs Named list with the inputs of one area (see
#'   [prl_project()]), including all the censuses.
#' @param spec An object of class `prl_spec` created with [prl_spec()].
#' @param holdout Number. Date (decimal years) of the census that is held out.
#'
#' @return When implemented, a list with `prediction` (predicted population
#'   at the held-out census with its interval, by sex and age) and `coverage`
#'   (proportion of sex-age cells whose interval covers the census).
#'
#' @section GBD equivalent:
#' **Replicates** the out-of-sample validation with held-out censuses of the
#' Global Burden of Disease (GBD) study (GBD 2023 demographics appendix,
#' section 4.4.4, p. 47).
#'
#' @section Method:
#' Fit with [prl_fit()] on the remaining censuses, draws with [prl_draw()],
#' and empirical coverage of the 95% interval at the held-out census.
#'
#' @examples
#' sim <- simulate_country(seed = 1, n_areas = 1, stochastic = FALSE)
#' # Skeleton: `spec` objects cannot be created yet, so the call stops at
#' # input validation.
#' tryCatch(
#'   prl_holdout(inputs = list(census = sim$census), spec = list(),
#'               holdout = 2010.5),
#'   prl_error_input = function(e) conditionMessage(e)
#' )
#'
#' @family population
#' @export
prl_holdout <- function(inputs, spec, holdout) {
  .prl_check_inputs(inputs)
  if (!inherits(spec, "prl_spec")) {
    .dl_abort("`spec` must be an object created with `prl_spec()`.")
  }
  .check_number(holdout, "holdout", lower = 0, upper = 9999)
  .dl_not_implemented("prl_holdout")
}
