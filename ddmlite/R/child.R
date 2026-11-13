#' Completeness of death registration under age 5
#'
#' Compares the deaths under age 5 that were registered with those expected
#' from an external estimate of the probability of dying before age 5,
#' typically from a household survey, and propagates the error of that
#' estimate draw by draw.
#'
#' The package does not read surveys or compute their rates: the caller
#' supplies the probability and its standard error, obtained for instance with
#' the 'DHS.rates', 'demogsurv' or 'SUMMER' packages.
#'
#' **Status: skeleton.** In this development version the function validates
#' its inputs and then signals a condition of class
#' `ddl_error_not_implemented`.
#'
#' @param registered Data frame or data.table with one row per cell:
#'   identifier columns (for instance `location_id`, `year_id`, `sex_id`) and
#'   `deaths` (registered deaths under age 5, count).
#' @param births Data frame or data.table with the same identifier columns and
#'   `births` (live births that the expected deaths refer to, count).
#' @param survey Data frame or data.table with the same identifier columns,
#'   `q5` (probability of dying before age 5, proportion) and `q5_se` (its
#'   standard error, proportion).
#' @param n_draws Whole number. Number of draws.
#' @param seed Whole number. The caller's seed; the package never chooses one.
#'
#' @return When implemented, a `demolite_draws` container (see [draws_new()])
#'   with one row per cell and the measure `"completeness"` (proportion, not
#'   capped at 1).
#'
#' @section GBD equivalent:
#' **Adapts** the approach of the Global Burden of Disease (GBD) study, which
#' measures the completeness of registered deaths under 5 against its own
#' estimate of under-5 mortality (GBD 2023 Demographics Collaborators, 2025;
#' the passage of the appendix still has to be cited here). **Differs** in that the reference is a survey estimate supplied by
#' the caller instead of a modelled series.
#'
#' @section Method:
#' \deqn{c_k = \frac{D^{reg}}{B \times q_k}}
#' with \eqn{D^{reg}} registered deaths under 5, \eqn{B} births and \eqn{q_k}
#' draw \eqn{k} of the survey probability, generated under the per-cell seed
#' (see [cell_seed()]). How births are matched to the cohorts exposed, and the
#' distribution used for \eqn{q_k}, will be fixed and documented when the
#' function is implemented.
#'
#' @references
#' GBD 2023 Demographics Collaborators (2025). Global age-sex-specific
#' all-cause mortality and life expectancy estimates for 204 countries and
#' territories. *The Lancet*, 406, 1731-1810.
#' \doi{10.1016/S0140-6736(25)01330-3}
#'
#' Rajaratnam JK, Tran LN, Lopez AD, Murray CJL (2010). Measuring under-five
#' mortality: validation of new low-cost methods. *PLoS Medicine*, 7,
#' e1000253. \doi{10.1371/journal.pmed.1000253}
#'
#' @examples
#' cells <- data.frame(location_id = 2:3, sex_id = 3L)
#' # Skeleton: valid inputs reach the "not implemented" condition.
#' tryCatch(
#'   ddl_child_completeness(
#'     registered = cbind(cells, deaths = c(310, 560)),
#'     births = cbind(cells, births = c(20000, 40000)),
#'     survey = cbind(cells, q5 = c(0.021, 0.019), q5_se = c(0.002, 0.002)),
#'     n_draws = 20, seed = 1
#'   ),
#'   ddl_error_not_implemented = function(e) conditionMessage(e)
#' )
#'
#' @family completeness
#' @export
ddl_child_completeness <- function(registered, births, survey, n_draws, seed) {
  .check_table(registered, "registered", cols = "deaths")
  .check_table(births, "births", cols = "births")
  .check_table(survey, "survey", cols = c("q5", "q5_se"))
  .check_numeric(survey[["q5"]], "survey$q5", lower = 0, upper = 1)
  .check_numeric(survey[["q5_se"]], "survey$q5_se", lower = 0, upper = 1)
  .check_count(n_draws, "n_draws")
  .check_seed(seed)
  .dl_not_implemented("ddl_child_completeness")
}
