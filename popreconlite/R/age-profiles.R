#' Graduate a population from age groups to single years
#'
#' Splits a population given by age groups into single years of age with a
#' smooth curve that never produces negative values and preserves the total
#' of every group.
#'
#' **Status: skeleton.** In this development version the function validates
#' its inputs and then signals a condition of class
#' `prl_error_not_implemented`.
#'
#' @param population Numeric vector. Population by age group, persons.
#' @param age_start,age_end Numeric vectors. Exact start and end of each age
#'   group in years; groups must be contiguous, start at 0 and end with an
#'   open group (`age_end = Inf`).
#'
#' @return When implemented, a data.table with one row per single year of
#'   age: `age_start`, `age_end` and `population` (persons). Group totals are
#'   conserved.
#'
#' @section GBD equivalent:
#' **Adapts** the need of single-year populations in the population model of
#' the Global Burden of Disease (GBD) study. **Differs** in the tool: a
#' monotone spline on the cumulative population.
#'
#' @section Method:
#' The cumulative population \eqn{N(x)} is interpolated at integer ages with a
#' monotonicity-preserving cubic spline (Hyman, 1983) and differenced:
#' \eqn{n_x = N(x + 1) - N(x)}. Monotonicity of \eqn{N} guarantees
#' \eqn{n_x \ge 0}. The treatment of the open group will be fixed and
#' documented when the function is implemented.
#'
#' @references
#' Hyman JM (1983). Accurate monotonicity preserving cubic interpolation.
#' *SIAM Journal on Scientific and Statistical Computing*, 4, 645-654.
#' \doi{10.1137/0904045}
#'
#' @examples
#' groups <- age_groups("five_year")
#' pop <- rev(seq(100, 2000, length.out = nrow(groups)))
#' # Skeleton: valid inputs reach the "not implemented" condition.
#' tryCatch(
#'   prl_graduate(pop, groups$age_start, groups$age_end),
#'   prl_error_not_implemented = function(e) conditionMessage(e)
#' )
#'
#' @family population
#' @export
prl_graduate <- function(population, age_start, age_end) {
  .check_numeric(population, "population", lower = 0)
  .check_ages(age_start, age_end, "age_start, age_end")
  .check_same_length(population = population, age_start = age_start)
  .dl_not_implemented("prl_graduate")
}

#' Age profile of migration from a model schedule
#'
#' Evaluates a model migration schedule (a childhood curve, a labour-force
#' peak and a constant) at the requested ages.
#'
#' **Status: skeleton.** In this development version the function validates
#' its inputs and then signals a condition of class
#' `prl_error_not_implemented`.
#'
#' @param age Numeric vector. Exact ages in years at which the profile is
#'   evaluated.
#' @param params Named numeric vector. Parameters of the schedule. The set of
#'   parameters accepted will be fixed when the function is implemented.
#'
#' @return When implemented, a numeric vector as long as `age`: the relative
#'   intensity of migration at each age (it sums to 1 over the ages given).
#'
#' @section GBD equivalent:
#' **Adapts** the role that an age pattern of migration plays as prior
#' information in the population model of the Global Burden of Disease (GBD)
#' study. **Differs** in using an explicit parametric schedule.
#'
#' @section Method:
#' A reduced form of the model migration schedule of Rogers and Castro
#' (1981). Which components are kept, and their parametrisation, will be
#' fixed and documented when the function is implemented.
#'
#' @references
#' Rogers A, Castro LJ (1981). *Model Migration Schedules*. Research Report
#' RR-81-30. Laxenburg: International Institute for Applied Systems Analysis.
#'
#' @examples
#' # Skeleton: valid inputs reach the "not implemented" condition.
#' tryCatch(
#'   prl_rogers_castro(age = 0:94, params = c(a1 = 0.02, alpha1 = 0.1)),
#'   prl_error_not_implemented = function(e) conditionMessage(e)
#' )
#'
#' @family population
#' @export
prl_rogers_castro <- function(age, params) {
  .check_numeric(age, "age", lower = 0)
  .check_numeric(params, "params")
  if (is.null(names(params)) || any(!nzchar(names(params)))) {
    .dl_abort("`params` must be a named numeric vector.")
  }
  .dl_not_implemented("prl_rogers_castro")
}

#' Fit a model migration schedule to proportions by age group
#'
#' Finds the parameters of the model migration schedule that best reproduce
#' the observed distribution of migrants by age group.
#'
#' **Status: skeleton.** In this development version the function validates
#' its inputs and then signals a condition of class
#' `prl_error_not_implemented`.
#'
#' @param proportions Numeric vector. Share of migrants in each age group
#'   (proportion; it must add up to 1).
#' @param age_start,age_end Numeric vectors. Exact start and end of each age
#'   group in years (contiguous, from 0, last group open).
#'
#' @return When implemented, a list with `params` (fitted parameters, as
#'   taken by [prl_rogers_castro()]), `fitted` (fitted share by age group)
#'   and `converged` (logical).
#'
#' @section GBD equivalent:
#' **Adapts**; see [prl_rogers_castro()].
#'
#' @section Method:
#' Least squares between observed and fitted shares by age group. The
#' objective and the optimiser will be fixed and documented when the
#' function is implemented.
#'
#' @references
#' Rogers A, Castro LJ (1981). *Model Migration Schedules*. Research Report
#' RR-81-30. Laxenburg: International Institute for Applied Systems Analysis.
#'
#' @examples
#' groups <- age_groups("five_year")
#' share <- rep(1 / nrow(groups), nrow(groups))
#' # Skeleton: valid inputs reach the "not implemented" condition.
#' tryCatch(
#'   prl_fit_rogers_castro(share, groups$age_start, groups$age_end),
#'   prl_error_not_implemented = function(e) conditionMessage(e)
#' )
#'
#' @family population
#' @export
prl_fit_rogers_castro <- function(proportions, age_start, age_end) {
  .check_numeric(proportions, "proportions", lower = 0, upper = 1)
  .check_ages(age_start, age_end, "age_start, age_end")
  .check_same_length(proportions = proportions, age_start = age_start)
  if (abs(sum(proportions) - 1) > 1e-6) {
    .dl_abort(c("`proportions` must add up to 1; they add up to ",
                format(sum(proportions)), "."))
  }
  .dl_not_implemented("prl_fit_rogers_castro")
}
