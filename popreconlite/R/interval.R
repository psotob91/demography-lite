#' Define the projection steps between two censuses
#'
#' Describes the period between a base census and a final census as a
#' sequence of annual steps from mid-year to mid-year, with the fractions of a
#' year needed to move the population to and from the census dates.
#'
#' **Status: skeleton.** In this development version the function validates
#' its inputs and then signals a condition of class
#' `prl_error_not_implemented`.
#'
#' @param base_date,end_date Numbers. Reference dates of the base and final
#'   census in decimal years (for instance `2010.5`); the second must be
#'   later than the first.
#' @param last_year Whole number or `NULL`. Last calendar year to project;
#'   `NULL` stops at the year of the final census.
#'
#' @return When implemented, an object of class `prl_interval`: a list with
#'   the two dates, the calendar years of the annual steps and the fraction of
#'   a year between each census date and the nearest mid-year.
#'
#' @section GBD equivalent:
#' **Own.** The Global Burden of Disease (GBD) study models mid-year
#' populations; how census dates are aligned to that grid is not published in
#' detail.
#'
#' @section Method:
#' Steps run from 1 July of one year to 1 July of the next. A census taken at
#' another date is linked to the grid by a partial step whose length is the
#' fraction of a year to the nearest mid-year. The exact convention will be
#' fixed and documented when the function is implemented.
#'
#' @examples
#' # Skeleton: valid inputs reach the "not implemented" condition.
#' tryCatch(
#'   prl_interval(2000.5, 2010.5),
#'   prl_error_not_implemented = function(e) conditionMessage(e)
#' )
#'
#' @family population
#' @export
prl_interval <- function(base_date, end_date, last_year = NULL) {
  .check_number(base_date, "base_date", lower = 0, upper = 9999)
  .check_number(end_date, "end_date", lower = 0, upper = 9999)
  if (end_date <= base_date) {
    .dl_abort("`end_date` must be later than `base_date`.")
  }
  if (!is.null(last_year)) {
    .check_count(last_year, "last_year", min = 0, max = 9999)
  }
  .dl_not_implemented("prl_interval")
}

#' Deaths by projection step and cohort
#'
#' Re-arranges deaths by calendar year and age into deaths by projection step
#' and birth cohort, splitting each age between the two cohorts that cross it
#' during the step.
#'
#' **Status: skeleton.** In this development version the function validates
#' its inputs and then signals a condition of class
#' `prl_error_not_implemented`.
#'
#' @param deaths Data frame or data.table with one row per year and single
#'   year of age: `year_id`, `age_start`, `age_end` and `deaths` (count), plus
#'   optional identifier columns such as `location_id` and `sex_id`.
#' @param interval An object of class `prl_interval` created with
#'   [prl_interval()].
#'
#' @return When implemented, a data.table with one row per projection step
#'   and cohort: the identifier columns, `step`, `cohort_age` (age of the
#'   cohort at the start of the step, years) and `deaths` (count). Totals are
#'   conserved.
#'
#' @section GBD equivalent:
#' **Adapts** the Lexis bookkeeping that any cohort-component projection
#' needs, including the population model of the Global Burden of Disease
#' (GBD) study (GBD 2023 demographics appendix, section 4.4.2, p. 45).
#' **Differs** in the single-year, annual-step grid aligned to mid-year.
#'
#' @section Method:
#' The deaths at age \eqn{x} in a step are divided between the cohort that
#' starts the step aged \eqn{x} and the one that starts it aged \eqn{x - 1}.
#' The share given to each (a separation factor) will be fixed and documented
#' when the function is implemented. Conservation: the output adds up to the
#' input.
#'
#' @examples
#' sim <- prl_country(seed = 1, n_areas = 1)
#' deaths <- sim$deaths
#' deaths$deaths <- deaths$deaths_registered
#' # Skeleton: `interval` objects cannot be created yet, so the call stops at
#' # input validation.
#' tryCatch(
#'   prl_deaths_by_cohort(deaths, interval = 10),
#'   prl_error_input = function(e) conditionMessage(e)
#' )
#'
#' @family population
#' @export
prl_deaths_by_cohort <- function(deaths, interval) {
  .check_table(deaths, "deaths",
               cols = c("year_id", "age_start", "age_end", "deaths"))
  if (!inherits(interval, "prl_interval")) {
    .dl_abort("`interval` must be an object created with `prl_interval()`.")
  }
  .dl_not_implemented("prl_deaths_by_cohort")
}
