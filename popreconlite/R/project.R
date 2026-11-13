#' Project a population by cohort components
#'
#' Projects the population of one area from a base census, by single year of
#' age, in annual steps and for both sexes, given deaths, births and net
#' migration, and returns it every year and at the census dates.
#'
#' **Status: skeleton.** In this development version the function validates
#' its inputs and then signals a condition of class
#' `prl_error_not_implemented`.
#'
#' @param inputs Named list with the inputs of one area: the interval (see
#'   [prl_interval()]), deaths by step and cohort (see
#'   [prl_deaths_by_cohort()]), births by sex and net migration by age and
#'   sex. The exact elements will be fixed when the function is implemented.
#' @param base_population Data frame or data.table with the population at the
#'   base census by sex and single year of age: `sex_id`, `age_start`,
#'   `age_end` and `population` (persons).
#' @param migration_deviation Numeric vector or `NULL`. Deviation of net
#'   migration from its prior, one value per parameter being estimated;
#'   `NULL` uses the prior as given.
#'
#' @return When implemented, a list with `annual` (population by `year_id`,
#'   `sex_id` and single year of age at mid-year, persons) and `census`
#'   (population at each census date, same layout).
#'
#' @section GBD equivalent:
#' **Adapts** the cohort-component projection inside the population model of
#' the Global Burden of Disease (GBD) study (GBD 2023 demographics appendix,
#' section 4.4.2, p. 45). **Differs** in that deaths enter as counts, not as
#' survival ratios, and in the single-year, annual-step grid for areas with
#' internal migration.
#'
#' @section Method:
#' Balancing equation by cohort and step:
#' \deqn{P_{x+1,t+1} = P_{x,t} - D_{x,t} + M_{x,t}}
#' with births entering at age 0. Assumptions: deaths and migrants are
#' attributed to cohorts as in [prl_deaths_by_cohort()]. The treatment of the
#' open age group and of partial steps will be documented when the function
#' is implemented.
#'
#' @references
#' Wheldon MC, Raftery AE, Clark SJ, Gerland P (2013). Reconstructing past
#' populations with uncertainty from fragmentary data. *Journal of the
#' American Statistical Association*, 108, 96-110.
#' \doi{10.1080/01621459.2012.737729}
#'
#' Leslie PH (1945). On the use of matrices in certain population
#' mathematics. *Biometrika*, 33, 183-212. \doi{10.1093/biomet/33.3.183}
#'
#' @examples
#' sim <- simulate_country(seed = 1, n_areas = 1, stochastic = FALSE)
#' base <- sim$census[sim$census$location_id == 2 & sim$census$census_id == 1, ]
#' # Skeleton: valid inputs reach the "not implemented" condition.
#' tryCatch(
#'   prl_project(inputs = list(deaths = sim$deaths), base_population = base),
#'   prl_error_not_implemented = function(e) conditionMessage(e)
#' )
#'
#' @family population
#' @export
prl_project <- function(inputs, base_population, migration_deviation = NULL) {
  .prl_check_inputs(inputs)
  .check_table(base_population, "base_population",
               cols = c("sex_id", "age_start", "age_end", "population"))
  .check_numeric(migration_deviation, "migration_deviation", null_ok = TRUE)
  .dl_not_implemented("prl_project")
}

#' Check the list of inputs of an area
#'
#' @param inputs Object to check. A non-empty named list.
#'
#' @return `inputs`, invisibly; otherwise signals a classed input error.
#' @noRd
.prl_check_inputs <- function(inputs) {
  if (!is.list(inputs) || is.data.frame(inputs) || !length(inputs) ||
        is.null(names(inputs)) || any(!nzchar(names(inputs)))) {
    .dl_abort("`inputs` must be a non-empty named list.")
  }
  invisible(inputs)
}

#' Births of a calendar year by sex
#'
#' Computes the live births of a calendar year from age-specific fertility
#' rates and the women exposed, and splits them by sex.
#'
#' **Status: skeleton.** In this development version the function validates
#' its inputs and then signals a condition of class
#' `prl_error_not_implemented`.
#'
#' @param asfr Numeric vector. Age-specific fertility rates, births per woman
#'   per year, one value per age of the mother.
#' @param women Numeric vector. Women exposed during the year at the same
#'   ages, person-years.
#' @param sex_ratio Number. Male births per female birth.
#'
#' @return When implemented, a named numeric vector with the births of the
#'   year by sex (`male`, `female`), count.
#'
#' @section GBD equivalent:
#' **Replicates** the calculation of live births in the Global Burden of
#' Disease (GBD) study: age-specific fertility applied to the female
#' population and split by the sex ratio at birth.
#'
#' @section Method:
#' \deqn{B = \sum_x f_x W_x, \qquad B_m = B\,\frac{s}{1 + s}, \qquad
#' B_f = \frac{B}{1 + s}}
#' with \eqn{f_x} fertility at age \eqn{x}, \eqn{W_x} the women exposed and
#' \eqn{s} the sex ratio at birth. How the women exposed during a calendar
#' year are obtained from mid-year populations will be fixed and documented
#' when the function is implemented.
#'
#' @examples
#' # Skeleton: valid inputs reach the "not implemented" condition.
#' tryCatch(
#'   prl_births(asfr = c(0.05, 0.11, 0.09, 0.04), women = c(900, 950, 870, 800),
#'              sex_ratio = 1.05),
#'   prl_error_not_implemented = function(e) conditionMessage(e)
#' )
#'
#' @family population
#' @export
prl_births <- function(asfr, women, sex_ratio) {
  .check_numeric(asfr, "asfr", lower = 0, upper = 1)
  .check_numeric(women, "women", lower = 0)
  .check_same_length(asfr = asfr, women = women)
  if (missing(sex_ratio)) {
    .dl_abort("`sex_ratio` must be supplied.")
  }
  .check_number(sex_ratio, "sex_ratio", lower = 0.5, upper = 2)
  .dl_not_implemented("prl_births")
}
