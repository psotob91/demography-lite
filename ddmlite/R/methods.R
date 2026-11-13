#' Completeness of death registration by general growth balance
#'
#' Estimates the proportion of deaths at ages 5 and over that was registered
#' between two censuses, by comparing, age by age, the entry rate into each
#' open-ended age segment with its growth rate and its registered death rate.
#'
#' **Status: skeleton.** In this development version the function validates
#' its inputs and then signals a condition of class
#' `ddl_error_not_implemented`.
#'
#' @param pop1,pop2 Numeric vectors. Population enumerated at the first and
#'   second census by five-year age group (0-4, 5-9, ..., open group),
#'   persons.
#' @param deaths Numeric vector. **Mean annual** registered deaths in the
#'   interval, same age groups, count per year.
#' @param interval Number. Years between the two censuses (decimal).
#' @param net_migrants Numeric vector or `NULL`. Mean annual net migrants by
#'   age at migration, same age groups, persons per year. `NULL` assumes a
#'   closed population.
#' @param age_trim Numeric vector of length two or `NULL`. First and last age
#'   of the fitted range, in years. `NULL` uses the method's default, which
#'   will be fixed and documented when the method is implemented.
#'
#' @return When implemented, a list with `completeness` (proportion, not
#'   capped at 1), `census_ratio` (coverage of the first census relative to
#'   the second), `slope`, `intercept` and `points` (a table with one row per
#'   age: `age`, `x`, `y`, `in_trim`).
#'
#' @section GBD equivalent:
#' **Replicates** the general growth balance method that the Global Burden of
#' Disease (GBD) study applies to vital registration (GBD 2023 demographics
#' appendix, section 2.2.8 and Table C, pp. 17-18). **Differs** in that net
#' migration can be supplied; the GBD study runs the method without it.
#'
#' @section Method:
#' For each age \eqn{a}:
#' \deqn{b(a) - r(a+) + m(a+) = k + \frac{1}{c}\, d(a+)}
#' with \eqn{b} the entry rate at age \eqn{a}, \eqn{r} the growth rate of the
#' population aged \eqn{a} and over, \eqn{m} its net migration rate, \eqn{d}
#' its registered death rate, \eqn{c} the completeness and \eqn{k} a constant
#' that captures the relative coverage of the two censuses. Assumptions:
#' completeness is the same at all fitted ages; census coverage has no age
#' pattern; without `net_migrants`, the population is closed.
#'
#' @references
#' Hill K (1987). Estimating census and death registration completeness.
#' *Asian and Pacific Population Forum*, 1(3), 8-13, 23-24.
#'
#' Hill K, You D, Choi Y (2009). Death distribution methods for estimating
#' adult mortality: sensitivity analysis with simulated data errors.
#' *Demographic Research*, 21, 235-254. \doi{10.4054/DemRes.2009.21.9}
#'
#' Hill K, Queiroz B (2010). Adjusting the general growth balance method for
#' migration. *Revista Brasileira de Estudos de Populacao*, 27, 7-20.
#' \doi{10.1590/S0102-30982010000100002}
#'
#' @examples
#' sim <- ddl_simulate(seed = 1, completeness = 0.8)
#' # Skeleton: valid inputs reach the "not implemented" condition.
#' tryCatch(
#'   ddl_ggb(sim$pop1, sim$pop2, sim$deaths, sim$interval, sim$net_migrants),
#'   ddl_error_not_implemented = function(e) conditionMessage(e)
#' )
#'
#' @family completeness
#' @export
ddl_ggb <- function(pop1, pop2, deaths, interval, net_migrants = NULL,
                    age_trim = NULL) {
  .ddl_check_inputs(pop1, pop2, deaths, interval, net_migrants, age_trim)
  .dl_not_implemented("ddl_ggb")
}

#' Completeness of death registration by synthetic extinct generations
#'
#' Estimates the proportion of deaths that was registered between two censuses
#' by rebuilding, from registered deaths and age-specific growth rates, the
#' population that should have been alive at each age, and comparing it with
#' the enumerated population.
#'
#' **Status: skeleton.** In this development version the function validates
#' its inputs and then signals a condition of class
#' `ddl_error_not_implemented`.
#'
#' @inheritParams ddl_ggb
#' @param e_open_factor Number. Multiplier of the life expectancy at the start
#'   of the open age group used to close the calculation; `1` (default) means
#'   no adjustment.
#'
#' @return When implemented, a list with `completeness` (proportion, not
#'   capped at 1), `e_open` (life expectancy at the open age, years) and
#'   `points` (a table with one row per age: `age`, `ratio`, `in_trim`).
#'
#' @section GBD equivalent:
#' **Replicates** the synthetic extinct generations method that the Global
#' Burden of Disease (GBD) study applies to vital registration (GBD 2023
#' demographics appendix, section 2.2.8 and Table C, pp. 17-18). **Differs**
#' in that net migration can be supplied.
#'
#' @section Method:
#' The population aged \eqn{a} is estimated from the deaths registered above
#' that age, inflated by the age-specific growth rates net of migration, and
#' divided by the enumerated population; completeness is the mean of those
#' ratios over the fitted ages. The open group is closed with the life
#' expectancy at its start (Horiuchi and Coale, 1982). Assumptions:
#' completeness constant over the fitted ages; both censuses equally
#' complete. The full recurrence will be written here when the method is
#' implemented.
#'
#' @references
#' Bennett NG, Horiuchi S (1981). Estimating the completeness of death
#' registration in a closed population. *Population Index*, 47, 207-221.
#' \doi{10.2307/2736447}
#'
#' Bennett NG, Horiuchi S (1984). Mortality estimation from registered deaths
#' in less developed countries. *Demography*, 21, 217-233.
#' \doi{10.2307/2061041}
#'
#' Horiuchi S, Coale AJ (1982). A simple equation for estimating the
#' expectation of life at old ages. *Population Studies*, 36, 317-326.
#' \doi{10.1080/00324728.1982.10409034}
#'
#' @examples
#' sim <- ddl_simulate(seed = 1, completeness = 0.8)
#' # Skeleton: valid inputs reach the "not implemented" condition.
#' tryCatch(
#'   ddl_seg(sim$pop1, sim$pop2, sim$deaths, sim$interval, sim$net_migrants),
#'   ddl_error_not_implemented = function(e) conditionMessage(e)
#' )
#'
#' @family completeness
#' @export
ddl_seg <- function(pop1, pop2, deaths, interval, net_migrants = NULL,
                    age_trim = NULL, e_open_factor = 1) {
  .ddl_check_inputs(pop1, pop2, deaths, interval, net_migrants, age_trim)
  .check_number(e_open_factor, "e_open_factor", lower = 0.1, upper = 10)
  .dl_not_implemented("ddl_seg")
}

#' Completeness of death registration by the combined method
#'
#' Applies the general growth balance to bring the first census to the
#' coverage of the second, and then estimates completeness with synthetic
#' extinct generations.
#'
#' **Status: skeleton.** In this development version the function validates
#' its inputs and then signals a condition of class
#' `ddl_error_not_implemented`.
#'
#' @inheritParams ddl_seg
#'
#' @return When implemented, a list with `completeness` (proportion, not
#'   capped at 1), `census_ratio` (coverage of the first census relative to
#'   the second, from the first step) and `e_open` (years).
#'
#' @section GBD equivalent:
#' **Replicates** the combined method that the Global Burden of Disease (GBD)
#' study applies to vital registration (GBD 2023 demographics appendix,
#' section 2.2.8 and Table C, pp. 17-18). **Differs** in that net migration
#' can be supplied.
#'
#' @section Method:
#' Step 1: [ddl_ggb()] gives the relative coverage of the two censuses.
#' Step 2: the first census is divided by that ratio. Step 3: [ddl_seg()] is
#' applied to the adjusted censuses. Assumptions: those of both methods,
#' except equal census coverage.
#'
#' @references
#' Hill K, You D, Choi Y (2009). Death distribution methods for estimating
#' adult mortality: sensitivity analysis with simulated data errors.
#' *Demographic Research*, 21, 235-254. \doi{10.4054/DemRes.2009.21.9}
#'
#' @examples
#' sim <- ddl_simulate(seed = 1, completeness = 0.8)
#' # Skeleton: valid inputs reach the "not implemented" condition.
#' tryCatch(
#'   ddl_ggbseg(sim$pop1, sim$pop2, sim$deaths, sim$interval, sim$net_migrants),
#'   ddl_error_not_implemented = function(e) conditionMessage(e)
#' )
#'
#' @family completeness
#' @export
ddl_ggbseg <- function(pop1, pop2, deaths, interval, net_migrants = NULL,
                       age_trim = NULL, e_open_factor = 1) {
  .ddl_check_inputs(pop1, pop2, deaths, interval, net_migrants, age_trim)
  .check_number(e_open_factor, "e_open_factor", lower = 0.1, upper = 10)
  .dl_not_implemented("ddl_ggbseg")
}

#' Completeness of death registration by the balance of old cohorts
#'
#' Follows the cohorts above a minimum age from the first census to the
#' second: the deaths they should have had are the people enumerated at the
#' first census, minus the survivors at the second, plus their net migrants.
#' Completeness is registered deaths of those cohorts over that number.
#'
#' **Status: skeleton.** In this development version the function validates
#' its inputs and then signals a condition of class
#' `ddl_error_not_implemented`.
#'
#' @param pop1,pop2 Numeric vectors. Population enumerated at the first and
#'   second census by single year of age (0, 1, ..., open group), persons.
#' @param deaths Numeric vector. Registered deaths of the whole interval by
#'   single year of age, same ages, count.
#' @param interval Number. Years between the two censuses (decimal).
#' @param cohort_migrants Number. Net migrants of the followed cohorts over
#'   the whole interval, persons; `0` assumes that they are closed.
#' @param min_age Number. Age at the first census from which cohorts are
#'   followed, in years.
#'
#' @return When implemented, a list with `completeness` (proportion, not
#'   capped at 1), `deaths_registered` and `deaths_expected` (counts for the
#'   followed cohorts).
#'
#' @section GBD equivalent:
#' **Own.** It is a direct accounting check used alongside the death
#' distribution methods; the Global Burden of Disease (GBD) study does not
#' describe an equivalent.
#'
#' @section Method:
#' \deqn{c = \frac{D^{reg}}{N_1(a_0+) - N_2((a_0 + t)+) + M}}
#' with \eqn{N_1} and \eqn{N_2} the two censuses, \eqn{a_0} the minimum age,
#' \eqn{t} the interval, \eqn{M} the net migrants of the cohorts and
#' \eqn{D^{reg}} their registered deaths. Assumptions: both censuses equally
#' complete at old ages; ages reported without systematic error.
#'
#' @examples
#' sim <- simulate_country(seed = 1, n_areas = 1, stochastic = FALSE)
#' one <- sim$intercensal[sim$intercensal$location_id == 2 &
#'                          sim$intercensal$sex_id == 1, ]
#' cen <- sim$census[sim$census$location_id == 2 & sim$census$sex_id == 1, ]
#' # Skeleton: valid inputs reach the "not implemented" condition.
#' tryCatch(
#'   ddl_cohort_balance(
#'     pop1 = cen$population[cen$census_id == 1],
#'     pop2 = cen$population[cen$census_id == 2],
#'     deaths = one$deaths_registered,
#'     interval = sim$truth$interval,
#'     min_age = 60
#'   ),
#'   ddl_error_not_implemented = function(e) conditionMessage(e)
#' )
#'
#' @family completeness
#' @export
ddl_cohort_balance <- function(pop1, pop2, deaths, interval, cohort_migrants = 0,
                               min_age) {
  .check_numeric(pop1, "pop1", lower = 0)
  .check_numeric(pop2, "pop2", lower = 0)
  .check_numeric(deaths, "deaths", lower = 0)
  .check_same_length(pop1 = pop1, pop2 = pop2, deaths = deaths)
  .check_number(interval, "interval", lower = 0.5, upper = 50)
  .check_number(cohort_migrants, "cohort_migrants")
  if (missing(min_age)) {
    .dl_abort("`min_age` must be supplied.")
  }
  .check_number(min_age, "min_age", lower = 0, upper = 120)
  .dl_not_implemented("ddl_cohort_balance")
}
