#' Mortality envelope: all-cause deaths by draw
#'
#' Divides registered deaths by the completeness of registration, draw by
#' draw, to obtain the estimated number of deaths from all causes (the
#' "envelope") in each cell.
#'
#' **Status: skeleton.** In this development version the function validates
#' its inputs and then signals a condition of class
#' `evl_error_not_implemented`.
#'
#' @param registered Data frame or data.table with one row per cell:
#'   `location_id`, `year_id`, `sex_id`, `age_start`, `age_end` and `deaths`
#'   (registered deaths, count). Ages under 5 use the six groups of
#'   `evl_age_groups("gbd_u5")`, so that the envelope keeps the four groups under
#'   1 year.
#' @param completeness A `demolite_draws` container (see [evl_draws_new()]) with
#'   the measure `"completeness"` (proportion) by location, year, sex and age
#'   band.
#'
#' @return When implemented, a `demolite_draws` container with the cells of
#'   `registered`, the measure `"deaths"` (count) and as many draws as
#'   `completeness`. Draw `k` of the envelope comes from draw `k` of
#'   completeness.
#'
#' @section GBD equivalent:
#' **Adapts.** The Global Burden of Disease (GBD) study obtains all-cause
#' deaths from its mortality model, in which completeness adjusts the vital
#' registration data. Here the envelope is the direct ratio, appropriate when
#' registration is the only source and every area is observed.
#'
#' @section Method:
#' \deqn{D_k = \frac{D^{reg}}{c_k}}
#' with \eqn{D^{reg}} registered deaths and \eqn{c_k} draw \eqn{k} of
#' completeness for the age band that contains the cell. Registered deaths
#' are treated as fixed. How age bands of completeness are matched to the
#' age groups of deaths will be documented when the function is implemented.
#'
#' @examples
#' sim <- evl_country(seed = 1, n_areas = 1)
#' completeness <- evl_draws_new(
#'   data.frame(location_id = 1:2),
#'   matrix(c(0.80, 0.80, 0.82, 0.82), nrow = 2),
#'   measure = "completeness", unit = "proportion"
#' )
#' registered <- sim$deaths
#' registered$deaths <- registered$deaths_registered
#' # Skeleton: valid inputs reach the "not implemented" condition.
#' tryCatch(
#'   evl_envelope(registered, completeness),
#'   evl_error_not_implemented = function(e) conditionMessage(e)
#' )
#'
#' @family envelope
#' @export
evl_envelope <- function(registered, completeness) {
  .check_table(registered, "registered",
               cols = c("location_id", "year_id", "sex_id", "age_start",
                        "age_end", "deaths"))
  .check_numeric(registered[["deaths"]], "registered$deaths", lower = 0)
  .check_draws(completeness, "completeness", measure = "completeness")
  .dl_not_implemented("evl_envelope")
}

#' Death rates by draw
#'
#' Divides the envelope by the population, pairing draw `k` of one with draw
#' `k` of the other.
#'
#' **Status: skeleton.** In this development version the function validates
#' its inputs and then signals a condition of class
#' `evl_error_not_implemented`.
#'
#' @param envelope A `demolite_draws` container (see [evl_draws_new()]) with the
#'   measure `"deaths"` (count).
#' @param population A `demolite_draws` container with the measure
#'   `"population"` (persons at mid-year, taken as person-years), the same
#'   cells and the same number of draws.
#'
#' @return When implemented, a `demolite_draws` container with the measure
#'   `"mx"` (deaths per person-year) and the cells of `envelope`.
#'
#' @section GBD equivalent:
#' **Replicates** the definition of the age-specific death rate used in the
#' life tables of the Global Burden of Disease (GBD) study: deaths divided by
#' mid-year population.
#'
#' @section Method:
#' \deqn{m_{x,k} = \frac{D_{x,k}}{P_{x,k}}}
#' for draw \eqn{k}. Draws are paired, never shuffled. How the population of
#' age 0 is divided among the four groups under 1 year will be documented
#' when the function is implemented.
#'
#' @examples
#' cells <- data.frame(location_id = 1L, year_id = 2005L, sex_id = 1L,
#'                     age_start = c(50, 51), age_end = c(51, 52))
#' deaths <- evl_draws_new(cells, matrix(c(10, 12, 11, 13), nrow = 2),
#'                     measure = "deaths", unit = "count")
#' pop <- evl_draws_new(cells, matrix(c(2000, 1900, 2010, 1890), nrow = 2),
#'                  measure = "population", unit = "persons")
#' # Skeleton: valid inputs reach the "not implemented" condition.
#' tryCatch(
#'   evl_mx(deaths, pop),
#'   evl_error_not_implemented = function(e) conditionMessage(e)
#' )
#'
#' @family envelope
#' @export
evl_mx <- function(envelope, population) {
  .check_draws(envelope, "envelope", measure = "deaths")
  .check_draws(population, "population", measure = "population")
  .evl_check_paired(envelope, population, "envelope", "population")
  .dl_not_implemented("evl_mx")
}

#' Check that two containers have the same number of draws
#'
#' @param x,y `demolite_draws` containers.
#' @param x_arg,y_arg Strings. Argument names used in the message.
#'
#' @return `TRUE`, invisibly; otherwise signals a classed input error.
#' @noRd
.evl_check_paired <- function(x, y, x_arg, y_arg) {
  if (ncol(x$value) != ncol(y$value)) {
    .dl_abort(c("`", x_arg, "` and `", y_arg, "` must have the same number ",
                "of draws; they have ", ncol(x$value), " and ", ncol(y$value),
                "."))
  }
  invisible(TRUE)
}
