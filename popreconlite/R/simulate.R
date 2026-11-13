#' Simulated country for population reconstruction
#'
#' Returns the simulated country of the family, in which the population at
#' two censuses, the deaths, the births and the net migration are known.
#'
#' **Status: implemented** as a direct call to [simulate_country()]. A layout
#' specific to the reconstruction functions will be added when they are
#' implemented.
#'
#' @param seed Whole number. The caller's seed; the package never chooses one.
#' @param ... Other arguments passed to [simulate_country()].
#'
#' @return A `demolite_country` object; see [simulate_country()] for its
#'   tables.
#'
#' @section GBD equivalent:
#' **Own.** See [simulate_country()].
#'
#' @section Method:
#' See [simulate_country()]: cohort-component accounting on a monthly grid,
#' with internal migration that adds up to zero over areas.
#'
#' @examples
#' sim <- prl_simulate(seed = 1, n_areas = 2, stochastic = FALSE)
#' sim$truth$net_migration_rate
#'
#' @family simulation
#' @export
prl_simulate <- function(seed, ...) {
  seed <- .check_seed(seed)
  simulate_country(seed = seed, ...)
}
