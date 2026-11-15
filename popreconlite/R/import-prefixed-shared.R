# ---
# repo: demography-lite
# file: prefixed-shared.R
# last-updated: 2026-11-14
# license: MIT (see the LICENSE file at the repository root)
# imports: none (base R only)
# dependencies: standalone-draws.R, standalone-age-groups.R,
#   standalone-simulate-country.R
# ---
#
# Single-source template. The shared code of the family is internal (nothing
# is exported twice under the same name), so each package exports a thin
# wrapper of it under its own function prefix. The copies inside the packages
# are named `R/import-prefixed-shared.R` and are this file with the token
# `PKGPREFIX` plus an underscore replaced by the prefix of the package
# (`ddl_`, `prl_`, `evl_`).
# Do not edit a copy: edit this file and run `Rscript tools/sync-standalone.R`.
#
# Contents: the smallest set of public names that lets a package be used on
# its own, that is, carry out a whole calculation without reaching into its
# internals: build a container of draws, convert it, summarise it, generate
# draws under per-cell random states, get a table of age groups and get the
# simulated country with planted truth.

#' Create a container of draws
#'
#' Public name in this package of the shared container of draws: a table with
#' one row per cell and a numeric matrix with one row per cell and one column
#' per draw. Containers are interchangeable between the packages of the
#' family.
#'
#' @inheritParams draws_new
#'
#' @inherit draws_new return sections
#'
#' @examples
#' x <- prl_draws_new(
#'   data.frame(location_id = c(1, 2), sex_id = c(1, 1)),
#'   matrix(c(0.80, 0.90, 0.82, 0.88, 0.78, 0.91), nrow = 2),
#'   measure = "completeness", unit = "proportion"
#' )
#' x
#' prl_summarise(x)
#'
#' # The same container from a long table
#' long <- prl_draws_long(x)
#' identical(prl_draws_new(long, measure = "completeness")$value, x$value)
#'
#' @family draws
#' @export
prl_draws_new <- function(index, value = NULL, measure,
                                unit = NA_character_, seed = NA_integer_,
                                stream = NA_character_) {
  draws_new(index, value, measure = measure, unit = unit, seed = seed,
            stream = stream)
}

#' Convert a container of draws to a long table
#'
#' Public name in this package of the long conversion of the shared container:
#' one row per cell and draw.
#'
#' @inheritParams as_draws_long
#'
#' @inherit as_draws_long return sections
#'
#' @examples
#' x <- prl_draws_new(data.frame(location_id = c(1, 2)),
#'                          matrix(1:6 / 10, nrow = 2), measure = "example")
#' prl_draws_long(x)
#'
#' @family draws
#' @export
prl_draws_long <- function(x) {
  as_draws_long(x)
}

#' Convert a container of draws to a wide table
#'
#' Public name in this package of the wide conversion of the shared container:
#' one row per cell and one column per draw, named as the Global Burden of
#' Disease (GBD) study names them.
#'
#' @inheritParams as_draws_wide
#'
#' @inherit as_draws_wide return sections
#'
#' @examples
#' x <- prl_draws_new(data.frame(location_id = c(1, 2)),
#'                          matrix(1:6 / 10, nrow = 2), measure = "example")
#' prl_draws_wide(x)
#'
#' @family draws
#' @export
prl_draws_wide <- function(x) {
  as_draws_wide(x)
}

#' Summarise draws by cell
#'
#' Public name in this package of the summary of the shared container: the
#' mean of the draws and the two percentiles of the uncertainty interval.
#'
#' @inheritParams draws_summarise
#'
#' @inherit draws_summarise return sections references
#'
#' @examples
#' value <- rbind(seq(0.70, 0.90, length.out = 41),
#'                seq(0.50, 0.60, length.out = 41))
#' x <- prl_draws_new(data.frame(location_id = c(1, 2)), value,
#'                          measure = "completeness", unit = "proportion")
#' prl_summarise(x)
#'
#' @family draws
#' @export
prl_summarise <- function(x, probs = c(0.025, 0.975), na_rm = FALSE) {
  draws_summarise(x, probs = probs, na_rm = na_rm)
}

#' Generate draws cell by cell with per-cell random states
#'
#' Public name in this package of the generator of draws: it calls a function
#' of the caller once per cell, each time under a random state derived from
#' the key of that cell, so that draw `k` of a cell does not depend on the
#' order of the cells, on how the work is split across processes, or on which
#' other cells are in the table.
#'
#' @inheritParams draws_generate
#'
#' @inherit draws_generate return sections
#'
#' @examples
#' cells <- data.frame(location_id = 1:3, sd = c(0.1, 0.2, 0.3))
#' gen <- function(n, cell) stats::rnorm(n, mean = 0, sd = cell$sd)
#' a <- prl_draws_generate(cells, 5, seed = 2024, stream = "noise", gen,
#'                               measure = "error", key_cols = "location_id")
#' b <- prl_draws_generate(cells[3:1, ], 5, seed = 2024,
#'                               stream = "noise", gen, measure = "error",
#'                               key_cols = "location_id")
#' identical(a$value, b$value[3:1, ])
#'
#' @family draws
#' @export
prl_draws_generate <- function(index, n_draws, seed, stream, generator,
                                     measure, unit = NA_character_,
                                     key_cols = names(index)) {
  draws_generate(index, n_draws = n_draws, seed = seed, stream = stream,
                 generator = generator, measure = measure, unit = unit,
                 key_cols = key_cols)
}

#' Tables of age groups
#'
#' Public name in this package of the shared tables of age groups, in which
#' every group is identified by its exact start and end in years.
#'
#' @inheritParams age_groups
#'
#' @inherit age_groups return sections references
#'
#' @examples
#' head(prl_age_groups("gbd_u5"), 8)
#' prl_age_groups("five_year", open_age = 85)
#'
#' @family age groups
#' @export
prl_age_groups <- function(scheme = c("gbd_u5", "single", "five_year",
                                            "abridged"),
                                 open_age = 95) {
  age_groups(scheme, open_age = open_age)
}

#' Simulated country with planted truth
#'
#' Public name in this package of the simulated country of the family: an
#' invented country whose completeness of death registration, census coverage
#' and migration are known, built to test the methods against a planted truth.
#'
#' @inheritParams simulate_country
#'
#' @inherit simulate_country return sections references
#'
#' @examples
#' sim <- prl_country(seed = 2024, n_areas = 2)
#' sim
#'
#' # Aggregating recovers the planted completeness (ages 5 and over)
#' d <- sim$deaths[sim$deaths$age_start >= 5, ]
#' observed <- tapply(d$deaths_registered, d$location_id, sum) /
#'   tapply(d$deaths_true, d$location_id, sum)
#' cbind(sim$truth$completeness, observed = as.numeric(observed))
#'
#' @family simulation
#' @export
prl_country <- function(seed, n_areas = 3,
                              census_dates = c(2000.5, 2010.5),
                              births_per_year = NULL, completeness = NULL,
                              completeness_range = c(0.6, 0.95),
                              completeness_u5_ratio = 1,
                              census_coverage = c(1, 1),
                              mortality_level = NULL,
                              internal_migration_rate = NULL,
                              international_migration_rate = 0,
                              stochastic = TRUE, monthly = FALSE,
                              open_age = 95) {
  simulate_country(
    seed = seed, n_areas = n_areas, census_dates = census_dates,
    births_per_year = births_per_year, completeness = completeness,
    completeness_range = completeness_range,
    completeness_u5_ratio = completeness_u5_ratio,
    census_coverage = census_coverage, mortality_level = mortality_level,
    internal_migration_rate = internal_migration_rate,
    international_migration_rate = international_migration_rate,
    stochastic = stochastic, monthly = monthly, open_age = open_age
  )
}
