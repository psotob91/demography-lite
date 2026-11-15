# ---
# repo: demography-lite
# file: standalone-simulate-country.R
# last-updated: 2026-11-14
# license: MIT (see the LICENSE file at the repository root)
# imports: [data.table, stats]
# dependencies: [standalone-checks.R, standalone-draws.R, standalone-age-groups.R]
# ---
#
# Single-source file. The copies inside the packages are named
# `R/import-standalone-simulate-country.R` and must stay byte-identical to
# `standalone/standalone-simulate-country.R`. Do not edit a copy: edit the
# source and run `Rscript tools/sync-standalone.R`.
#
# Contents: a small, fully invented country with planted truth. Nothing in
# this file comes from real data: every rate is an analytic function with
# made-up parameters.
#
# `simulate_country()` is not exported: each package of the family exports a
# thin wrapper under its own prefix (see standalone/prefixed-shared.R).

#' Simulate a small country with planted truth
#'
#' Generates an invented country (a few areas, two sexes) by cohort-component
#' accounting on a monthly grid: population by age and sex at two censuses and
#' at mid-year, births, net migration, true deaths, a death registration
#' completeness planted in each area, and the deaths that end up registered.
#' Because the truth is known, the output is the test bed for every method of
#' the family. No number, name or structure is taken from any real country.
#'
#' Shared infrastructure, not exported: every package of the family exports a
#' thin wrapper of it under its own prefix, `<prefix>_country()`.
#'
#' @param seed Whole number. The caller's seed; it drives the planted
#'   parameters that are left as `NULL` and the random counts. The function
#'   never chooses a seed and leaves the caller's random stream untouched.
#' @param n_areas Whole number. Number of areas (1 to 50). The national total
#'   is added as one more location.
#' @param census_dates Numeric vector of length two. Dates of the two censuses
#'   in decimal years; each must fall on a month boundary (year + m/12) and
#'   they must be 1 to 30 years apart. The years themselves are arbitrary.
#' @param births_per_year Numeric vector (one value per area, or one value for
#'   all) or `NULL`. Births per year, both sexes, when the simulated period
#'   starts. `NULL` uses `2000 * (1:n_areas)`.
#' @param completeness Numeric vector (one value per area, or one value for
#'   all) or `NULL`. Proportion of deaths at ages 5 and over that is
#'   registered. `NULL` draws one value per area from a uniform distribution
#'   on `completeness_range`, using the seed.
#' @param completeness_range Numeric vector of length two. Range of the
#'   planted completeness when `completeness` is `NULL`.
#' @param completeness_u5_ratio Number in `(0, 1]`. Completeness under age 5
#'   relative to completeness at ages 5 and over. Values below 1 create a
#'   scenario in which completeness varies with age.
#' @param census_coverage Numeric vector of length two. Proportion of the true
#'   population enumerated by each census (1 = perfect).
#' @param mortality_level Numeric vector (one value per area, or one value for
#'   all) or `NULL`. Multiplier of the standard force of mortality. `NULL`
#'   draws one value per area between 0.8 and 1.25, using the seed.
#' @param internal_migration_rate Numeric vector (one value per area, or one
#'   value for all) or `NULL`. Annual net internal migrants as a proportion of
#'   the population at the start of the period. Counts are re-centred so that
#'   internal migration sums to zero over areas for each sex and age. `NULL`
#'   draws one value per area between -0.004 and 0.004, using the seed.
#' @param international_migration_rate Number. Annual net international
#'   migrants as a proportion of the population at the start of the period,
#'   applied to every area. Zero (default) gives a closed country.
#' @param stochastic Logical. If `TRUE` (default), true deaths are Poisson
#'   counts around their expected value and registered deaths are binomial
#'   counts given the true deaths. If `FALSE`, true deaths equal the expected
#'   values and registered deaths are exactly true deaths times completeness.
#' @param monthly Logical. If `TRUE`, the table of deaths has one row per
#'   month (column `month`); otherwise one row per year.
#' @param open_age Whole number. Start of the open age group, 95 by default.
#'
#' @return An object of class `demolite_country`: a list of data.tables that
#'   share the columns `location_id` (integer; 1 is the national total, 2 and
#'   up are the areas), `sex_id` (1 male, 2 female), and `age_start` and
#'   `age_end` (exact age in years).
#'   * `locations`: `location_id`, `parent_id`, `level`, `location_name`.
#'   * `census`: `census_id` (1 or 2), `census_date`, `population` (true,
#'     persons) and `population_enumerated` (true times coverage), by single
#'     year of age.
#'   * `population`: true mid-year `population` by `year_id` and single year
#'     of age.
#'   * `births`: `births` by `year_id` and sex.
#'   * `deaths`: `deaths_expected`, `deaths_true` and `deaths_registered` by
#'     `year_id` (and `month` if requested), with the six GBD groups under 5,
#'     single years from 5 and an open group; `age_group_id` as in
#'     [age_groups()].
#'   * `migration`: `net_migrants` by `year_id` and single year of age at
#'     migration.
#'   * `intercensal`: `deaths_expected`, `deaths_true`, `deaths_registered` and
#'     `net_migrants` between the two census dates, by single year of age.
#'   * `truth`: a list with the planted values: `completeness` (by location,
#'     with `completeness` for ages 5 and over and `completeness_u5` under 5;
#'     the national values are death-weighted means), `mortality_level`,
#'     `net_migration_rate` (by location and sex, after re-centring),
#'     `census_coverage`, `interval` (years between censuses) and
#'     `intercensal_births`.
#'   * `meta`: the seed and the arguments of the call.
#'
#'   All national rows are exact sums of the area rows.
#'
#' @section GBD equivalent:
#' **Own.** The Global Burden of Disease (GBD) study does not publish a
#' simulator with planted truth. The age groups of the death table follow the
#' GBD 2023 demographics appendix (see [age_groups()]).
#'
#' @section Method:
#' Each area and sex is projected on a grid of one-month age cells and
#' one-month steps (ages 0 to 110, last cell absorbing). In every step, half
#' of the net migrants are added, deaths are removed with the probability
#' \eqn{1 - \exp\{-\lambda\,[H(x + 1/12) - H(x)]\}}, the other half of the
#' migrants is added, cohorts age by one cell and the births of the step
#' enter at exact age 0. \eqn{H} is the cumulative hazard of a four-term
#' analytic force of mortality (two decaying exponentials for infancy, a
#' constant and an exponential for old age) with made-up parameters by sex,
#' and \eqn{\lambda} is the area's mortality level. Births are exogenous: an
#' exponential path whose growth rate fell linearly from 2.5% to 0.5% a year
#' over the 110 years before the period and stays at 0.5%, with 1.05 male
#' births per female birth. Net migration follows a fixed age profile (a
#' childhood curve plus a labour-force peak, after Rogers and Castro, 1981)
#' up to age 80. The population at the start is the one implied by that
#' history without migration.
#'
#' The accounting identity \eqn{P_2 = P_1 + B - D + M} holds exactly for the
#' expected values. Deaths are attributed to the age cell occupied at the
#' start of each step, so age at death is blurred by up to one month; deaths
#' in the first month of life are split into 0-6 days, 7-27 days and the rest
#' with the analytic hazard. With `stochastic = TRUE`, counts are drawn under
#' per-cell seeds (see [cell_seed()]) so that an area's counts do not depend
#' on the other areas.
#'
#' Assumptions that the generator can violate on purpose, to measure how a
#' method degrades: a closed population (`international_migration_rate`,
#' `internal_migration_rate`), completeness constant over age
#' (`completeness_u5_ratio`) and perfect censuses (`census_coverage`).
#' Completeness that changes over time is not simulated yet.
#'
#' @references
#' Rogers A, Castro LJ (1981). *Model Migration Schedules*. Research Report
#' RR-81-30. Laxenburg: International Institute for Applied Systems Analysis.
#'
#' @family simulation
#' @keywords internal
simulate_country <- function(seed, n_areas = 3, census_dates = c(2000.5, 2010.5),
                             births_per_year = NULL, completeness = NULL,
                             completeness_range = c(0.6, 0.95),
                             completeness_u5_ratio = 1,
                             census_coverage = c(1, 1), mortality_level = NULL,
                             internal_migration_rate = NULL,
                             international_migration_rate = 0,
                             stochastic = TRUE, monthly = FALSE, open_age = 95) {
  seed <- .check_seed(seed)
  n_areas <- .check_count(n_areas, "n_areas", min = 1, max = 50)
  .check_numeric(census_dates, "census_dates", len = 2L, lower = 0)
  .check_numeric(completeness_range, "completeness_range", len = 2L,
                 lower = 0.01, upper = 1)
  .check_number(completeness_u5_ratio, "completeness_u5_ratio", lower = 0.01,
                upper = 1)
  .check_numeric(census_coverage, "census_coverage", len = 2L, lower = 0.01,
                 upper = 2)
  .check_number(international_migration_rate, "international_migration_rate",
                lower = -0.05, upper = 0.05)
  .check_flag(stochastic, "stochastic")
  .check_flag(monthly, "monthly")
  open_age <- .check_count(open_age, "open_age", min = 10, max = 105)

  first_year <- floor(census_dates[1L])
  step_cen <- (census_dates - first_year) * 12
  span <- census_dates[2L] - census_dates[1L]
  if (any(abs(step_cen - round(step_cen)) > 1e-6) || span < 1 || span > 30) {
    .dl_abort(c("`census_dates` must fall on month boundaries (year + m/12) ",
                "and be 1 to 30 years apart."))
  }
  step_cen <- as.integer(round(step_cen))
  n_year <- as.integer(ceiling(census_dates[2L] - 1e-9)) - as.integer(first_year)
  years <- as.integer(first_year) + seq_len(n_year) - 1L
  area_id <- 1L + seq_len(n_areas)

  births0 <- .dl_sim_param(births_per_year, n_areas, "births_per_year",
                           2000 * seq_len(n_areas), lower = 1, upper = 1e7)
  comp <- .dl_sim_param(completeness, n_areas, "completeness",
                        .dl_sim_draw(seed, "sim_completeness", area_id,
                                     completeness_range[1L], completeness_range[2L]),
                        lower = 0.01, upper = 1)
  level <- .dl_sim_param(mortality_level, n_areas, "mortality_level",
                         .dl_sim_draw(seed, "sim_mortality_level", area_id, 0.8, 1.25),
                         lower = 0.2, upper = 5)
  mig_int <- .dl_sim_param(internal_migration_rate, n_areas,
                           "internal_migration_rate",
                           .dl_sim_draw(seed, "sim_internal_migration", area_id,
                                        -0.004, 0.004),
                           lower = -0.05, upper = 0.05)

  grid <- .dl_sim_grid(open_age)
  # Population at the start of the period, by area and sex, without migration.
  start <- lapply(seq_len(n_areas), function(a) {
    lapply(1:2, function(s) .dl_sim_start(births0[a], s, level[a], grid))
  })
  total0 <- vapply(start, function(z) vapply(z, sum, 0), c(0, 0))
  total0 <- matrix(total0, nrow = 2L)
  # Annual net migrants: internal counts re-centred to sum to zero over areas.
  count_int <- sweep(total0, 2L, mig_int, `*`)
  count_int <- count_int - total0 * (rowSums(count_int) / rowSums(total0))
  count_mig <- count_int + total0 * international_migration_rate

  parts <- vector("list", n_areas)
  for (a in seq_len(n_areas)) {
    parts[[a]] <- vector("list", 2L)
    for (s in 1:2) {
      p <- .dl_sim_project(start[[a]][[s]], births0[a], s, level[a],
                           count_mig[s, a], grid, n_year, step_cen)
      comp_fine <- ifelse(grid$fine$age_end <= 5, comp[a] * completeness_u5_ratio,
                          comp[a])
      if (stochastic) {
        sd <- cell_seed(seed, "sim_deaths",
                        list(location_id = area_id[a], sex_id = s))
        counts <- with_cell_seed(sd, {
          true <- stats::rpois(length(p$deaths), p$deaths)
          reg <- stats::rbinom(length(true), true,
                               rep(comp_fine, times = ncol(p$deaths)))
          list(true = true, reg = reg)
        })
        p$deaths_true <- matrix(as.numeric(counts$true), nrow = nrow(p$deaths))
        p$deaths_reg <- matrix(as.numeric(counts$reg), nrow = nrow(p$deaths))
      } else {
        p$deaths_true <- p$deaths
        p$deaths_reg <- p$deaths * comp_fine
      }
      p$deaths_reg_expected <- p$deaths * comp_fine
      parts[[a]][[s]] <- p
    }
  }
  # National total: exact sums of the areas, sex by sex.
  national <- lapply(1:2, function(s) {
    Reduce(function(x, y) Map(`+`, x, y), lapply(parts, `[[`, s))
  })

  loc_ids <- c(1L, area_id)
  all_parts <- c(list(national), parts)
  tabs <- lapply(seq_along(loc_ids), function(i) {
    lapply(1:2, function(s) {
      .dl_sim_tables(all_parts[[i]][[s]], loc_ids[i], s, grid, years,
                     census_dates, census_coverage, step_cen, monthly)
    })
  })
  tabs <- unlist(tabs, recursive = FALSE)
  bind <- function(name) data.table::rbindlist(lapply(tabs, `[[`, name))

  adult <- grid$fine$age_start >= 5
  dead <- vapply(all_parts, function(z) {
    sum(z[[1L]]$deaths[adult, ]) + sum(z[[2L]]$deaths[adult, ])
  }, 0)
  dead_reg <- vapply(all_parts, function(z) {
    sum(z[[1L]]$deaths_reg_expected[adult, ]) + sum(z[[2L]]$deaths_reg_expected[adult, ])
  }, 0)
  comp_all <- dead_reg / dead
  child <- !adult
  comp_u5 <- vapply(all_parts, function(z) {
    (sum(z[[1L]]$deaths_reg_expected[child, ]) + sum(z[[2L]]$deaths_reg_expected[child, ])) /
      (sum(z[[1L]]$deaths[child, ]) + sum(z[[2L]]$deaths[child, ]))
  }, 0)
  ib <- vapply(all_parts, function(z) {
    k <- seq(step_cen[1L] + 1L, step_cen[2L])
    c(sum(z[[1L]]$births[k]), sum(z[[2L]]$births[k]))
  }, c(0, 0))
  rate_all <- cbind(rowSums(count_mig) / rowSums(total0), count_mig / total0)

  structure(
    list(
      locations = data.table::data.table(
        location_id = loc_ids,
        parent_id = c(NA_integer_, rep(1L, n_areas)),
        level = c(0L, rep(1L, n_areas)),
        location_name = c("Simulated country", paste("Area", seq_len(n_areas)))
      ),
      census = bind("census"),
      population = bind("population"),
      births = bind("births"),
      deaths = bind("deaths"),
      migration = bind("migration"),
      intercensal = bind("intercensal"),
      truth = list(
        completeness = data.table::data.table(
          location_id = loc_ids,
          completeness = comp_all,
          completeness_u5 = comp_u5
        ),
        mortality_level = data.table::data.table(
          location_id = area_id, mortality_level = level
        ),
        net_migration_rate = data.table::data.table(
          location_id = rep(loc_ids, each = 2L),
          sex_id = rep(1:2, times = length(loc_ids)),
          net_migration_rate = as.vector(rate_all)
        ),
        census_coverage = census_coverage,
        interval = span,
        intercensal_births = data.table::data.table(
          location_id = rep(loc_ids, each = 2L),
          sex_id = rep(1:2, times = length(loc_ids)),
          births = as.vector(ib)
        )
      ),
      meta = list(
        seed = seed, n_areas = n_areas, census_dates = census_dates,
        stochastic = stochastic, monthly = monthly, open_age = open_age,
        version = .dl_contract_version
      )
    ),
    class = "demolite_country"
  )
}

#' Print a simulated country
#'
#' @param x A `demolite_country` object.
#' @param ... Ignored.
#'
#' @return `x`, invisibly.
#' @noRd
#' @exportS3Method
print.demolite_country <- function(x, ...) {
  tables <- c("locations", "census", "population", "births", "deaths",
              "migration", "intercensal")
  cat("<demolite_country> ", x$meta$n_areas, " area(s) plus the national ",
      "total; censuses at ", format(x$meta$census_dates[1L]), " and ",
      format(x$meta$census_dates[2L]), "; seed ", x$meta$seed,
      if (x$meta$stochastic) "; random counts" else "; expected values", "\n",
      sep = "")
  rows <- unlist(lapply(x[tables], nrow), use.names = FALSE)
  cat(paste0("  $", format(tables, width = 12), rows, " rows\n"), sep = "")
  cat("  $truth       planted completeness, mortality level, migration, ",
      "coverage\n", sep = "")
  invisible(x)
}

# One and the same object in the three packages: see the comment next to
# `print.demolite_draws()` in standalone-draws.R. The method must also stay
# free of `for`, `while` and `repeat`: R caches the compiled form of a loop
# inside the syntax tree the first time the function runs, which would make a
# copy that has already printed something differ from one that has not, and R
# would then report an overwrite when the second package registers its copy.
environment(print.demolite_country) <- baseenv()

#' Resolve a per-area parameter of the simulator
#'
#' @param x Numeric vector of length 1 or `n`, or `NULL`.
#' @param n Whole number. Number of areas.
#' @param arg String. Argument name used in the message.
#' @param default Numeric vector of length `n` used when `x` is `NULL`.
#' @param lower,upper Numbers. Inclusive bounds.
#'
#' @return Numeric vector of length `n`.
#' @noRd
.dl_sim_param <- function(x, n, arg, default, lower, upper) {
  if (is.null(x)) {
    return(default)
  }
  .check_numeric(x, arg, lower = lower, upper = upper)
  if (!length(x) %in% c(1L, n)) {
    .dl_abort(c("`", arg, "` must have one value, or one value per area (", n,
                "); it has ", length(x), "."))
  }
  rep_len(as.numeric(x), n)
}

#' Draw one planted parameter per area under per-cell seeds
#'
#' @param seed Whole number. The caller's seed.
#' @param stream String. Name of the random stream.
#' @param area_id Integer vector of location identifiers.
#' @param lower,upper Numbers. Limits of the uniform distribution.
#'
#' @return Numeric vector, one value per area; the value of an area depends
#'   only on the seed, the stream and its identifier.
#' @noRd
.dl_sim_draw <- function(seed, stream, area_id, lower, upper) {
  states <- cell_seed(seed, stream, list(location_id = area_id))
  vapply(seq_len(nrow(states)),
         function(i) with_cell_seed(states[i, ], stats::runif(1L, lower, upper)),
         0)
}

#' Cumulative hazard of the invented standard mortality schedule
#'
#' @param x Numeric vector. Exact age in years.
#' @param sex_id 1 (male) or 2 (female).
#'
#' @return Numeric vector, cumulative hazard from birth to age `x`.
#' @noRd
.dl_sim_cumhaz <- function(x, sex_id) {
  male <- sex_id == 1L
  a0 <- if (male) 0.42 else 0.35
  a1 <- if (male) 0.0144 else 0.012
  a2 <- if (male) 0.00048 else 0.0003
  a3 <- if (male) 0.0000375 else 0.000025
  a0 / 60 * (1 - exp(-60 * x)) + a1 / 3 * (1 - exp(-3 * x)) + a2 * x +
    a3 / 0.096 * (exp(0.096 * x) - 1)
}

#' Age grids of the simulator
#'
#' @param open_age Whole number. Start of the open age group.
#'
#' @return A list with the monthly cells (`n_cell`, `age`), the fine age
#'   groups used to realise deaths (`fine`: four groups under 1, single years,
#'   open group), the output schemes (`gbd`, `single`) and the maps between
#'   them (`cell_fine`, `cell_single`, `fine_gbd`, `fine_single`), plus the
#'   normalised age profile of migration by cell (`profile`).
#' @noRd
.dl_sim_grid <- function(open_age) {
  n_cell <- 110L * 12L
  month <- seq_len(n_cell) - 1L
  year <- month %/% 12L
  gbd <- age_groups("gbd_u5", open_age)
  single <- age_groups("single", open_age)
  fine <- rbind(gbd[1:4, ], single[-1L, ])
  cell_fine <- ifelse(month < 6L, 3L, ifelse(month < 12L, 4L,
                                             4L + pmin(year, open_age)))
  x <- (month + 0.5) / 12
  profile <- 0.02 * exp(-0.1 * x) +
    0.06 * exp(-0.1 * (x - 20) - exp(-0.4 * (x - 20)))
  profile[x >= 80] <- 0
  n_fine <- nrow(fine)
  list(
    n_cell = n_cell, age = month / 12, open_age = open_age,
    fine = fine, gbd = gbd, single = single,
    cell_fine = cell_fine,
    cell_single = pmin(year, open_age) + 1L,
    fine_gbd = c(1:4, 5L, 6L, 6L, 6L, seq(7L, length.out = n_fine - 8L)),
    fine_single = c(1L, 1L, 1L, 1L, seq(2L, length.out = n_fine - 4L)),
    profile = profile / sum(profile)
  )
}

#' Births per monthly step of the simulator
#'
#' @param births0 Number. Births per year, both sexes, at the start.
#' @param sex_id 1 (male) or 2 (female).
#' @param t Numeric vector. Mid-point of each step in years from the start of
#'   the period (negative before it).
#'
#' @return Numeric vector of births of that sex in each step.
#' @noRd
.dl_sim_births <- function(births0, sex_id, t) {
  share <- if (sex_id == 1L) 1.05 / 2.05 else 1 / 2.05
  back <- pmax(-t, 0)
  growth <- ifelse(t >= 0, 0.005 * t,
                   -(0.005 * back + 0.02 * back^2 / (2 * 110)))
  births0 * share / 12 * exp(growth)
}

#' Population by monthly age cell at the start of the simulated period
#'
#' @param births0 Number. Births per year, both sexes, at the start.
#' @param sex_id 1 (male) or 2 (female).
#' @param level Number. Mortality level of the area.
#' @param grid List returned by `.dl_sim_grid()`.
#'
#' @return Numeric vector with one value per monthly age cell.
#' @noRd
.dl_sim_start <- function(births0, sex_id, level, grid) {
  born <- .dl_sim_births(births0, sex_id, -(grid$age + 0.5 / 12))
  born * exp(-level * .dl_sim_cumhaz(grid$age, sex_id))
}

#' Project one area and sex over the simulated period
#'
#' @param n0 Numeric vector. Population by monthly age cell at the start.
#' @param births0 Number. Births per year, both sexes, at the start.
#' @param sex_id 1 (male) or 2 (female).
#' @param level Number. Mortality level of the area.
#' @param migrants Number. Annual net migrants of this area and sex.
#' @param grid List returned by `.dl_sim_grid()`.
#' @param n_year Whole number. Calendar years simulated.
#' @param step_cen Integer vector of length two. Steps elapsed at each census.
#'
#' @return A list of numeric arrays with expected values: `census` (single
#'   ages x 2), `midyear` (single ages x years), `births` (per step), `deaths`
#'   (fine age groups x steps) and `migrants` (single ages x steps).
#' @noRd
.dl_sim_project <- function(n0, births0, sex_id, level, migrants, grid, n_year,
                            step_cen) {
  n_cell <- grid$n_cell
  n_step <- n_year * 12L
  h <- 1 / 12
  cum <- .dl_sim_cumhaz(c(grid$age, grid$age[n_cell] + h), sex_id)
  q <- 1 - exp(-level * diff(cum))
  # Split of the deaths of the first month of life: 0-6 days, 7-27 days, rest.
  s0 <- exp(-level * .dl_sim_cumhaz(c(0, 7, 28) / 365.25, sex_id))
  w <- -diff(c(s0, 1 - q[1L])) / q[1L]
  births <- .dl_sim_births(births0, sex_id, (seq_len(n_step) - 0.5) * h)
  g <- migrants * grid$profile * h
  n_single <- nrow(grid$single)
  census <- matrix(0, n_single, 2L)
  midyear <- matrix(0, n_single, n_year)
  d_cell <- matrix(0, n_cell, n_step)
  snap <- function(n) as.vector(rowsum(n, grid$cell_single))
  n <- n0
  lowest <- min(n)
  for (j in which(step_cen == 0L)) {
    census[, j] <- snap(n)
  }
  for (k in seq_len(n_step)) {
    n <- n + g / 2
    d <- n * q
    n <- n - d + g / 2
    d_cell[, k] <- d
    lowest <- min(lowest, n)
    last <- n[n_cell]
    n <- c(births[k], n[-n_cell])
    n[n_cell] <- n[n_cell] + last
    if (k %% 12L == 6L) {
      midyear[, (k + 6L) %/% 12L] <- snap(n)
    }
    for (j in which(step_cen == k)) {
      census[, j] <- snap(n)
    }
  }
  if (lowest < 0) {
    .dl_abort(c("Net out-migration empties an age group; use migration rates ",
                "closer to zero."))
  }
  deaths <- rowsum(d_cell[-1L, , drop = FALSE], grid$cell_fine[-1L])
  deaths <- rbind(w[1L] * d_cell[1L, ], w[2L] * d_cell[1L, ], deaths)
  deaths[3L, ] <- deaths[3L, ] + w[3L] * d_cell[1L, ]
  dimnames(deaths) <- NULL
  mig_single <- as.vector(rowsum(g, grid$cell_single))
  list(
    census = census, midyear = midyear, births = births, deaths = deaths,
    migrants = matrix(mig_single, n_single, n_step)
  )
}

#' Tables of one location and sex of the simulated country
#'
#' @param p List of arrays returned by `.dl_sim_project()`, with the realised
#'   counts added (`deaths_true`, `deaths_reg`).
#' @param location_id,sex_id Whole numbers identifying the location and sex.
#' @param grid List returned by `.dl_sim_grid()`.
#' @param years Integer vector of calendar years.
#' @param census_dates,census_coverage Numeric vectors of length two.
#' @param step_cen Integer vector of length two. Steps elapsed at each census.
#' @param monthly Logical. Whether deaths are returned by month.
#'
#' @return A named list of data.tables: `census`, `population`, `births`,
#'   `deaths`, `migration` and `intercensal`.
#' @noRd
.dl_sim_tables <- function(p, location_id, sex_id, grid, years, census_dates,
                           census_coverage, step_cen, monthly) {
  n_year <- length(years)
  n_step <- n_year * 12L
  step_year <- rep(seq_len(n_year), each = 12L)
  by_year <- function(m) t(rowsum(t(m), step_year))
  ns <- nrow(grid$single)
  ng <- nrow(grid$gbd)

  census <- data.table::data.table(
    location_id = location_id,
    census_id = rep(1:2, each = ns),
    census_date = rep(census_dates, each = ns),
    sex_id = sex_id,
    age_start = rep(grid$single$age_start, 2L),
    age_end = rep(grid$single$age_end, 2L),
    population = as.vector(p$census),
    population_enumerated = as.vector(sweep(p$census, 2L, census_coverage, `*`))
  )
  population <- data.table::data.table(
    location_id = location_id,
    year_id = rep(years, each = ns),
    sex_id = sex_id,
    age_start = rep(grid$single$age_start, n_year),
    age_end = rep(grid$single$age_end, n_year),
    population = as.vector(p$midyear)
  )
  births <- data.table::data.table(
    location_id = location_id, year_id = years, sex_id = sex_id,
    births = as.vector(rowsum(p$births, step_year))
  )
  to_gbd <- function(m) rowsum(m, grid$fine_gbd)
  shape <- if (monthly) identity else by_year
  n_per <- if (monthly) n_step else n_year
  deaths <- data.table::data.table(
    location_id = location_id,
    year_id = rep(if (monthly) years[step_year] else years, each = ng)
  )
  if (monthly) {
    data.table::set(deaths, j = "month",
                    value = rep(rep(1:12, times = n_year), each = ng))
  }
  data.table::set(deaths, j = "sex_id", value = sex_id)
  data.table::set(deaths, j = "age_start", value = rep(grid$gbd$age_start, n_per))
  data.table::set(deaths, j = "age_end", value = rep(grid$gbd$age_end, n_per))
  data.table::set(deaths, j = "age_group_id",
                  value = rep(grid$gbd$age_group_id, n_per))
  data.table::set(deaths, j = "deaths_expected",
                  value = as.vector(shape(to_gbd(p$deaths))))
  data.table::set(deaths, j = "deaths_true",
                  value = as.vector(shape(to_gbd(p$deaths_true))))
  data.table::set(deaths, j = "deaths_registered",
                  value = as.vector(shape(to_gbd(p$deaths_reg))))
  migration <- data.table::data.table(
    location_id = location_id,
    year_id = rep(years, each = ns),
    sex_id = sex_id,
    age_start = rep(grid$single$age_start, n_year),
    age_end = rep(grid$single$age_end, n_year),
    net_migrants = as.vector(by_year(p$migrants))
  )
  k <- seq(step_cen[1L] + 1L, step_cen[2L])
  between <- function(m) as.vector(rowsum(rowSums(m[, k, drop = FALSE]),
                                          grid$fine_single))
  intercensal <- data.table::data.table(
    location_id = location_id, sex_id = sex_id,
    age_start = grid$single$age_start, age_end = grid$single$age_end,
    deaths_expected = between(p$deaths),
    deaths_true = between(p$deaths_true),
    deaths_registered = between(p$deaths_reg),
    net_migrants = rowSums(p$migrants[, k, drop = FALSE])
  )
  list(census = census, population = population, births = births,
       deaths = deaths, migration = migration, intercensal = intercensal)
}
