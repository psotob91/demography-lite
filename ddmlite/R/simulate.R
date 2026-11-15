#' Simulated inputs for the death distribution methods
#'
#' Runs [ddl_country()] and arranges its output as the inputs of the
#' death distribution methods: two censuses, mean annual registered deaths and
#' mean annual net migrants by five-year age group, with the completeness that
#' was planted.
#'
#' **Status: implemented** as a thin wrapper of [ddl_country()].
#'
#' @param seed Whole number. The caller's seed; the package never chooses one.
#' @param completeness Number in `(0, 1]`. Proportion of deaths registered,
#'   planted at all ages and in all areas.
#' @param census_coverage Numeric vector of length two. Proportion of the true
#'   population enumerated by each census.
#' @param net_rate Number. Annual net international migrants as a proportion
#'   of the population at the start of the period; `0` gives a closed
#'   population.
#' @param stochastic Logical. If `FALSE` (default), registered deaths are
#'   exactly true deaths times completeness, which isolates the bias of a
#'   method; if `TRUE`, deaths are random counts.
#' @param location_id,sex_id Whole numbers. Location and sex returned as
#'   vectors. With the default single area, locations 1 (national) and 2 (the
#'   area) are identical.
#' @param ... Other arguments passed to [ddl_country()], for instance
#'   `n_areas` or `census_dates`.
#'
#' @return A list with
#'   * `pop1`, `pop2`: enumerated population at each census by five-year age
#'     group (0-4, ..., 90-94, 95 and over), persons;
#'   * `deaths`: mean annual registered deaths between the censuses, same
#'     groups, count per year;
#'   * `net_migrants`: mean annual net migrants by age at migration, same
#'     groups, persons per year;
#'   * `interval`: years between the censuses;
#'   * `age_start`: start of each age group, years;
#'   * `cells`: a data.table with the same quantities for every location and
#'     sex (`location_id`, `sex_id`, `age_start`, `age_end`, `pop1`, `pop2`,
#'     `deaths`, `net_migrants`), the layout that [ddl_estimate()] takes;
#'   * `truth`: a list with the planted `completeness`, `census_coverage` and
#'     `net_rate`;
#'   * `country`: the full output of [ddl_country()].
#'
#' @section GBD equivalent:
#' **Own.** See [ddl_country()].
#'
#' @section Method:
#' The country is simulated by cohort-component accounting (see
#' [ddl_country()]), which is a different rule from the one used by the
#' methods under test. Single years of age are added up to five-year groups;
#' deaths and migrants of the intercensal period are divided by its length.
#'
#' @examples
#' sim <- ddl_simulate(seed = 1, completeness = 0.8)
#' round(sim$deaths)
#' sim$truth$completeness
#'
#' @family simulation
#' @export
ddl_simulate <- function(seed, completeness = 0.8, census_coverage = c(1, 1),
                         net_rate = 0, stochastic = FALSE, location_id = 2,
                         sex_id = 1, ...) {
  seed <- .check_seed(seed)
  .check_number(completeness, "completeness", lower = 0.01, upper = 1)
  .check_number(net_rate, "net_rate", lower = -0.05, upper = 0.05)
  location_id <- .check_count(location_id, "location_id")
  sex_id <- .check_count(sex_id, "sex_id", min = 1, max = 2)
  args <- list(...)
  reserved <- intersect(names(args), c("completeness_u5_ratio",
                                       "international_migration_rate",
                                       "monthly", "open_age"))
  if (length(reserved)) {
    .dl_abort(c("`...` cannot set ", paste0("`", reserved, "`", collapse = ", "),
                ": `ddl_simulate()` fixes it."))
  }
  if (is.null(args$n_areas)) {
    args$n_areas <- 1
  }
  if (is.null(args$internal_migration_rate)) {
    args$internal_migration_rate <- 0
  }
  country <- do.call(simulate_country, c(
    list(seed = seed, completeness = completeness,
         census_coverage = census_coverage,
         international_migration_rate = net_rate, stochastic = stochastic,
         open_age = 95),
    args
  ))
  if (!location_id %in% country$locations$location_id) {
    .dl_abort(c("`location_id` must be one of ",
                paste(country$locations$location_id, collapse = ", "), "."))
  }
  years <- country$truth$interval
  cen <- country$census
  ic <- country$intercensal
  group <- function(x) pmin(x %/% 5, 19) * 5
  key <- function(d) list(location_id = d$location_id, sex_id = d$sex_id,
                          age_start = group(d$age_start))
  add <- function(d, col) {
    stats::aggregate(d[[col]], by = key(d), FUN = sum)
  }
  c1 <- cen[cen$census_id == 1L, ]
  c2 <- cen[cen$census_id == 2L, ]
  cells <- add(c1, "population_enumerated")
  names(cells)[4L] <- "pop1"
  cells$pop2 <- add(c2, "population_enumerated")$x
  cells$deaths <- add(ic, "deaths_registered")$x / years
  cells$net_migrants <- add(ic, "net_migrants")$x / years
  cells$age_end <- ifelse(cells$age_start >= 95, Inf, cells$age_start + 5)
  cells <- data.table::as.data.table(cells)
  data.table::setcolorder(cells, c("location_id", "sex_id", "age_start",
                                   "age_end", "pop1", "pop2", "deaths",
                                   "net_migrants"))
  data.table::setorderv(cells, c("location_id", "sex_id", "age_start"))
  # computed outside `[` so that the arguments are not mistaken for columns
  keep <- cells[["location_id"]] == location_id & cells[["sex_id"]] == sex_id
  one <- cells[keep, ]
  list(
    pop1 = one$pop1, pop2 = one$pop2, deaths = one$deaths,
    net_migrants = one$net_migrants, interval = years,
    age_start = one$age_start, cells = cells,
    truth = list(completeness = completeness,
                 census_coverage = census_coverage, net_rate = net_rate),
    country = country
  )
}
