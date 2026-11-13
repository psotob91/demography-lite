# Single source: standalone/tests/test-standalone-simulate-country.R (copied to
# each package by tools/sync-standalone.R; do not edit the copies).

exact <- simulate_country(seed = 11, n_areas = 3, stochastic = FALSE,
                          completeness_u5_ratio = 0.8,
                          census_coverage = c(0.97, 1.02))
noisy <- simulate_country(seed = 11, n_areas = 3)

sum_by <- function(d, value, by) {
  out <- stats::aggregate(d[[value]], by = as.list(as.data.frame(d)[by]), FUN = sum)
  names(out)[ncol(out)] <- value
  out[do.call(order, out[by]), ]
}

test_that("the output has the documented tables and age groups", {
  expect_s3_class(noisy, "demolite_country")
  expect_named(noisy, c("locations", "census", "population", "births", "deaths",
                        "migration", "intercensal", "truth", "meta"))
  expect_identical(noisy$locations$location_id, 1:4)
  expect_identical(noisy$locations$level, c(0L, 1L, 1L, 1L))
  one <- noisy$deaths[noisy$deaths$location_id == 2 & noisy$deaths$sex_id == 1 &
                        noisy$deaths$year_id == 2003, ]
  g <- age_groups("gbd_u5")
  expect_equal(one$age_start, g$age_start)
  expect_equal(one$age_end, g$age_end)
  expect_identical(one$age_group_id, g$age_group_id)
  cen <- noisy$census[noisy$census$location_id == 2 & noisy$census$sex_id == 1 &
                        noisy$census$census_id == 1, ]
  expect_equal(cen$age_start, age_groups("single")$age_start)
  expect_identical(sort(unique(noisy$deaths$year_id)), 2000:2010)
  expect_identical(noisy$truth$interval, 10)
  expect_true(all(noisy$census$population > 0))
  expect_true(all(noisy$deaths$deaths_expected > 0))
  expect_output(print(noisy), "demolite_country")
})

test_that("aggregating recovers the planted completeness exactly without noise", {
  d <- exact$deaths
  truth <- exact$truth$completeness
  adult <- d[d$age_start >= 5, ]
  got <- sum_by(adult, "deaths_registered", "location_id")$deaths_registered /
    sum_by(adult, "deaths_true", "location_id")$deaths_true
  expect_equal(got, truth$completeness, tolerance = 1e-12)
  child <- d[d$age_end <= 5, ]
  got5 <- sum_by(child, "deaths_registered", "location_id")$deaths_registered /
    sum_by(child, "deaths_true", "location_id")$deaths_true
  expect_equal(got5, truth$completeness_u5, tolerance = 1e-12)
  # planted values of the areas: inside the requested range, 0.8 under age 5
  areas <- truth[truth$location_id > 1, ]
  expect_true(all(areas$completeness >= 0.6 & areas$completeness <= 0.95))
  expect_equal(areas$completeness_u5, 0.8 * areas$completeness, tolerance = 1e-12)
  # and cell by cell, not only in aggregate
  in_area <- adult[adult$location_id > 1, ]
  expect_equal(in_area$deaths_registered / in_area$deaths_true,
               truth$completeness[match(in_area$location_id, truth$location_id)],
               tolerance = 1e-9)
  expect_identical(exact$deaths$deaths_true, exact$deaths$deaths_expected)
})

test_that("with noise, completeness is recovered within sampling error", {
  d <- noisy$deaths[noisy$deaths$age_start >= 5 & noisy$deaths$location_id > 1, ]
  reg <- sum_by(d, "deaths_registered", "location_id")$deaths_registered
  true <- sum_by(d, "deaths_true", "location_id")$deaths_true
  planted <- noisy$truth$completeness$completeness[-1]
  se <- sqrt(planted * (1 - planted) / true)
  # criterion written before measuring: within 4 binomial standard errors
  expect_true(all(abs(reg / true - planted) < 4 * se))
  # counts are whole numbers and registered deaths never exceed true deaths
  expect_true(all(noisy$deaths$deaths_true == round(noisy$deaths$deaths_true)))
  expect_true(all(noisy$deaths$deaths_registered <= noisy$deaths$deaths_true))
  # true deaths are close to their expectation (Poisson, 5 standard errors)
  tot_true <- sum_by(d, "deaths_true", "location_id")$deaths_true
  tot_exp <- sum_by(d, "deaths_expected", "location_id")$deaths_expected
  expect_true(all(abs(tot_true - tot_exp) < 5 * sqrt(tot_exp)))
})

test_that("national rows are exact sums of the areas in every table", {
  for (sim in list(exact, noisy)) {
    checks <- list(
      list("census", c("census_id", "sex_id", "age_start"),
           c("population", "population_enumerated")),
      list("population", c("year_id", "sex_id", "age_start"), "population"),
      list("births", c("year_id", "sex_id"), "births"),
      list("deaths", c("year_id", "sex_id", "age_start"),
           c("deaths_expected", "deaths_true", "deaths_registered")),
      list("migration", c("year_id", "sex_id", "age_start"), "net_migrants"),
      list("intercensal", c("sex_id", "age_start"),
           c("deaths_expected", "deaths_true", "deaths_registered", "net_migrants"))
    )
    for (ch in checks) {
      tab <- sim[[ch[[1]]]]
      for (value in ch[[3]]) {
        nat <- sum_by(tab[tab$location_id == 1, ], value, ch[[2]])[[value]]
        areas <- sum_by(tab[tab$location_id > 1, ], value, ch[[2]])[[value]]
        expect_equal(areas, nat, tolerance = 1e-10, info = paste(ch[[1]], value))
      }
    }
  }
})

test_that("the accounting identity closes for every location and sex", {
  cen <- sum_by(exact$census, "population", c("location_id", "sex_id", "census_id"))
  p1 <- cen$population[cen$census_id == 1]
  p2 <- cen$population[cen$census_id == 2]
  ic <- exact$intercensal
  dead <- sum_by(ic, "deaths_expected", c("location_id", "sex_id"))$deaths_expected
  mig <- sum_by(ic, "net_migrants", c("location_id", "sex_id"))$net_migrants
  b <- exact$truth$intercensal_births
  b <- b[order(b$location_id, b$sex_id), ]
  expect_equal(p2, p1 + b$births - dead + mig, tolerance = 1e-10)
})

test_that("census coverage and internal migration are planted as requested", {
  cen <- exact$census
  expect_equal(cen$population_enumerated[cen$census_id == 1] /
                 cen$population[cen$census_id == 1], rep(0.97, sum(cen$census_id == 1)))
  expect_equal(cen$population_enumerated[cen$census_id == 2] /
                 cen$population[cen$census_id == 2], rep(1.02, sum(cen$census_id == 2)))
  # internal migration adds up to zero over areas at every age, sex and year
  nat <- exact$migration[exact$migration$location_id == 1, ]
  expect_lt(max(abs(nat$net_migrants)), 1e-8)
  areas <- exact$migration[exact$migration$location_id > 1, ]
  expect_gt(max(abs(areas$net_migrants)), 1)
  rate <- exact$truth$net_migration_rate
  expect_lt(max(abs(rate$net_migration_rate[rate$location_id == 1])), 1e-12)
  # an open country: the national net migration is the requested rate
  open <- simulate_country(seed = 11, n_areas = 2, stochastic = FALSE,
                           international_migration_rate = 0.002)
  r <- open$truth$net_migration_rate
  expect_equal(r$net_migration_rate[r$location_id == 1], c(0.002, 0.002))
})

test_that("the seed makes the country reproducible and nothing else does", {
  set.seed(77)
  before <- get(".Random.seed", envir = globalenv())
  again <- simulate_country(seed = 11, n_areas = 3)
  expect_identical(get(".Random.seed", envir = globalenv()), before)
  expect_identical(again, noisy)
  other <- simulate_country(seed = 12, n_areas = 3)
  expect_false(identical(other$truth$completeness, noisy$truth$completeness))
  expect_false(identical(other$deaths$deaths_true, noisy$deaths$deaths_true))
  expect_error(simulate_country(), class = "demolite_error_input")
})

test_that("an area does not depend on how many other areas are simulated", {
  small <- simulate_country(seed = 11, n_areas = 2, internal_migration_rate = 0)
  large <- simulate_country(seed = 11, n_areas = 4, internal_migration_rate = 0)
  pick <- function(sim) sim$deaths[sim$deaths$location_id == 3, ]
  expect_identical(pick(small), pick(large))
  expect_identical(small$truth$completeness$completeness[3],
                   large$truth$completeness$completeness[3])
})

test_that("monthly deaths add up to yearly and intercensal deaths", {
  monthly <- simulate_country(seed = 11, n_areas = 3, monthly = TRUE)
  expect_identical(nrow(monthly$deaths), 12L * nrow(noisy$deaths))
  key <- c("location_id", "year_id", "sex_id", "age_start")
  for (value in c("deaths_true", "deaths_registered")) {
    expect_equal(sum_by(monthly$deaths, value, key)[[value]],
                 sum_by(noisy$deaths, value, key)[[value]])
  }
  # months between the censuses (July 2000 to June 2010) give the intercensal table
  d <- monthly$deaths
  time <- d$year_id + (d$month - 1) / 12
  inside <- d[time >= 2000.5 - 1e-9 & time < 2010.5 - 1e-9, ]
  expect_equal(sum_by(inside, "deaths_registered", c("location_id", "sex_id"))$deaths_registered,
               sum_by(monthly$intercensal, "deaths_registered",
                      c("location_id", "sex_id"))$deaths_registered)
})

test_that("fixed parameters are honoured and bad input is rejected", {
  sim <- simulate_country(seed = 3, n_areas = 2, completeness = c(0.7, 0.9),
                          mortality_level = 1, births_per_year = 1500,
                          census_dates = c(1990, 1995), stochastic = FALSE)
  expect_equal(sim$truth$completeness$completeness[-1], c(0.7, 0.9))
  expect_identical(sim$truth$mortality_level$mortality_level, c(1, 1))
  expect_identical(sort(unique(sim$deaths$year_id)), 1990:1994)
  expect_identical(sim$truth$interval, 5)
  b <- sim$births[sim$births$year_id == 1990 & sim$births$location_id == 2, ]
  expect_equal(sum(b$births), 1500, tolerance = 0.01)
  cls <- "demolite_error_input"
  expect_error(simulate_country(seed = 1, n_areas = 0), class = cls)
  expect_error(simulate_country(seed = 1, census_dates = c(2000.5, 2000.9)), class = cls)
  expect_error(simulate_country(seed = 1, census_dates = c(2000.03, 2010)), class = cls)
  expect_error(simulate_country(seed = 1, completeness = c(0.5, 0.6)), class = cls)
  expect_error(simulate_country(seed = 1, completeness = 1.2), class = cls)
  expect_error(simulate_country(seed = 1, stochastic = NA), class = cls)
  expect_error(simulate_country(seed = 1.5), class = cls)
  expect_error(simulate_country(seed = 1, n_areas = 2,
                                internal_migration_rate = c(0.05, -0.05),
                                international_migration_rate = -0.05), class = cls)
})
