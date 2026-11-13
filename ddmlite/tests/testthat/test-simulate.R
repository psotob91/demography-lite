test_that("ddl_simulate returns the planted truth in the layout of the methods", {
  s <- sim_ddl
  expect_named(s, c("pop1", "pop2", "deaths", "net_migrants", "interval", "age_start",
                    "cells", "truth", "country"))
  expect_identical(s$age_start, seq(0, 95, by = 5))
  expect_identical(length(s$pop1), 20L)
  expect_identical(s$interval, 10)
  expect_identical(s$truth$completeness, 0.8)
  # five-year groups add up to the single-year tables of the simulated country
  cen <- s$country$census
  one <- cen[cen$location_id == 2 & cen$sex_id == 1 & cen$census_id == 1, ]
  expect_equal(sum(s$pop1), sum(one$population_enumerated))
  expect_equal(s$pop1[2], sum(one$population_enumerated[one$age_start %in% 5:9]))
  expect_equal(s$pop1[20], one$population_enumerated[one$age_start == 95])
})

test_that("the registered deaths of ddl_simulate carry the planted completeness", {
  s <- sim_ddl
  ic <- s$country$intercensal
  one <- ic[ic$location_id == 2 & ic$sex_id == 1, ]
  true_annual <- sum(one$deaths_expected) / s$interval
  expect_equal(sum(s$deaths) / true_annual, 0.8, tolerance = 1e-12)
  # closed population by default, open when a net rate is given
  expect_equal(s$net_migrants, rep(0, 20))
  open <- ddl_simulate(seed = 21, completeness = 0.8, net_rate = 0.003)
  expect_gt(sum(open$net_migrants), 0)
  # census coverage is applied to the enumerated populations
  cov <- ddl_simulate(seed = 21, completeness = 0.8, census_coverage = c(0.95, 1))
  expect_equal(cov$pop1 / s$pop1, rep(0.95, 20))
  expect_equal(cov$pop2, s$pop2)
})

test_that("ddl_simulate returns every location and sex in `cells`", {
  s <- ddl_simulate(seed = 21, completeness = 0.7, n_areas = 2, location_id = 3, sex_id = 2)
  expect_identical(sort(unique(s$cells$location_id)), 1:3)
  expect_identical(nrow(s$cells), 3L * 2L * 20L)
  pick <- s$cells[s$cells$location_id == 3 & s$cells$sex_id == 2, ]
  expect_identical(pick$pop1, s$pop1)
  nat <- s$cells[s$cells$location_id == 1, ]
  areas <- s$cells[s$cells$location_id > 1, ]
  expect_equal(sum(nat$deaths), sum(areas$deaths))
})

test_that("ddl_simulate rejects bad input with a classed error", {
  expect_error(ddl_simulate(completeness = 0.8), class = "ddl_error_input")
  expect_error(ddl_simulate(seed = 1, completeness = 1.5), class = "ddl_error_input")
  expect_error(ddl_simulate(seed = 1, location_id = 9), class = "ddl_error_input")
  expect_error(ddl_simulate(seed = 1, sex_id = 3), class = "ddl_error_input")
  expect_error(ddl_simulate(seed = 1, monthly = TRUE), class = "ddl_error_input")
})
