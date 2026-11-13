test_that("prl_interval and prl_deaths_by_cohort validate their inputs", {
  expect_skeleton(prl_interval(2000.5, 2010.5))
  expect_skeleton(prl_interval(2000.5, 2010.5, last_year = 2012))
  expect_error(prl_interval(2010.5, 2000.5), class = "prl_error_input")
  expect_error(prl_interval("a", 2010), class = "prl_error_input")
  deaths <- sim_prl$deaths
  deaths$deaths <- deaths$deaths_registered
  fake <- structure(list(), class = "prl_interval")
  expect_error(prl_deaths_by_cohort(deaths, 10), class = "prl_error_input")
  expect_error(prl_deaths_by_cohort(sim_prl$deaths, fake), class = "prl_error_input")
  expect_skeleton(prl_deaths_by_cohort(deaths, fake))
})

test_that("the age-profile functions validate their inputs", {
  g <- age_groups("five_year")
  pop <- rev(seq(100, 2000, length.out = nrow(g)))
  expect_skeleton(prl_graduate(pop, g$age_start, g$age_end))
  expect_error(prl_graduate(pop[-1], g$age_start, g$age_end), class = "prl_error_input")
  expect_error(prl_graduate(-pop, g$age_start, g$age_end), class = "prl_error_input")
  expect_error(prl_graduate(pop, g$age_start, rev(g$age_end)), class = "prl_error_input")
  expect_skeleton(prl_rogers_castro(0:94, c(a1 = 0.02, alpha1 = 0.1)))
  expect_error(prl_rogers_castro(0:94, c(0.02, 0.1)), class = "prl_error_input")
  expect_error(prl_rogers_castro(-1, c(a1 = 0.02)), class = "prl_error_input")
  share <- rep(1 / nrow(g), nrow(g))
  expect_skeleton(prl_fit_rogers_castro(share, g$age_start, g$age_end))
  expect_error(prl_fit_rogers_castro(share * 2, g$age_start, g$age_end),
               class = "prl_error_input")
})

test_that("projection and births validate their inputs", {
  base <- sim_prl$census[sim_prl$census$location_id == 2 & sim_prl$census$census_id == 1, ]
  expect_skeleton(prl_project(list(deaths = sim_prl$deaths), base))
  expect_error(prl_project(list(), base), class = "prl_error_input")
  expect_error(prl_project(list(1), base), class = "prl_error_input")
  expect_error(prl_project(list(deaths = 1), base[, 1:2]), class = "prl_error_input")
  expect_skeleton(prl_births(c(0.05, 0.11), c(900, 950), sex_ratio = 1.05))
  expect_error(prl_births(c(0.05, 0.11), c(900, 950)), class = "prl_error_input")
  expect_error(prl_births(c(0.05, 0.11), 900, sex_ratio = 1.05), class = "prl_error_input")
  expect_error(prl_births(c(0.05, 0.11), c(900, 950), sex_ratio = 10), class = "prl_error_input")
})

test_that("the fitting functions validate their inputs", {
  expect_skeleton(prl_spec())
  expect_skeleton(prl_spec(estimate = "migration", priors = list()))
  expect_error(prl_spec(estimate = "fertility"), class = "prl_error_input")
  expect_error(prl_spec(priors = 1), class = "prl_error_input")
  spec <- structure(list(), class = "prl_spec")
  fit <- structure(list(), class = "prl_fit")
  inputs <- list(census = sim_prl$census)
  expect_skeleton(prl_fit(inputs, spec))
  expect_error(prl_fit(inputs, list()), class = "prl_error_input")
  expect_skeleton(prl_draw(fit, inputs, draw = 1, seed = 1))
  expect_error(prl_draw(list(), inputs, draw = 1, seed = 1), class = "prl_error_input")
  expect_error(prl_draw(fit, inputs, draw = 0, seed = 1), class = "prl_error_input")
  expect_error(prl_draw(fit, inputs, draw = 1), class = "prl_error_input")
  expect_skeleton(prl_check_linearisation(fit, n = 5, seed = 1))
  expect_error(prl_check_linearisation(fit, n = 5), class = "prl_error_input")
  expect_skeleton(prl_holdout(inputs, spec, holdout = 2010.5))
  expect_error(prl_holdout(inputs, spec, holdout = "2010"), class = "prl_error_input")
})

test_that("prl_rake validates its inputs", {
  areas <- draws_new(data.frame(location_id = 2:3), matrix(1:4 * 1, 2), measure = "population")
  nat <- draws_new(data.frame(location_id = 1L), matrix(c(3, 7), 1), measure = "population")
  expect_skeleton(prl_rake(areas, nat))
  expect_error(prl_rake(areas, 1), class = "prl_error_input")
  nat3 <- draws_new(data.frame(location_id = 1L), matrix(1:3 * 1, 1), measure = "population")
  expect_error(prl_rake(areas, nat3), class = "prl_error_input")
})
