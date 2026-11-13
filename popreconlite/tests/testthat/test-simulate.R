test_that("prl_simulate returns the simulated country of the family", {
  expect_s3_class(sim_prl, "demolite_country")
  expect_identical(sim_prl, simulate_country(seed = 31, n_areas = 2, stochastic = FALSE))
  expect_error(prl_simulate(), class = "prl_error_input")
})

test_that("the planted population closes by cohort components", {
  # truth by construction: P2 = P1 + births - deaths + net migrants
  tot <- function(d, col) tapply(d[[col]], list(d$location_id, d$sex_id), sum)
  cen <- sim_prl$census
  p1 <- tot(cen[cen$census_id == 1, ], "population")
  p2 <- tot(cen[cen$census_id == 2, ], "population")
  dead <- tot(sim_prl$intercensal, "deaths_expected")
  mig <- tot(sim_prl$intercensal, "net_migrants")
  births <- tot(sim_prl$truth$intercensal_births, "births")
  expect_equal(p2, p1 + births - dead + mig, tolerance = 1e-10)
  # internal migration adds up to zero over areas
  expect_lt(max(abs(mig["1", ])), 1e-8)
  expect_gt(max(abs(mig[c("2", "3"), ])), 1)
})

test_that("the list of implemented and skeleton functions is the declared one", {
  exported <- getNamespaceExports("popreconlite")
  own <- sort(grep("^prl_", exported, value = TRUE))
  skeleton <- c("prl_births", "prl_check_linearisation", "prl_deaths_by_cohort",
                "prl_draw", "prl_fit", "prl_fit_rogers_castro", "prl_graduate",
                "prl_holdout", "prl_interval", "prl_project", "prl_rake",
                "prl_rogers_castro", "prl_spec")
  expect_identical(own, sort(c("prl_simulate", skeleton)))
  is_skeleton <- vapply(own, function(f) {
    any(grepl(".dl_not_implemented", deparse(body(get(f))), fixed = TRUE))
  }, logical(1))
  expect_identical(sort(own[is_skeleton]), skeleton)
  expect_identical(.dl_prefix(), "prl")
})
