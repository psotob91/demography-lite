test_that("ddl_interval validates and then says it is a skeleton", {
  expect_skeleton(ddl_interval(2000.5, 2010.5))
  expect_skeleton(ddl_interval(as.Date("2000-07-01"), as.Date("2010-07-01"), 2009))
  expect_error(ddl_interval(2010.5, 2000.5), class = "ddl_error_input")
  expect_error(ddl_interval("2000", 2010), class = "ddl_error_input")
  expect_error(ddl_interval(2000, 2010, last_death_year = 2009.5), class = "ddl_error_input")
})

test_that("dates are converted to decimal years with the length of their year", {
  expect_identical(.ddl_decimal_date(2010.5, "x"), 2010.5)
  expect_identical(.ddl_decimal_date(as.Date("2010-01-01"), "x"), 2010)
  # 2 July is day 182 after 1 January in a common year and day 183 in a leap year
  expect_equal(.ddl_decimal_date(as.Date("2011-07-02"), "x"), 2011 + 182 / 365)
  expect_equal(.ddl_decimal_date(as.Date("2012-07-02"), "x"), 2012.5)
})

test_that("ddl_deaths_in_interval validates its inputs", {
  d <- data.frame(year_id = 2001L, month = 1L, age_start = 0, age_end = 1, deaths = 3)
  expect_error(ddl_deaths_in_interval(d, interval = 10), class = "ddl_error_input")
  expect_error(ddl_deaths_in_interval(d[, 1:3], interval = 10), class = "ddl_error_input")
  fake <- structure(list(), class = "ddl_interval")
  expect_skeleton(ddl_deaths_in_interval(d, fake))
})

test_that("ddl_child_completeness validates and then says it is a skeleton", {
  cells <- data.frame(location_id = 2:3, sex_id = 3L)
  reg <- cbind(cells, deaths = c(310, 560))
  births <- cbind(cells, births = c(20000, 40000))
  survey <- cbind(cells, q5 = c(0.021, 0.019), q5_se = c(0.002, 0.002))
  expect_skeleton(ddl_child_completeness(reg, births, survey, n_draws = 10, seed = 1))
  expect_error(ddl_child_completeness(reg, births, survey[, 1:3], 10, 1),
               class = "ddl_error_input")
  bad <- survey
  bad$q5 <- c(1.2, 0.02)
  expect_error(ddl_child_completeness(reg, births, bad, 10, 1), class = "ddl_error_input")
  expect_error(ddl_child_completeness(reg, births, survey, 10), class = "ddl_error_input")
})

test_that("ddl_smooth_series and ddl_area_mismatch validate and then say they are skeletons", {
  est <- data.frame(location_id = 2L, year_start = c(1990.5, 2000.5),
                    year_end = c(2000.5, 2010.5), completeness = c(0.71, 0.80))
  expect_skeleton(ddl_smooth_series(est))
  expect_skeleton(ddl_smooth_series(est, breaks = 2005))
  expect_error(ddl_smooth_series(est[, 1:2]), class = "ddl_error_input")
  areas <- draws_new(data.frame(location_id = 2:3), matrix(1:4 * 1, 2), measure = "deaths")
  nat <- draws_new(data.frame(location_id = 1L), matrix(c(3, 7), 1), measure = "deaths")
  expect_skeleton(ddl_area_mismatch(areas, nat))
  expect_error(ddl_area_mismatch(areas, list()), class = "ddl_error_input")
  nat3 <- draws_new(data.frame(location_id = 1L), matrix(1:3 * 1, 1), measure = "deaths")
  expect_error(ddl_area_mismatch(areas, nat3), class = "ddl_error_input")
})
