test_that("the three methods validate their inputs and then say they are skeletons", {
  s <- sim_ddl
  expect_skeleton(ddl_ggb(s$pop1, s$pop2, s$deaths, s$interval, s$net_migrants))
  expect_skeleton(ddl_ggb(s$pop1, s$pop2, s$deaths, s$interval, age_trim = c(5, 65)))
  expect_skeleton(ddl_seg(s$pop1, s$pop2, s$deaths, s$interval, s$net_migrants))
  expect_skeleton(ddl_ggbseg(s$pop1, s$pop2, s$deaths, s$interval, s$net_migrants))
})

test_that("the three methods reject malformed input with a classed error", {
  s <- sim_ddl
  cls <- "ddl_error_input"
  expect_error(ddl_ggb(1:3, 1:4, 1:3, 8), class = cls)
  expect_error(ddl_ggb(s$pop1, s$pop2, s$deaths[-1], s$interval), class = cls)
  expect_error(ddl_ggb(s$pop1, s$pop2, s$deaths, -1), class = cls)
  expect_error(ddl_ggb(s$pop1, -s$pop2, s$deaths, s$interval), class = cls)
  expect_error(ddl_ggb(s$pop1, s$pop2, s$deaths, s$interval, s$net_migrants[-1]), class = cls)
  expect_error(ddl_ggb(s$pop1, s$pop2, s$deaths, s$interval, age_trim = c(65, 5)), class = cls)
  expect_error(ddl_ggb(1:3, 1:3, 1:3, 8), class = cls)
  expect_error(ddl_seg(s$pop1, s$pop2, s$deaths, s$interval, e_open_factor = 0), class = cls)
  expect_error(ddl_ggbseg(s$pop1, s$pop2, c(NA, s$deaths[-1]), s$interval), class = cls)
})

test_that("ddl_cohort_balance validates and then says it is a skeleton", {
  n <- 96
  expect_skeleton(ddl_cohort_balance(rep(100, n), rep(90, n), rep(5, n), 10, min_age = 60))
  expect_error(ddl_cohort_balance(rep(100, n), rep(90, n), rep(5, n), 10),
               class = "ddl_error_input")
  expect_error(ddl_cohort_balance(rep(100, n), rep(90, n - 1), rep(5, n), 10, min_age = 60),
               class = "ddl_error_input")
})
