test_that("ddl_combine returns the median after dropping the largest and the smallest", {
  expect_identical(ddl_combine(c(0.78, 0.84, 0.81)), 0.81)
  expect_identical(ddl_combine(c(0.70, 0.78, 0.81, 0.84, 0.99)), 0.81)
  expect_equal(ddl_combine(c(0.70, 0.78, 0.82, 0.99)), 0.80)
  # order does not matter; only one largest and one smallest are removed
  expect_identical(ddl_combine(c(0.99, 0.81, 0.70, 0.84, 0.78)), 0.81)
  expect_identical(ddl_combine(c(0.5, 0.5, 0.9, 0.9, 0.9)), 0.9)
  # with three methods it is their median
  set.seed(1)
  v <- stats::runif(3)
  expect_identical(ddl_combine(v), stats::median(v))
})

test_that("ddl_combine treats failed methods as missing and never guesses", {
  expect_identical(ddl_combine(c(0.70, NA, 0.81, 0.84, 0.99)), mean(c(0.81, 0.84)))
  expect_true(is.na(ddl_combine(c(0.8, 0.9))))
  expect_true(is.na(ddl_combine(c(0.8, NA, NA))))
  m <- rbind(c(0.78, 0.84, 0.81), c(0.75, NA, 0.80), c(0.6, 0.7, 0.65))
  expect_identical(ddl_combine(m), c(0.81, NA, 0.65))
})

test_that("ddl_combine rejects bad input with a classed error", {
  expect_error(ddl_combine("a"), class = "ddl_error_input")
  expect_error(ddl_combine(numeric()), class = "ddl_error_input")
  expect_error(ddl_combine(c(0.8, 0.9, 0.7), rule = "mean"), class = "ddl_error_input")
})

test_that("ddl_cap sets completeness to 1 from the threshold and leaves the rest", {
  cap <- function(...) suppressWarnings(ddl_cap(...))
  expect_identical(cap(c(0.62, 0.949, 0.95, 1.08, NA)), c(0.62, 0.949, 1, 1, NA))
  expect_identical(cap(0.9, threshold = 0.9), 1)
  expect_identical(cap(0.8999, threshold = 0.9), 0.8999)
  m <- matrix(c(0.5, 0.96, 0.94, 1.2), nrow = 2)
  expect_identical(cap(m), matrix(c(0.5, 1, 0.94, 1), nrow = 2))
  # idempotent, never lowers a value below the threshold, never exceeds 1 above it
  x <- seq(0, 1.5, by = 0.01)
  expect_identical(cap(cap(x)), cap(x))
  expect_true(all(cap(x)[x < 0.95] == x[x < 0.95]))
  expect_true(all(cap(x)[x >= 0.95] == 1))
})

test_that("ddl_cap warns where the threshold decides the result, and nothing else", {
  # the rule is a jump: values just below the threshold are the ones that the
  # choice of threshold moves from "inflate the deaths" to "leave them alone"
  expect_warning(ddl_cap(c(0.62, 0.90, 0.9499)), class = "ddl_warning_threshold")
  expect_warning(ddl_cap(c(0.62, 0.90, 0.9499)), class = "demolite_warning_threshold")
  expect_warning(ddl_cap(c(0.62, 0.90, 0.9499)), "jump, not a")
  expect_warning(ddl_cap(c(0.62, 0.90, 0.9499)), "2 value")
  # no value in [0.90, 0.95): no warning
  expect_silent(ddl_cap(c(0.62, 0.8999, 0.95, 1.08, NA)))
  expect_silent(ddl_cap(matrix(c(0.5, 0.96, 0.89, 1.2), nrow = 2)))
  # the warning does not change what is returned
  loud <- withCallingHandlers(
    ddl_cap(c(0.62, 0.90, 0.9499, 0.95)),
    warning = function(w) invokeRestart("muffleWarning")
  )
  expect_identical(loud, c(0.62, 0.90, 0.9499, 1))
  # the window follows the threshold
  expect_silent(ddl_cap(0.93, threshold = 0.99))
  expect_warning(ddl_cap(0.95, threshold = 0.99), class = "ddl_warning_threshold")
})

test_that("ddl_cap rejects bad input with a classed error", {
  expect_error(ddl_cap("a"), class = "ddl_error_input")
  expect_error(ddl_cap(-0.1), class = "ddl_error_input")
  expect_error(ddl_cap(0.5, rule = "other"), class = "ddl_error_input")
  expect_error(ddl_cap(0.5, threshold = 1.5), class = "ddl_error_input")
})

test_that("ddl_estimate and ddl_draws validate and then say they are skeletons", {
  expect_skeleton(ddl_estimate(sim_ddl$cells, sim_ddl$interval))
  expect_skeleton(ddl_draws(sim_ddl$cells, sim_ddl$interval, n_draws = 10, seed = 1,
                            level_error = 0.02, age_error = 0.01))
  expect_error(ddl_estimate(sim_ddl$cells[, 1:4], 10), class = "ddl_error_input")
  expect_error(ddl_estimate(sim_ddl$cells, 10, methods = "magic"), class = "ddl_error_input")
  expect_error(ddl_estimate(sim_ddl$cells, 10, migration = "maybe"), class = "ddl_error_input")
  expect_error(ddl_draws(sim_ddl$cells, 10, n_draws = 10, level_error = 0.02,
                         age_error = 0.01), class = "ddl_error_input")
  expect_error(ddl_draws(sim_ddl$cells, 10, n_draws = 0, seed = 1, level_error = 0.02,
                         age_error = 0.01), class = "ddl_error_input")
  expect_error(ddl_draws(sim_ddl$cells, 10, n_draws = 10, seed = 1),
               class = "ddl_error_input")
})
