test_that("evl_summarise gives the mean and the 2.5 and 97.5 percentiles", {
  cells <- data.frame(location_id = 1:2)
  value <- rbind(seq(90, 110, length.out = 41), seq(40, 60, length.out = 41))
  x <- draws_new(cells, value, measure = "deaths", unit = "count")
  s <- evl_summarise(x)
  expect_named(s, c("location_id", "mean", "lower", "upper"))
  expect_equal(s$mean, c(100, 50))
  expect_equal(s$lower, c(90.5, 40.5))
  expect_equal(s$upper, c(109.5, 59.5))
  expect_identical(s, draws_summarise(x))
  expect_equal(evl_summarise(x, probs = c(0.25, 0.75))$lower, c(95, 45))
  expect_error(evl_summarise(value), class = "evl_error_input")
  expect_error(evl_summarise(x, probs = 0.5), class = "evl_error_input")
})

test_that("the interval of a sum is not the sum of the intervals", {
  # the reason why draws are aggregated first and summarised last
  idx <- data.frame(location_id = 1:2)
  gen <- function(n, cell) stats::rnorm(n, 100, 10)
  x <- draws_generate(idx, 500, seed = 7, stream = "deaths", gen, measure = "deaths")
  total <- draws_new(data.frame(location_id = 0L), matrix(colSums(x$value), nrow = 1),
                     measure = "deaths")
  width_sum <- diff(unlist(evl_summarise(total)[, c("lower", "upper")]))
  parts <- evl_summarise(x)
  sum_width <- sum(parts$upper - parts$lower)
  expect_lt(width_sum, sum_width)
  expect_equal(evl_summarise(total)$mean, sum(parts$mean))
})

test_that("evl_envelope validates and then says it is a skeleton", {
  reg <- sim_evl$deaths
  reg$deaths <- reg$deaths_registered
  comp <- draws_new(data.frame(location_id = 1:3), matrix(0.8, 3, 4),
                    measure = "completeness", unit = "proportion")
  expect_skeleton(evl_envelope(reg, comp))
  expect_error(evl_envelope(reg[, 1:3], comp), class = "evl_error_input")
  expect_error(evl_envelope(reg, list()), class = "evl_error_input")
  wrong <- draws_new(data.frame(location_id = 1:3), matrix(0.8, 3, 4), measure = "deaths")
  expect_error(evl_envelope(reg, wrong), class = "evl_error_input")
  neg <- reg
  neg$deaths[1] <- -1
  expect_error(evl_envelope(neg, comp), class = "evl_error_input")
})

test_that("evl_mx validates and then says it is a skeleton", {
  cells <- data.frame(location_id = 1L, age_start = c(50, 51), age_end = c(51, 52))
  deaths <- draws_new(cells, matrix(c(10, 12, 11, 13), 2), measure = "deaths")
  pop <- draws_new(cells, matrix(c(2000, 1900, 2010, 1890), 2), measure = "population")
  expect_skeleton(evl_mx(deaths, pop))
  expect_error(evl_mx(pop, deaths), class = "evl_error_input")
  pop3 <- draws_new(cells, matrix(1, 2, 3), measure = "population")
  expect_error(evl_mx(deaths, pop3), class = "evl_error_input")
})
