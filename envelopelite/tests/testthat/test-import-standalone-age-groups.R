# Single source: standalone/tests/test-standalone-age-groups.R (copied to each
# package by tools/sync-standalone.R; do not edit the copies).

test_that("the scheme gbd_u5 has the six groups under 5 with exact limits", {
  g <- age_groups("gbd_u5")
  expect_identical(nrow(g), 97L)
  expect_named(g, c("age_start", "age_end", "age_group_id", "age_group_name"))
  expect_equal(g$age_start[1:7], c(0, 7 / 365.25, 28 / 365.25, 0.5, 1, 2, 5))
  expect_equal(g$age_end[1:6], c(7 / 365.25, 28 / 365.25, 0.5, 1, 2, 5))
  expect_identical(g$age_group_id[1:6], c(2L, 3L, 388L, 389L, 238L, 34L))
  expect_true(all(is.na(g$age_group_id[-(1:6)])))
  expect_identical(g$age_group_name[1:6],
                   c("0-6 days", "7-27 days", "1-5 months", "6-11 months",
                     "12-23 months", "2-4 years"))
  expect_equal(g$age_start[7:96], 5:94)
  expect_identical(g$age_start[97], 95)
  expect_identical(g$age_end[97], Inf)
  expect_identical(g$age_group_name[97], "95+")
  # the groups under 5 cover exactly five years
  expect_equal(sum(g$age_end[1:6] - g$age_start[1:6]), 5)
})

test_that("every scheme is contiguous, starts at 0 and ends open", {
  for (scheme in c("gbd_u5", "single", "five_year", "abridged")) {
    g <- age_groups(scheme)
    expect_identical(g$age_start[1], 0)
    expect_identical(g$age_end[nrow(g)], Inf)
    expect_equal(g$age_start[-1], g$age_end[-nrow(g)])
    expect_true(.check_ages(g$age_start, g$age_end))
  }
  expect_identical(nrow(age_groups("single")), 96L)
  expect_identical(nrow(age_groups("five_year")), 20L)
  expect_identical(nrow(age_groups("abridged")), 21L)
  expect_identical(age_groups("abridged")$age_group_name[1:3], c("0", "1-4", "5-9"))
  expect_identical(nrow(age_groups("five_year", open_age = 85)), 18L)
  expect_identical(age_groups("single", open_age = 100)$age_group_name[101], "100+")
})

test_that("invalid requests fail with a classed error", {
  cls <- "demolite_error_input"
  expect_error(age_groups("decades"), class = cls)
  expect_error(age_groups("five_year", open_age = 93), class = cls)
  expect_error(age_groups("single", open_age = 3), class = cls)
  expect_error(age_groups("single", open_age = 95.5), class = cls)
  expect_error(.check_ages(c(0, 5), c(4, Inf)), class = cls)
  expect_error(.check_ages(c(1, 5), c(5, Inf)), class = cls)
  expect_error(.check_ages(c(0, 5), c(5, 10)), class = cls)
  expect_error(.check_ages(c(0, 5), c(5, NA)), class = cls)
  expect_true(.check_ages(c(1, 5), c(5, 10), from_zero = FALSE, open = FALSE))
})
