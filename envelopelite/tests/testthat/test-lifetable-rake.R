test_that("the life-table functions validate and then say they are skeletons", {
  ages <- age_groups("single")
  mx <- 0.0003 + 0.00003 * exp(0.095 * ages$age_start)
  expect_skeleton(evl_lifetable(mx, ages$age_start, ages$age_end))
  expect_skeleton(evl_lifetable(mx, ages$age_start, ages$age_end, infant_detail = TRUE))
  expect_error(evl_lifetable(mx[-1], ages$age_start, ages$age_end), class = "evl_error_input")
  expect_error(evl_lifetable(-mx, ages$age_start, ages$age_end), class = "evl_error_input")
  expect_error(evl_lifetable(mx, ages$age_start, ages$age_end, ax_rule = "other"),
               class = "evl_error_input")
  expect_error(evl_lifetable(mx[-96], ages$age_start[-96], ages$age_end[-96]),
               class = "evl_error_input")
  expect_skeleton(evl_close_open_age(0.28))
  expect_skeleton(evl_close_open_age(0.28, floor = 0.2))
  expect_error(evl_close_open_age(-1), class = "evl_error_input")
  expect_error(evl_close_open_age(0.28, floor = "a"), class = "evl_error_input")
})

test_that("aggregation and abridging validate and then say they are skeletons", {
  lt <- data.frame(location_id = rep(2:3, each = 2), age_start = c(50, 51),
                   age_end = c(51, 52), mx = c(0.004, 0.005, 0.006, 0.007), ax = 0.5)
  pop <- data.frame(location_id = rep(2:3, each = 2), age_start = c(50, 51),
                    age_end = c(51, 52), population = c(900, 880, 400, 390))
  expect_skeleton(evl_aggregate(lt, pop, by = "location_id"))
  expect_error(evl_aggregate(lt, pop), class = "evl_error_input")
  expect_error(evl_aggregate(lt, pop, by = "sex_id"), class = "evl_error_input")
  expect_error(evl_aggregate(lt[, -4], pop, by = "location_id"), class = "evl_error_input")
  single <- data.frame(age_start = 0:2, age_end = c(1, 2, Inf),
                       lx = c(1, 0.99, 0.985), nLx = c(0.992, 0.987, 70))
  expect_skeleton(evl_abridge(single))
  expect_error(evl_abridge(single[, 1:2]), class = "evl_error_input")
  expect_error(evl_abridge(single, groups = data.frame(age_start = c(0, 5), age_end = c(4, Inf))),
               class = "evl_error_input")
})

test_that("the scaling functions validate and then say they are skeletons", {
  children <- draws_new(data.frame(location_id = 2:3), matrix(c(100, 210, 104, 205), 2),
                        measure = "deaths")
  parent <- draws_new(data.frame(location_id = 1L), matrix(c(318, 300), 1), measure = "deaths")
  expect_skeleton(evl_rake(children, parent))
  expect_skeleton(evl_rake_mismatch(children, parent))
  expect_error(evl_rake(children, 1), class = "evl_error_input")
  parent3 <- draws_new(data.frame(location_id = 1L), matrix(1, 1, 3), measure = "deaths")
  expect_error(evl_rake(children, parent3), class = "evl_error_input")
  other <- draws_new(data.frame(location_id = 1L), matrix(c(318, 300), 1), measure = "population")
  expect_error(evl_rake_mismatch(children, other), class = "evl_error_input")
})

test_that("the list of implemented and skeleton functions is the declared one", {
  exported <- getNamespaceExports("envelopelite")
  own <- sort(grep("^evl_", exported, value = TRUE))
  skeleton <- c("evl_abridge", "evl_aggregate", "evl_close_open_age", "evl_envelope",
                "evl_lifetable", "evl_mx", "evl_rake", "evl_rake_mismatch")
  shared <- c("evl_age_groups", "evl_country", "evl_draws_generate",
              "evl_draws_long", "evl_draws_new", "evl_draws_wide",
              "evl_summarise")
  expect_identical(own, sort(c(shared, skeleton)))
  # the shared wrappers are thin: they call the internal function and nothing
  # else, so none of them is a skeleton
  is_skeleton <- vapply(own, function(f) {
    any(grepl(".dl_not_implemented", deparse(body(get(f))), fixed = TRUE))
  }, logical(1))
  expect_identical(sort(own[is_skeleton]), skeleton)
  expect_identical(.dl_prefix(), "evl")
})
