test_that("the list of implemented and skeleton functions is the declared one", {
  exported <- getNamespaceExports("ddmlite")
  own <- sort(grep("^ddl_", exported, value = TRUE))
  implemented <- c("ddl_cap", "ddl_combine", "ddl_simulate")
  # the public names of the shared infrastructure, one per package
  shared <- c("ddl_age_groups", "ddl_country", "ddl_draws_generate",
              "ddl_draws_long", "ddl_draws_new", "ddl_draws_wide",
              "ddl_summarise")
  skeleton <- c("ddl_area_mismatch", "ddl_child_completeness", "ddl_cohort_balance",
                "ddl_deaths_in_interval", "ddl_draws", "ddl_estimate", "ddl_ggb",
                "ddl_ggbseg", "ddl_interval", "ddl_seg", "ddl_smooth_series")
  expect_identical(own, sort(c(implemented, shared, skeleton)))
  # a skeleton is a function whose body ends by signalling "not implemented"
  is_skeleton <- vapply(own, function(f) {
    any(grepl(".dl_not_implemented", deparse(body(get(f))), fixed = TRUE))
  }, logical(1))
  expect_identical(sort(own[is_skeleton]), skeleton)
  # the shared infrastructure is internal: no package exports it under its
  # own name, so attaching two packages of the family masks nothing
  common <- c("age_groups", "as_draws_long", "as_draws_wide", "cell_seed",
              "draws_generate", "draws_new", "draws_summarise", "simulate_country",
              "with_cell_seed")
  expect_identical(intersect(common, exported), character(0))
  expect_true(all(vapply(common, exists, logical(1),
                         where = asNamespace("ddmlite"), inherits = FALSE)))
  expect_identical(.dl_prefix(), "ddl")
})
