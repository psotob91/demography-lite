# Shared fixtures: one simulated country with planted truth, built once.
sim_prl <- prl_simulate(seed = 31, n_areas = 2, stochastic = FALSE)

# A skeleton validates its inputs and then signals "not implemented".
expect_skeleton <- function(expr) {
  err <- tryCatch({
    expr
    NULL
  }, error = function(e) e)
  testthat::expect_s3_class(err, "prl_error_not_implemented")
  testthat::expect_s3_class(err, "demolite_error_not_implemented")
}
