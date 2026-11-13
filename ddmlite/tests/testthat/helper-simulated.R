# Shared fixtures: one simulated case with planted truth, built once.
sim_ddl <- ddl_simulate(seed = 21, completeness = 0.8)

# A skeleton validates its inputs and then signals "not implemented".
expect_skeleton <- function(expr) {
  err <- tryCatch({
    expr
    NULL
  }, error = function(e) e)
  testthat::expect_s3_class(err, "ddl_error_not_implemented")
  testthat::expect_s3_class(err, "demolite_error_not_implemented")
}
