#!/usr/bin/env Rscript
# Runs the four controls on the repository and then the tests of the controls
# (each control against a broken case). Exit status 1 if anything fails.
#
#   Rscript tools/run-controls.R [--history]

here <- dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])))
root <- normalizePath(file.path(here, ".."))
history <- "--history" %in% commandArgs(trailingOnly = TRUE)
rscript <- file.path(R.home("bin"), "Rscript")
failed <- character()
run <- function(label, script, ...) {
  cat("\n==", label, "\n")
  status <- system2(rscript, c("--vanilla", shQuote(file.path(here, script)),
                               paste0("--root=", shQuote(root)), ...))
  if (status != 0) failed <<- c(failed, label)
}
run("single-source copies", "sync-standalone.R", "--check")
run("clean package", "check-clean.R", if (history) "--history")
run("CRAN text rules", "check-cran-text.R")
run("documentation", "check-docs.R")

cat("\n== tests of the controls (each one against a broken case)\n")
if (!requireNamespace("testthat", quietly = TRUE)) {
  cat("testthat is not available: tests of the controls NOT run\n")
  failed <- c(failed, "tests of the controls (not run)")
} else {
  Sys.setenv(DEMOLITE_TOOLS = here)
  res <- as.data.frame(testthat::test_file(file.path(here, "tests", "test-controls.R"),
                                           reporter = "summary", stop_on_failure = FALSE))
  cat(sprintf("tests of the controls: %d expectations passed, %d failed, %d errors, %d skipped\n",
              sum(res$passed), sum(res$failed), sum(res$error), sum(res$skipped)))
  if (sum(res$failed) + sum(res$error) > 0) failed <- c(failed, "tests of the controls")
}

cat("\n")
if (length(failed)) {
  cat("run-controls: FAILED:", paste(failed, collapse = "; "), "\n")
  quit(status = 1, save = "no")
}
cat("run-controls: all controls OK\n")
