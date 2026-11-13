#!/usr/bin/env Rscript
# Regenerates the documentation with 'roxygen2' and runs R CMD check on every
# package of the repository. Needs the development tools of renv.lock.
#
#   Rscript tools/check-packages.R [--as-cran] [--no-manual] [--pkgdown]
#
# Results are printed as they come out and written to check-results/ (not
# versioned). Exit status 1 if any package has an error or a warning.

args <- commandArgs(trailingOnly = TRUE)
here <- dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])))
root <- normalizePath(file.path(here, ".."))
pkgs <- list.dirs(root, recursive = FALSE)
pkgs <- sort(pkgs[file.exists(file.path(pkgs, "DESCRIPTION"))])
out_dir <- file.path(root, "check-results")
dir.create(out_dir, showWarnings = FALSE)
check_args <- c(if ("--as-cran" %in% args) "--as-cran", if ("--no-manual" %in% args) "--no-manual")
# No network during the check: the CRAN incoming test would query the web.
Sys.setenv("_R_CHECK_CRAN_INCOMING_REMOTE_" = "false", "_R_CHECK_FORCE_SUGGESTS_" = "true")

bad <- FALSE
for (pkg in pkgs) {
  name <- basename(pkg)
  cat("\n==== ", name, "\n", sep = "")
  roxygen2::roxygenise(pkg)
  res <- rcmdcheck::rcmdcheck(pkg, args = check_args, quiet = TRUE,
                              error_on = "never", check_dir = file.path(out_dir, name))
  cat(sprintf("R CMD check %s %s: %d error(s), %d warning(s), %d note(s)\n", name,
              paste(check_args, collapse = " "), length(res$errors), length(res$warnings),
              length(res$notes)))
  for (kind in c("errors", "warnings", "notes")) {
    for (msg in res[[kind]]) cat("--- ", kind, ":\n", msg, "\n", sep = "")
  }
  writeLines(c(res$errors, res$warnings, res$notes),
             file.path(out_dir, paste0(name, "-findings.txt")))
  tests <- file.path(out_dir, name, paste0(name, ".Rcheck"), "tests", "testthat.Rout")
  if (file.exists(tests)) {
    cat(grep("^\\[ FAIL", readLines(tests), value = TRUE), sep = "\n")
  }
  bad <- bad || length(res$errors) > 0 || length(res$warnings) > 0
  if ("--pkgdown" %in% args) {
    site <- tryCatch({
      pkgdown::build_site(pkg, preview = FALSE, devel = FALSE, install = TRUE)
      "built"
    }, error = function(e) paste("FAILED:", conditionMessage(e)))
    cat(sprintf("pkgdown site of %s: %s\n", name, site))
  }
}
quit(status = if (bad) 1 else 0, save = "no")
