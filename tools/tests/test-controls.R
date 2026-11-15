# Tests of the controls themselves: each control must fail on a broken case
# and pass on a clean one. Run with `Rscript tools/run-controls.R`.
#
# Forbidden words are assembled from pieces so that this file stays clean.

tools_dir <- normalizePath(Sys.getenv("DEMOLITE_TOOLS", "tools"))

run_tool <- function(script, ...) {
  out <- suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"),
    c("--vanilla", shQuote(file.path(tools_dir, script)), ...),
    stdout = TRUE, stderr = TRUE
  ))
  status <- attr(out, "status")
  list(status = if (is.null(status)) 0L else status, out = paste(out, collapse = "\n"))
}

new_dir <- function() {
  d <- tempfile("demolite-control-")
  dir.create(d)
  d
}

write_pkg <- function(root, name = "pkg", r_code, rd = NULL, exports = "f",
                      title = "A Clean Title for a Small Test Package") {
  pkg <- file.path(root, name)
  dir.create(file.path(pkg, "R"), recursive = TRUE)
  dir.create(file.path(pkg, "man"))
  writeLines(c(paste("Package:", name), paste("Title:", title), "Version: 0.0.1",
               "Description: Does one small thing for the tests of the controls.",
               "License: MIT + file LICENSE"), file.path(pkg, "DESCRIPTION"))
  writeLines(paste0("export(", exports, ")"), file.path(pkg, "NAMESPACE"))
  writeLines(r_code, file.path(pkg, "R", "f.R"))
  if (!is.null(rd)) writeLines(rd, file.path(pkg, "man", "f.Rd"))
  pkg
}

good_rd <- c(
  "\\name{f}", "\\alias{f}", "\\title{Add one}",
  "\\usage{f(x, y)}",
  "\\arguments{\\item{x}{A number.}\\item{y}{Another number.}}",
  "\\value{A number.}", "\\description{Adds.}",
  "\\section{GBD equivalent}{Own.}", "\\section{Method}{Addition.}",
  "\\examples{f(1, 2)}"
)

# ---- clean-package control ---------------------------------------------------

test_that("the clean-package control passes on a clean tree and counts files", {
  d <- new_dir()
  writeLines(c("A generic sentence about deaths per unit of time.",
               "DOI 10.4054/DemRes.2009.21.9, draw_{K-1}, superuser, D1, period 2010-2020."),
             file.path(d, "notes.md"))
  writeLines("x <- 1", file.path(d, "code.R"))
  res <- run_tool("check-clean.R", paste0("--root=", d))
  expect_identical(res$status, 0L)
  expect_match(res$out, "2 file(s) reviewed", fixed = TRUE)
  expect_match(res$out, "check-clean: OK", fixed = TRUE)
})

test_that("the clean-package control fails on each kind of leak", {
  broken <- list(
    country = paste0("Estimates for Pe", "ru in 2020"),
    country = paste0("the pe", "ruvian registry"),
    institution = paste0("source: IN", "EI"),
    institution = paste0("from the SINA", "DEF file"),
    institution = paste0("survey EN", "DES 2019"),
    institution = paste0("MIN", "SA"),
    department = paste0("Are", "quipa and others"),
    `department-short` = paste0("the province of Li", "ma"),
    `data-path` = paste0("read('/data", "-cdc/raw/x.csv')"),
    `decision-code` = paste0("see decision D", "47 for details"),
    `limitation-code` = paste0("limitation L-6", "-12"),
    `run-id` = paste0("seal 2026", "1109-114300"),
    `internal-rule` = paste0("as required by reg", "la 63")
  )
  for (i in seq_along(broken)) {
    d <- new_dir()
    writeLines(c("first line is fine", broken[[i]]), file.path(d, "file.txt"))
    res <- run_tool("check-clean.R", paste0("--root=", d))
    expect_identical(res$status, 1L, info = names(broken)[i])
    expect_match(res$out, paste0("file.txt:2: [", names(broken)[i], "]"), fixed = TRUE)
    expect_match(res$out, "FAILED with 1 finding", fixed = TRUE)
  }
})

test_that("the clean-package control also looks at file names", {
  d <- new_dir()
  writeLines("nothing here", file.path(d, paste0("deaths-pe", "ru.csv")))
  res <- run_tool("check-clean.R", paste0("--root=", d))
  expect_identical(res$status, 1L)
  expect_match(res$out, ":name: [country]", fixed = TRUE)
})

test_that("the clean-package control scans the history of a git repository", {
  skip_if(Sys.which("git") == "")
  d <- new_dir()
  git <- function(...) system2("git", c("-C", shQuote(d), ...), stdout = TRUE, stderr = TRUE)
  git("init", "-q")
  git("config", "user.email", "test@example.invalid")
  git("config", "user.name", "test")
  writeLines(paste0("source: IN", "EI"), file.path(d, "a.txt"))
  git("add", "a.txt")
  git("commit", "-q", "-m", "first")
  writeLines("now clean", file.path(d, "a.txt"))
  git("add", "a.txt")
  git("commit", "-q", "-m", "second")
  now <- run_tool("check-clean.R", paste0("--root=", d))
  expect_identical(now$status, 0L)
  hist <- run_tool("check-clean.R", paste0("--root=", d), "--history")
  expect_identical(hist$status, 1L)
  expect_match(hist$out, "[institution]", fixed = TRUE)
  expect_match(hist$out, "2 commit(s) of history scanned", fixed = TRUE)
})

# ---- CRAN text control -------------------------------------------------------

test_that("the CRAN text control passes on a clean package", {
  d <- new_dir()
  write_pkg(d, r_code = "f <- function(x, y) x + y", rd = good_rd)
  res <- run_tool("check-cran-text.R", paste0("--root=", d))
  expect_identical(res$status, 0L)
  expect_match(res$out, "in 1 package(s)", fixed = TRUE)
})

test_that("the CRAN text control fails on each detectable problem", {
  broken <- list(
    `tf-logical` = "f <- function(x, y) if (T) x else y",
    `fixed-seed` = "f <- function(x, y) { set.seed(123); x + y }",
    console = "f <- function(x, y) { cat('working\\n'); x + y }",
    console = "f <- function(x, y) { print(x); x + y }",
    `global-state` = "f <- function(x, y) { options(digits = 3); x + y }",
    `global-state` = "f <- function(x, y) { setwd('..'); x + y }",
    `writes-files` = "f <- function(x, y) { saveRDS(x, 'x.rds'); x + y }",
    `global-env` = "f <- function(x, y) { total <<- x + y; total }",
    attach = "f <- function(x, y) { library(stats); x + y }",
    installed = "f <- function(x, y) nrow(installed.packages())",
    cores = "f <- function(x, y) parallel::mclapply(x, y, mc.cores = 8)",
    dontrun = c("#' @examples", "#' \\dontrun{", "#' f(1, 2)", "#' }",
                "f <- function(x, y) x + y"),
    `non-ascii` = "f <- function(x, y) paste(x, y, 'é')"
  )
  for (i in seq_along(broken)) {
    d <- new_dir()
    write_pkg(d, r_code = broken[[i]], rd = good_rd)
    res <- run_tool("check-cran-text.R", paste0("--root=", d))
    expect_identical(res$status, 1L, info = names(broken)[i])
    expect_match(res$out, paste0("[", names(broken)[i], "]"), fixed = TRUE)
  }
})

test_that("a seed passed by the caller and console output in print methods are fine", {
  d <- new_dir()
  write_pkg(d, rd = good_rd, r_code = c(
    "f <- function(x, y) { set.seed(y); x }",
    "print.thing <- function(x, ...) { cat('thing\\n'); invisible(x) }"
  ))
  res <- run_tool("check-cran-text.R", paste0("--root=", d))
  expect_identical(res$status, 0L)
})

test_that("the CRAN text control checks the DESCRIPTION title", {
  for (bad in c("a title that is not in title case", "A Title That Ends with a Period.",
                paste(rep("Long", 20), collapse = " "))) {
    d <- new_dir()
    write_pkg(d, r_code = "f <- function(x, y) x + y", rd = good_rd, title = bad)
    res <- run_tool("check-cran-text.R", paste0("--root=", d))
    expect_identical(res$status, 1L, info = bad)
    expect_match(res$out, "[title]", fixed = TRUE)
  }
})

# ---- documentation control ---------------------------------------------------

test_that("the documentation control passes when every field is present", {
  d <- new_dir()
  write_pkg(d, r_code = "f <- function(x, y) x + y", rd = good_rd)
  res <- run_tool("check-docs.R", paste0("--root=", d))
  expect_identical(res$status, 0L)
  expect_match(res$out, "1 exported function(s) examined", fixed = TRUE)
})

test_that("the documentation control fails on each missing field", {
  drop <- list(
    "return value" = "^\\\\value",
    "example" = "^\\\\examples",
    "title" = "^\\\\title",
    "section 'GBD equivalent'" = "GBD equivalent",
    "section 'Method'" = "section\\{Method\\}"
  )
  for (i in seq_along(drop)) {
    d <- new_dir()
    write_pkg(d, r_code = "f <- function(x, y) x + y",
              rd = good_rd[!grepl(drop[[i]], good_rd)])
    res <- run_tool("check-docs.R", paste0("--root=", d))
    expect_identical(res$status, 1L, info = names(drop)[i])
    expect_match(res$out, paste0("pkg::f: missing ", names(drop)[i]), fixed = TRUE)
  }
  d <- new_dir()
  write_pkg(d, r_code = "f <- function(x, y, z) x + y + z", rd = good_rd)
  res <- run_tool("check-docs.R", paste0("--root=", d))
  expect_identical(res$status, 1L)
  expect_match(res$out, "argument(s) z", fixed = TRUE)
  d <- new_dir()
  write_pkg(d, r_code = "f <- function(x, y) x + y", rd = NULL)
  res <- run_tool("check-docs.R", paste0("--root=", d))
  expect_identical(res$status, 1L)
  expect_match(res$out, "no help page", fixed = TRUE)
})

# ---- single-source copies ----------------------------------------------------

test_that("the sync tool copies the shared files and detects any divergence", {
  d <- new_dir()
  dir.create(file.path(d, "standalone", "tests"), recursive = TRUE)
  writeLines("shared <- function() 1", file.path(d, "standalone", "standalone-x.R"))
  writeLines("# test", file.path(d, "standalone", "tests", "test-standalone-x.R"))
  for (p in c("one", "two")) {
    write_pkg(d, name = p, r_code = "f <- function(x, y) x + y", rd = good_rd)
  }
  missing <- run_tool("sync-standalone.R", paste0("--root=", d), "--check")
  expect_identical(missing$status, 1L)
  expect_match(missing$out, "differs or missing", fixed = TRUE)
  copied <- run_tool("sync-standalone.R", paste0("--root=", d))
  expect_identical(copied$status, 0L)
  expect_match(copied$out, "4 copies compared", fixed = TRUE)
  expect_true(file.exists(file.path(d, "two", "R", "import-standalone-x.R")))
  expect_true(file.exists(file.path(d, "one", "tests", "testthat",
                                    "test-import-standalone-x.R")))
  expect_identical(run_tool("sync-standalone.R", paste0("--root=", d), "--check")$status, 0L)
  # a copy edited by hand
  cat("# edited by hand\n", file = file.path(d, "two", "R", "import-standalone-x.R"),
      append = TRUE)
  edited <- run_tool("sync-standalone.R", paste0("--root=", d), "--check")
  expect_identical(edited$status, 1L)
  expect_match(edited$out, "two/R/import-standalone-x.R", fixed = TRUE)
  # copying again repairs it; a copy without a source is reported
  expect_identical(run_tool("sync-standalone.R", paste0("--root=", d))$status, 0L)
  writeLines("orphan <- 1", file.path(d, "one", "R", "import-standalone-gone.R"))
  orphan <- run_tool("sync-standalone.R", paste0("--root=", d), "--check")
  expect_identical(orphan$status, 1L)
  expect_match(orphan$out, "copy without a source", fixed = TRUE)
})

test_that("the sync tool writes the public wrappers with the prefix of each package", {
  d <- new_dir()
  dir.create(file.path(d, "standalone", "tests"), recursive = TRUE)
  writeLines("shared <- function() 1", file.path(d, "standalone", "standalone-x.R"))
  writeLines("# test", file.path(d, "standalone", "tests", "test-standalone-x.R"))
  writeLines(c("#' @export", "PKGPREFIX_shared <- function() shared()"),
             file.path(d, "standalone", "prefixed-w.R"))
  for (p in c("one", "two")) {
    write_pkg(d, name = p, r_code = "f <- function(x, y) x + y", rd = good_rd)
  }
  cat("\n.demolite_prefix <- \"aaa\"\n", file = file.path(d, "one", "R", "f.R"),
      append = TRUE)
  cat("\n.demolite_prefix <- \"bbb\"\n", file = file.path(d, "two", "R", "f.R"),
      append = TRUE)
  res <- run_tool("sync-standalone.R", paste0("--root=", d))
  expect_identical(res$status, 0L)
  expect_match(res$out, "2 prefixed file(s) compared, 2 as generated", fixed = TRUE)
  expect_match(readLines(file.path(d, "one", "R", "import-prefixed-w.R"))[2],
               "aaa_shared", fixed = TRUE)
  expect_match(readLines(file.path(d, "two", "R", "import-prefixed-w.R"))[2],
               "bbb_shared", fixed = TRUE)
  expect_identical(run_tool("sync-standalone.R", paste0("--root=", d), "--check")$status, 0L)
  # a wrapper edited by hand
  cat("# edited by hand\n", file = file.path(d, "two", "R", "import-prefixed-w.R"),
      append = TRUE)
  edited <- run_tool("sync-standalone.R", paste0("--root=", d), "--check")
  expect_identical(edited$status, 1L)
  expect_match(edited$out, "two/R/import-prefixed-w.R", fixed = TRUE)
  # a package that does not declare its prefix
  expect_identical(run_tool("sync-standalone.R", paste0("--root=", d))$status, 0L)
  writeLines("f <- function(x, y) x + y", file.path(d, "two", "R", "f.R"))
  no_prefix <- run_tool("sync-standalone.R", paste0("--root=", d), "--check")
  expect_identical(no_prefix$status, 1L)
  expect_match(no_prefix$out, "no function prefix", fixed = TRUE)
})
