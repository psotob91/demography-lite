#!/usr/bin/env Rscript
# Documentation control: every exported function has a title, a description of
# every argument, a return value, an example, and the two sections that the
# family requires ("GBD equivalent" and "Method").
#
#   Rscript tools/check-docs.R [--root=DIR]
#
# It reads NAMESPACE, the generated man/*.Rd files and the function
# definitions in R/. Run roxygen2 first. Exit status: 0 complete, 1 findings.

source(file.path(
  dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))),
  "common.R"
))
args <- tool_args()
root <- normalizePath(if (is.null(args$root)) file.path(tool_dir(), "..") else args$root)
pkgs <- package_dirs(root)
required_sections <- c("GBD equivalent", "Method")

rd_text <- function(x) trimws(paste(unlist(x), collapse = ""))
rd_tag <- function(rd, tag) Filter(function(e) identical(attr(e, "Rd_tag"), tag), rd)

findings <- character()
n_fun <- 0L
for (pkg in pkgs) {
  name <- basename(pkg)
  ns_file <- file.path(pkg, "NAMESPACE")
  if (!file.exists(ns_file)) {
    findings <- c(findings, sprintf("  %s: no NAMESPACE (run roxygen2)", name))
    next
  }
  ns <- parse(ns_file, keep.source = FALSE)
  exports <- unlist(lapply(ns, function(e) {
    if (identical(as.character(e[[1]]), "export")) vapply(as.list(e)[-1], as.character, "")
  }))
  # formals of every top-level function defined in R/
  formals_of <- list()
  for (f in list.files(file.path(pkg, "R"), pattern = "\\.[Rr]$", full.names = TRUE)) {
    for (e in parse(f, keep.source = FALSE)) {
      if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=") && is.name(e[[2]]) &&
            is.call(e[[3]]) && identical(as.character(e[[3]][[1]]), "function")) {
        formals_of[[as.character(e[[2]])]] <- names(e[[3]][[2]])
      }
    }
  }
  rd_files <- list.files(file.path(pkg, "man"), pattern = "\\.Rd$", full.names = TRUE)
  by_alias <- list()
  for (f in rd_files) {
    rd <- tools::parse_Rd(f)
    for (a in rd_tag(rd, "\\alias")) by_alias[[rd_text(a)]] <- rd
  }
  for (fn in exports) {
    n_fun <- n_fun + 1L
    miss <- character()
    rd <- by_alias[[fn]]
    if (is.null(rd)) {
      findings <- c(findings, sprintf("  %s::%s: no help page", name, fn))
      next
    }
    has <- function(tag) {
      x <- rd_tag(rd, tag)
      length(x) > 0 && nzchar(rd_text(x[[1]]))
    }
    if (!has("\\title")) miss <- c(miss, "title")
    if (!has("\\description")) miss <- c(miss, "description")
    if (!has("\\value")) miss <- c(miss, "return value")
    if (!has("\\examples")) miss <- c(miss, "example")
    documented <- character()
    for (a in rd_tag(rd, "\\arguments")) {
      for (item in rd_tag(a, "\\item")) {
        documented <- c(documented, trimws(strsplit(rd_text(item[[1]]), ",")[[1]]))
      }
    }
    documented <- sub("^\\\\dots$", "...", documented)
    undocumented <- setdiff(formals_of[[fn]], documented)
    if (length(undocumented)) {
      miss <- c(miss, paste0("argument(s) ", paste(undocumented, collapse = ", ")))
    }
    sections <- vapply(rd_tag(rd, "\\section"), function(s) rd_text(s[[1]]), "")
    for (s in setdiff(required_sections, sections)) miss <- c(miss, paste0("section '", s, "'"))
    if (length(miss)) {
      findings <- c(findings, sprintf("  %s::%s: missing %s", name, fn,
                                      paste(miss, collapse = "; ")))
    }
  }
}

if (length(findings)) cat(findings, sep = "\n")
finish(
  "check-docs", length(findings),
  sprintf("%d exported function(s) examined in %d package(s) for: title, description, ",
          n_fun, length(pkgs)) |>
    paste0("arguments, return value, example, sections 'GBD equivalent' and 'Method'")
)
