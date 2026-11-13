#!/usr/bin/env Rscript
# Frequent reasons for rejection by CRAN that can be detected from text.
# It complements R CMD check; it does not replace it.
#
#   Rscript tools/check-cran-text.R [--root=DIR]
#
# Rules (each finding names its rule):
#   tf-logical     T or F used instead of TRUE or FALSE
#   fixed-seed     set.seed() called with a literal number in package code
#   console        print() or cat() outside a print or format method
#   global-state   options(), par(), setwd(), Sys.setenv(), Sys.setlocale()
#   writes-files   functions that write or delete files in package code
#   global-env     assignments to, or removals from, the global environment
#   attach         library() or require() in package code
#   installed      installed.packages()
#   cores          signs of more than two cores in code, tests or examples
#   dontrun        \dontrun in examples
#   non-ascii      non-ASCII characters in R code
#   title          DESCRIPTION title: title case, no final period, <= 65 chars
#   description    DESCRIPTION text: starts badly or lacks a final period
#   size           a file above 5 MB or a package above 10 MB
# Reviewed exceptions live in tools/cran-text-allow.txt and are counted.
# Exit status: 0 clean, 1 findings.

source(file.path(
  dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))),
  "common.R"
))
args <- tool_args()
root <- normalizePath(if (is.null(args$root)) file.path(tool_dir(), "..") else args$root)
allow_file <- if (is.null(args$allow)) file.path(tool_dir(), "cran-text-allow.txt") else args$allow
allow <- if (file.exists(allow_file)) {
  a <- readLines(allow_file, warn = FALSE)
  a <- a[nzchar(trimws(a)) & !startsWith(a, "#")]
  strsplit(a, "\t", fixed = TRUE)
} else {
  list()
}

pkgs <- package_dirs(root)
findings <- character()
n_allowed <- 0L
n_files <- 0L
add <- function(pkg, rel, line, rule, text) {
  for (a in allow) {
    if (identical(a[1], rule) && grepl(a[2], rel, perl = TRUE)) {
      n_allowed <<- n_allowed + 1L
      return(invisible())
    }
  }
  findings <<- c(findings, sprintf("  %s/%s:%s: [%s] %s", basename(pkg), rel, line, rule,
                                   substr(trimws(text), 1, 110)))
}

calls_in <- function(pd, names) {
  pd[pd$token == "SYMBOL_FUNCTION_CALL" & pd$text %in% names, , drop = FALSE]
}

check_r_file <- function(pkg, rel, in_package_code) {
  path <- file.path(pkg, rel)
  src <- readLines(path, warn = FALSE)
  exprs <- tryCatch(parse(path, keep.source = TRUE), error = function(e) e)
  if (inherits(exprs, "error")) {
    add(pkg, rel, 0, "parse", conditionMessage(exprs))
    return(invisible())
  }
  pd <- utils::getParseData(exprs)
  if (is.null(pd)) return(invisible())
  for (i in which(pd$token == "SYMBOL" & pd$text %in% c("T", "F"))) {
    add(pkg, rel, pd$line1[i], "tf-logical", src[pd$line1[i]])
  }
  cores <- grep("detectCores|mc\\.cores\\s*=\\s*([3-9]|[1-9][0-9])|makeCluster\\(\\s*([3-9]|[1-9][0-9])|setDTthreads\\(\\s*([3-9]|[1-9][0-9])|workers\\s*=\\s*([3-9]|[1-9][0-9])",
                src, perl = TRUE)
  for (i in cores) add(pkg, rel, i, "cores", src[i])
  if (!in_package_code) return(invisible())
  for (i in grep("[^\\x01-\\x7F]", src, perl = TRUE, useBytes = TRUE)) {
    add(pkg, rel, i, "non-ascii", src[i])
  }
  seed <- calls_in(pd, "set.seed")
  for (i in seq_len(nrow(seed))) {
    after <- pd[pd$line1 >= seed$line1[i] & pd$token != "expr", ]
    after <- after[order(after$line1, after$col1), ]
    pos <- which(after$line1 == seed$line1[i] & after$col1 == seed$col1[i])[1]
    nxt <- after$token[pos + 2L]
    if (!is.na(nxt) && nxt == "NUM_CONST") {
      add(pkg, rel, seed$line1[i], "fixed-seed", src[seed$line1[i]])
    }
  }
  simple <- list(
    "global-state" = c("options", "par", "setwd", "Sys.setenv", "Sys.setlocale"),
    "writes-files" = c("write.csv", "write.table", "saveRDS", "save", "writeLines",
                       "file.create", "dir.create", "download.file", "sink", "unlink",
                       "file.remove", "fwrite", "pdf", "png"),
    "attach" = c("library", "require"),
    "installed" = "installed.packages"
  )
  for (rule in names(simple)) {
    hit <- calls_in(pd, simple[[rule]])
    for (i in seq_len(nrow(hit))) add(pkg, rel, hit$line1[i], rule, src[hit$line1[i]])
  }
  genv <- grep("<<-|globalenv\\(\\)|\\.GlobalEnv", src, perl = TRUE)
  genv <- genv[!grepl("^\\s*#", src[genv])]
  for (i in genv) add(pkg, rel, i, "global-env", src[i])
  # console output: allowed only inside print or format methods
  for (e in seq_along(exprs)) {
    ex <- exprs[[e]]
    name <- if (is.call(ex) && as.character(ex[[1]]) %in% c("<-", "=") && is.name(ex[[2]])) {
      as.character(ex[[2]])
    } else {
      ""
    }
    if (grepl("^(print|format|summary)\\.", name)) next
    rng <- utils::getSrcLocation(attr(exprs, "srcref")[[e]], "line", first = TRUE):
      utils::getSrcLocation(attr(exprs, "srcref")[[e]], "line", first = FALSE)
    hit <- calls_in(pd[pd$line1 %in% rng, ], c("print", "cat"))
    for (i in seq_len(nrow(hit))) add(pkg, rel, hit$line1[i], "console", src[hit$line1[i]])
  }
}

for (pkg in pkgs) {
  r_files <- list.files(file.path(pkg, "R"), pattern = "\\.[Rr]$")
  for (f in r_files) check_r_file(pkg, file.path("R", f), TRUE)
  other <- c(list.files(file.path(pkg, "tests"), pattern = "\\.[Rr]$", recursive = TRUE,
                        full.names = FALSE))
  for (f in other) check_r_file(pkg, file.path("tests", f), FALSE)
  rd <- list.files(file.path(pkg, "man"), pattern = "\\.Rd$")
  doc_like <- c(file.path("man", rd), file.path("R", r_files),
                list.files(file.path(pkg, "vignettes"), recursive = TRUE, full.names = FALSE))
  for (f in doc_like) {
    path <- file.path(pkg, f)
    if (!file.exists(path)) path <- file.path(pkg, "vignettes", f)
    if (!file.exists(path)) next
    lines <- readLines(path, warn = FALSE)
    for (i in grep("\\\\dontrun\\s*\\{", lines)) add(pkg, f, i, "dontrun", lines[i])
  }
  n_files <- n_files + length(r_files) + length(other) + length(rd) + 1L

  desc <- read.dcf(file.path(pkg, "DESCRIPTION"))
  title <- gsub("\\s+", " ", desc[1, "Title"])
  if (nchar(title) > 65 || grepl("\\.$", title) || tools::toTitleCase(title) != title ||
        grepl("^(A|An|The)\\s+package|^Package", title, ignore.case = TRUE)) {
    add(pkg, "DESCRIPTION", "Title", "title", title)
  }
  text <- gsub("\\s+", " ", desc[1, "Description"])
  if (!grepl("\\.$", text) ||
        grepl(sprintf("^(This package|A package|The package|Package|%s)\\b", desc[1, "Package"]),
              text, ignore.case = TRUE)) {
    add(pkg, "DESCRIPTION", "Description", "description", text)
  }
  all_files <- list.files(pkg, recursive = TRUE, full.names = TRUE)
  all_files <- all_files[!grepl("/(docs|\\.Rcheck)/", all_files)]
  sizes <- file.size(all_files)
  for (i in which(sizes > 5 * 1024^2)) {
    add(pkg, sub(paste0(pkg, "/"), "", all_files[i], fixed = TRUE), "-", "size",
        sprintf("%.1f MB", sizes[i] / 1024^2))
  }
  if (sum(sizes) > 10 * 1024^2) {
    add(pkg, ".", "-", "size", sprintf("package is %.1f MB", sum(sizes) / 1024^2))
  }
}

if (length(findings)) cat(findings, sep = "\n")
finish(
  "check-cran-text", length(findings),
  sprintf("%d file(s) reviewed in %d package(s); %d reviewed exception(s) allowed",
          n_files, length(pkgs), n_allowed)
)
