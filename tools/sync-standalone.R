#!/usr/bin/env Rscript
# Copies the single-source files of standalone/ into every package and checks
# that the copies are identical to the source.
#
#   Rscript tools/sync-standalone.R            copy, then verify
#   Rscript tools/sync-standalone.R --check    verify only (exit status 1 if a
#                                              copy is missing or differs)
#   --root=DIR                                 repository root (default: parent
#                                              of tools/)
#
# standalone/standalone-<name>.R            -> <pkg>/R/import-standalone-<name>.R
# standalone/tests/test-standalone-<name>.R -> <pkg>/tests/testthat/test-import-standalone-<name>.R

source(file.path(
  dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]))),
  "common.R"
))
args <- tool_args()
root <- normalizePath(if (is.null(args$root)) file.path(tool_dir(), "..") else args$root)
check_only <- isTRUE(args$check)

src_r <- sort(list.files(file.path(root, "standalone"), pattern = "^standalone-.*\\.R$",
                         full.names = TRUE))
src_t <- sort(list.files(file.path(root, "standalone", "tests"),
                         pattern = "^test-standalone-.*\\.R$", full.names = TRUE))
pkgs <- package_dirs(root)
if (!length(src_r) || !length(pkgs)) {
  cat("sync-standalone: no standalone sources or no packages found under", root, "\n")
  quit(status = 1, save = "no")
}

pairs <- do.call(rbind, lapply(pkgs, function(pkg) {
  rbind(
    data.frame(src = src_r,
               dst = file.path(pkg, "R", sub("^standalone-", "import-standalone-",
                                             basename(src_r)))),
    if (length(src_t)) data.frame(
      src = src_t,
      dst = file.path(pkg, "tests", "testthat",
                      sub("^test-standalone-", "test-import-standalone-", basename(src_t)))
    )
  )
}))

if (!check_only) {
  for (i in seq_len(nrow(pairs))) {
    dir.create(dirname(pairs$dst[i]), recursive = TRUE, showWarnings = FALSE)
    file.copy(pairs$src[i], pairs$dst[i], overwrite = TRUE)
  }
}

md5 <- function(f) unname(tools::md5sum(f))
same <- file.exists(pairs$dst) & md5(pairs$src) == md5(pairs$dst)
same[is.na(same)] <- FALSE
# A copy without a source is an orphan: it would silently diverge.
copies <- unlist(lapply(pkgs, function(pkg) {
  c(list.files(file.path(pkg, "R"), pattern = "^import-standalone-", full.names = TRUE),
    list.files(file.path(pkg, "tests", "testthat"), pattern = "^test-import-standalone-",
               full.names = TRUE))
}))
orphans <- setdiff(normalizePath(copies), normalizePath(pairs$dst[file.exists(pairs$dst)]))

for (f in pairs$dst[!same]) cat("  differs or missing:", sub(paste0(root, "/"), "", f), "\n")
for (f in orphans) cat("  copy without a source:", sub(paste0(root, "/"), "", f), "\n")
finish(
  "sync-standalone", sum(!same) + length(orphans),
  sprintf("%d source file(s) x %d package(s) = %d copies compared (%s), %d identical",
          length(src_r) + length(src_t), length(pkgs), nrow(pairs),
          if (check_only) "check only" else "after copying", sum(same))
)
