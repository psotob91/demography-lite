#!/usr/bin/env Rscript
# Copies the single-source files of standalone/ into every package and checks
# that the copies are identical to the source. The templates of the public
# wrappers are copied with their token replaced by the function prefix of the
# package, and verified the same way.
#
#   Rscript tools/sync-standalone.R            copy, then verify
#   Rscript tools/sync-standalone.R --check    verify only (exit status 1 if a
#                                              copy is missing or differs)
#   --root=DIR                                 repository root (default: parent
#                                              of tools/)
#
# standalone/standalone-<name>.R            -> <pkg>/R/import-standalone-<name>.R
# standalone/tests/test-standalone-<name>.R -> <pkg>/tests/testthat/test-import-standalone-<name>.R
# standalone/prefixed-<name>.R              -> <pkg>/R/import-prefixed-<name>.R,
#                                              with PKGPREFIX_ -> <prefix>_

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
src_p <- sort(list.files(file.path(root, "standalone"), pattern = "^prefixed-.*\\.R$",
                         full.names = TRUE))
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

# Templates of the public wrappers: the same file in every package, with the
# token replaced by the function prefix that the package declares.
prefix_of <- function(pkg) {
  for (f in list.files(file.path(pkg, "R"), pattern = "\\.[Rr]$", full.names = TRUE)) {
    hit <- grep("^\\.demolite_prefix <- \"([a-z]+)\"", readLines(f, warn = FALSE),
                value = TRUE)
    if (length(hit)) return(sub("^.*\"([a-z]+)\".*$", "\\1", hit[1]))
  }
  NA_character_
}
prefixed <- data.frame(dst = character(), ok = logical())
for (pkg in pkgs) {
  if (!length(src_p)) break
  prefix <- prefix_of(pkg)
  if (is.na(prefix)) {
    cat("  no function prefix (`.demolite_prefix`) in:",
        sub(paste0(root, "/"), "", pkg), "\n")
    prefixed <- rbind(prefixed, data.frame(dst = file.path(pkg, "R"), ok = FALSE))
    next
  }
  for (src in src_p) {
    dst <- file.path(pkg, "R", sub("^prefixed-", "import-prefixed-", basename(src)))
    want <- gsub("PKGPREFIX_", paste0(prefix, "_"), readLines(src, warn = FALSE),
                 fixed = TRUE)
    if (!check_only) {
      dir.create(dirname(dst), recursive = TRUE, showWarnings = FALSE)
      writeLines(want, dst)
    }
    ok <- file.exists(dst) && identical(readLines(dst, warn = FALSE), want)
    prefixed <- rbind(prefixed, data.frame(dst = dst, ok = ok))
  }
}

# A copy without a source is an orphan: it would silently diverge.
copies <- unlist(lapply(pkgs, function(pkg) {
  c(list.files(file.path(pkg, "R"), pattern = "^import-standalone-|^import-prefixed-",
               full.names = TRUE),
    list.files(file.path(pkg, "tests", "testthat"), pattern = "^test-import-standalone-",
               full.names = TRUE))
}))
written <- c(pairs$dst, prefixed$dst)
orphans <- setdiff(normalizePath(copies), normalizePath(written[file.exists(written)]))

for (f in pairs$dst[!same]) cat("  differs or missing:", sub(paste0(root, "/"), "", f), "\n")
for (f in prefixed$dst[!prefixed$ok]) {
  cat("  differs or missing:", sub(paste0(root, "/"), "", f), "\n")
}
for (f in orphans) cat("  copy without a source:", sub(paste0(root, "/"), "", f), "\n")
finish(
  "sync-standalone", sum(!same) + sum(!prefixed$ok) + length(orphans),
  sprintf(paste0("%d source file(s) x %d package(s) = %d copies compared (%s), ",
                 "%d identical; %d prefixed file(s) compared, %d as generated"),
          length(src_r) + length(src_t), length(pkgs), nrow(pairs),
          if (check_only) "check only" else "after copying", sum(same),
          nrow(prefixed), sum(prefixed$ok))
)
