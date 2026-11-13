# Helpers shared by the control scripts in tools/. Base R only.

# Parse arguments of the form --name=value or --flag.
tool_args <- function() {
  raw <- commandArgs(trailingOnly = TRUE)
  out <- list()
  for (a in raw) {
    if (!startsWith(a, "--")) next
    kv <- strsplit(sub("^--", "", a), "=", fixed = TRUE)[[1]]
    out[[kv[1]]] <- if (length(kv) > 1) paste(kv[-1], collapse = "=") else TRUE
  }
  out
}

# Directory that holds the tools (works with Rscript tools/x.R from anywhere).
tool_dir <- function() {
  f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  if (length(f)) dirname(normalizePath(f[1])) else normalizePath("tools")
}

# Package directories under a root: sub-directories with a DESCRIPTION file.
package_dirs <- function(root) {
  d <- list.dirs(root, recursive = FALSE, full.names = TRUE)
  d <- d[file.exists(file.path(d, "DESCRIPTION"))]
  sort(d)
}

# Files of a repository that would be published: tracked files plus untracked
# files that are not ignored. Without git, every file except .git.
repo_files <- function(root) {
  old <- setwd(root)
  on.exit(setwd(old))
  is_git <- suppressWarnings(
    system2("git", c("rev-parse", "--is-inside-work-tree"), stdout = TRUE, stderr = FALSE)
  )
  if (length(is_git) && identical(is_git[1], "true")) {
    f <- system2("git", c("ls-files", "--cached", "--others", "--exclude-standard"),
                 stdout = TRUE)
  } else {
    f <- list.files(".", recursive = TRUE, all.files = TRUE, no.. = TRUE)
    f <- sub("^\\./", "", f)
    f <- f[!grepl("^\\.git/", f)]
  }
  sort(unique(f[file.exists(f) & !dir.exists(f)]))
}

# TRUE when a file looks binary (contains a NUL byte in its first 64 kB).
is_binary <- function(path) {
  con <- file(path, "rb")
  on.exit(close(con))
  bytes <- readBin(con, "raw", n = 65536L)
  any(bytes == as.raw(0))
}

finish <- function(name, n_findings, summary) {
  cat(sprintf("%s: %s\n", name, summary))
  if (n_findings > 0) {
    cat(sprintf("%s: FAILED with %d finding(s)\n", name, n_findings))
    quit(status = 1, save = "no")
  }
  cat(sprintf("%s: OK\n", name))
  quit(status = 0, save = "no")
}
