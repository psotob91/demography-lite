# ---
# repo: demography-lite
# file: standalone-draws.R
# last-updated: 2026-11-13
# license: MIT (see the LICENSE file at the repository root)
# imports: [data.table, stats]
# dependencies: standalone-checks.R
# ---
#
# Single-source file. The copies inside the packages are named
# `R/import-standalone-draws.R` and must stay byte-identical to
# `standalone/standalone-draws.R`. Do not edit a copy: edit the source and run
# `Rscript tools/sync-standalone.R`.
#
# Contents: the draws container shared by the family (a table of cells plus a
# cells x draws matrix), its conversions, its summary, and the per-cell seed.

# Version of the shared data contract written into every container.
.dl_contract_version <- "0.1.0"

# Index columns that are identifiers and are stored as integers.
.dl_id_cols <- c("location_id", "year_id", "sex_id", "age_group_id")

# Column names that an index cannot use because the conversions create them.
.dl_reserved_cols <- c("draw", "value", "mean", "lower", "upper")

#' Create a container of draws
#'
#' Builds the object that every package of the family uses to carry
#' uncertainty: a table with one row per cell and a numeric matrix with one row
#' per cell and one column per draw. Draw `k` is the same simulated "world" in
#' every container that descends from the same inputs, so containers must never
#' be shuffled or re-ordered along the draw dimension.
#'
#' @param index Data frame or data.table. Either the table of cells (one row
#'   per cell, no duplicated rows), or, when `value` is `NULL`, a complete
#'   table of draws in long format (a column `draw` with whole numbers `1`, ...,
#'   `K` and a column `value`) or in wide format (columns `draw_0`, ...,
#'   `draw_{K-1}`). The columns `location_id`, `year_id`, `sex_id` and
#'   `age_group_id`, when present, must hold whole numbers and are stored as
#'   integers. The columns `age_start` and `age_end`, when present, are exact
#'   ages in years (start included, end excluded, `Inf` for an open group).
#' @param value Numeric matrix with `nrow(index)` rows and one column per draw,
#'   or `NULL` when `index` already holds the draws. Missing values are
#'   accepted and mean "this draw could not be computed for this cell".
#' @param measure String. What the values are, for instance `"deaths"`,
#'   `"population"`, `"completeness"` or `"mx"`.
#' @param unit String. Unit of the values, for instance `"count"`,
#'   `"proportion"` or `"per person-year"`. `NA` when not stated.
#' @param seed Whole number or `NA`. The caller's seed that produced the draws.
#' @param stream String or `NA`. Name of the random stream that produced the
#'   draws (see [cell_seed()]).
#'
#' @return An object of class `demolite_draws`: a list with
#'   * `index`: a data.table with one row per cell;
#'   * `value`: a numeric matrix, cells in rows and draws in columns;
#'   * `meta`: a list with `measure`, `unit`, `n_draws`, `seed`, `stream` and
#'     `version` (version of the shared data contract).
#'
#' @section GBD equivalent:
#' **Own.** The Global Burden of Disease (GBD) study exchanges draws as wide
#' tables with columns `draw_0`, ..., `draw_999`; [as_draws_wide()] writes that
#' layout. The matrix layout is a working format chosen here to keep memory
#' proportional to cells x draws.
#'
#' @section Method:
#' No estimation is involved. The container enforces the invariants that the
#' rest of the family relies on: one row per cell, as many matrix rows as
#' cells, identifiers stored as integers, and age identified by exact
#' `age_start` and `age_end`.
#'
#' @examples
#' index <- data.frame(location_id = c(1, 2), sex_id = c(1, 1))
#' value <- matrix(c(0.80, 0.90, 0.82, 0.88, 0.78, 0.91), nrow = 2)
#' x <- draws_new(index, value, measure = "completeness", unit = "proportion")
#' x
#'
#' # The same container from a long table
#' long <- as_draws_long(x)
#' identical(draws_new(long, measure = "completeness")$value, x$value)
#'
#' @family draws
#' @export
draws_new <- function(index, value = NULL, measure, unit = NA_character_,
                      seed = NA_integer_, stream = NA_character_) {
  if (missing(measure)) {
    .dl_abort("`measure` must be supplied: say what the values are.")
  }
  .check_string(measure, "measure")
  .check_table(index, "index")
  if (is.null(value)) {
    parts <- .dl_draws_from_table(index)
    index <- parts$index
    value <- parts$value
  }
  if (!is.matrix(value) || !is.numeric(value)) {
    .dl_abort("`value` must be a numeric matrix (cells in rows, draws in columns).")
  }
  if (nrow(value) != nrow(index) || ncol(value) < 1L) {
    .dl_abort(c("`value` must have one row per row of `index` (", nrow(index),
                ") and at least one column; it is ", nrow(value), " x ",
                ncol(value), "."))
  }
  index <- .dl_index_clean(index)
  storage.mode(value) <- "double"
  dimnames(value) <- NULL
  if (!(length(seed) == 1L && is.na(seed))) {
    seed <- .check_seed(seed)
  }
  structure(
    list(
      index = index,
      value = value,
      meta = list(
        measure = measure,
        unit = as.character(unit)[1L],
        n_draws = ncol(value),
        seed = as.integer(seed),
        stream = as.character(stream)[1L],
        version = .dl_contract_version
      )
    ),
    class = "demolite_draws"
  )
}

#' Validate and normalise the table of cells of a container
#'
#' @param index Data frame with one row per cell.
#'
#' @return A data.table copy of `index` with identifier columns as integers.
#' @noRd
.dl_index_clean <- function(index) {
  index <- data.table::as.data.table(index)
  cols <- names(index)
  if (!length(cols) || anyDuplicated(cols)) {
    .dl_abort("`index` must have at least one column and unique column names.")
  }
  reserved <- cols[cols %in% .dl_reserved_cols | grepl("^draw_[0-9]+$", cols)]
  if (length(reserved)) {
    .dl_abort(c("`index` uses reserved column name(s): ",
                paste0("`", reserved, "`", collapse = ", "), "."))
  }
  for (col in intersect(cols, .dl_id_cols)) {
    v <- index[[col]]
    if (!is.numeric(v)) {
      .dl_abort(c("Column `", col, "` of `index` must hold whole numbers."))
    }
    bad <- which(!is.na(v) & v != trunc(v))
    if (length(bad)) {
      .dl_abort(c("Column `", col, "` of `index` must hold whole numbers ",
                  "(first violation in row ", bad[1L], ")."))
    }
    if (col != "age_group_id" && anyNA(v)) {
      .dl_abort(c("Column `", col, "` of `index` has a missing value in row ",
                  which(is.na(v))[1L], "."))
    }
    data.table::set(index, j = col, value = as.integer(v))
  }
  if (all(c("age_start", "age_end") %in% cols)) {
    a <- index[["age_start"]]
    b <- index[["age_end"]]
    if (!is.numeric(a) || !is.numeric(b)) {
      .dl_abort("`age_start` and `age_end` of `index` must be numeric.")
    }
    bad <- which(!(is.finite(a) & !is.na(b) & a >= 0 & a < b))
    if (length(bad)) {
      .dl_abort(c("`age_start` and `age_end` of `index` must satisfy ",
                  "0 <= age_start < age_end (first violation in row ",
                  bad[1L], ")."))
    }
  }
  dup <- anyDuplicated(index)
  if (dup) {
    .dl_abort(c("`index` must have one row per cell; row ", dup,
                " repeats an earlier row."))
  }
  index
}

#' Split a long or wide table of draws into cells and matrix
#'
#' @param data Data frame in long format (`draw`, `value`) or in wide format
#'   (`draw_0`, ..., `draw_{K-1}`).
#'
#' @return A list with `index` (data.table of cells, in order of first
#'   appearance) and `value` (numeric matrix).
#' @noRd
.dl_draws_from_table <- function(data) {
  data <- data.table::as.data.table(data)
  cols <- names(data)
  wide <- grep("^draw_[0-9]+$", cols, value = TRUE)
  is_long <- all(c("draw", "value") %in% cols)
  if (is_long == (length(wide) > 0L)) {
    .dl_abort(c("When `value` is NULL, `index` must be either a long table ",
                "(columns `draw` and `value`) or a wide table (columns ",
                "`draw_0`, `draw_1`, ...), not both and not neither."))
  }
  if (!is_long) {
    k <- as.integer(sub("^draw_", "", wide))
    if (!identical(sort(k), seq_along(k) - 1L)) {
      .dl_abort(c("Wide draw columns must be `draw_0`, ..., `draw_",
                  length(k) - 1L, "` without gaps."))
    }
    wide <- wide[order(k)]
    value <- as.matrix(data[, wide, with = FALSE])
    if (!is.numeric(value)) {
      .dl_abort("Wide draw columns must be numeric.")
    }
    ids <- setdiff(cols, wide)
    return(list(index = data[, ids, with = FALSE], value = value))
  }
  ids <- setdiff(cols, c("draw", "value"))
  if (!length(ids)) {
    .dl_abort("A long table of draws needs at least one column identifying the cell.")
  }
  draw <- data[["draw"]]
  if (!is.numeric(draw) || anyNA(draw) || any(draw != trunc(draw)) ||
        min(draw) != 1) {
    .dl_abort("Column `draw` must hold whole numbers starting at 1.")
  }
  if (!is.numeric(data[["value"]])) {
    .dl_abort("Column `value` must be numeric.")
  }
  index <- unique(data[, ids, with = FALSE])
  n <- nrow(index)
  k <- as.integer(max(draw))
  key_all <- do.call(paste, c(as.list(data[, ids, with = FALSE]), sep = "\r"))
  key_idx <- do.call(paste, c(as.list(index), sep = "\r"))
  cell <- match(key_all, key_idx)
  pos <- cell + (as.integer(draw) - 1L) * n
  if (nrow(data) != n * k || anyDuplicated(pos)) {
    .dl_abort(c("A long table of draws must hold every draw 1..", k,
                " exactly once for each of its ", n, " cells; it has ",
                nrow(data), " rows instead of ", n * k, "."))
  }
  value <- matrix(NA_real_, nrow = n, ncol = k)
  value[pos] <- data[["value"]]
  list(index = index, value = value)
}

#' Print a container of draws
#'
#' @param x A `demolite_draws` object.
#' @param ... Ignored.
#'
#' Not registered through `NAMESPACE`: see `.dl_register_print()`.
#'
#' @return `x`, invisibly.
#' @noRd
#' @exportS3Method NULL
print.demolite_draws <- function(x, ...) {
  cat("<demolite_draws> ", x$meta$measure,
      if (!is.na(x$meta$unit)) paste0(" [", x$meta$unit, "]"), "\n", sep = "")
  cat("  cells: ", nrow(x$index), " (", paste(names(x$index), collapse = ", "),
      ")\n", sep = "")
  cat("  draws: ", x$meta$n_draws, "; seed: ", x$meta$seed, "; stream: ",
      x$meta$stream, "; contract version: ", x$meta$version, "\n", sep = "")
  invisible(x)
}

#' Register the print methods of the shared classes, once per session
#'
#' The three packages of the family carry identical copies of the print
#' methods. Each package calls this function from `.onLoad()`; the first one
#' loaded registers the methods and the others find them already registered,
#' so that loading two packages does not overwrite anything.
#'
#' @return `NULL`, invisibly.
#' @noRd
.dl_register_print <- function() {
  host <- parent.env(environment())
  # The registry of S3 methods of the generic, not the functions visible from
  # here: the copy of this very package is always visible and proves nothing.
  registry <- get(".__S3MethodsTable__.", envir = baseenv())
  for (cls in c("demolite_draws", "demolite_country")) {
    name <- paste0("print.", cls)
    if (!exists(name, envir = registry, inherits = FALSE) &&
          exists(name, envir = host, inherits = FALSE)) {
      registerS3method("print", cls, get(name, envir = host), envir = baseenv())
    }
  }
  invisible(NULL)
}

#' Convert a container of draws to a long table
#'
#' Returns one row per cell and draw. This is a conversion for exchange and
#' inspection, not a working format: a long table multiplies memory by the
#' number of draws.
#'
#' @param x A `demolite_draws` object created with [draws_new()].
#'
#' @return A data.table with the columns of `x$index`, a column `draw`
#'   (integer, `1` to `K`) and a column `value` (numeric). Rows are ordered by
#'   draw and, within a draw, in the order of the cells of `x`.
#'
#' @section GBD equivalent:
#' **Own.** Long tables with a `draw` column are a common exchange layout; the
#' GBD study itself publishes wide tables (see [as_draws_wide()]).
#'
#' @section Method:
#' No estimation is involved; the matrix is unrolled column by column.
#'
#' @examples
#' index <- data.frame(location_id = c(1, 2), sex_id = c(1, 1))
#' x <- draws_new(index, matrix(1:6 / 10, nrow = 2), measure = "completeness")
#' as_draws_long(x)
#'
#' @family draws
#' @export
as_draws_long <- function(x) {
  .check_draws(x, "x")
  n <- nrow(x$index)
  k <- ncol(x$value)
  out <- x$index[rep(seq_len(n), times = k), ]
  data.table::set(out, j = "draw", value = rep(seq_len(k), each = n))
  data.table::set(out, j = "value", value = as.vector(x$value))
  out[]
}

#' Convert a container of draws to a wide table
#'
#' Returns one row per cell and one column per draw, with the column names
#' used by the Global Burden of Disease (GBD) study: `draw_0`, ...,
#' `draw_{K-1}`.
#'
#' @param x A `demolite_draws` object created with [draws_new()].
#'
#' @return A data.table with the columns of `x$index` followed by `draw_0`,
#'   ..., `draw_{K-1}` (numeric), one row per cell in the order of `x`. Draw
#'   `k` of the container is column `draw_{k-1}`.
#'
#' @section GBD equivalent:
#' **Replicates** the wide exchange layout of the GBD study (zero-based draw
#' columns). Only the layout is replicated; no GBD code or data is used.
#'
#' @section Method:
#' No estimation is involved; the matrix is bound to the table of cells.
#'
#' @examples
#' index <- data.frame(location_id = c(1, 2), sex_id = c(1, 1))
#' x <- draws_new(index, matrix(1:6 / 10, nrow = 2), measure = "completeness")
#' as_draws_wide(x)
#'
#' @family draws
#' @export
as_draws_wide <- function(x) {
  .check_draws(x, "x")
  out <- data.table::copy(x$index)
  k <- ncol(x$value)
  for (j in seq_len(k)) {
    data.table::set(out, j = paste0("draw_", j - 1L), value = x$value[, j])
  }
  out[]
}

#' Summarise draws by cell
#'
#' Reduces each cell to its mean and to two percentiles of its draws. Across
#' the family the central estimate is the mean of the draws and the 95%
#' uncertainty interval runs from the 2.5th to the 97.5th percentile.
#'
#' @param x A `demolite_draws` object created with [draws_new()].
#' @param probs Numeric vector of length two. Lower and upper probabilities of
#'   the interval, `c(0.025, 0.975)` by default.
#' @param na_rm Logical. If `FALSE` (default), a cell with a missing draw gets
#'   a missing summary; if `TRUE`, missing draws are dropped cell by cell.
#'
#' @return A data.table with the columns of `x$index` and the numeric columns
#'   `mean`, `lower` and `upper`, one row per cell in the order of `x`, in the
#'   unit of the container.
#'
#' @section GBD equivalent:
#' **Replicates** the convention of the GBD study: mean of the draws as the
#' point estimate and the 2.5th and 97.5th percentiles as the 95% uncertainty
#' interval (GBD 2023 Demographics Collaborators, 2025).
#'
#' @section Method:
#' For a cell with draws \eqn{x_1, \dots, x_K}: mean
#' \eqn{\bar{x} = K^{-1} \sum_k x_k}; percentiles by linear interpolation of
#' the order statistics (type 7 of [stats::quantile()]). The summary of a sum
#' is not the sum of the summaries: aggregate draws first, then summarise.
#'
#' @references
#' GBD 2023 Demographics Collaborators (2025). Global age-sex-specific
#' all-cause mortality and life expectancy estimates for 204 countries and
#' territories. *The Lancet*, 406, 1731-1810.
#' \doi{10.1016/S0140-6736(25)01330-3}
#'
#' @examples
#' index <- data.frame(location_id = c(1, 2))
#' value <- rbind(seq(0.70, 0.90, length.out = 41), seq(0.50, 0.60, length.out = 41))
#' x <- draws_new(index, value, measure = "completeness", unit = "proportion")
#' draws_summarise(x)
#'
#' @family draws
#' @export
draws_summarise <- function(x, probs = c(0.025, 0.975), na_rm = FALSE) {
  .check_draws(x, "x")
  .check_probs(probs)
  .check_flag(na_rm, "na_rm")
  q <- apply(x$value, 1L, function(v) {
    if (anyNA(v)) {
      if (!na_rm) {
        return(c(NA_real_, NA_real_))
      }
      v <- v[!is.na(v)]
    }
    if (!length(v)) {
      return(c(NA_real_, NA_real_))
    }
    stats::quantile(v, probs = probs, names = FALSE, type = 7)
  })
  out <- data.table::copy(x$index)
  m <- rowMeans(x$value, na.rm = na_rm)
  m[is.nan(m)] <- NA_real_
  data.table::set(out, j = "mean", value = m)
  data.table::set(out, j = "lower", value = q[1L, ])
  data.table::set(out, j = "upper", value = q[2L, ])
  out[]
}

#' Derive the seed of a cell
#'
#' Maps the caller's seed, the name of a random stream and the key of a cell to
#' a seed for that cell alone. Because the seed depends only on those three
#' things, draw `k` of a cell is the same whatever the order of the cells and
#' however the work is split across processes.
#'
#' @param seed Whole number. The caller's seed. The packages never choose one.
#' @param stream String. Name of the random stream, for instance
#'   `"census_error"`. Different streams give unrelated seeds for the same
#'   cell.
#' @param key Data frame or named list of equal-length vectors identifying the
#'   cells (one row per cell), or `NULL` for a factor common to all cells, in
#'   which case the seed depends only on `seed` and `stream`. Column order
#'   does not matter; column names and values do. Numbers are compared after
#'   rounding to 15 significant digits.
#'
#' @return An integer vector with one seed per row of `key` (length one when
#'   `key` is `NULL`), suitable for [with_cell_seed()].
#'
#' @section GBD equivalent:
#' **Own.** The published GBD methods do not describe how seeds are derived.
#' The scheme exists so that results do not depend on execution order.
#'
#' @section Method:
#' The text `seed|stream|name=value|...` (names sorted in the C locale) is
#' hashed with the 32-bit FNV-1a function followed by the final mixing step of
#' MurmurHash3, and the result is shifted to the range of R integers. The
#' hash is implemented in base R arithmetic, so it gives the same seeds on
#' every platform. Limitation: with 32 bits, two of `N` cells share a seed with
#' probability of about \eqn{N^2 / 2^{33}}; such cells would receive identical
#' random numbers within a stream.
#'
#' @examples
#' cells <- data.frame(location_id = c(1, 2, 3), sex_id = c(1, 1, 2))
#' cell_seed(2024, "census_error", cells)
#'
#' # The seed of a cell does not depend on the order of the cells
#' rev(cell_seed(2024, "census_error", cells[3:1, ]))
#'
#' # A factor common to all cells
#' cell_seed(2024, "common_level")
#'
#' @family draws
#' @export
cell_seed <- function(seed, stream, key = NULL) {
  seed <- .check_seed(seed)
  .check_string(stream, "stream")
  head <- paste0(seed, "|", stream)
  if (is.null(key)) {
    return(.dl_hash_to_seed(.dl_fmix32(.dl_fnv1a32(head))))
  }
  if (!is.list(key) || is.null(names(key)) || !length(key) ||
        any(!nzchar(names(key))) || anyDuplicated(names(key))) {
    .dl_abort("`key` must be a data frame or a named list with unique names.")
  }
  n <- unique(lengths(key))
  if (length(n) != 1L) {
    .dl_abort("All elements of `key` must have the same length.")
  }
  nm <- sort(names(key), method = "radix")
  parts <- lapply(nm, function(col) paste0(col, "=", .dl_key_text(key[[col]], col)))
  text <- paste0(head, "|", do.call(paste, c(parts, sep = "|")))
  .dl_hash_to_seed(.dl_fmix32(.dl_fnv1a32(text)))
}

#' Canonical text of a key column
#'
#' @param x Atomic vector. A column of a cell key.
#' @param col String. Column name used in the error message.
#'
#' @return A character vector as long as `x`.
#' @noRd
.dl_key_text <- function(x, col) {
  if (is.factor(x)) {
    x <- as.character(x)
  }
  if (is.character(x) || is.logical(x)) {
    return(ifelse(is.na(x), "NA", as.character(x)))
  }
  if (is.numeric(x)) {
    return(sprintf("%.15g", as.double(x)))
  }
  .dl_abort(c("Column `", col, "` of `key` must be numeric, character, ",
              "logical or factor."))
}

#' Multiply 32-bit unsigned values modulo 2^32 in double arithmetic
#'
#' @param a Numeric vector of whole numbers in `[0, 2^32)`.
#' @param b A whole number in `[0, 2^32)`.
#'
#' @return Numeric vector, `(a * b) mod 2^32`.
#' @noRd
.dl_mul32 <- function(a, b) {
  ah <- a %/% 65536
  al <- a %% 65536
  bh <- b %/% 65536
  bl <- b %% 65536
  (((ah * bl + al * bh) %% 65536) * 65536 + al * bl) %% 4294967296
}

#' Bitwise exclusive-or of 32-bit unsigned values held as doubles
#'
#' @param a,b Numeric vectors of whole numbers in `[0, 2^32)`.
#'
#' @return Numeric vector of whole numbers in `[0, 2^32)`.
#' @noRd
.dl_xor32 <- function(a, b) {
  hi <- bitwXor(as.integer(a %/% 65536), as.integer(b %/% 65536))
  lo <- bitwXor(as.integer(a %% 65536), as.integer(b %% 65536))
  hi * 65536 + lo
}

#' 32-bit FNV-1a hash of strings
#'
#' @param text Character vector.
#'
#' @return Numeric vector of whole numbers in `[0, 2^32)`, one per string.
#' @noRd
.dl_fnv1a32 <- function(text) {
  text <- enc2utf8(text)
  n <- length(text)
  len <- nchar(text, type = "bytes")
  bytes <- as.integer(charToRaw(paste(text, collapse = "")))
  start <- cumsum(c(0L, len))[seq_len(n)]
  h <- rep(2166136261, n)
  for (j in seq_len(max(c(len, 0L)))) {
    act <- which(len >= j)
    hj <- h[act]
    lo <- hj %% 256
    hj <- hj - lo + bitwXor(as.integer(lo), bytes[start[act] + j])
    h[act] <- .dl_mul32(hj, 16777619)
  }
  h
}

#' Final mixing step of MurmurHash3 (32 bits)
#'
#' @param h Numeric vector of whole numbers in `[0, 2^32)`.
#'
#' @return Numeric vector of whole numbers in `[0, 2^32)`.
#' @noRd
.dl_fmix32 <- function(h) {
  h <- .dl_xor32(h, h %/% 65536)
  h <- .dl_mul32(h, 2246822507)
  h <- .dl_xor32(h, h %/% 8192)
  h <- .dl_mul32(h, 3266489909)
  .dl_xor32(h, h %/% 65536)
}

#' Map a 32-bit hash to a valid R seed
#'
#' @param h Numeric vector of whole numbers in `[0, 2^32)`.
#'
#' @return Integer vector without missing values.
#' @noRd
.dl_hash_to_seed <- function(h) {
  s <- h - 2147483648
  s[s == -2147483648] <- 0
  as.integer(s)
}

#' Evaluate code with the seed of a cell, restoring the generator afterwards
#'
#' Sets the random number generator to a known kind and seed, evaluates an
#' expression and then puts the generator back exactly as it was, so that the
#' caller's own random stream is not disturbed.
#'
#' @param seed Whole number. Usually one element of [cell_seed()].
#' @param expr Expression to evaluate with that seed.
#'
#' @return The value of `expr`.
#'
#' @section GBD equivalent:
#' **Own.** See [cell_seed()].
#'
#' @section Method:
#' The state of the generator (the object `.Random.seed` of the global
#' environment, which also encodes the kind of generator) is saved; the
#' generator is set to Mersenne-Twister with inversion for normal deviates and
#' rejection sampling, seeded with `seed`; `expr` is evaluated; and the saved
#' state is restored on exit, even if `expr` fails. No fixed seed is ever
#' used.
#'
#' @examples
#' s <- cell_seed(2024, "example", data.frame(location_id = 1))
#' a <- with_cell_seed(s, stats::rnorm(3))
#' b <- with_cell_seed(s, stats::rnorm(3))
#' identical(a, b)
#'
#' @family draws
#' @export
with_cell_seed <- function(seed, expr) {
  seed <- .check_seed(seed)
  genv <- globalenv()
  old_seed <- get0(".Random.seed", envir = genv, inherits = FALSE)
  old_kind <- RNGkind()
  on.exit({
    if (is.null(old_seed)) {
      RNGkind(old_kind[1L], old_kind[2L], old_kind[3L])
      if (exists(".Random.seed", envir = genv, inherits = FALSE)) {
        rm(".Random.seed", envir = genv)
      }
    } else {
      assign(".Random.seed", old_seed, envir = genv)
    }
  }, add = TRUE)
  set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion",
           sample.kind = "Rejection")
  expr
}

#' Generate draws cell by cell with per-cell seeds
#'
#' Calls a generator once per cell, each time under the seed of that cell, and
#' collects the results in a container. Draw `k` of a cell therefore depends
#' only on the caller's seed, the stream name and the key of the cell.
#'
#' @param index Data frame or data.table with one row per cell.
#' @param n_draws Whole number. Number of draws per cell.
#' @param seed Whole number. The caller's seed.
#' @param stream String. Name of the random stream (see [cell_seed()]).
#' @param generator Function `function(n, cell)` that returns `n` numbers for
#'   one cell; `cell` is a named list with the values of that row of `index`.
#' @param measure String. What the values are (see [draws_new()]).
#' @param unit String. Unit of the values; `NA` when not stated.
#' @param key_cols Character vector. Columns of `index` that identify the cell
#'   for seeding; all columns by default.
#'
#' @return A `demolite_draws` object (see [draws_new()]) with `n_draws` columns
#'   and the cells of `index` in the same order.
#'
#' @section GBD equivalent:
#' **Own.** See [cell_seed()].
#'
#' @section Method:
#' For each cell `i`, the seed is `cell_seed(seed, stream, key_i)` and the
#' generator runs inside [with_cell_seed()], which restores the state of the
#' random number generator. Memory: one double per cell and draw.
#'
#' @examples
#' cells <- data.frame(location_id = 1:3, sd = c(0.1, 0.2, 0.3))
#' gen <- function(n, cell) stats::rnorm(n, mean = 0, sd = cell$sd)
#' a <- draws_generate(cells, 5, seed = 2024, stream = "noise", gen,
#'                     measure = "error", key_cols = "location_id")
#' b <- draws_generate(cells[3:1, ], 5, seed = 2024, stream = "noise", gen,
#'                     measure = "error", key_cols = "location_id")
#' identical(a$value, b$value[3:1, ])
#'
#' @family draws
#' @export
draws_generate <- function(index, n_draws, seed, stream, generator, measure,
                           unit = NA_character_, key_cols = names(index)) {
  .check_table(index, "index")
  n_draws <- .check_count(n_draws, "n_draws")
  seed <- .check_seed(seed)
  .check_string(stream, "stream")
  if (!is.function(generator)) {
    .dl_abort("`generator` must be a function `function(n, cell)`.")
  }
  if (!is.character(key_cols) || !length(key_cols) ||
        !all(key_cols %in% names(index))) {
    .dl_abort("`key_cols` must name one or more columns of `index`.")
  }
  index <- .dl_index_clean(index)
  seeds <- cell_seed(seed, stream, as.list(index)[key_cols])
  value <- matrix(NA_real_, nrow = nrow(index), ncol = n_draws)
  for (i in seq_len(nrow(index))) {
    cell <- as.list(index[i, ])
    v <- with_cell_seed(seeds[i], generator(n_draws, cell))
    if (!is.numeric(v) || length(v) != n_draws) {
      .dl_abort(c("`generator` must return ", n_draws, " numbers per cell; ",
                  "for row ", i, " it returned ", length(v), " value(s) of ",
                  "type ", typeof(v), "."))
    }
    value[i, ] <- v
  }
  draws_new(index, value, measure = measure, unit = unit, seed = seed,
            stream = stream)
}
