# ---
# repo: demography-lite
# file: standalone-draws.R
# last-updated: 2026-11-14
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
# cells x draws matrix), its conversions, its summary, and the random state of
# a cell.
#
# None of these functions is exported. Each package of the family exports thin
# wrappers under its own prefix (see standalone/prefixed-shared.R), so that
# attaching two packages of the family masks nothing.

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
#' Shared infrastructure, not exported: every package of the family exports a
#' thin wrapper of it under its own prefix, `<prefix>_draws_new()`.
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
#' @family draws
#' @keywords internal
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
#' Registered through `NAMESPACE` by every package of the family, never
#' exported as a function; see the comment after the function for why its
#' enclosing environment is replaced.
#'
#' @return `x`, invisibly.
#' @noRd
#' @exportS3Method
print.demolite_draws <- function(x, ...) {
  cat("<demolite_draws> ", x$meta$measure,
      if (!is.na(x$meta$unit)) paste0(" [", x$meta$unit, "]"), "\n", sep = "")
  cat("  cells: ", nrow(x$index), " (", paste(names(x$index), collapse = ", "),
      ")\n", sep = "")
  cat("  draws: ", x$meta$n_draws, "; seed: ", x$meta$seed, "; stream: ",
      x$meta$stream, "; contract version: ", x$meta$version, "\n", sep = "")
  invisible(x)
}

# Why the enclosing environment of the method is replaced: the three packages
# of the family carry identical copies of the methods that print the shared
# classes, and each of them registers its copy through `NAMESPACE`. When a
# method for the same generic and class is already registered, R compares the
# two and reports an overwrite if they differ; two copies of the same code
# differ only in their enclosure, the namespace of their own package. With the
# base environment as enclosure the copies are the same object, so loading the
# three packages together registers the method once, says nothing and does not
# depend on the order of loading. These methods call base functions only, and
# they contain no loop: R caches the compiled form of a loop inside the syntax
# tree the first time the function runs, so a copy that has already printed
# something would stop being identical to a copy that has not.
environment(print.demolite_draws) <- baseenv()

#' Convert a container of draws to a long table
#'
#' Returns one row per cell and draw. This is a conversion for exchange and
#' inspection, not a working format: a long table multiplies memory by the
#' number of draws.
#'
#' Shared infrastructure, not exported: every package of the family exports a
#' thin wrapper of it under its own prefix, `<prefix>_draws_long()`.
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
#' @family draws
#' @keywords internal
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
#' Shared infrastructure, not exported: every package of the family exports a
#' thin wrapper of it under its own prefix, `<prefix>_draws_wide()`.
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
#' @family draws
#' @keywords internal
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
#' Shared infrastructure, not exported: every package of the family exports a
#' thin wrapper of it under its own prefix, `<prefix>_summarise()`.
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
#' @family draws
#' @keywords internal
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

#' Derive the random state of a cell
#'
#' Maps the caller's seed, the name of a random stream and the key of a cell to
#' a state of the L'Ecuyer-CMRG random number generator for that cell alone.
#' Because the state is derived from the key and from nothing else, draw `k` of
#' a cell is the same whatever the order of the cells, however the work is
#' split across processes, and whatever cells are added later: a new year of
#' data does not move the draws of the cells already computed.
#'
#' Shared infrastructure, not exported: the packages of the family use it
#' inside `<prefix>_draws_generate()`, which is what a user calls.
#'
#' @param seed Whole number. The caller's seed. The packages never choose one.
#' @param stream String. Name of the random stream, for instance
#'   `"census_error"`. Different streams give unrelated states for the same
#'   cell.
#' @param key Data frame or named list of equal-length vectors identifying the
#'   cells (one row per cell), or `NULL` for a factor common to all cells, in
#'   which case the state depends only on `seed` and `stream`. Column order
#'   does not matter; column names and values do. Numbers are compared after
#'   rounding to 15 significant digits.
#'
#' @return An integer matrix with one row per row of `key` (one row when `key`
#'   is `NULL`) and six columns: the six words of the state of the
#'   L'Ecuyer-CMRG generator, encoded as `.Random.seed` encodes them, so that a
#'   word of \eqn{2^{31}} or more appears as a negative integer. One row is
#'   what [with_cell_seed()] takes.
#'
#' @section GBD equivalent:
#' **Own.** The published GBD methods do not describe how seeds are derived.
#' The scheme exists so that results do not depend on execution order.
#'
#' @section Method:
#' The text `seed|stream|name=value|...` (names sorted in the C locale) is
#' hashed once per word of the state, each time behind its own salt (`1#` to
#' `6#`), with the 32-bit FNV-1a function followed by the final mixing step of
#' MurmurHash3. The first three words are then reduced modulo
#' \eqn{m_1 = 4294967087} and the last three modulo \eqn{m_2 = 4294944443},
#' the moduli of the two components of the generator. A trio of zeros, which
#' would stop its half of the generator, becomes a one in its first word; the
#' single word \eqn{2^{31}}, which R cannot hold in an integer, becomes
#' \eqn{2^{31} + 1}. The hash is implemented in base R arithmetic, so it gives
#' the same state on every platform.
#'
#' Two cells share a state only if all six words agree. Taking the words as
#' independent and uniform, the probability that some two of \eqn{N} cells
#' share a state is about
#' \deqn{N (N - 1) / (2 m_1^3 m_2^3) \approx N^2 / 2^{193}}
#' which for a grid of \eqn{N = 168000} cells (25 areas x 2 sexes x 96 ages x
#' 35 years) is about \eqn{2 \times 10^{-48}}: a collision is not expected
#' ever. The same formula for the single 32-bit seed used up to version
#' 0.0.0.9000, where the space was \eqn{2^{32}}, gives 3.3 expected pairs and a
#' probability of 96% for that grid; the cells of such a pair received
#' identical random numbers within a stream. FNV-1a is not a cryptographic
#' hash, so labels could be built on purpose to collide; the labels of cells
#' are not chosen by an adversary.
#'
#' @family draws
#' @keywords internal
cell_seed <- function(seed, stream, key = NULL) {
  seed <- .check_seed(seed)
  .check_string(stream, "stream")
  head <- paste0(seed, "|", stream)
  if (is.null(key)) {
    return(.dl_text_to_state(head))
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
  .dl_text_to_state(text)
}

#' Map the label of a cell to a state of the L'Ecuyer-CMRG generator
#'
#' @param text Character vector. One canonical label per cell.
#'
#' @return An integer matrix with one row per label and six columns, encoded
#'   as `.Random.seed` encodes the state.
#' @noRd
.dl_text_to_state <- function(text) {
  m1 <- 4294967087
  m2 <- 4294944443
  n <- length(text)
  state <- matrix(0, nrow = n, ncol = 6L)
  for (j in seq_len(6L)) {
    # One salt per word: the six words of a cell come from six hashes.
    h <- .dl_fmix32(.dl_fnv1a32(paste0(j, "#", text)))
    state[, j] <- h %% if (j <= 3L) m1 else m2
  }
  zero1 <- rowSums(state[, 1:3, drop = FALSE]) == 0
  zero2 <- rowSums(state[, 4:6, drop = FALSE]) == 0
  state[zero1, 1L] <- 1
  state[zero2, 4L] <- 1
  # R integers cannot hold 2^31; the next value is as good a word.
  state[state == 2147483648] <- 2147483649
  big <- state >= 2147483648
  state[big] <- state[big] - 4294967296
  storage.mode(state) <- "integer"
  dimnames(state) <- NULL
  state
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

#' Evaluate code with the random state of a cell, restoring the generator
#'
#' Sets the random number generator to a known kind and state, evaluates an
#' expression and then puts the generator back exactly as it was, so that the
#' caller's own random stream is not disturbed.
#'
#' Shared infrastructure, not exported: see [cell_seed()].
#'
#' @param state Integer vector of length six, or a matrix with one row and six
#'   columns: the state of a cell, usually one row of [cell_seed()].
#' @param expr Expression to evaluate with that state.
#'
#' @return The value of `expr`.
#'
#' @section GBD equivalent:
#' **Own.** See [cell_seed()].
#'
#' @section Method:
#' The state of the generator (the object `.Random.seed` of the global
#' environment, which also encodes the kind of generator) is saved; the
#' generator is set to L'Ecuyer-CMRG with inversion for normal deviates and
#' rejection sampling, and its six words are overwritten with `state`; `expr`
#' is evaluated; and the saved state is restored on exit, even if `expr`
#' fails. The words are written rather than passed to [set.seed()], because
#' [set.seed()] takes one number of 32 bits and would throw away the width of
#' the state. No fixed seed is ever used.
#'
#' @family draws
#' @keywords internal
with_cell_seed <- function(state, expr) {
  state <- .check_cell_state(state)
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
  RNGkind("L'Ecuyer-CMRG", normal.kind = "Inversion", sample.kind = "Rejection")
  current <- get(".Random.seed", envir = genv, inherits = FALSE)
  current[2:7] <- state
  assign(".Random.seed", current, envir = genv)
  expr
}

#' Generate draws cell by cell with per-cell seeds
#'
#' Calls a generator once per cell, each time under the random state of that
#' cell, and collects the results in a container. Draw `k` of a cell therefore
#' depends only on the caller's seed, the stream name and the key of the cell.
#'
#' Shared infrastructure, not exported: every package of the family exports a
#' thin wrapper of it under its own prefix, `<prefix>_draws_generate()`.
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
#' For each cell `i`, the state is row `i` of
#' `cell_seed(seed, stream, key_i)` and the generator runs inside
#' [with_cell_seed()], which restores the state of the random number
#' generator. Memory: one double per cell and draw.
#'
#' @family draws
#' @keywords internal
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
  states <- cell_seed(seed, stream, as.list(index)[key_cols])
  value <- matrix(NA_real_, nrow = nrow(index), ncol = n_draws)
  for (i in seq_len(nrow(index))) {
    cell <- as.list(index[i, ])
    v <- with_cell_seed(states[i, ], generator(n_draws, cell))
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
