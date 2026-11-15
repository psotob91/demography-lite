# ---
# repo: demography-lite
# file: standalone-checks.R
# last-updated: 2026-11-14
# license: MIT (see the LICENSE file at the repository root)
# imports: none (base R only)
# ---
#
# Single-source file. The copies inside the packages are named
# `R/import-standalone-checks.R` and must stay byte-identical to
# `standalone/standalone-checks.R`. Do not edit a copy: edit the source and run
# `Rscript tools/sync-standalone.R`.
#
# Contents: classed conditions and input validators shared by the family.
# Every validator returns its (possibly coerced) input invisibly on success and
# signals a condition of class `<prefix>_error_input` and `demolite_error_input`
# on failure, where `<prefix>` is the function prefix of the host package.

#' Function prefix of the host package
#'
#' Looks up `.demolite_prefix`, a string that each package of the family
#' defines once. It is used to build package-specific condition classes.
#'
#' @return A string; `"demolite"` when the host does not define a prefix.
#' @noRd
.dl_prefix <- function() {
  get0(".demolite_prefix", envir = parent.env(environment()), inherits = TRUE,
       ifnotfound = "demolite")
}

#' Signal a classed error
#'
#' @param message Character vector. Pasted together to form the message.
#' @param class String. Suffix of the condition class, `"input"` by default.
#' @param call Call or `NULL`. Call stored in the condition.
#'
#' @return Does not return; signals a condition with classes
#'   `<prefix>_error_<class>`, `<prefix>_error`, `demolite_error_<class>`,
#'   `demolite_error`, `error` and `condition`.
#' @noRd
.dl_abort <- function(message, class = "input", call = NULL) {
  prefix <- .dl_prefix()
  classes <- unique(c(
    paste0(prefix, "_error_", class), paste0(prefix, "_error"),
    paste0("demolite_error_", class), "demolite_error", "error", "condition"
  ))
  stop(structure(
    class = classes,
    list(message = paste(message, collapse = ""), call = call)
  ))
}

#' Signal that a function is a skeleton
#'
#' @param what String. Name of the function that is not implemented yet.
#'
#' @return Does not return; signals a condition of class
#'   `<prefix>_error_not_implemented` and `demolite_error_not_implemented`.
#' @noRd
.dl_not_implemented <- function(what) {
  .dl_abort(
    c("`", what, "()` is not implemented yet: this development version only ",
      "provides its interface, documentation and input validation."),
    class = "not_implemented"
  )
}

#' Signal a classed warning
#'
#' @param message Character vector. Pasted together to form the message.
#' @param class String. Suffix of the condition class.
#'
#' @return `NULL`, invisibly; signals a condition with classes
#'   `<prefix>_warning_<class>`, `<prefix>_warning`,
#'   `demolite_warning_<class>`, `demolite_warning`, `warning` and
#'   `condition`.
#' @noRd
.dl_warn <- function(message, class) {
  prefix <- .dl_prefix()
  classes <- unique(c(
    paste0(prefix, "_warning_", class), paste0(prefix, "_warning"),
    paste0("demolite_warning_", class), "demolite_warning", "warning",
    "condition"
  ))
  warning(structure(
    class = classes,
    list(message = paste(message, collapse = ""), call = NULL)
  ))
  invisible(NULL)
}

#' Check a single number
#'
#' @param x Object to check.
#' @param arg String. Argument name used in the message.
#' @param lower,upper Numbers. Inclusive bounds.
#' @param null_ok Logical. Whether `NULL` is accepted.
#'
#' @return `x`, invisibly.
#' @noRd
.check_number <- function(x, arg, lower = -Inf, upper = Inf, null_ok = FALSE) {
  if (is.null(x) && null_ok) {
    return(invisible(x))
  }
  if (!is.numeric(x) || length(x) != 1L || is.na(x)) {
    .dl_abort(c("`", arg, "` must be a single non-missing number."))
  }
  if (x < lower || x > upper) {
    .dl_abort(c("`", arg, "` must be between ", format(lower), " and ",
                format(upper), "; got ", format(x), "."))
  }
  invisible(x)
}

#' Check a single whole number and return it as an integer
#'
#' @param x Object to check.
#' @param arg String. Argument name used in the message.
#' @param min,max Numbers. Inclusive bounds.
#'
#' @return `x` as an integer, invisibly.
#' @noRd
.check_count <- function(x, arg, min = 1, max = .Machine$integer.max) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x != trunc(x)) {
    .dl_abort(c("`", arg, "` must be a single whole number."))
  }
  if (x < min || x > max) {
    .dl_abort(c("`", arg, "` must be between ", format(min), " and ",
                format(max), "; got ", format(x), "."))
  }
  invisible(as.integer(x))
}

#' Check a numeric vector
#'
#' @param x Object to check.
#' @param arg String. Argument name used in the message.
#' @param len Whole number or `NULL`. Required length.
#' @param lower,upper Numbers. Inclusive bounds for every element.
#' @param any_missing Logical. Whether missing values are accepted.
#' @param null_ok Logical. Whether `NULL` is accepted.
#'
#' @return `x`, invisibly.
#' @noRd
.check_numeric <- function(x, arg, len = NULL, lower = -Inf, upper = Inf,
                           any_missing = FALSE, null_ok = FALSE) {
  if (is.null(x) && null_ok) {
    return(invisible(x))
  }
  if (!is.numeric(x) || is.matrix(x) || length(x) == 0L) {
    .dl_abort(c("`", arg, "` must be a non-empty numeric vector."))
  }
  if (!is.null(len) && length(x) != len) {
    .dl_abort(c("`", arg, "` must have length ", len, "; it has length ",
                length(x), "."))
  }
  if (!any_missing && anyNA(x)) {
    .dl_abort(c("`", arg, "` must not contain missing values (first at ",
                "position ", which(is.na(x))[1L], ")."))
  }
  bad <- which(x < lower | x > upper)
  if (length(bad)) {
    .dl_abort(c("`", arg, "` must lie between ", format(lower), " and ",
                format(upper), " (first violation at position ", bad[1L],
                ")."))
  }
  invisible(x)
}

#' Check that several vectors have the same length
#'
#' @param ... Named vectors; `NULL` entries are ignored.
#'
#' @return The common length, invisibly.
#' @noRd
.check_same_length <- function(...) {
  x <- list(...)
  x <- x[!vapply(x, is.null, logical(1))]
  n <- lengths(x)
  if (length(unique(n)) > 1L) {
    .dl_abort(c(
      "Arguments must have the same length: ",
      paste0("`", names(n), "` has ", n, collapse = ", "), "."
    ))
  }
  invisible(if (length(n)) n[[1L]] else 0L)
}

#' Check a single string
#'
#' @param x Object to check.
#' @param arg String. Argument name used in the message.
#'
#' @return `x`, invisibly.
#' @noRd
.check_string <- function(x, arg) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    .dl_abort(c("`", arg, "` must be a single non-empty string."))
  }
  invisible(x)
}

#' Check a single logical value
#'
#' @param x Object to check.
#' @param arg String. Argument name used in the message.
#'
#' @return `x`, invisibly.
#' @noRd
.check_flag <- function(x, arg) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    .dl_abort(c("`", arg, "` must be `TRUE` or `FALSE`."))
  }
  invisible(x)
}

#' Check that a string is one of a set of choices
#'
#' @param x Object to check. A vector equal to `choices` (an unevaluated
#'   default) is resolved to its first element.
#' @param arg String. Argument name used in the message.
#' @param choices Character vector of accepted values.
#' @param several Logical. Whether more than one choice may be selected.
#'
#' @return The selected choice or choices, invisibly.
#' @noRd
.check_choice <- function(x, arg, choices, several = FALSE) {
  if (!several && identical(x, choices)) {
    x <- choices[1L]
  }
  ok <- is.character(x) && length(x) >= 1L && !anyNA(x) &&
    all(x %in% choices) && (several || length(x) == 1L)
  if (!ok) {
    .dl_abort(c("`", arg, "` must be ", if (several) "a subset of " else
      "one of ", paste0("\"", choices, "\"", collapse = ", "), "."))
  }
  invisible(x)
}

#' Check a table and its required columns
#'
#' @param x Object to check.
#' @param arg String. Argument name used in the message.
#' @param cols Character vector of required column names, or `NULL`.
#' @param min_rows Whole number. Minimum number of rows.
#'
#' @return `x`, invisibly.
#' @noRd
.check_table <- function(x, arg, cols = NULL, min_rows = 1L) {
  if (!is.data.frame(x)) {
    .dl_abort(c("`", arg, "` must be a data frame or a data.table."))
  }
  missing_cols <- setdiff(cols, names(x))
  if (length(missing_cols)) {
    .dl_abort(c("`", arg, "` lacks the required column(s): ",
                paste0("`", missing_cols, "`", collapse = ", "), "."))
  }
  if (nrow(x) < min_rows) {
    .dl_abort(c("`", arg, "` must have at least ", min_rows, " row(s); it ",
                "has ", nrow(x), "."))
  }
  invisible(x)
}

#' Check a user seed and return it as an integer
#'
#' @param seed Object to check. A single whole number that fits in an integer.
#' @param arg String. Argument name used in the message.
#'
#' @return `seed` as an integer, invisibly.
#' @noRd
.check_seed <- function(seed, arg = "seed") {
  if (missing(seed) || !is.numeric(seed) || length(seed) != 1L ||
        is.na(seed) || seed != trunc(seed) ||
        abs(seed) > .Machine$integer.max) {
    .dl_abort(c("`", arg, "` must be a single whole number supplied by the ",
                "caller; the packages never choose a seed."))
  }
  invisible(as.integer(seed))
}

#' Check the random state of a cell and return its six words
#'
#' @param state Object to check. The six words of the state of the
#'   L'Ecuyer-CMRG generator, as [cell_seed()] returns them: a length-six
#'   vector of whole numbers, or a matrix with one row and six columns.
#' @param arg String. Argument name used in the message.
#'
#' @return `state` as an integer vector of length six, invisibly.
#' @noRd
.check_cell_state <- function(state, arg = "state") {
  if (is.matrix(state) && nrow(state) == 1L) {
    state <- as.vector(state)
  }
  ok <- is.numeric(state) && length(state) == 6L && !anyNA(state) &&
    all(state == trunc(state)) && all(abs(state) <= 2147483647)
  if (ok) {
    # `.Random.seed` stores the six unsigned words as signed integers.
    u <- ifelse(state < 0, state + 4294967296, state)
    ok <- all(u[1:3] < 4294967087) && all(u[4:6] < 4294944443) &&
      sum(u[1:3]) > 0 && sum(u[4:6]) > 0
  }
  if (!ok) {
    .dl_abort(c("`", arg, "` must be the six words of the state of a cell, ",
                "as `cell_seed()` returns them: the first three below ",
                "4294967087, the last three below 4294944443, neither trio ",
                "all zero."))
  }
  invisible(as.integer(state))
}

#' Check a pair of probabilities for an interval
#'
#' @param probs Object to check. Two increasing numbers in `[0, 1]`.
#' @param arg String. Argument name used in the message.
#'
#' @return `probs`, invisibly.
#' @noRd
.check_probs <- function(probs, arg = "probs") {
  ok <- is.numeric(probs) && length(probs) == 2L && !anyNA(probs) &&
    all(probs >= 0 & probs <= 1) && probs[1L] < probs[2L]
  if (!ok) {
    .dl_abort(c("`", arg, "` must be two increasing probabilities in ",
                "[0, 1], lower bound first."))
  }
  invisible(probs)
}

#' Check that an object is a draws container
#'
#' @param x Object to check.
#' @param arg String. Argument name used in the message.
#' @param measure String or `NULL`. Required value of `x$meta$measure`.
#'
#' @return `x`, invisibly.
#' @noRd
.check_draws <- function(x, arg, measure = NULL) {
  ok <- inherits(x, "demolite_draws") && is.list(x) &&
    all(c("index", "value", "meta") %in% names(x)) &&
    is.data.frame(x$index) && is.matrix(x$value) && is.numeric(x$value) &&
    nrow(x$index) == nrow(x$value)
  if (!ok) {
    .dl_abort(c("`", arg, "` must be a draws container created with ",
                "`draws_new()`."))
  }
  if (!is.null(measure) && !identical(x$meta$measure, measure)) {
    .dl_abort(c("`", arg, "` must hold the measure \"", measure, "\"; it ",
                "holds \"", x$meta$measure, "\"."))
  }
  invisible(x)
}
