# ---
# repo: demography-lite
# file: standalone-age-groups.R
# last-updated: 2026-11-14
# license: MIT (see the LICENSE file at the repository root)
# imports: data.table
# dependencies: standalone-checks.R
# ---
#
# Single-source file. The copies inside the packages are named
# `R/import-standalone-age-groups.R` and must stay byte-identical to
# `standalone/standalone-age-groups.R`. Do not edit a copy: edit the source and
# run `Rscript tools/sync-standalone.R`.
#
# Contents: tables of age groups identified by exact start and end ages.
#
# Not exported: each package of the family exports a thin wrapper under its own
# prefix (see standalone/prefixed-shared.R).

#' Tables of age groups
#'
#' Returns a table of age groups in which every group is identified by its
#' exact start and end in years. Across the family, age is always matched on
#' `age_start` and `age_end`, never on a label.
#'
#' Shared infrastructure, not exported: every package of the family exports a
#' thin wrapper of it under its own prefix, `<prefix>_age_groups()`.
#'
#' @param scheme String. One of
#'   * `"gbd_u5"`: the six groups under 5 years used by the Global Burden of
#'     Disease (GBD) study (0-6 days, 7-27 days, 1-5 months, 6-11 months,
#'     12-23 months, 2-4 years), then single years from 5, then an open group;
#'   * `"single"`: single years from 0, then an open group;
#'   * `"five_year"`: 0-4, 5-9, ..., then an open group;
#'   * `"abridged"`: 0, 1-4, 5-9, ..., then an open group.
#' @param open_age Whole number. Start of the open group in years, 95 by
#'   default. It must be at least 10 and, for `"five_year"` and `"abridged"`,
#'   a multiple of 5.
#'
#' @return A data.table with one row per age group, ordered by age, and the
#'   columns `age_start` and `age_end` (numeric, exact age in years, start
#'   included and end excluded, `Inf` for the open group), `age_group_id`
#'   (integer, the GBD identifier for the six groups under 5 of `"gbd_u5"` and
#'   `NA` elsewhere: users who need other GBD identifiers supply their own map)
#'   and `age_group_name` (character label in English).
#'
#' @section GBD equivalent:
#' **Replicates** the age groups under 5 years of the GBD 2023 demographics
#' appendix (section 2.1.1, p. 10) and their identifiers (2, 3, 388, 389, 238
#' and 34). Days are converted to years with a year of 365.25 days, so that
#' the groups end at 7/365.25, 28/365.25, 0.5, 1, 2 and 5 years. Single years
#' from 5 to 94 and an open group at 95 follow the single-year life tables of
#' that study. **Differs** in that identifiers are not provided for the other
#' groups.
#'
#' @section Method:
#' No estimation is involved. Groups are contiguous, do not overlap, start at
#' age 0 and end with one open group.
#'
#' @references
#' GBD 2023 Demographics Collaborators (2025). Global age-sex-specific
#' all-cause mortality and life expectancy estimates for 204 countries and
#' territories. *The Lancet*, 406, 1731-1810.
#' \doi{10.1016/S0140-6736(25)01330-3}
#'
#' @family age groups
#' @keywords internal
age_groups <- function(scheme = c("gbd_u5", "single", "five_year", "abridged"),
                       open_age = 95) {
  scheme <- .check_choice(scheme, "scheme",
                          c("gbd_u5", "single", "five_year", "abridged"))
  open_age <- .check_count(open_age, "open_age", min = 10, max = 130)
  if (scheme %in% c("five_year", "abridged") && open_age %% 5L != 0L) {
    .dl_abort(c("`open_age` must be a multiple of 5 for the scheme \"", scheme,
                "\"; got ", open_age, "."))
  }
  day <- 1 / 365.25
  if (scheme == "gbd_u5") {
    start <- c(0, 7 * day, 28 * day, 0.5, 1, 2, seq(5, open_age))
    id <- c(2L, 3L, 388L, 389L, 238L, 34L, rep(NA_integer_, open_age - 4L))
    label <- c("0-6 days", "7-27 days", "1-5 months", "6-11 months",
               "12-23 months", "2-4 years", as.character(seq(5, open_age - 1L)))
  } else if (scheme == "single") {
    start <- seq(0, open_age)
    id <- rep(NA_integer_, length(start))
    label <- as.character(seq(0, open_age - 1L))
  } else {
    start <- seq(0, open_age, by = 5)
    label <- paste0(start, "-", start + 4)[-length(start)]
    if (scheme == "abridged") {
      start <- c(0, 1, start[-1L])
      label <- c("0", "1-4", label[-1L])
    }
    id <- rep(NA_integer_, length(start))
  }
  data.table::data.table(
    age_start = as.numeric(start),
    age_end = c(as.numeric(start[-1L]), Inf),
    age_group_id = id,
    age_group_name = c(label, paste0(open_age, "+"))
  )
}

#' Check a set of age groups
#'
#' @param age_start,age_end Numeric vectors. Exact start and end ages in years.
#' @param arg String. Name used in the message.
#' @param from_zero Logical. Whether the first group must start at age 0.
#' @param open Logical. Whether the last group must be open (`age_end = Inf`).
#'
#' @return `TRUE`, invisibly, when the groups are ordered, contiguous and do
#'   not overlap; otherwise signals a classed input error.
#' @noRd
.check_ages <- function(age_start, age_end, arg = "ages", from_zero = TRUE,
                        open = TRUE) {
  n <- length(age_start)
  ok <- is.numeric(age_start) && is.numeric(age_end) && n >= 1L &&
    length(age_end) == n && !anyNA(age_start) && !anyNA(age_end) &&
    all(is.finite(age_start)) && all(age_start < age_end)
  if (!ok) {
    .dl_abort(c("`", arg, "`: `age_start` and `age_end` must be numeric, of ",
                "equal length, without missing values and with age_start < ",
                "age_end."))
  }
  gap <- which(abs(age_start[-1L] - age_end[-n]) > 1e-9)
  if (length(gap)) {
    .dl_abort(c("`", arg, "`: age groups must be ordered and contiguous; ",
                "group ", gap[1L] + 1L, " does not start where group ",
                gap[1L], " ends."))
  }
  if (from_zero && age_start[1L] != 0) {
    .dl_abort(c("`", arg, "`: the first age group must start at age 0."))
  }
  if (open && is.finite(age_end[n])) {
    .dl_abort(c("`", arg, "`: the last age group must be open ",
                "(`age_end = Inf`)."))
  }
  invisible(TRUE)
}
