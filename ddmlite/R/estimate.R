#' Run the death distribution methods for a table of cells
#'
#' Runs every requested method, with and without net migration, for each cell
#' (an area and sex) of a table.
#'
#' **Status: skeleton.** In this development version the function validates
#' its inputs and then signals a condition of class
#' `ddl_error_not_implemented`. The layout of `cells` is a draft and may
#' change.
#'
#' @param cells Data frame or data.table with one row per cell and five-year
#'   age group: `location_id`, `sex_id`, `age_start`, `age_end`, `pop1`,
#'   `pop2` (persons enumerated at each census), `deaths` (mean annual
#'   registered deaths) and, optionally, `net_migrants` (mean annual net
#'   migrants). [ddl_simulate()] returns a table in this layout.
#' @param interval Number. Years between the two censuses (decimal).
#' @param methods Character vector. Any of `"ggb"`, `"seg"` and `"ggbseg"`.
#' @param migration String. `"both"` runs each method with and without net
#'   migration, `"with"` and `"without"` run one variant.
#'
#' @return When implemented, a data.table with one row per cell, method and
#'   migration variant: `location_id`, `sex_id`, `method`, `with_migration`
#'   and `completeness` (proportion, not capped at 1).
#'
#' @section GBD equivalent:
#' **Adapts** the step of the Global Burden of Disease (GBD) study that runs
#' the three death distribution methods for every location and sex (GBD 2023
#' demographics appendix, section 2.2.8, p. 17). **Differs** in that each
#' method is also run with net migration.
#'
#' @section Method:
#' Applies [ddl_ggb()], [ddl_seg()] and [ddl_ggbseg()] cell by cell. A method
#' that fails in a cell returns a missing value for that cell instead of
#' stopping the run.
#'
#' @examples
#' sim <- ddl_simulate(seed = 1, completeness = 0.8)
#' # Skeleton: valid inputs reach the "not implemented" condition.
#' tryCatch(
#'   ddl_estimate(sim$cells, sim$interval),
#'   ddl_error_not_implemented = function(e) conditionMessage(e)
#' )
#'
#' @family completeness
#' @export
ddl_estimate <- function(cells, interval, methods = c("ggb", "seg", "ggbseg"),
                         migration = c("both", "with", "without")) {
  .check_table(cells, "cells",
               cols = c("location_id", "sex_id", "age_start", "age_end",
                        "pop1", "pop2", "deaths"))
  .check_number(interval, "interval", lower = 0.5, upper = 50)
  .check_choice(methods, "methods", c("ggb", "seg", "ggbseg"), several = TRUE)
  .check_choice(migration, "migration", c("both", "with", "without"))
  .dl_not_implemented("ddl_estimate")
}

#' Combine the estimates of several methods
#'
#' Reduces the completeness estimated by several methods to one central value:
#' the median of what remains after removing the largest and the smallest
#' estimate.
#'
#' **Status: implemented.**
#'
#' @param estimates Numeric vector with one completeness estimate per method
#'   (proportion), or numeric matrix with one row per cell or draw and one
#'   column per method. Missing values mark methods that failed.
#' @param rule String. Only `"trimmed_median"` is available.
#'
#' @return A number (for a vector) or a numeric vector with one value per row
#'   (for a matrix): the central completeness, a proportion. The result is
#'   missing when fewer than three estimates are available, because nothing
#'   is left after removing the largest and the smallest.
#'
#' @section GBD equivalent:
#' **Replicates** the combination rule of the Global Burden of Disease (GBD)
#' study, which drops the highest and the lowest of its completeness estimates
#' before combining the rest (GBD 2023 demographics appendix, section 2.2.8
#' and Table C, pp. 17-18). **Differs** in the set of methods: the GBD study
#' combines five (three death distribution methods and two variants based on
#' its population model); here the caller decides which estimates to pass.
#' With three estimates the rule returns their median.
#'
#' @section Method:
#' For the non-missing estimates \eqn{c_{(1)} \le \dots \le c_{(n)}}, with
#' \eqn{n \ge 3}: \deqn{c = \mathrm{median}(c_{(2)}, \dots, c_{(n-1)})}
#' One largest and one smallest value are removed, also when there are ties.
#'
#' @references
#' GBD 2023 Demographics Collaborators (2025). Global age-sex-specific
#' all-cause mortality and life expectancy estimates for 204 countries and
#' territories. *The Lancet*, 406, 1731-1810.
#' \doi{10.1016/S0140-6736(25)01330-3}
#'
#' @examples
#' ddl_combine(c(ggb = 0.78, seg = 0.84, ggbseg = 0.81))
#' ddl_combine(c(0.70, 0.78, 0.81, 0.84, 0.99))
#'
#' # One row per draw, one column per method
#' ddl_combine(rbind(c(0.78, 0.84, 0.81), c(0.75, NA, 0.80)))
#'
#' @family completeness
#' @export
ddl_combine <- function(estimates, rule = "trimmed_median") {
  .check_choice(rule, "rule", "trimmed_median")
  if (!is.numeric(estimates) || length(estimates) == 0L) {
    .dl_abort("`estimates` must be a non-empty numeric vector or matrix.")
  }
  one <- function(v) {
    v <- sort(v[!is.na(v)])
    n <- length(v)
    if (n < 3L) {
      return(NA_real_)
    }
    stats::median(v[-c(1L, n)])
  }
  if (is.matrix(estimates)) {
    return(apply(estimates, 1L, one))
  }
  one(estimates)
}

#' Draws of completeness by cell and method
#'
#' Perturbs the censuses, the registered deaths and the net migrants of each
#' cell and re-runs the methods, so that every draw is one coherent "world".
#' Random numbers come from per-cell seeds: draw `k` of a cell does not depend
#' on the order of the cells or on how the work is split.
#'
#' **Status: skeleton.** In this development version the function validates
#' its inputs and then signals a condition of class
#' `ddl_error_not_implemented`.
#'
#' @inheritParams ddl_estimate
#' @param n_draws Whole number. Number of draws.
#' @param seed Whole number. The caller's seed; the package never chooses one.
#' @param level_error Number. Relative error of the overall level of each
#'   census (standard deviation, proportion).
#' @param age_error Number. Relative error of each age group of each census
#'   (standard deviation, proportion).
#'
#' @return When implemented, a `demolite_draws` container (see [ddl_draws_new()])
#'   with one row per cell, method and migration variant, and the measure
#'   `"completeness"` (proportion).
#'
#' @section GBD equivalent:
#' **Own.** The Global Burden of Disease (GBD) study propagates the
#' uncertainty of completeness inside its mortality model; here it is
#' generated by perturbing the inputs of the methods.
#'
#' @section Method:
#' For draw \eqn{k}, each input is multiplied by a random factor: one common
#' to all ages of a census (level) and one per age group (age pattern). The
#' factors of a cell are generated under `cell_seed(seed, stream, cell)`;
#' factors common to all cells use the stream name alone (see [cell_seed()]).
#' The distributions and their scales will be fixed and documented when the
#' function is implemented.
#'
#' @examples
#' sim <- ddl_simulate(seed = 1, completeness = 0.8)
#' # Skeleton: valid inputs reach the "not implemented" condition.
#' tryCatch(
#'   ddl_draws(sim$cells, sim$interval, n_draws = 20, seed = 1,
#'             level_error = 0.02, age_error = 0.01),
#'   ddl_error_not_implemented = function(e) conditionMessage(e)
#' )
#'
#' @family completeness
#' @export
ddl_draws <- function(cells, interval, n_draws, seed, level_error, age_error,
                      methods = c("ggb", "seg", "ggbseg"),
                      migration = c("both", "with", "without")) {
  .check_table(cells, "cells",
               cols = c("location_id", "sex_id", "age_start", "age_end",
                        "pop1", "pop2", "deaths"))
  .check_number(interval, "interval", lower = 0.5, upper = 50)
  .check_count(n_draws, "n_draws")
  .check_seed(seed)
  if (missing(level_error) || missing(age_error)) {
    .dl_abort("`level_error` and `age_error` must be supplied.")
  }
  .check_number(level_error, "level_error", lower = 0, upper = 1)
  .check_number(age_error, "age_error", lower = 0, upper = 1)
  .check_choice(methods, "methods", c("ggb", "seg", "ggbseg"), several = TRUE)
  .check_choice(migration, "migration", c("both", "with", "without"))
  .dl_not_implemented("ddl_draws")
}

#' Treat nearly complete registration as complete
#'
#' Applies the rule that sets completeness to 1 when the estimate reaches a
#' threshold, 0.95 by default. The rule is a jump, not a ramp: nothing
#' smooths the step at the threshold, and the function says so with a warning
#' when some of its input falls just below it.
#'
#' **Status: implemented.**
#'
#' @param completeness Numeric vector or matrix. Estimated completeness
#'   (proportion; values above 1 are possible before the rule). Missing values
#'   stay missing.
#' @param rule String. Only `"gbd95"` is available.
#' @param threshold Number in `(0, 1]`. Completeness from which registration
#'   is taken as complete; 0.95 for the rule `"gbd95"`.
#'
#' @return An object of the same shape as `completeness`: values at or above
#'   `threshold` become 1; the others are returned unchanged.
#'
#' @section GBD equivalent:
#' **Replicates** the rule attributed to the Global Burden of Disease (GBD)
#' study, which treats vital registration with estimated completeness of 95%
#' or more as complete. The passage of the GBD 2023 demographics appendix
#' that states the rule still has to be cited here before any release.
#' **Differs** in nothing: only the published rule is offered. No transition
#' of our own is added just below the threshold, so that what the function
#' does is exactly what can be cited (decision of the repository owner, 14
#' November 2026). A caller who wants a transition has to write it.
#'
#' @section Method:
#' \deqn{c^{*} = 1 \text{ if } c \ge \tau, \qquad c^{*} = c \text{ otherwise}}
#' with \eqn{\tau} the threshold.
#'
#' **How big the jump is.** Completeness enters the next step as a divisor of
#' registered deaths, so the step in completeness is a step in the deaths that
#' are estimated. With the default threshold, a cell with completeness 0.9499
#' keeps it and its deaths are divided by 0.9499, that is multiplied by
#' \eqn{1/0.9499 = 1.053}; a cell with 0.9501 is set to 1 and its deaths are
#' left untouched. Two cells four ten-thousandths apart therefore differ by
#' 5.3% in estimated deaths. The jump is the size of
#' \eqn{1/\tau - 1} = 5.3% at \eqn{\tau = 0.95}.
#'
#' **Where the threshold decides the result.** The function signals a warning
#' of class `ddl_warning_threshold` when some value falls in
#' \eqn{[\tau - 0.05, \tau)}, that is in `[0.90, 0.95)` with the default
#' threshold: there, moving the threshold by a little moves those cells from
#' "inflate the deaths" to "leave them alone". The returned values do not
#' change because of the warning. Applied draw by draw, the rule also pulls
#' the mean of the draws towards 1.
#'
#' @references
#' GBD 2023 Demographics Collaborators (2025). Global age-sex-specific
#' all-cause mortality and life expectancy estimates for 204 countries and
#' territories. *The Lancet*, 406, 1731-1810.
#' \doi{10.1016/S0140-6736(25)01330-3}
#'
#' @examples
#' # 0.949 is left alone and 0.95 becomes 1: the rule is a jump of 5.3% in
#' # the deaths that the next step estimates
#' suppressWarnings(ddl_cap(c(0.62, 0.949, 0.95, 1.08, NA)))
#'
#' @family completeness
#' @export
ddl_cap <- function(completeness, rule = "gbd95", threshold = 0.95) {
  .check_choice(rule, "rule", "gbd95")
  .check_number(threshold, "threshold", lower = 1e-8, upper = 1)
  if (!is.numeric(completeness)) {
    .dl_abort("`completeness` must be numeric.")
  }
  bad <- which(completeness < 0)
  if (length(bad)) {
    .dl_abort(c("`completeness` must not be negative (first violation at ",
                "position ", bad[1L], ")."))
  }
  # The rule is a jump: say so when the input sits where the threshold decides
  # the result. The returned values are the same with or without the warning.
  near <- sum(!is.na(completeness) & completeness >= threshold - 0.05 &
                completeness < threshold)
  if (near > 0L) {
    .dl_warn(c(near, " value(s) of `completeness` fall in [",
               format(threshold - 0.05), ", ", format(threshold), "), where ",
               "the threshold decides the result: the rule is a jump, not a ",
               "ramp. Just below the threshold the value is kept and the ",
               "deaths of the cell are divided by it (",
               format(round(1 / (threshold - 0.0001), 3)),
               " times the registered deaths at ",
               format(threshold - 0.0001),
               "); at the threshold it becomes 1 and the deaths are left ",
               "unchanged."), class = "threshold")
  }
  completeness[!is.na(completeness) & completeness >= threshold] <- 1
  completeness
}
