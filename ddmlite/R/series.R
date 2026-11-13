#' Annual series of completeness
#'
#' Turns completeness estimated for intercensal periods into an annual series
#' that changes slowly, with jumps only at the dates the caller declares (for
#' instance a documented change in the registration system).
#'
#' **Status: skeleton.** In this development version the function validates
#' its inputs and then signals a condition of class
#' `ddl_error_not_implemented`.
#'
#' @param completeness Data frame or data.table with one row per cell and
#'   period: identifier columns, `year_start` and `year_end` (period covered,
#'   decimal years) and `completeness` (proportion).
#' @param breaks Numeric vector or `NULL`. Years at which a jump is allowed.
#'   `NULL` allows none.
#'
#' @return When implemented, a data.table with one row per cell and year:
#'   the identifier columns, `year_id` and `completeness` (proportion).
#'
#' @section GBD equivalent:
#' **Adapts** the smoothing over time that the Global Burden of Disease (GBD)
#' study applies to its completeness estimates (GBD 2023 Demographics
#' Collaborators, 2025; the passage of the appendix still has to be cited
#' here). **Differs** in that jumps are allowed only at
#' declared dates. The smoother of the GBD study is not reproduced.
#'
#' @section Method:
#' To be specified when the function is implemented. The design constraint is
#' that completeness changes slowly, so that a jump must be justified by the
#' caller and not inferred from the data.
#'
#' @examples
#' est <- data.frame(location_id = 2L, sex_id = 1L,
#'                   year_start = c(1990.5, 2000.5), year_end = c(2000.5, 2010.5),
#'                   completeness = c(0.71, 0.80))
#' # Skeleton: valid inputs reach the "not implemented" condition.
#' tryCatch(
#'   ddl_smooth_series(est),
#'   ddl_error_not_implemented = function(e) conditionMessage(e)
#' )
#'
#' @family completeness
#' @export
ddl_smooth_series <- function(completeness, breaks = NULL) {
  .check_table(completeness, "completeness",
               cols = c("year_start", "year_end", "completeness"))
  .check_numeric(breaks, "breaks", null_ok = TRUE)
  .dl_not_implemented("ddl_smooth_series")
}

#' Mismatch between the areas and the national total
#'
#' Measures, draw by draw, how far the deaths implied by the completeness of
#' the areas are from the deaths implied by the national completeness, before
#' any adjustment forces them to agree.
#'
#' **Status: skeleton.** In this development version the function validates
#' its inputs and then signals a condition of class
#' `ddl_error_not_implemented`.
#'
#' @param areas A `demolite_draws` container (see [draws_new()]) with the
#'   estimated deaths of each area (registered deaths divided by completeness,
#'   count), one row per area and stratum.
#' @param national A `demolite_draws` container with the same strata for the
#'   national total and the same number of draws.
#'
#' @return When implemented, a `demolite_draws` container with one row per
#'   stratum and the measure `"area_mismatch"`: the sum over areas divided by
#'   the national value (ratio; 1 means agreement).
#'
#' @section GBD equivalent:
#' **Own.** The Global Burden of Disease (GBD) study scales subnational
#' estimates to the national one (GBD 2023 demographics appendix, section
#' 2.5.1) without publishing the mismatch that the scaling removes.
#'
#' @section Method:
#' \deqn{R_k = \frac{\sum_{j} \hat{D}_{j,k}}{\hat{D}_{0,k}}}
#' with \eqn{\hat{D}_{j,k}} the estimated deaths of area \eqn{j} in draw
#' \eqn{k} and \eqn{\hat{D}_{0,k}} the national estimate in the same draw.
#'
#' @examples
#' areas <- draws_new(data.frame(location_id = 2:3, sex_id = 1L),
#'                    matrix(c(100, 210, 104, 205), nrow = 2),
#'                    measure = "deaths", unit = "count")
#' national <- draws_new(data.frame(location_id = 1L, sex_id = 1L),
#'                       matrix(c(318, 300), nrow = 1),
#'                       measure = "deaths", unit = "count")
#' # Skeleton: valid inputs reach the "not implemented" condition.
#' tryCatch(
#'   ddl_area_mismatch(areas, national),
#'   ddl_error_not_implemented = function(e) conditionMessage(e)
#' )
#'
#' @family completeness
#' @export
ddl_area_mismatch <- function(areas, national) {
  .check_draws(areas, "areas")
  .check_draws(national, "national")
  if (ncol(areas$value) != ncol(national$value)) {
    .dl_abort(c("`areas` and `national` must have the same number of draws; ",
                "they have ", ncol(areas$value), " and ", ncol(national$value),
                "."))
  }
  .dl_not_implemented("ddl_area_mismatch")
}
