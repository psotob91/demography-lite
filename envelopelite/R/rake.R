#' Scale areas to their total, draw by draw
#'
#' Multiplies the values of the areas ("children") by one factor per stratum
#' and draw so that they add up to the value of their total ("parent") in the
#' same draw.
#'
#' **Status: skeleton.** In this development version the function validates
#' its inputs and then signals a condition of class
#' `evl_error_not_implemented`.
#'
#' @param children A `demolite_draws` container (see [draws_new()]) with an
#'   additive measure (for instance `"deaths"`, count) for the areas, one row
#'   per area and stratum.
#' @param parent A `demolite_draws` container with the same measure for the
#'   total, one row per stratum, and the same number of draws.
#'
#' @return When implemented, a list with `children` (the scaled container;
#'   within each stratum and draw it adds up to `parent`) and `factor` (a
#'   `demolite_draws` container with the multiplicative factor applied, one
#'   row per stratum).
#'
#' @section GBD equivalent:
#' **Replicates** the scaling of subnational estimates to the national one in
#' the Global Burden of Disease (GBD) study (GBD 2023 demographics appendix,
#' section 2.5.1): one multiplicative factor per draw.
#'
#' @section Method:
#' \deqn{f_k = \frac{Y_{0,k}}{\sum_j Y_{j,k}}, \qquad
#' Y^{*}_{j,k} = f_k\, Y_{j,k}}
#' with \eqn{j} the children and \eqn{0} the parent, for draw \eqn{k}.
#' Conservation: after scaling, children add up to the parent in every
#' draw. The factor is returned so that the adjustment is never silent; see
#' also [evl_rake_mismatch()].
#'
#' @references
#' Deming WE, Stephan FF (1940). On a least squares adjustment of a sampled
#' frequency table when the expected marginal totals are known. *Annals of
#' Mathematical Statistics*, 11, 427-444. \doi{10.1214/aoms/1177731829}
#'
#' @examples
#' children <- draws_new(data.frame(location_id = 2:3, sex_id = 1L),
#'                       matrix(c(100, 210, 104, 205), nrow = 2),
#'                       measure = "deaths", unit = "count")
#' parent <- draws_new(data.frame(location_id = 1L, sex_id = 1L),
#'                     matrix(c(318, 300), nrow = 1),
#'                     measure = "deaths", unit = "count")
#' # Skeleton: valid inputs reach the "not implemented" condition.
#' tryCatch(
#'   evl_rake(children, parent),
#'   evl_error_not_implemented = function(e) conditionMessage(e)
#' )
#'
#' @family scaling
#' @export
evl_rake <- function(children, parent) {
  .evl_check_rake(children, parent)
  .dl_not_implemented("evl_rake")
}

#' Mismatch between areas and their total before scaling
#'
#' Reports, for each stratum and draw, the sum of the areas divided by the
#' total, so that the size of the adjustment made by [evl_rake()] can be
#' published.
#'
#' **Status: skeleton.** In this development version the function validates
#' its inputs and then signals a condition of class
#' `evl_error_not_implemented`.
#'
#' @inheritParams evl_rake
#'
#' @return When implemented, a `demolite_draws` container with one row per
#'   stratum and the measure `"rake_mismatch"`: the sum over children divided
#'   by the parent (ratio; 1 means agreement).
#'
#' @section GBD equivalent:
#' **Own.** The Global Burden of Disease (GBD) study applies the scaling
#' without publishing the mismatch that it removes.
#'
#' @section Method:
#' \deqn{R_k = \frac{\sum_j Y_{j,k}}{Y_{0,k}} = \frac{1}{f_k}}
#' with the notation of [evl_rake()].
#'
#' @examples
#' children <- draws_new(data.frame(location_id = 2:3, sex_id = 1L),
#'                       matrix(c(100, 210, 104, 205), nrow = 2),
#'                       measure = "deaths", unit = "count")
#' parent <- draws_new(data.frame(location_id = 1L, sex_id = 1L),
#'                     matrix(c(318, 300), nrow = 1),
#'                     measure = "deaths", unit = "count")
#' # Skeleton: valid inputs reach the "not implemented" condition.
#' tryCatch(
#'   evl_rake_mismatch(children, parent),
#'   evl_error_not_implemented = function(e) conditionMessage(e)
#' )
#'
#' @family scaling
#' @export
evl_rake_mismatch <- function(children, parent) {
  .evl_check_rake(children, parent)
  .dl_not_implemented("evl_rake_mismatch")
}

#' Check the inputs of the scaling functions
#'
#' @param children,parent `demolite_draws` containers.
#'
#' @return `TRUE`, invisibly; otherwise signals a classed input error.
#' @noRd
.evl_check_rake <- function(children, parent) {
  .check_draws(children, "children")
  .check_draws(parent, "parent")
  .evl_check_paired(children, parent, "children", "parent")
  if (!identical(children$meta$measure, parent$meta$measure)) {
    .dl_abort(c("`children` and `parent` must hold the same measure; they ",
                "hold \"", children$meta$measure, "\" and \"",
                parent$meta$measure, "\"."))
  }
  invisible(TRUE)
}
