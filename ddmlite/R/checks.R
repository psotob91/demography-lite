#' Check the common inputs of the death distribution methods
#'
#' @param pop1,pop2 Numeric vectors. Population by age group at each census.
#' @param deaths Numeric vector. Mean annual registered deaths by age group.
#' @param interval Number. Years between the two censuses.
#' @param net_migrants Numeric vector or `NULL`. Mean annual net migrants by
#'   age group.
#' @param age_trim Numeric vector of length two or `NULL`. First and last age
#'   of the fitting range.
#'
#' @return `TRUE`, invisibly; otherwise signals a classed input error.
#' @noRd
.ddl_check_inputs <- function(pop1, pop2, deaths, interval, net_migrants = NULL,
                              age_trim = NULL) {
  .check_numeric(pop1, "pop1", lower = 0)
  .check_numeric(pop2, "pop2", lower = 0)
  .check_numeric(deaths, "deaths", lower = 0)
  .check_numeric(net_migrants, "net_migrants", null_ok = TRUE)
  .check_same_length(pop1 = pop1, pop2 = pop2, deaths = deaths,
                     net_migrants = net_migrants)
  if (length(pop1) < 5L) {
    .dl_abort(c("`pop1`, `pop2` and `deaths` need at least five age groups; ",
                "they have ", length(pop1), "."))
  }
  .check_number(interval, "interval", lower = 0.5, upper = 50)
  .check_numeric(age_trim, "age_trim", len = 2L, lower = 0, null_ok = TRUE)
  if (!is.null(age_trim) && age_trim[1L] >= age_trim[2L]) {
    .dl_abort("`age_trim` must give the first and then the last age of the fit.")
  }
  invisible(TRUE)
}
