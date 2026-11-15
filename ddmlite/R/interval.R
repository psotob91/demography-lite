#' Define the interval between two censuses
#'
#' Describes the period between two censuses: its length in decimal years and
#' the share of each calendar month that falls inside it, so that registered
#' deaths can be matched to the intercensal period.
#'
#' **Status: skeleton.** In this development version the function validates
#' its inputs and then signals a condition of class
#' `ddl_error_not_implemented`.
#'
#' @param census1_date,census2_date Reference dates of the first and second
#'   census, each a `Date` or a number in decimal years (for instance
#'   `2010.5`). The second must be later than the first.
#' @param last_death_year Whole number or `NULL`. Last calendar year with
#'   registered deaths; months after it are flagged as extrapolated. `NULL`
#'   means that deaths cover the whole interval.
#'
#' @return When implemented, an object of class `ddl_interval`: a list with
#'   the two dates in decimal years, `years` (length of the interval) and a
#'   table with one row per calendar month (`year_id`, `month`, `weight` in
#'   `[0, 1]`, `extrapolated`).
#'
#' @section GBD equivalent:
#' **Own.** The published methods of the Global Burden of Disease (GBD) study
#' do not describe how the intercensal period is matched to calendar months.
#'
#' @section Method:
#' The weight of a month is the fraction of its days between the two census
#' dates. The exact day-count convention will be fixed and documented when the
#' function is implemented.
#'
#' @examples
#' # Skeleton: valid inputs reach the "not implemented" condition.
#' tryCatch(
#'   ddl_interval(2000.5, 2010.5),
#'   ddl_error_not_implemented = function(e) conditionMessage(e)
#' )
#'
#' @family completeness
#' @export
ddl_interval <- function(census1_date, census2_date, last_death_year = NULL) {
  d1 <- .ddl_decimal_date(census1_date, "census1_date")
  d2 <- .ddl_decimal_date(census2_date, "census2_date")
  if (d2 <= d1) {
    .dl_abort("`census2_date` must be later than `census1_date`.")
  }
  if (!is.null(last_death_year)) {
    .check_count(last_death_year, "last_death_year", min = 0, max = 9999)
  }
  .dl_not_implemented("ddl_interval")
}

#' Convert a census date to decimal years
#'
#' @param x A single `Date` or a single number in decimal years.
#' @param arg String. Argument name used in the message.
#'
#' @return A number: the date in decimal years. A `Date` is converted with
#'   the actual length of its year.
#' @noRd
.ddl_decimal_date <- function(x, arg) {
  if (inherits(x, "Date") && length(x) == 1L && !is.na(x)) {
    year <- as.integer(format(x, "%Y"))
    start <- as.Date(sprintf("%04d-01-01", year))
    end <- as.Date(sprintf("%04d-01-01", year + 1L))
    return(year + as.numeric(x - start) / as.numeric(end - start))
  }
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x)) {
    .dl_abort(c("`", arg, "` must be a single `Date` or a single number in ",
                "decimal years."))
  }
  as.numeric(x)
}

#' Registered deaths inside the intercensal interval
#'
#' Adds up the registered deaths that fall between the two censuses and
#' returns their annual mean by age, stating how much of the interval had to
#' be extrapolated because the register ends earlier.
#'
#' **Status: skeleton.** In this development version the function validates
#' its inputs and then signals a condition of class
#' `ddl_error_not_implemented`.
#'
#' @param deaths Data frame or data.table with one row per calendar month and
#'   age group: `year_id` and `month` (whole numbers), `age_start` and
#'   `age_end` (exact age in years) and `deaths` (registered deaths, count).
#' @param interval An object of class `ddl_interval` created with
#'   [ddl_interval()].
#'
#' @return When implemented, a data.table with one row per age group:
#'   `age_start`, `age_end`, `deaths` (mean annual registered deaths in the
#'   interval) and `share_extrapolated` (proportion of the interval without
#'   registered deaths).
#'
#' @section GBD equivalent:
#' **Adapts** the role of `deaths_per_year()` in the 'DDMethods' package of
#' the Institute for Health Metrics and Evaluation (IHME). **Differs** in that
#' months are weighted by the share of their days inside the interval and the
#' extrapolated share is reported.
#'
#' @section Method:
#' \deqn{D(a) = \frac{1}{t} \sum_{y,m} w_{y,m}\, d_{y,m}(a)}
#' with \eqn{w} the weight of each month (see [ddl_interval()]), \eqn{d} the
#' registered deaths and \eqn{t} the length of the interval in years.
#'
#' @examples
#' sim <- ddl_country(seed = 1, n_areas = 1, monthly = TRUE)
#' deaths <- sim$deaths
#' deaths$deaths <- deaths$deaths_registered
#' # Skeleton: `interval` objects cannot be created yet, so the call stops at
#' # input validation.
#' tryCatch(
#'   ddl_deaths_in_interval(deaths, interval = 10),
#'   ddl_error_input = function(e) conditionMessage(e)
#' )
#'
#' @family completeness
#' @export
ddl_deaths_in_interval <- function(deaths, interval) {
  .check_table(deaths, "deaths",
               cols = c("year_id", "month", "age_start", "age_end", "deaths"))
  if (!inherits(interval, "ddl_interval")) {
    .dl_abort("`interval` must be an object created with `ddl_interval()`.")
  }
  .dl_not_implemented("ddl_deaths_in_interval")
}
