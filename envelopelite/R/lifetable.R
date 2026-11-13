#' Life table from death rates
#'
#' Builds a life table from age-specific death rates with the conventions of
#' the Global Burden of Disease (GBD) study. The package does not provide
#' mortality laws, model life tables or graduation, which exist elsewhere;
#' its formulas are cross-checked against the 'MortalityLaws' package.
#'
#' **Status: skeleton.** In this development version the function validates
#' its inputs and then signals a condition of class
#' `evl_error_not_implemented`.
#'
#' @param mx Numeric vector. Death rates by age group, deaths per
#'   person-year.
#' @param age_start,age_end Numeric vectors. Exact start and end of each age
#'   group in years (contiguous, from 0, last group open).
#' @param ax_rule String. Rule for the mean years lived in an age group by
#'   those who die in it. Only `"gbd"` is planned.
#' @param closure String. Rule that closes the open age group. Only `"gbd"`
#'   is planned (see [evl_close_open_age()]).
#' @param infant_detail Logical. If `FALSE` (default), age 0 is one row; if
#'   `TRUE`, the four groups under 1 year are kept as separate rows.
#'
#' @return When implemented, a data.table with one row per age group and the
#'   columns `age_start`, `age_end`, `mx` (deaths per person-year), `ax`
#'   (years), `qx` (probability), `lx` (survivors, radix 1), `dx` (deaths),
#'   `nLx` and `Tx` (person-years) and `ex` (years).
#'
#' @section GBD equivalent:
#' **Replicates** the single-year life table of the GBD study, with
#' \eqn{a_x = 0.5} at single years of age and specific values under 1 year
#' (GBD 2023 demographics appendix, section 2.5.3, p. 30).
#'
#' @section Method:
#' For an age group of width \eqn{n}:
#' \deqn{q_x = \frac{n\, m_x}{1 + (n - a_x)\, m_x}, \qquad
#' l_{x+n} = l_x (1 - q_x), \qquad
#' L_x = n\, l_{x+n} + a_x\, d_x, \qquad
#' e_x = \frac{T_x}{l_x}}
#' with \eqn{d_x = l_x q_x} and \eqn{T_x} the sum of \eqn{L} from age
#' \eqn{x}. The values of \eqn{a_x} under 1 year and the closure of the open
#' group will be documented when the function is implemented.
#'
#' @references
#' Chiang CL (1984). *The Life Table and Its Applications*. Malabar: Krieger.
#'
#' Preston SH, Heuveline P, Guillot M (2001). *Demography: Measuring and
#' Modeling Population Processes*. Oxford: Blackwell.
#'
#' @examples
#' ages <- age_groups("single")
#' mx <- 0.0003 + 0.00003 * exp(0.095 * ages$age_start)
#' # Skeleton: valid inputs reach the "not implemented" condition.
#' tryCatch(
#'   evl_lifetable(mx, ages$age_start, ages$age_end),
#'   evl_error_not_implemented = function(e) conditionMessage(e)
#' )
#'
#' @family life table
#' @export
evl_lifetable <- function(mx, age_start, age_end, ax_rule = "gbd",
                          closure = "gbd", infant_detail = FALSE) {
  .check_numeric(mx, "mx", lower = 0)
  .check_ages(age_start, age_end, "age_start, age_end")
  .check_same_length(mx = mx, age_start = age_start)
  .check_choice(ax_rule, "ax_rule", "gbd")
  .check_choice(closure, "closure", "gbd")
  .check_flag(infant_detail, "infant_detail")
  .dl_not_implemented("evl_lifetable")
}

#' Close the open age group of a life table
#'
#' Computes the life expectancy at the start of the open age group (95 and
#' over by default in the family) from its death rate.
#'
#' **Status: skeleton.** In this development version the function validates
#' its inputs and then signals a condition of class
#' `evl_error_not_implemented`.
#'
#' @param mx_open Numeric vector. Death rate of the open age group, deaths
#'   per person-year.
#' @param floor Number or `NULL`. Optional lower bound for `mx_open`, deaths
#'   per person-year, declared by the caller; `NULL` applies none.
#'
#' @return When implemented, a list with `ex_open` (life expectancy at the
#'   start of the open group, years) and `floored` (logical, whether the
#'   floor was applied).
#'
#' @section GBD equivalent:
#' **Adapts** the closure of the life table of the Global Burden of Disease
#' (GBD) study (GBD 2023 demographics appendix, section 2.5.3, p. 30).
#' **Differs** in that the optional floor on the rate of the open group is an
#' explicit argument, off by default.
#'
#' @section Method:
#' With a constant death rate in the open group,
#' \deqn{e_{\omega} = \frac{1}{m_{\omega}}}
#' If a floor is given, \eqn{m_{\omega}} is replaced by
#' \eqn{\max(m_{\omega}, \mathrm{floor})} first. Whether another closure is
#' needed at very old ages will be decided when the function is implemented.
#'
#' @references
#' Thatcher AR, Kannisto V, Vaupel JW (1998). *The Force of Mortality at Ages
#' 80 to 120*. Odense: Odense University Press.
#'
#' @examples
#' # Skeleton: valid inputs reach the "not implemented" condition.
#' tryCatch(
#'   evl_close_open_age(mx_open = 0.28),
#'   evl_error_not_implemented = function(e) conditionMessage(e)
#' )
#'
#' @family life table
#' @export
evl_close_open_age <- function(mx_open, floor = NULL) {
  .check_numeric(mx_open, "mx_open", lower = 0)
  .check_number(floor, "floor", lower = 0, null_ok = TRUE)
  .dl_not_implemented("evl_close_open_age")
}

#' Aggregate life tables over areas or sexes
#'
#' Combines the life tables of several areas, or of both sexes, into the life
#' table of their union by adding up deaths and population, never by
#' averaging rates.
#'
#' **Status: skeleton.** In this development version the function validates
#' its inputs and then signals a condition of class
#' `evl_error_not_implemented`.
#'
#' @param lifetables Data frame or data.table with the life tables to combine,
#'   one row per unit and age group: the columns named in `by`, `age_start`,
#'   `age_end`, `mx` (deaths per person-year) and `ax` (years).
#' @param population Data frame or data.table with the population of the same
#'   units and age groups: the columns named in `by`, `age_start`, `age_end`
#'   and `population` (person-years).
#' @param by Character vector. Columns that identify the units being combined,
#'   for instance `"location_id"` or `"sex_id"`.
#'
#' @return When implemented, a data.table with the aggregated life table, in
#'   the layout of [evl_lifetable()].
#'
#' @section GBD equivalent:
#' **Replicates** the aggregation of life tables in the Global Burden of
#' Disease (GBD) study: deaths and population are added up and \eqn{a_x} is
#' weighted by deaths.
#'
#' @section Method:
#' \deqn{m_x = \frac{\sum_j m_{x,j} P_{x,j}}{\sum_j P_{x,j}}, \qquad
#' a_x = \frac{\sum_j a_{x,j} D_{x,j}}{\sum_j D_{x,j}}}
#' with \eqn{D_{x,j} = m_{x,j} P_{x,j}}; the life table is then rebuilt with
#' [evl_lifetable()].
#'
#' @examples
#' lt <- data.frame(location_id = rep(2:3, each = 2), age_start = c(50, 51),
#'                  age_end = c(51, 52), mx = c(0.004, 0.005, 0.006, 0.007),
#'                  ax = 0.5)
#' pop <- data.frame(location_id = rep(2:3, each = 2), age_start = c(50, 51),
#'                   age_end = c(51, 52), population = c(900, 880, 400, 390))
#' # Skeleton: valid inputs reach the "not implemented" condition.
#' tryCatch(
#'   evl_aggregate(lt, pop, by = "location_id"),
#'   evl_error_not_implemented = function(e) conditionMessage(e)
#' )
#'
#' @family life table
#' @export
evl_aggregate <- function(lifetables, population, by) {
  if (missing(by) || !is.character(by) || !length(by) || anyNA(by)) {
    .dl_abort("`by` must name the column(s) that identify the units to combine.")
  }
  .check_table(lifetables, "lifetables",
               cols = c(by, "age_start", "age_end", "mx", "ax"))
  .check_table(population, "population",
               cols = c(by, "age_start", "age_end", "population"))
  .dl_not_implemented("evl_aggregate")
}

#' Abridge a single-year life table
#'
#' Converts a life table by single year of age into a life table by age
#' groups.
#'
#' **Status: skeleton.** In this development version the function validates
#' its inputs and then signals a condition of class
#' `evl_error_not_implemented`.
#'
#' @param lifetable Data frame or data.table with a single-year life table in
#'   the layout of [evl_lifetable()]: at least `age_start`, `age_end`, `lx`
#'   and `nLx`.
#' @param groups Data frame or data.table with the target age groups
#'   (`age_start`, `age_end`), `age_groups("abridged")` by default.
#'
#' @return When implemented, a data.table with one row per target age group,
#'   in the layout of [evl_lifetable()].
#'
#' @section GBD equivalent:
#' **Replicates** the abridged life tables that the Global Burden of Disease
#' (GBD) study derives from its single-year life tables.
#'
#' @section Method:
#' Survivors and person-years are added up within each target group:
#' \eqn{{}_nq_x = 1 - l_{x+n} / l_x}, \eqn{{}_nL_x = \sum L}, and
#' \eqn{{}_nm_x = {}_nd_x / {}_nL_x}; \eqn{e_x} is unchanged at the ages
#' kept.
#'
#' @examples
#' lt <- data.frame(age_start = 0:2, age_end = c(1, 2, Inf),
#'                  lx = c(1, 0.99, 0.985), nLx = c(0.992, 0.987, 70))
#' # Skeleton: valid inputs reach the "not implemented" condition.
#' tryCatch(
#'   evl_abridge(lt),
#'   evl_error_not_implemented = function(e) conditionMessage(e)
#' )
#'
#' @family life table
#' @export
evl_abridge <- function(lifetable, groups = age_groups("abridged")) {
  .check_table(lifetable, "lifetable",
               cols = c("age_start", "age_end", "lx", "nLx"))
  .check_table(groups, "groups", cols = c("age_start", "age_end"))
  .check_ages(groups[["age_start"]], groups[["age_end"]], "groups")
  .dl_not_implemented("evl_abridge")
}
