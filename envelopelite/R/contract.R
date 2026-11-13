#' Data contract with packages that estimate causes of death
#'
#' Describes what this family delivers to a package that splits all-cause
#' deaths into causes and reconciles them with the envelope. It is
#' documentation only: no function is attached to this topic.
#'
#' @section Columns:
#' Column names follow the Global Burden of Disease (GBD) study:
#' * `location_id` (integer): the area; the national total is one more area.
#' * `year_id` (integer): calendar year.
#' * `sex_id` (integer): 1 male, 2 female, 3 both.
#' * `age_start`, `age_end` (numeric): exact age in years, start included and
#'   end excluded, `Inf` for the open group. **This pair is the age key.**
#' * `age_group_id` (integer, optional): GBD identifier where the user
#'   supplies the map; see [age_groups()].
#'
#' @section Draws:
#' Every product is a container created with [draws_new()]: a table of cells
#' and a cells x draws matrix. Draw `k` is the same "world" in every product:
#' envelope `k` comes from completeness `k`, and rate `k` from envelope `k`
#' and population `k`. Consumers must not shuffle or re-order draws. The
#' central estimate is the mean of the draws and the interval runs from the
#' 2.5th to the 97.5th percentile ([evl_summarise()]).
#'
#' @section Products:
#' | Product | Key | Value |
#' |---|---|---|
#' | envelope | location, year, sex, age (six GBD groups under 5), draw | deaths from all causes |
#' | population | location, year, sex, age, draw | persons at mid-year |
#' | empirical life table | location, year, sex, age, draw | mx, ax, qx, lx, ex |
#' | completeness | location, year, sex, age band, draw | proportion registered |
#'
#' The envelope keeps the four GBD groups under 1 year; the life table uses
#' age 0 as a whole, with the detail as an option. Years of life lost are
#' computed by the consumer with a reference life table that the user
#' supplies; the empirical life table of this family is for life expectancy
#' and probabilities of dying.
#'
#' @section Status:
#' Draft, version `0.1.0` of the contract (stored in `meta$version` of every
#' container). It will be fixed together with the family of packages for
#' causes of death.
#'
#' @name envelopelite-contract
#' @aliases contract
#' @family envelope
NULL
