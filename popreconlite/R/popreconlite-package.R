#' popreconlite: Population Reconstruction by Cohort Components Between Censuses
#'
#' Development version. The shared infrastructure (draws container, per-cell
#' seeds, age groups, simulated country) is implemented and tested; the
#' estimation functions are documented skeletons that validate their inputs
#' and signal a condition of class `prl_error_not_implemented`.
#'
#' This package is an independent implementation. It is not affiliated with,
#' endorsed by, or supported by the Institute for Health Metrics and
#' Evaluation (IHME), and it does not contain IHME code or data.
#'
#' @keywords internal
#' @importFrom data.table data.table
"_PACKAGE"

# Function prefix used to build the condition classes of this package.
.demolite_prefix <- "prl"

# The package uses data.table syntax on data.table objects.
.datatable.aware <- TRUE
