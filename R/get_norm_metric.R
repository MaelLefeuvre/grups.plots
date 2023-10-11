#' (private) Parse a user-defined string value into a base R function
#' @export
#' @param norm_metric (string) User-defined normalization metric.
#'        Valid values are:
#'        - "Median" : use the median of all targeted comparisons
#'        - "Mean"   : use the mean of all targeted comparisons
#'        - "Min"    : use the minimum of all targeted comparisons
#'        - "Max"    : use the maximum of all targeted comparisons.
#' @return a summary statistic function corresponding to the provided string.
#' @export
get_norm_metric <- function(norm_metric) {
  switch(norm_metric,
    "Median" = median,
    "Mean"   = mean,
    "Min"    = min,
    "Max"    = max,
  )
}