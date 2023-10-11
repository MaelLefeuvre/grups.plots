
#' (private) Get the normalization value required to normalize the results of
#' the 'pwd-from-stin' module of GRUPS-rs
#' @export
#' @param norm_request (bool) Whether or not results should be output as ratios
#'        of the normalization value.
#' @param norm_method (string) User-defined string specifying which method, or
#'        subset of comparisons should be used. Valid values are:
#'        - "Pairwise": use pairwise comparisons
#'        - "Self"    : use self comparisons
#'        - "Value"   : use a predefined normalization value
#' @param norm_metric (string) User-defined normalization metric.
#'        Valid values are:
#'        - "Median" : use the median of all targeted comparisons
#'        - "Mean"   : use the mean of all targeted comparisons
#'        - "Min"    : use the minimum of all targeted comparisons
#'        - "Max"    : use the maximum of all targeted comparisons.
#' @param norm_value (string) User input optional set normalization value.
#'                   This is only useful when setting norm_method = "Value"
#' @return A lambda expression applying the appropriate summary statistic.
get_norm_method <- function(
  norm_request,
  norm_method,
  norm_metric,
  norm_value = NULL
) {

  norm_metric_function <- grups.plots::get_norm_metric(norm_metric)
  if (norm_request) {
    switch(norm_method,
      "Pairwise" = function(x) {
        norm_metric_function(x[which(x$Self == FALSE), ]$Raw.Avg.PWD)
      },
      "Self"     = function(x) {
        norm_metric_function(x[which(x$Self == TRUE), ]$Raw.Avg.PWD)
      },
      "Value"                = function(x) {
        norm_value
      }
    )
  } else {
    function(x) 1
  }
}
