#' Normalize the results from the 'pwd-from-stdin' output of GRUPS-rs
#' @export
#' @param norm_method (string) User-defined string specifying which method, or
#'        subset of comparisons should be used. Valid values are:
#'        - "Pairwise": use pairwise comparisons
#'        - "Self"    : use self comparisons
#'        - "Value"   : use a predefined normalization value
#' @param norm_request (bool) Whether or not results should be output as ratios
#'        of the normalization value.
#' @param norm_metric (string) User-defined normalization metric.
#'        Valid values are:
#'        - "Median" : use the median of all targeted comparisons
#'        - "Mean"   : use the mean of all targeted comparisons
#'        - "Min"    : use the minimum of all targeted comparisons
#'        - "Max"    : use the maximum of all targeted comparisons.
#' @param norm_value (sting) User input optional set normalization value.
#'        This is only useful when setting norm_method = "Value"
#' @param pwd_data (dataframe) A `.pwd` results file dataframe. Most likely the
#'        output of grups.plot::load_pairwise_file()
#' @return a list of thresholds defining the degree of relatedness of each
#'         pbserved pairwise misamtch rate.
get_norm_values <- function(
  norm_method,
  norm_request = FALSE,
  norm_metric,
  norm_value,
  pwd_data
) {
  norm_metric_function <- grups.plots::get_norm_metric(norm_metric)

  norm_value <- switch(norm_method,
    "Pairwise" = norm_metric_function(
      pwd_data[which(pwd_data$Self == FALSE), ]$Norm.Avg, na.rm = TRUE
    ),
    "Self"     = norm_metric_function(
      pwd_data[which(pwd_data$Self == TRUE), ]$Norm.Avg, na.rm = TRUE
    ),
    "Value"    = ifelse(norm_request, 1, norm_value)
  )

  switch(norm_method,
    "Pairwise" = list(Self      = norm_value / 2,
                      First     = (norm_value / 2) * (3  / 2),
                      Second    = (norm_value / 2) * (7  / 4),
                      Third     = (norm_value / 2) * (15 / 8),
                      Unrelated = norm_value
                     ),
    "Self"     = list(Self      = norm_value,
                      First     = (3  / 2) * norm_value,
                      Second    = (7  / 4) * norm_value,
                      Third     = (15 / 8) * norm_value,
                      Unrelated = 2 * norm_value
                     ),
    "Value"   = list(Self      = norm_value / 2,
                     First     = (norm_value / 2) * (3  / 2),
                     Second    = (norm_value / 2) * (7  / 4),
                     Third     = (norm_value / 2) * (15 / 8),
                     Unrelated = norm_value
                    )
  )
}