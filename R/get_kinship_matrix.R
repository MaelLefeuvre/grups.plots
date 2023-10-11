#' Generate a kinship matrix summarizing the classification results of the
#' 'pedigree-sims' of GRUPS-rs.
#' @export
#' @importFrom tidyr separate_wider_regex
#' @importFrom dplyr mutate
#' @param data (dataframe) a GRUPS-rs '.result' dataframe, containing pairs of
#'        individuals and their estimated most likely relationship. This will
#'        most likely be the output of grups.plots::load_res_file()
#' @param sample_regex (string) Specify the regular expression used to extract
#'        sample names from the 'Pair_name' column.
#' @return A dataframe, representing a "melted" kinship matrix.
get_kinship_matrix <- function(data, sample_regex) {
  data.frame(
    pair = data$Pair_name,
    rel  = data$Most_Likely_rel
  ) %>% tidyr::separate_wider_regex(
    col   = pair,
    patterns = c(Left_ind = sample_regex, Right_ind = sample_regex),
  ) %>% dplyr::mutate( # Remove delimiter from first match.
    Left_ind = sub("-$", "", Left_ind)
  )
}