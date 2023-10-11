#' Utility wrapper to load a '.result' file into a dataframe
#' @export
#' @importFrom utils read.table
#' @param path path leading to a GRUPS-rs .result summary file
#' @return dataframe containing simulation results for all pairs.
load_res_file <- function(path) {
  read.table(path, sep = "\t", header = TRUE)
}