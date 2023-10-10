#' @export
#' @importFrom utils read.table
#' @param path path leading to a GRUPS .probs summary file
#' @return dataframe containing svmop_probs for all pairs.
load_svmop_probs <- function(path) {
  read.table(path, sep = "\t", header = TRUE)
}
