#' Utility wrapper to load a pair-specific '.sims' file into a dataframe
#' @export
#' @importFrom utils read.table
#' @param path path leading to a GRUPS '.sims' results file
#' @return dataframe containing simulation results for a given pair.
load_simfile <- function(path) {
  # check if header
  expected_header <- c(
    "replicate",
    "label",
    "parent0",
    "parent1",
    "parent0.id",
    "parent1.id",
    "pwd",
    "overlap",
    "avg"
  )
  has_header <- all(
    expected_header %in% gsub(" ", "", read.table(path, sep = "\t", nrow = 1))
  )
  data           <- read.table(path, sep = "\t", header = has_header)
  if (!has_header) {
    colnames(data) <- expected_header
  }
  # Reorder labels according to distribution average.
  data$label <- factor(data$label)

  tryCatch(
    expr = {
      data$label <- factor(
        data$label,
        levels = levels(data$label)[
          order(aggregate(avg ~ label, data, FUN = mean)$avg)
        ]
      )
    },
    error = function(cond) {
      msg <- paste(
        "Failed to aggregate relatedness labels according to average PWD",
        "values in simfile '", path, "'. This might be caused by a complete",
        "lack of overlap between the two individuals."
      )
    }
  )
  data
}