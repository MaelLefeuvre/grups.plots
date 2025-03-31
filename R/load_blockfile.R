#' Load a pair-specific '.blk' file into a simple dataframe.
#' @export
#' @importFrom utils read.table
#' @import zoo
#' @importFrom zoo rollapply
#' @param path path leading to a GRUPS pair-specific '.blk' results file
#' @param width The desired which of every window (in Mbases)
#' @param step The desired sliding step between every window (in Mbases)
#' @return a dataframe with columns "chr" "start" "end" "overlap" "pwd"
load_blockfile <- function(path, width, step = 1) {
  # -- Check if header:
  expected_header <- c("chr", "start", "end", "overlap", "pwd")
  has_header <- all(
    gsub(" ", "", read.table(path, sep = "\t", nrow = 1)) == expected_header
  )
  data <- read.table(
    path,
    sep       = "\t",
    header    = has_header,
    col.names = expected_header
  )

  do.call("rbind",
    lapply(
      unique(data$chr),
      FUN = function(chr) {
        subset_data <- data[which(data$chr == chr), ]
        rolled <- zoo::rollapply(
          data  = zoo::zoo(subset_data),
          width = width,
          by    = step,
          FUN   = sum,
          align = "left"
        )

        # Get window start values. (output $start of rollapply is the sum)
        rolled$start <- na.omit(
          head(subset_data$start, -width + step)[
            seq(nrow(subset_data)) %% step == 0
          ]
        )

        data.frame(
          chr = chr,
          avg_pwd = rolled$pwd / rolled$overlap,
          start = rolled$start,
          row.names = NULL
        )
      }
    )
  )
}
