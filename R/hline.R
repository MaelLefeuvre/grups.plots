#' (private) Generate a simple horizontal line in plotly
#' @export
#' @param y (numeric) vertical coordinate of the horizontal line
#' @param color (string) color of the horizontal line.
#' @return (list) a list defining a simple horizontal line for plotly
hline <- function(y, color = "red") {
  list(
    type = "line",
    y0   = y,
    y1   = y,
    x0   = 0,
    x1   = 1,
    xref = "paper",
    line = list(color = color, dash = "dot")
  )
}
