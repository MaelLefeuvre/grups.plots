#' @export
#' @import shiny
#' @import plotly
#' @import catmaply
#' @import viridis
#' @importFrom dplyr %>% bind_rows
#' @importFrom reshape2 melt
#' @param kinship_matrix Kinship matrix
#' @param dimensions Shiny input dimensions. in pixels c(row, height)
#' @param order Order in which kinship degrees should be arranged.
#' @param ... Additional catmaply::catmaply() arguments.
#' @return a Plotted kinship matrix
plot_kinship_matrix <- function(kinship_matrix, dimensions, order, color_palette = NULL, ...) {

  plot_title   <- "<b>Kinship matrix</b>"
  legend_title <- "<b>Relationship</b>"

  kinship_matrix$rel <- factor(
    x      = kinship_matrix$rel,
    levels = order
  )

  # Add self-comparisons with NA values if there are none
  no_twins_found <- !any(kinship_matrix$Left_ind == kinship_matrix$Right_ind)
  if (no_twins_found) {
    unique_samples <- unique(c(kinship_matrix$Left_ind, kinship_matrix$Right_ind))
    kinship_matrix <- dplyr::bind_rows(
      kinship_matrix,
      data.frame(Left_ind = unique_samples, Right_ind = unique_samples, rel = NA)
    )
  }

  # Force catmaply to preserve the order found within the .result file, and thus neatly print a half corr.plot
  kinship_matrix$x_order <- as.numeric(factor(kinship_matrix$Left_ind, levels = unique(kinship_matrix$Left_ind)))
  kinship_matrix$y_order <- as.numeric(factor(kinship_matrix$Right_ind, levels = unique(kinship_matrix$Left_ind)))

  # Size of the kinship matrix is the minimum between
  # the width and the height of the window.
  # Offset by the approx. size of the banner.
  plot_size <- -100 + as.numeric(min(dimensions))

  if (is.null(color_palette)) {
    color_palette <- viridis::viridis_pal(
      option    = "D",
      direction = -1
    )(length(unique(kinship_matrix$rel)))
  }

  tickfont        <- list(size = 16)
  legendfont      <- list(size = 16)
  legendtitlefont <- list(size = 20)

  catmaply::catmaply(
    df             = kinship_matrix,
    x              = Left_ind,
    x_tickangle    = 60,
    x_order        = x_order,
    y_order        = y_order,
    y              = Right_ind,
    y_tickangle    = 20,
    z              = rel,
    rangeslider    = FALSE,
    color_palette  = color_palette,
    hover_template = paste(
      "<b>Pair</b>:", Left_ind, "-", Right_ind,
      "<br><b>Relationship:</b>:", rel,
      "<extra></extra>"
    ),
    ...
  ) %>% plotly::layout(
    title    = list(text = plot_title),
    autosize = TRUE,
    width    = plot_size,
    height   = plot_size,
    xaxis    = list(
      fixedrange = FALSE,
      autorange  = TRUE,
      automargin = TRUE,
      tickfont   = tickfont
    ),
    yaxis    = list(
      fixedrange = FALSE,
      autorange  = TRUE,
      automargin = TRUE,
      tickfont   = tickfont
    ),
    legend   = list(
        font    = legendfont,
        title   = list(text = legend_title, font = legendtitlefont),
        y       = 0,
        x       = 1,
        yanchor = "bottom",
        xanchor = "right",
        yref    = "paper",
        xref    = "paper"
    )
  ) %>%
  plotly::config(
    editable             = TRUE,
    displaylogo          = FALSE,
    scrollZoom           = TRUE,
    toImageButtonOptions = list(
      format   = "svg",
      filename = "kinship-matrix"
    )
  )
}
