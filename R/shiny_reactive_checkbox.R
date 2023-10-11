#' (private) generate a reactive Shiny UI checkbox
#' @export
#' @import shiny
#' @param values [string] vector of checkbox elements.
#' @param title  (string) title of the div / section.
#' @param input_id shiny input id of the target shiny html/css.
#' @param ncol number of display columns.
#' @return list containing shiny css style, header, and checkboxgroupinput.
shiny_reactive_checkbox <- function(values, title, input_id, ncol = 2) {

  css_style <- paste0(
    "column-count: ", ncol, ";
    -webkit-column-count: ", ncol, "; /* Chrome, Safari, Opera */ 
    -moz-column-count: ", ncol, ";    /* Firefox */ 
    -moz-column-fill: auto;
    -column-fill: balance;
    margin: 10px
    "
  )

  list(
    shiny::h5(title),
    shiny::actionButton(paste0(input_id, "_select"),   label = "Select all"),
    shiny::actionButton(paste0(input_id, "_deselect"), label = "Deselect All"),
    shiny::tags$div(
      align = "justify",
      style = css_style,
      shiny::checkboxGroupInput(
        inputId  = input_id,
        label    = NULL,
        selected = values,
        inline   = FALSE,
        values
      )
    )
  )
}