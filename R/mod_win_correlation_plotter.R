#' win_correlation_plotter UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_win_correlation_plotter_ui <- function(id){
  ns <- NS(id)
  plotOutput(ns("plot"))
}

#' win_correlation_plotter Server Functions
#'
#' @noRd
mod_win_correlation_plotter_server <- function(id){
  moduleServer( id, function(input, output, session){
    output$plot <- renderPlot({
      plot_off_eff(season = win_correlation$season(),
                   variable = win_correlation$variable(),
                   team = win_correlation$team())
    })

  })
}


## To be copied in the UI
# mod_win_correlation_plotter_ui("win_correlation_plotter_ui_1")

## To be copied in the server
# mod_win_correlation_plotter_server("win_correlation_plotter_ui_1")
