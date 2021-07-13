#' scoring_perf_plotter UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_scoring_perf_plotter_ui <- function(id){
  ns <- NS(id)
  girafeOutput(ns("plot"))
}

#' scoring_perf_plotter Server Functions
#'
#' @noRd
mod_scoring_perf_plotter_server <- function(id){
  moduleServer( id, function(input, output, session){
    output$plot <- renderGirafe({
      plot_scoring_rate(team = scoring_perf$team(),
                 season = scoring_perf$season())
    })

  })
}

## To be copied in the UI
# mod_scoring_perf_plotter_ui("scoring_perf_plotter_ui_1")

## To be copied in the server
# mod_scoring_perf_plotter_server("scoring_perf_plotter_ui_1")
