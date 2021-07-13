#' net_rating_plotter UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_net_rating_plotter_ui <- function(id){
  ns <- NS(id)
  girafeOutput(ns("plot"))
}

#' net_rating_plotter Server Functions
#'
#' @noRd
mod_net_rating_plotter_server <- function(id){
  moduleServer( id, function(input, output, session){
    output$plot <- renderGirafe({
      plot_teams_efficiency_interactive(season = net_rating$season())
    })
  })
}

## To be copied in the UI
# mod_net_rating_plotter_ui("net_rating_plotter_ui_1")

## To be copied in the server
# mod_net_rating_plotter_server("net_rating_plotter_ui_1")
