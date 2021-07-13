#' season_recap_plotter UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_season_recap_plotter_ui <- function(id){
  ns <- NS(id)
  girafeOutput(ns("plot"))
}

#' season_recap_plotter Server Functions
#'
#' @noRd
mod_season_recap_plotter_server <- function(id){
  moduleServer( id, function(input, output, session){
    output$plot <- renderGirafe({
      plot_season_recap( season = season_recap$season())
    })

  })
}

## To be copied in the UI
# mod_season_recap_plotter_ui("season_recap_plotter_ui_1")

## To be copied in the server
# mod_season_recap_plotter_server("season_recap_plotter_ui_1")
