#' off_eff_plotter UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_off_eff_plotter_ui <- function(id){
  ns <- NS(id)
  girafeOutput(ns("plot"))
}

#' off_eff_plotter Server Functions
#'
#' @noRd
mod_off_eff_plotter_server <- function(id){
  moduleServer( id, function(input, output, session){
    output$plot <- renderGirafe({
      plot_teams_efficiency_interactive(team = off_eff$team())
    })

  })
}

## To be copied in the UI
# mod_off_eff_plotter_ui("off_eff_plotter_ui_1")

## To be copied in the server
# mod_off_eff_plotter_server("off_eff_plotter_ui_1")
