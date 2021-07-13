#' teams_table_plotter UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_teams_table_plotter_ui <- function(id){
  ns <- NS(id)
  gt_output(ns("table"))
}

#' teams_table_plotter Server Functions
#'
#' @noRd
mod_teams_table_plotter_server <- function(id){
  moduleServer( id, function(input, output, session){
    output$table <- render_gt({
      plot_players_table(conference = teams_table$conference(),
                         season = teams_table$season())
    })

  })
}

## To be copied in the UI
# mod_teams_table_plotter_ui("teams_table_plotter_ui_1")

## To be copied in the server
# mod_teams_table_plotter_server("teams_table_plotter_ui_1")
