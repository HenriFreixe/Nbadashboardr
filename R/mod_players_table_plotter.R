#' players_table_plotter UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_players_table_plotter_ui <- function(id){
  ns <- NS(id)
  gt_output("table")
}

#' players_table_plotter Server Functions
#'
#' @noRd
mod_players_table_plotter_server <- function(id){
  moduleServer( id, function(input, output, session){
    output$table <- render_gt({
      plot_players_table(variable = players_table$variable(),
                                      season = players_table$season())
    })

  })
}

## To be copied in the UI
# mod_players_table_plotter_ui("players_table_plotter_ui_1")

## To be copied in the server
# mod_players_table_plotter_server("players_table_plotter_ui_1")
