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
  shinycustomloader::withLoader(gt::gt_output(ns("table")),
  type = "html",
  loader = "dnaspin")
}

#' teams_table_plotter Server Functions
#'
#' @noRd
mod_teams_table_plotter_server <- function(id, teams_table){
  moduleServer( id, function(input, output, session){

    change_plot <- eventReactive(teams_table$change(),
                                 {plot_teams_table(conf = teams_table$conference(),
                                                   season = teams_table$season())})

    output$table <- gt::render_gt({
      change_plot()
    })

  })
}

## To be copied in the UI
# mod_teams_table_plotter_ui("teams_table_plotter_ui_1")

## To be copied in the server
# mod_teams_table_plotter_server("teams_table_plotter_ui_1")
