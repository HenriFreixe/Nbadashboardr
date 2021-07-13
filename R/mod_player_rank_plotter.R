#' player_rank_plotter UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_player_rank_plotter_ui <- function(id){
  ns <- NS(id)
  girafeOutput(ns("plot"))
}

#' player_rank_plotter Server Functions
#'
#' @noRd
mod_player_rank_plotter_server <- function(id){
  moduleServer( id, function(input, output, session){
    output$plot <- renderGirafe({
      plot_player_ranking_interactive(variable = player_rank$variable(),
                 season = player_rank$season())
    })

  })
}


## To be copied in the UI
# mod_player_rank_plotter_ui("player_rank_plotter_ui_1")

## To be copied in the server
# mod_player_rank_plotter_server("player_rank_plotter_ui_1")
