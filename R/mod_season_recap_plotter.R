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
  shinycustomloader::withLoader(ggiraph::girafeOutput(ns("plot")),
  type = "html",
  loader = "dnaspin")
}

#' season_recap_plotter Server Functions
#'
#' @noRd
mod_season_recap_plotter_server <- function(id, season_recap){
  moduleServer( id, function(input, output, session){

    change_plot <- eventReactive(season_recap$change(),
                                 {plot_season_recap( season = season_recap$season())})

    output$plot <- ggiraph::renderGirafe({
      change_plot()
    })

  })
}

## To be copied in the UI
# mod_season_recap_plotter_ui("season_recap_plotter_ui_1")

## To be copied in the server
# mod_season_recap_plotter_server("season_recap_plotter_ui_1")
