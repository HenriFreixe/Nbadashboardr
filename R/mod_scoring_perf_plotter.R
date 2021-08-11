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
  shinycustomloader::withLoader(ggiraph::girafeOutput(ns("plot")),
  type = "html",
  loader = "dnaspin")
}

#' scoring_perf_plotter Server Functions
#'
#' @noRd
mod_scoring_perf_plotter_server <- function(id, scoring_perf){
  moduleServer( id, function(input, output, session){

    change_plot <- eventReactive(scoring_perf$change(),
                                 {print(plot_scoring_rate(team = scoring_perf$team(),
                                                          season = scoring_perf$season()))})

    output$plot <- ggiraph::renderGirafe({
      change_plot()
    })

  })
}

## To be copied in the UI
# mod_scoring_perf_plotter_ui("scoring_perf_plotter_ui_1")

## To be copied in the server
# mod_scoring_perf_plotter_server("scoring_perf_plotter_ui_1")
