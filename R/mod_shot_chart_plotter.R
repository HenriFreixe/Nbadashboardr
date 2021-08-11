#' shot_chart_plotter UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_shot_chart_plotter_ui <- function(id){
  ns <- NS(id)

  shinycustomloader::withLoader(ggiraph::girafeOutput(ns("plot")),
  type = "html",
  loader = "dnaspin")
}

#' shot_chart_plotter Server Function
#'
#' @noRd
mod_shot_chart_plotter_server <- function(id, shot_chart){
  moduleServer(id, function(input, output, session) {


    change_plot <- eventReactive(shot_chart$change(),
                                 {plot_court(player = shot_chart$player(),
                                             season = shot_chart$season(),
                                             summary = TRUE)}
                            )

    output$plot <- ggiraph::renderGirafe(
      change_plot()
    )
  })
}

## To be copied in the UI
# mod_shot_chart_plotter_ui("shot_chart_plotter_ui_1")

## To be copied in the server
# callModule(mod_shot_chart_plotter_server, "shot_chart_plotter_ui_1")

