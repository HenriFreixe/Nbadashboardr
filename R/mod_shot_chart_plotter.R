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
  plotOutput(ns("plot"))
}

#' shot_chart_plotter Server Function
#'
#' @noRd
mod_shot_chart_plotter_server <- function(input, output, session){
  moduleServer(id, function(input, output, session) {
    output$plot <- renderPlot({
      plotting_function(season_choice = seasons$season_choice(),
                        all_seasons = seasons$all_seasons(),
                        data,
                        characters)
    })
  })
#### Encoursssss
}

## To be copied in the UI
# mod_shot_chart_plotter_ui("shot_chart_plotter_ui_1")

## To be copied in the server
# callModule(mod_shot_chart_plotter_server, "shot_chart_plotter_ui_1")

