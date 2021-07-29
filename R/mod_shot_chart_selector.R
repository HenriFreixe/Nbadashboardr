#' shot_chart_selector UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_shot_chart_selector_ui <- function(id){
  ns <- NS(id)
  fluidRow(
    column(6,
           selectInput(ns("season"),
                       label="Select a Season :",
                       choices = scope_seasons())),
    column(6,
           uiOutput(ns("secondSelection")))
    )
}



#' shot_chart_selector Server Functions
#'
#' @noRd
mod_shot_chart_selector_server <- function(id){
  moduleServer( id, function(input, output, session){
    return(
      list(
        season = reactive({input$season})
      )
    )
        output$secondSelection <- renderUI({selectInput(ns("player"),
                                               "Select a Player :",
                                               choices = scope_players(season))})
  })
}

## To be copied in the UI
# mod_shot_chart_selector_ui("shot_chart_selector_ui_1")

## To be copied in the server
# mod_shot_chart_selector_server("shot_chart_selector_ui_1")
