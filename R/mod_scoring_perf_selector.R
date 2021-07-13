#' scoring_perf_selector UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_scoring_perf_selector_ui <- function(id){
  ns <- NS(id)
  fluidRow(
    column(6,
           selectInput(ns("season"),
                       label="Select a Season :",
                       choices=c("2020-21","2019-20","2018-19","2017-18"))),
    column(6,
           selectInput(ns("team"),
                       label="Select a Team :",
                       choices=c("No Team","Los Angeles Lakers","San Antoio Spurs","Miami Heat")))
  )
}

#' scoring_perf_selector Server Functions
#'
#' @noRd
mod_scoring_perf_selector_server <- function(id){
  moduleServer( id, function(input, output, session){
    return(
      list(
        team = reactive({input$team}),
        season = reactive({input$season})
      )
    )

  })
}

## To be copied in the UI
# mod_scoring_perf_selector_ui("scoring_perf_selector_ui_1")

## To be copied in the server
# mod_scoring_perf_selector_server("scoring_perf_selector_ui_1")
