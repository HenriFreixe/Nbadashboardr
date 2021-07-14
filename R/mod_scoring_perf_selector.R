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
                       choices=glue::glue("{seq(2000,2020)}-{stringr::str_sub(seq(2000,2020)+1,start = -2)}"))),
    column(6,
           selectInput(ns("team"),
                       label="Select a Team :",
                       choices=get_team_standings()$team_name))
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
