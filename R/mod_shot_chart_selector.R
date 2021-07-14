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
                       choices=glue::glue("{seq(2000,2020)}-{stringr::str_sub(seq(2000,2020)+1,start = -2)}"))),
    column(6,
           selectInput(ns("player"),
                       label="Select a Player :",
                       choices=c("LeBron James","Kevin Durant","Stephen Curry","Kawhi Leonard")))
    )
}




#' shot_chart_selector Server Functions
#'
#' @noRd
mod_shot_chart_selector_server <- function(id){
  moduleServer( id, function(input, output, session){
    return(
      list(
        player = reactive({input$player}),
        season = reactive({input$season})
      )
    )
  })
}

## To be copied in the UI
# mod_shot_chart_selector_ui("shot_chart_selector_ui_1")

## To be copied in the server
# mod_shot_chart_selector_server("shot_chart_selector_ui_1")
