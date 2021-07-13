#' players_table_selector UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_players_table_selector_ui <- function(id){
  ns <- NS(id)
  fluidRow(
    column(6,
           selectInput(ns("season"),
                       label="Select a Season :",
                       choices=c("2020-21","2019-20","2018-19","2017-18"))),
    column(6,
           selectInput(ns("variable"),
                       label="Select a Ranking Variable :",
                       choices=c("Points per Game","Rebounds per Game","Assists per Game","Blocks per Game","Steals per Game","Box Plus-Minus")))
  )
}

#' players_table_selector Server Functions
#'
#' @noRd
mod_players_table_selector_server <- function(id){
  moduleServer( id, function(input, output, session){
    return(
      list(
        variable = reactive({input$variable}),
        season = reactive({input$season})
      )
    )

  })
}

## To be copied in the UI
# mod_players_table_selector_ui("players_table_selector_ui_1")

## To be copied in the server
# mod_players_table_selector_server("players_table_selector_ui_1")
