#' player_rank_selector UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_player_rank_selector_ui <- function(id){
  ns <- NS(id)
  fluidRow(
    column(6,
           selectInput(ns("season"),
                       label="Select a Season :",
                       choices=scope_seasons())),
    column(6,
           selectInput(ns("variable"),
                       label="Select a Ranking Variable :",
                       choices=c("Points per Game","Rebounds per Game","Assists per Game","Blocks per Game","Steals per Game","Box Plus-Minus")))
  )
}

#' player_rank_selector Server Functions
#'
#' @noRd
mod_player_rank_selector_server <- function(id){
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
# mod_player_rank_selector_ui("player_rank_selector_ui_1")

## To be copied in the server
# mod_player_rank_selector_server("player_rank_selector_ui_1")
