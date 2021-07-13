#' net_rating_selector UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_net_rating_selector_ui <- function(id){
  ns <- NS(id)
  fluidRow(
    column(12,
           selectInput(ns("season"),
                       label="Select a Season :",
                       choices=c("2020-21","2019-20","2018-19","2017-18")))

  )
}

#' net_rating_selector Server Functions
#'
#' @noRd
mod_net_rating_selector_server <- function(id){
  moduleServer( id, function(input, output, session){
    return(
      list(
        season = reactive({input$season})
      )
    )

  })
}

## To be copied in the UI
# mod_net_rating_selector_ui("net_rating_selector_ui_1")

## To be copied in the server
# mod_net_rating_selector_server("net_rating_selector_ui_1")
