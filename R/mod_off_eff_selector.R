#' off_eff_selector UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_off_eff_selector_ui <- function(id){
  ns <- NS(id)
  fluidRow(
    column(12,
           selectInput(ns("team"),
                       label="Select a specific Team :",
                       choices=c("League Average","Los Angeles Lakers","Miami Heat","San Antonio Spurs")))

  )
}

#' off_eff_selector Server Functions
#'
#' @noRd
mod_off_eff_selector_server <- function(id){
  moduleServer( id, function(input, output, session){
    return(
      list(
        team = reactive({input$team})
      )
    )

  })
}

## To be copied in the UI
# mod_off_eff_selector_ui("off_eff_selector_ui_1")

## To be copied in the server
# mod_off_eff_selector_server("off_eff_selector_ui_1")
