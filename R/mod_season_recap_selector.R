#' season_recap_selector UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_season_recap_selector_ui <- function(id){
  ns <- NS(id)
  fluidRow(
    column(12,
           selectInput(ns("season"),
                       label="Select a Season :",
                       choices=glue::glue("{seq(2000,2020)}-{stringr::str_sub(seq(2000,2020)+1,start = -2)}")))

  )
}

#' season_recap_selector Server Functions
#'
#' @noRd
mod_season_recap_selector_server <- function(id){
  moduleServer( id, function(input, output, session){
    return(
      list(
        season = reactive({input$season})
      )
    )

  })

}

## To be copied in the UI
# mod_season_recap_selector_ui("season_recap_selector_ui_1")

## To be copied in the server
# mod_season_recap_selector_server("season_recap_selector_ui_1")
