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
  wellPanel(fluidRow(
    column(12,
           selectInput(ns("season"),
                       label="Select a Season",
                       choices=scope_seasons(),
                       selected = "2020-21"))),
    actionButton(ns("change"),"Visualize")

  )
}

#' net_rating_selector Server Functions
#'
#' @noRd
mod_net_rating_selector_server <- function(id){
  moduleServer( id, function(input, output, session){
    return(
      list(
        season = reactive({input$season}),
        change = reactive({input$change})
      )
    )

  })
}

## To be copied in the UI
# mod_net_rating_selector_ui("net_rating_selector_ui_1")

## To be copied in the server
# mod_net_rating_selector_server("net_rating_selector_ui_1")
