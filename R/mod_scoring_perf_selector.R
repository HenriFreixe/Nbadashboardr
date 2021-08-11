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
  wellPanel(fluidRow(
    column(6,
           selectInput(ns("season"),
                       label="Select a Season",
                       choices=scope_seasons(),
                       selected = "2020-21")),
    column(6,
           selectInput(ns("team"),
                       label="Select a Team",
                       choices=scope_teams(type = "no"),
                       selected = "global"))
  ),
  actionButton(ns("change"),"Visualize"))
}

#' scoring_perf_selector Server Functions
#'
#' @noRd
mod_scoring_perf_selector_server <- function(id){
  moduleServer( id, function(input, output, session){
    return(
      list(
        team = reactive({input$team}),
        season = reactive({input$season}),
        change = reactive({input$change})
      )
    )

  })
}

## To be copied in the UI
# mod_scoring_perf_selector_ui("scoring_perf_selector_ui_1")

## To be copied in the server
# mod_scoring_perf_selector_server("scoring_perf_selector_ui_1")
