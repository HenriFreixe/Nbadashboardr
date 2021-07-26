#' win_correlation_selector UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_win_correlation_selector_ui <- function(id){
  ns <- NS(id)
  fluidRow(
    column(4,
           selectInput(ns("season"),
                       label="Select a Season :",
                       choices=scope_seasons())),
    column(4,
           selectInput(ns("variable"),
                       label="Select a Variable to Correlate to Win Percentage :",
                       choices=c("Salary","Offensive Efficiency","Defensive Efficiency"))),
    column(4,
           selectInput(ns("team"),
                       label="Highlight a specific team :",
                       choices=scope_teams(type = "teams")))
  )
}

#' win_correlation_selector Server Functions
#'
#' @noRd
mod_win_correlation_selector_server <- function(id){
  moduleServer( id, function(input, output, session){
    return(
      list(
        season = reactive({input$season}),
        variable = reactive({input$variable}),
        team = reactive({input$team})
      )
    )

  })
}

## To be copied in the UI
# mod_win_correlation_selector_ui("win_correlation_selector_ui_1")

## To be copied in the server
# mod_win_correlation_selector_server("win_correlation_selector_ui_1")
