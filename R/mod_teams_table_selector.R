#' teams_table_selector UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_teams_table_selector_ui <- function(id){
  ns <- NS(id)
  wellPanel(fluidRow(
    column(6,
           selectInput(ns("season"),
                       label="Select a Season",
                       choices=scope_seasons(),
                       selected = "2020-21")),
    column(6,
           selectInput(ns("conference"),
                       label="Select a Conference",
                       choices=c("Both Conferences" = "both","Eastern Conference" ="East","Western Conference"="West"),
                       selected = "both"))
  ),
  actionButton(ns("change"),"Visualize"))
}

#' teams_table_selector Server Functionsboth
#'
#' @noRd
mod_teams_table_selector_server <- function(id){
  moduleServer( id, function(input, output, session){
    return(
      list(
        season = reactive({input$season}),
        conference = reactive({input$conference}),
        change = reactive({input$change})
      )
    )

  })
}

## To be copied in the UI
# mod_teams_table_selector_ui("teams_table_selector_ui_1")

## To be copied in the server
# mod_teams_table_selector_server("teams_table_selector_ui_1")
