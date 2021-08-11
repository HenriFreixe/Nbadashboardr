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
  wellPanel(fluidRow(
    column(6,
           selectInput(ns("season"),
                       label="Select a Season",
                       choices=scope_seasons(),
                       selected = "2020-21")),
    column(6,
           selectInput(ns("variable"),
                       label="Select a Ranking Variable",
                       choices=scope_variables("player_rank"),
                       selected = "bpm"))
  ),
  actionButton(ns("change"),"Visualize"))
}

#' player_rank_selector Server Functions
#'
#' @noRd
mod_player_rank_selector_server <- function(id){
  moduleServer( id, function(input, output, session){
    return(
      list(
        variable = reactive({input$variable}),
        season = reactive({input$season}),
        change = reactive({input$change})
      )
    )

  })
}

## To be copied in the UI
# mod_player_rank_selector_ui("player_rank_selector_ui_1")

## To be copied in the server
# mod_player_rank_selector_server("player_rank_selector_ui_1")
