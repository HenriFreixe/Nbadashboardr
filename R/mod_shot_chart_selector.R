#' shot_chart_selector UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_shot_chart_selector_ui <- function(id){
  ns <- NS(id)
  wellPanel(fluidRow(
    column(6,
           selectInput(ns("season"),
                       label="Select a Season",
                       choices = scope_seasons(),
                       selected = "2020-21")),
    column(6,
           selectInput(ns("player"),
                       label = "Select a Player",
                       choices = scope_players("2020-21"),
                       selected = "Stephen Curry"))
    ),
    actionButton(ns("change"),"Visualize"))
}



#' shot_chart_selector Server Functions
#'
#' @noRd
mod_shot_chart_selector_server <- function(id){
  moduleServer( id, function(input, output, session){

        #output$secondSelection <- renderUI({selectInput(session$ns("player"),
        #                                       "Select a Player",
        #                                       choices = scope_players(input$season),
        #                                       selected = "Stephen Curry")})



    observeEvent(input$season,
                 {updateSelectInput(session,
                                   "player",
                                   label = "Select a Player",
                                   choices = scope_players(input$season))})
        return(
          list(
            season = reactive({input$season}),
            player = reactive({input$player}),
            change = reactive({input$change})
          )
        )
  })
}

## To be copied in the UI
# mod_shot_chart_selector_ui("shot_chart_selector_ui_1")

## To be copied in the server
# mod_shot_chart_selector_server("shot_chart_selector_ui_1")
