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
  tagList(wellPanel(fluidRow(
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
  actionButton(ns("change"),"Visualise", class = "btn-success"),
  downloadButton(ns("download"),"Download Visualisation",class = "btn-success"),
  br(),
  br(),
  HTML("<details>
  <summary>Visualisation Choices and Definitions</summary>
<h5 class = 'text-primary'> 1) Visualisation choices</h5>
This visualisation is a <b>mapping</b> of the best scorers in the NBA in terms of volume and efficiency.
  The players located on the top-right are the better scorers as they are both prolific and efficient scorers.
         <br>
         <h5 class = 'text-primary'> 2) Definitions</h5>
        <ul style = 'padding-left:20px'>
           <li><b>True Shooting Percentage</b> : A Compounded measure of a player's efficiency taking into consideration its 2 point shot %, 3 point shot % and Free Throw % as well as the volume of each shot type</li>
           <li><b>Scoring Rate</b> : Points scored every 75 possessions by a player, it is a measure of a player's scoring volume independent of the pace of a basket-ball game (meaning the number of possessions played in a game</li>
         </ul>
         </details>"))
  )
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
        change = reactive({input$change}),
        download = reactive({input$download})
      )
    )

  })
}

## To be copied in the UI
# mod_scoring_perf_selector_ui("scoring_perf_selector_ui_1")

## To be copied in the server
# mod_scoring_perf_selector_server("scoring_perf_selector_ui_1")
