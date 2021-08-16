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
  tagList(wellPanel(fluidRow(
    column(12,
           selectInput(ns("season"),
                       label="Select a Season",
                       choices=scope_seasons(),
                       selected = "2020-21"))),
    actionButton(ns("change"),"Visualise", class = "btn-success"),
    downloadButton(ns("download"),"Download Visualisation",class = "btn-success"),
    br(),
    br(),
    HTML("<details>
  <summary>Visualisation Choices and Definitions</summary>
<h5 class = 'text-primary'> 1) Visualisation choices</h5>
  This visualisation is a <b>mapping</b> of the 30 NBA teams in terms of Offensive and Defensive Efficiency.
  The better a given team is at Offense and Defense, the higher and to the right its position is within the mapping.
         <br>
         <h5 class = 'text-primary'> 2) Definitions</h5>
        <ul style = 'padding-left:20px'>
           <li><b>Offensive Efficiency</b> : Offensive efficiency (or Offensive Rating) corresponds to the number of points scored by a team every 100 possessions - The higher the number the better the team is at offense</li>
           <li><b>Defensive Efficiency</b> : Defensive efficiency (or Defensive Rating) corresponds to the number of points scored against a team every 100 possessions - The lower the number the better the team is at defense</li>
         </ul>
         </details>")))


}

#' net_rating_selector Server Functions
#'
#' @noRd
mod_net_rating_selector_server <- function(id){
  moduleServer( id, function(input, output, session){
    return(
      list(
        season = reactive({input$season}),
        change = reactive({input$change}),
        download = reactive({input$download})
      )
    )

  })
}

## To be copied in the UI
# mod_net_rating_selector_ui("net_rating_selector_ui_1")

## To be copied in the server
# mod_net_rating_selector_server("net_rating_selector_ui_1")
