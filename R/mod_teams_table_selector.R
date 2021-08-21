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
  tagList(wellPanel(fluidRow(
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
  actionButton(ns("change"),"Visualise", class = "btn-success"),
  shinyjs::disabled(downloadButton(ns("download"),"Download Visualisation",class = "btn-success")),
  br(),
  br(),
  HTML("<details>
  <summary>Visualisation Choices and Definitions</summary>
<h5 class = 'text-primary'> 1) Visualisation choices</h5>
  This visualisation is a <b>table</b> ranking of the NBA teams in terms of Win Percentage, in a given conference, in a given season.
         <br>
         <h5 class = 'text-primary'> 2) Definitions</h5>
        <ul style = 'padding-left:20px'>
           <li><b>Offensive Efficiency</b> : Offensive efficiency (or Offensive Rating) corresponds to the number of points scored by a team every 100 possessions - The higher the number the better the team is at offense</li>
           <li><b>Defensive Efficiency</b> : Defensive efficiency (or Defensive Rating) corresponds to the number of points scored against a team every 100 possessions - The lower the number the better the team is at defense</li>
           <li><b>Net-rating</b> : Sixth Man of the Year is an award granted to the player perceived as the best among players who start the game off the bench</li>
           <li><b>All-Star</b> : An All-Star is a player selected to participate to the NBA All-Star Game. It is an award typically granted to the 24 best players in the NBA</li>
         </ul>
         </details>")))
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
        change = reactive({input$change}),
        download = reactive({input$download})
      )
    )

  })
}

## To be copied in the UI
# mod_teams_table_selector_ui("teams_table_selector_ui_1")

## To be copied in the server
# mod_teams_table_selector_server("teams_table_selector_ui_1")
