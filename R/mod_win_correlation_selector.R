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
  tagList(wellPanel(fluidRow(
    column(4,
           selectInput(ns("season"),
                       label="Select a Season",
                       choices=scope_seasons(),
                       selected = "2020-21")),
    column(4,
           selectInput(ns("variable"),
                       label="Select a Variable",
                       choices=scope_variables("win_pct"),
                       selected = "salary")),
    column(4,
           selectInput(ns("team"),
                       label="Highlight a team",
                       choices=scope_teams(type = "regular"),
                       selected = "Phoenix Suns"))
  ),
  actionButton(ns("change"),"Visualise", class = "btn-success"),
  shinyjs::disabled(downloadButton(ns("download"),"Download Visualisation",class = "btn-success")),
  br(),
  br(),
  HTML("<details>
  <summary>Visualisation Choices and Definitions</summary>
<h5 class = 'text-primary'> 1) Visualisation choices</h5>
  This visualisation is a <b>bump chart</b> of teams ranked by Win Percentage, and a specific variable (salary, off. efficiency or def. efficiency) in a given season, with a spotlight on the selected team.
  It is a nice way to highlight, for instance, the teams that manage great results with a lower roster salary.
         <br>
         <h5 class = 'text-primary'> 2) Definitions</h5>
        <ul style = 'padding-left:20px'>
           <li><b>Offensive Efficiency</b> : Offensive efficiency (or Offensive Rating) corresponds to the number of points scored by a team every 100 possessions - The higher the number the better the team is at offense</li>
           <li><b>Defensive Efficiency</b> : Defensive efficiency (or Defensive Rating) corresponds to the number of points scored against a team every 100 possessions - The lower the number the better the team is at defense</li>
         </ul>
         </details>")))
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
        team = reactive({input$team}),
        change = reactive({input$change}),
        download = reactive({input$download})
      )
    )

  })
}

## To be copied in the UI
# mod_win_correlation_selector_ui("win_correlation_selector_ui_1")

## To be copied in the server
# mod_win_correlation_selector_server("win_correlation_selector_ui_1")
