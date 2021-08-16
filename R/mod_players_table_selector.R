#' players_table_selector UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_players_table_selector_ui <- function(id){
  ns <- NS(id)
  tagList(wellPanel(fluidRow(
    column(6,
           selectInput(ns("season"),
                       label="Select a Season",
                       choices=scope_seasons(),
                       selected = "2020-21")),
    column(6,
           selectInput(ns("variable"),
                       label="Select a Ranking Variable",
                       choices=scope_variables("players_table"),
                       selected = "bpm"))
  ),actionButton(ns("change"),"Visualise", class = "btn-success"),
  downloadButton(ns("download"),"Download Visualisation",class = "btn-success"),
  br(),
  br(),
  HTML("<details>
  <summary>Visualisation Choices and Definitions</summary>
<h5 class = 'text-primary'> 1) Visualisation choices</h5>
  This visualisation is a <b>table</b> of the 25 best NBA players in a given statistical category, in a given season.
  Apart from the player's individual achievements, the table also highlights team success.
         <br>
         <h5 class = 'text-primary'> 2) Definitions</h5>
        <ul style = 'padding-left:20px'>
           <li><b>BPM</b> : Box Plus-Minus is an all-in-one metric that assesses a player's value based on its statistical profile as well as the results of the team while on the court.</li>
           <li><b>Rebounds</b> : A rebound is performed when a player obtains the ball after a missed field goal attempt. Rebounds can be Offensive (when the player's team missed the shot) or Defensive (when the opponent missed the shot) </li>
           <li><b>Blocks</b> : A block is performed when a player tips or deflects an opponent's shot</li>
           <li><b>Steals</b> : A steal is performed when a player intercepts the ball</li>
           <li><b>All-Star</b> : An All-Star is a player selected to participate to the NBA All-Star Game. It is an award typically granted to the 24 best players in the NBA</li>
         </ul>
         </details>")))
}

#' players_table_selector Server Functions
#'
#' @noRd
mod_players_table_selector_server <- function(id){
  moduleServer( id, function(input, output, session){
    return(
      list(
        variable = reactive({input$variable}),
        season = reactive({input$season}),
        change = reactive({input$change}),
        download = reactive({input$download})
      )
    )

  })
}

## To be copied in the UI
# mod_players_table_selector_ui("players_table_selector_ui_1")

## To be copied in the server
# mod_players_table_selector_server("players_table_selector_ui_1")
