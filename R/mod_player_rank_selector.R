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
  tagList(wellPanel(fluidRow(
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
  actionButton(ns("change"),"Visualize", class = "btn-success"),
  downloadButton(ns("download"),"Download Visualization",class = "btn-success"),
  br(),
  br(),
  HTML("<details>
  <summary>Visualisation Choices and Definitions</summary>
<h5 class = 'text-primary'> 1) Visualisation choices</h5>
This visualisation is a <b>ranked bar chart</b> of the 10 best NBA players in a given statistical category, in a given season.
  In relevant instances (Box Plus-Minus and Rebounding), the offensive and defensive components of the metric are highlighted so as to put forth the statistical particularities of each player.
         Furthermore, it gives valuable information concerning the player's preferred shooting spots.
         <br>
         <h5 class = 'text-primary'> 2) Definitions</h5>
        <ul style = 'padding-left:20px'>
           <li><b>BPM</b> : Box Plus-Minus is an all-in-one metric that assesses a player's value based on its statistical profile as well as the results of the team while on the court.</li>
           <li><b>Rebounds</b> : A rebound is performed when a player obtains the ball after a missed field goal attempt. Rebounds can be Offensive (when the player's team missed the shot) or Defensive (when the opponent missed the shot) </li>
           <li><b>Blocks</b> : A block is performed when a player tips or deflects an opponent's shot</li>
           <li><b>Steals</b> : A steal is performed when a player intercepts the ball</li>
         </ul>
         </details>")))
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
        change = reactive({input$change}),
        download = reactive({input$download})
      )
    )

  })
}

## To be copied in the UI
# mod_player_rank_selector_ui("player_rank_selector_ui_1")

## To be copied in the server
# mod_player_rank_selector_server("player_rank_selector_ui_1")
