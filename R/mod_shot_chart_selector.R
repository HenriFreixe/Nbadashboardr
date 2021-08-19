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
  tagList(wellPanel(fluidRow(
    column(6,
           selectInput(ns("season"),
                       label="Select a Season",
                       choices = scope_seasons(),
                       selected = "2020-21")),
    column(6,
           selectInput(ns("player"),
                       label = "Select a Player",
                       choices = scope_players("2020-21")))
    ),
    actionButton(ns("change"),"Visualise", class = "btn-success"),
    downloadButton(ns("download"),"Download Visualisation",class = "btn-success"),
    br(),
    br(),
    HTML("<details>
  <summary>Visualisation Choices and Definitions</summary>
<h5 class = 'text-primary'> 1) Visualisation choices</h5>
  The visualisation is inspired from Espn's Kirk Goldsberry's infamous <b>shot charts</b>, and <a href='https://www.owenlhjphillips.com/new-blog/2020/6/25/how-to-make-nba-shots-charts-in-r' class = 'text-secondary'>Owen Phillips' adaptation of the ballr package</a>.
         It is a <b>shooting proficiency scatterplot</b> with a size variable associated with volume and a color variable associated with efficiency.
         It provides insights as to how prolific and efficient the selected player was at shooting.
         Furthermore, it gives valuable information concerning the player's preferred shooting spots.
         <br>
         <h5 class = 'text-primary'> 2) Definitions</h5>
        <ul style = 'padding-left:20px'>
           <li><b>Shooting Volume</b> : Number of shots attempted by the player in each location</li>
           <li><b>Shooting Efficiency</b> : Difference between the Shooting Percentage the player achieved and the League Average in each location</li>
         </ul>
         </details>")))
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
            change = reactive({input$change}),
            download = reactive({input$download})
          )
        )
  })
}

## To be copied in the UI
# mod_shot_chart_selector_ui("shot_chart_selector_ui_1")

## To be copied in the server
# mod_shot_chart_selector_server("shot_chart_selector_ui_1")
