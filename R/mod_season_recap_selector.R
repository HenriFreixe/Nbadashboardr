#' season_recap_selector UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_season_recap_selector_ui <- function(id){
  ns <- NS(id)
  tagList(wellPanel(fluidRow(
    column(12,
           selectInput(ns("season"),
                       label="Select a Season",
                       choices=scope_seasons(),
                       selected = "2020-21"))

  ),
  actionButton(ns("change"),"Visualise", class = "btn-success"),
  shinyjs::disabled(downloadButton(ns("download"),"Download Visualisation",class = "btn-success")),
  br(),
  br(),
  HTML("<details>
  <summary>Visualisation Choices and Definitions</summary>
<h5 class = 'text-primary'> 1) Visualisation choices</h5>
  This visualisation regroups three different visualisations and aims at providing a <b>visual overview</b> of a given NBA Season.
The three combined visualisations are a <b>scatterplotted lollipop plot</b> that highlights the outcome of the NBA Finals, a <b>text plot</b> providing statistical insights about the recipients of the main NBA Awards and an <b>interactive facetted scatterplot</b> about the NBA All-Stars.
         <br>
         <h5 class = 'text-primary'> 2) Definitions</h5>
        <ul style = 'padding-left:20px'>
           <li><b>All-Star</b> : An All-Star is a player selected to participate to the NBA All-Star Game. It is an award typically granted to the 24 best players in the NBA</li>
           <li><b>Sixth Man of the Year</b> : Sixth Man of the Year is an award granted to the player perceived as the best among players who start the game off the bench</li>
           <li><b>Rookie of the Year</b> : Rookie of the Year is an award granted to the player perceived as the best among new players</li>
         </ul>
         </details>")))
}

#' season_recap_selector Server Functions
#'
#' @noRd
mod_season_recap_selector_server <- function(id){
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
# mod_season_recap_selector_ui("season_recap_selector_ui_1")

## To be copied in the server
# mod_season_recap_selector_server("season_recap_selector_ui_1")
