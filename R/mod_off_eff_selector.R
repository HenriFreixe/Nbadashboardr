#' off_eff_selector UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_off_eff_selector_ui <- function(id){
  ns <- NS(id)
  tagList(wellPanel(fluidRow(
    column(12,
           selectInput(ns("team"),
                       label = "Select a specific Team",
                       choices = scope_teams(type = "average"),
                       selected = "global"))

  ),
  actionButton(ns("change"),"Visualise", class = "btn-success"),
  downloadButton(ns("download"),"Download Visualisation",class = "btn-success"),
  br(),
  br(),
  HTML("<details>
  <summary>Visualisation Choices and Definitions</summary>
<h5 class = 'text-primary'> 1) Visualisation choices</h5>
  This visualisation is a <b>parallel line chart and bar chart</b> of the evolution of average offensive efficiency on the one hand, and of the share of shots taken that are three point shots on the other hand.
It turns out that trading off mid-range shots for three point shots correlates nicely with offensive efficiency.
         <br>
         <h5 class = 'text-primary'> 2) Definitions</h5>
        <ul style = 'padding-left:20px'>
           <li><b>Offensive Efficiency</b> : Offensive efficiency (or Offensive Rating) corresponds to the number of points scored by a team every 100 possessions. The higher the number the better the team is at offense</li>
           <li><b>Mid-range shots</b> : Mid-range shots are shots that are neither worth 3 points nor near the basket. While they are typically scored at a higher percentage than 3 point shots since they are shorter-ranged, they typically yield much lower value in terms of efficiency as the percentage difference does not make-up for the difference in value</li>
         </ul>
         </details>")))
}

#' off_eff_selector Server Functions
#'
#' @noRd
mod_off_eff_selector_server <- function(id){
  moduleServer( id, function(input, output, session){


    return(
      list(
        team = reactive({input$team}),
        change = reactive({input$change}),
        download = reactive({input$download})
      )
    )

  })
}

## To be copied in the UI
# mod_off_eff_selector_ui("off_eff_selector_ui_1")

## To be copied in the server
# mod_off_eff_selector_server("off_eff_selector_ui_1")
