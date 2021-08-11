#' off_eff_plotter UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_off_eff_plotter_ui <- function(id){
  ns <- NS(id)
  shinycustomloader::withLoader(ggiraph::girafeOutput(ns("plot")),
                                type = "html",
                                loader = "dnaspin")
}

#' off_eff_plotter Server Functions
#'
#' @noRd
mod_off_eff_plotter_server <- function(id, off_eff){
  moduleServer( id, function(input, output, session){

    change_plot <- eventReactive(off_eff$change(),
                                 {plot_off_evo_interactive(team = off_eff$team())}
    )

    output$plot <- ggiraph::renderGirafe({
      change_plot()
    })

  })
}

## To be copied in the UI
# mod_off_eff_plotter_ui("off_eff_plotter_ui_1")

## To be copied in the server
# mod_off_eff_plotter_server("off_eff_plotter_ui_1")
