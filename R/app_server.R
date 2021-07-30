#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
shot_chart <- mod_shot_chart_selector_server("shot_chart")
scoring_perf <- mod_scoring_perf_selector_server("scoring_perf")

mod_shot_chart_plotter_server("shot_chart")
mod_scoring_perf_plotter_server("scoring_perf")
}
