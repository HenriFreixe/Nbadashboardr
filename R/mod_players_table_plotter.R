#' players_table_plotter UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_players_table_plotter_ui <- function(id){
  ns <- NS(id)
  shinycustomloader::withLoader(gt::gt_output(ns("table")),
  type = "html",
  loader = "dnaspin")
}

#' players_table_plotter Server Functions
#'
#' @noRd
mod_players_table_plotter_server <- function(id, players_table){
  moduleServer( id, function(input, output, session){

change_plot <- eventReactive(players_table$change(),
                             {plot_players_table(season = players_table$season(),
                                                 variable = players_table$variable())})


output$download <- downloadHandler(
  filename = function() {
    glue::glue("players_table_{players_table$variable()}_{players_table$season() %>% stringr::str_sub(end = 4) %>% as.integer() +1}.html")
  },
  content = function(file) {
    gt::gtsave(data = change_plot(),
               filename = file)

  }
)

output$table <- gt::render_gt({
      change_plot()}
    )

  })
}

## To be copied in the UI
# mod_players_table_plotter_ui("players_table_plotter_ui_1")

## To be copied in the server
# mod_players_table_plotter_server("players_table_plotter_ui_1")
