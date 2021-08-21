#' scoring_perf_plotter UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_scoring_perf_plotter_ui <- function(id){
  ns <- NS(id)
  shinycustomloader::withLoader(ggiraph::girafeOutput(ns("plot")),
  type = "html",
  loader = "dnaspin")
}

#' scoring_perf_plotter Server Functions
#'
#' @noRd
mod_scoring_perf_plotter_server <- function(id, scoring_perf){
  moduleServer( id, function(input, output, session){

    change_plot <- eventReactive(scoring_perf$change(),
                                 {plot_scoring_rate(team = scoring_perf$team(),
                                                          season = scoring_perf$season())})


    observeEvent(scoring_perf$change(),
                 {shinyjs::enable("download")})

    output$download <- downloadHandler(
      filename = function() {
        glue::glue("scoring_perf_{scoring_perf$season() %>% stringr::str_sub(end = 4) %>% as.integer() +1}_{scoring_perf$team() %>% get_last_name() %>% stringr::str_to_lower()}.png")
      },
      content = function(file) {
        ggplot2::ggsave(file,
                        plot = print(change_plot()),
                        height = 12,
                        width = 12,
                        units = "in"
        )

      }
    )

    output$plot <- ggiraph::renderGirafe({

        ggiraph::girafe(ggobj = change_plot(),
                        width_svg = 12,
                        height_svg = 12,
                        options = list(ggiraph::opts_tooltip(css="background-color:transparent"),
                                       ggiraph::opts_hover(css = "fill:red;"),
                                       ggiraph::opts_toolbar(saveaspng = FALSE),
                                       ggiraph::opts_sizing(rescale = FALSE)))
    })

  })
}

## To be copied in the UI
# mod_scoring_perf_plotter_ui("scoring_perf_plotter_ui_1")

## To be copied in the server
# mod_scoring_perf_plotter_server("scoring_perf_plotter_ui_1")
