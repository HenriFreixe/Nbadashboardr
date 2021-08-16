#' win_correlation_plotter UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_win_correlation_plotter_ui <- function(id){
  ns <- NS(id)
  shinycustomloader::withLoader(ggiraph::girafeOutput(ns("plot")),
  type = "html",
  loader = "dnaspin")
}

#' win_correlation_plotter Server Functions
#'
#' @noRd
mod_win_correlation_plotter_server <- function(id, win_correlation){
  moduleServer( id, function(input, output, session){

    change_plot <- eventReactive(win_correlation$change(),
                                 {plot_bump_chart(season = win_correlation$season(),
                                                  variable = win_correlation$variable(),
                                                  team = win_correlation$team())}
    )


    output$download <- downloadHandler(
      filename = function() {
        glue::glue("win_correlation_{win_correlation$variable()}_{win_correlation$season() %>% stringr::str_sub(end = 4) %>% as.integer() +1}_{win_correlation$team() %>% get_last_name() %>% stringr::str_to_lower()}.png")
      },
      content = function(file) {
        ggplot2::ggsave(file,
                        plot = print(change_plot()),
                        height = 12,
                        width = 16,
                        units = "in"
        )

      }
    )


    output$plot <- ggiraph::renderGirafe(
      expr =
    ggiraph::girafe(ggobj = change_plot(),
                    width_svg = 16,
                    height_svg = 12,
                    options = list(ggiraph::opts_toolbar(saveaspng = FALSE),
                                   ggiraph::opts_sizing(rescale = FALSE))))
  })
}


## To be copied in the UI
# mod_win_correlation_plotter_ui("win_correlation_plotter_ui_1")

## To be copied in the server
# mod_win_correlation_plotter_server("win_correlation_plotter_ui_1")
