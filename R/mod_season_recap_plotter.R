#' season_recap_plotter UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_season_recap_plotter_ui <- function(id){
  ns <- NS(id)
  shinycustomloader::withLoader(ggiraph::girafeOutput(ns("plot")),
  type = "html",
  loader = "dnaspin")
}

#' season_recap_plotter Server Functions
#'
#' @noRd
mod_season_recap_plotter_server <- function(id, season_recap){
  moduleServer( id, function(input, output, session){

    change_plot <- eventReactive(season_recap$change(),
                                 {plot_season_recap(season = season_recap$season())})

    observeEvent(season_recap$change(),
                 {shinyjs::enable("download")})

    output$download <- downloadHandler(
      filename = function() {
        glue::glue("season_recap_{season_recap$season() %>% stringr::str_sub(end = 4) %>% as.integer() +1}.png")
      },
      content = function(file) {
        ggplot2::ggsave(file,
                        plot = print(change_plot()),
                        height = 34,
                        width = 10,
                        units = "in",
                        device = png,
                        dpi = 150
        )

      }
    )

    output$plot <- ggiraph::renderGirafe({
        ggiraph::girafe(ggobj = change_plot(),
                        width_svg = 10,
                        height_svg = 34,
                        options = list(ggiraph::opts_tooltip(css="background-color:transparent"),
                                       ggiraph::opts_toolbar(saveaspng = FALSE),
                                       ggiraph::opts_hover(css = "fill:red;"),
                                       ggiraph::opts_sizing(rescale = FALSE)))
    })

  })
}

## To be copied in the UI
# mod_season_recap_plotter_ui("season_recap_plotter_ui_1")

## To be copied in the server
# mod_season_recap_plotter_server("season_recap_plotter_ui_1")
