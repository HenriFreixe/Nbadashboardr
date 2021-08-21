#' net_rating_plotter UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_net_rating_plotter_ui <- function(id){
  ns <- NS(id)
  shinycustomloader::withLoader(ggiraph::girafeOutput(ns("plot")),
                                type = "html",
                                loader = "dnaspin")
}

#' net_rating_plotter Server Functions
#'
#' @noRd
mod_net_rating_plotter_server <- function(id, net_rating){
  moduleServer( id, function(input, output, session){

    change_plot <- eventReactive(net_rating$change(),
                                 {plot_teams_efficiency_interactive(season = net_rating$season())}
    )


    observeEvent(net_rating$change(),
                 {shinyjs::enable("download")})

    output$download <- downloadHandler(
      filename = function() {
        glue::glue("net_rating_{net_rating$season() %>% stringr::str_sub(end = 4) %>% as.integer() +1}.png")
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

      output$plot <- ggiraph::renderGirafe(
        expr = ggiraph::girafe(ggobj = change_plot(),
                          width_svg = 12,
                          height_svg = 12,
                          options = list(ggiraph::opts_tooltip(css="background-color:transparent"),
                                         ggiraph::opts_hover(css = "fill:red;"),
                                         ggiraph::opts_toolbar(saveaspng = FALSE),
                                         ggiraph::opts_sizing(rescale = FALSE))))

  })
}

## To be copied in the UI
# mod_net_rating_plotter_ui("net_rating_plotter_ui_1")

## To be copied in the server
# mod_net_rating_plotter_server("net_rating_plotter_ui_1")
