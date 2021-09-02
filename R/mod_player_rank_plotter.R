#' player_rank_plotter UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_player_rank_plotter_ui <- function(id){
  ns <- NS(id)
  shinycustomloader::withLoader(ggiraph::girafeOutput(ns("plot")),
  type = "html",
  loader = "dnaspin")
}

#' player_rank_plotter Server Functions
#'
#' @noRd
mod_player_rank_plotter_server <- function(id, player_rank){
  moduleServer( id, function(input, output, session){

    change_plot <- eventReactive(player_rank$change(),
                                 {plot_player_ranking_interactive(variable = player_rank$variable(),
                                                                  season = player_rank$season())})

    observeEvent(player_rank$change(),
                 {shinyjs::enable("download")})


    output$download <- downloadHandler(
      filename = function() {
        glue::glue("top_players_{player_rank$variable()}_{player_rank$season() %>% stringr::str_sub(end = 4) %>% as.integer() +1}.png")
      },
      content = function(file) {
        ggplot2::ggsave(file,
                        plot = print(change_plot()),
                        height = 12,
                        width = 16,
                        units = "in",
                        device = png,
                        dpi = 150
        )

      }
    )
    output$plot <- ggiraph::renderGirafe({
      ggiraph::girafe(ggobj = change_plot(),
                      width_svg = 16,
                      height_svg = 12,
                      options = list(ggiraph::opts_tooltip(css="background-color:transparent"),
                                     ggiraph::opts_hover(css = "fill:red;"),
                                     ggiraph::opts_toolbar(saveaspng = FALSE),
                                     ggiraph::opts_sizing(rescale = FALSE)))
    })

  })
}


## To be copied in the UI
# mod_player_rank_plotter_ui("player_rank_plotter_ui_1")

## To be copied in the server
# mod_player_rank_plotter_server("player_rank_plotter_ui_1")
