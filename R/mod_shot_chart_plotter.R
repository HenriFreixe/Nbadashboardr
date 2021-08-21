#' shot_chart_plotter UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_shot_chart_plotter_ui <- function(id){
  ns <- NS(id)

  shinycustomloader::withLoader(ggiraph::girafeOutput(ns("plot")),
  type = "html",
  loader = "dnaspin")
}

#' shot_chart_plotter Server Function
#'
#' @noRd
mod_shot_chart_plotter_server <- function(id, shot_chart){
  moduleServer(id, function(input, output, session) {


    change_plot <- eventReactive(shot_chart$change(),
                                 {plot_court(player = shot_chart$player(),
                                             season = shot_chart$season(),
                                             summary = TRUE)}
                            )


    observeEvent(shot_chart$change(),
                 {shinyjs::enable("download")})


    output$download <- downloadHandler(
      filename = function() {
        glue::glue("shot_chart_{shot_chart$player() %>% get_last_name() %>% stringr::str_to_lower()}_{shot_chart$season() %>% stringr::str_sub(end = 4) %>% as.integer() +1}.png")
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
        ggiraph::girafe(ggobj = change_plot(),
                        width_svg = 12,
                        height_svg = 12,
                        options = list(ggiraph::opts_toolbar(saveaspng = FALSE),
                                       ggiraph::opts_sizing(rescale = FALSE)))
    )
  })
}

## To be copied in the UI
# mod_shot_chart_plotter_ui("shot_chart_plotter_ui_1")

## To be copied in the server
# callModule(mod_shot_chart_plotter_server, "shot_chart_plotter_ui_1")

