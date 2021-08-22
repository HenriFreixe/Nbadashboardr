#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {

  #sysfonts::font_add_google("Kiwi Maru")
  #showtext::showtext_auto()

 tab <- function(...) {
   shiny::tabPanel(..., class = "justify-content-center")#class = "p-3 border border-top-0 rounded-bottom text-secondary")
 }



  #https://github.com/daattali/shinycssloaders

  my_theme <- bslib::bs_theme(version = "4",
                  bootswatch = "minty",
                  #...,
                  bg = "#E8E8E8",
                  fg = "#333333",
                  primary = "#685bb5",
                  secondary = "#E5284C",
                  sucess = "#0fbf79",
                  base_font = bslib::font_google("Lato"),
                  heading_font = bslib::font_google("Kiwi Maru"),
                  code_font = bslib::font_google("Lato")
                  )



  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here
    fluidPage(
      theme = my_theme,
      shinyjs::useShinyjs(),
      tags$style(HTML(
        ".nav { justify-content: center; }
        h1, .row {text-align: center!important;}
        .jumbotron {background-color:#333333; color:#999999;}"
      )),
      tags$h1("NBA Analytics Dashboard",HTML("<img src='https://a.espncdn.com/combiner/i?img=/i/teamlogos/leagues/500/nba.png' alt='NBA Logo' width = 15%'>")),
      class = "content",
      tags$br(),
      tags$div(class = "jumbotron",
          tags$p(
            'This project aims at providing a thought-provoking outlook on the evolution of the NBA in the 21st Century.',
             tags$br(),
            'The document is organized in three tabs with a deep-dive on ',
            tags$span(class = "text-secondary", 'Player Performance'),
           '- with 3 visualisations and a table - a breakdown of ',
           tags$span(class = "text-secondary",'Team Performance'),
           ' - with 3 visualisations and a table - and a final ',
             tags$span(class = "text-secondary",'Season Recap Visualisation.')
          )),
      tabsetPanel(type = "pills",
      tab("NBA Players Breakdown",
          HTML('<h3 class = "text-secondary">A) Shot Chart</h3>'),
          mod_shot_chart_selector_ui("shot_chart"),
          tags$br(),
          mod_shot_chart_plotter_ui("shot_chart"),
          tags$hr(),
          HTML('<h3 class = "text-secondary">B) Scoring performance analysis</h3>'),
          mod_scoring_perf_selector_ui("scoring_perf"),
          tags$br(),
          mod_scoring_perf_plotter_ui("scoring_perf"),
          tags$hr(),
          HTML('<h3 class = "text-secondary">C) Player rankings</h3>'),
          mod_player_rank_selector_ui("player_rank"),
          tags$br(),
          mod_player_rank_plotter_ui("player_rank"),
          tags$hr(),
          HTML('<h3 class = "text-secondary">D) Players performance table</h3>'),
          mod_players_table_selector_ui("players_table"),
          tags$br(),
          mod_players_table_plotter_ui("players_table")),
      tab("NBA Teams Breakdown",
          HTML('<h3 class = "text-secondary">A) Teams Offensive / Defensive mapping</h3>'),
          mod_net_rating_selector_ui("net_rating"),
          tags$br(),
          mod_net_rating_plotter_ui("net_rating"),
          tags$hr(),
          HTML('<h3 class = "text-secondary">B) Offensive Efficiency evolution</h3>'),
          mod_off_eff_selector_ui("off_eff"),
          tags$br(),
          mod_off_eff_plotter_ui("off_eff"),
          tags$hr(),
          HTML('<h3 class = "text-secondary">C) How do variables correlate to win percentage ?</h3>'),
          mod_win_correlation_selector_ui("win_correlation"),
          tags$br(),
          mod_win_correlation_plotter_ui("win_correlation"),
          tags$hr(),
          HTML('<h3 class = "text-secondary">D) Teams performance table</h3>'),
          mod_teams_table_selector_ui("teams_table"),
          tags$br(),
          mod_teams_table_plotter_ui("teams_table")),
      tab("Season Recap",
          HTML('<h3 class = "text-secondary"> Season Overview Visualisation</h3>'),
          mod_season_recap_selector_ui("season_recap"),
          tags$br(),
          mod_season_recap_plotter_ui("season_recap"))),
      HTML("<br><div style='text-align:center;padding-bottom:10px;padding-top:30px;color:#999999;background-color:#333333;font-size:14px'>
             <a href='mailto:henri.freixe@edhec.com'><img border='0' alt='Email' src='https://assets.dryicons.com/uploads/icon/svg/12421/8a68a1d5-1b61-4e1f-8e91-6f7e30516e1d.svg' width='30' height='30'></a>&nbsp;&nbsp;&nbsp;&nbsp;
           <a href='https://www.linkedin.com/in/henri-freixe/'><img border='0' alt='LinkedIn' src='https://assets.dryicons.com/uploads/icon/svg/8336/2bdc9209-f5bb-4103-acf2-0685de181010.svg' width='30' height='30'></a>&nbsp;&nbsp;&nbsp;&nbsp;
           <a href='https://twitter.com/freixehenri'><img border='0' alt='Twitter' src='https://assets.dryicons.com/uploads/icon/svg/8384/ecf4ac0a-f8ce-4ffe-86bf-918b636ec99e.svg' width='30' height='30'></a>&nbsp;&nbsp;&nbsp;&nbsp;
           <a href='https://github.com/HenriFreixe'><img border='0' alt='Github' src='https://assets.dryicons.com/uploads/icon/svg/8312/cc33248a-e56e-4e7f-93f4-0e16350e5768.svg' width='30' height='30'></a><br><br>
             <span style = 'font-size:14px'>A Project by Henri Freixe as part of the EPFL Applied Data Science Communication & Visualisation Certificate of Open Studies</span><br>
             <span style = 'font-size:12px'>Sources: nba.com, espn.com, basketball-reference.com, basketball.realgm.com, hoopshype.com </span>
             </div>")))

}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){

  add_resource_path(
    'www', app_sys('app/www')
  )

  tags$head(
    favicon(ext = 'png'),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'NBA Analytics Dashboard'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

