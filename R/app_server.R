#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  ## Font management
  sysfonts::font_add("Kiwi Maru",regular = "fonts/fonts/kiwi-maru-v6-latin-regular.ttf")
  showtext::showtext_auto()


  # List the first level callModules here
shot_chart <- mod_shot_chart_selector_server("shot_chart")
mod_shot_chart_plotter_server("shot_chart", shot_chart)

scoring_perf <- mod_scoring_perf_selector_server("scoring_perf")
mod_scoring_perf_plotter_server("scoring_perf", scoring_perf)

player_rank <- mod_player_rank_selector_server("player_rank")
mod_player_rank_plotter_server("player_rank", player_rank)

players_table <- mod_players_table_selector_server("players_table")
mod_players_table_plotter_server("players_table", players_table)

net_rating <- mod_net_rating_selector_server("net_rating")
mod_net_rating_plotter_server("net_rating", net_rating)

off_eff <- mod_off_eff_selector_server("off_eff")
mod_off_eff_plotter_server("off_eff", off_eff)

win_correlation <- mod_win_correlation_selector_server("win_correlation")
mod_win_correlation_plotter_server("win_correlation", win_correlation)

teams_table <- mod_teams_table_selector_server("teams_table")
mod_teams_table_plotter_server("teams_table", teams_table)

season_recap <- mod_season_recap_selector_server("season_recap")
mod_season_recap_plotter_server("season_recap", season_recap)

}
