# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Add one line by package you want to add as dependency
usethis::use_package( "thinkr" )
usethis::use_package( "readr" )
usethis::use_package( "dplyr" )
usethis::use_package( "tibble" )
usethis::use_package( "magrittr" )
usethis::use_package( "httr" )
usethis::use_package( "jsonlite" )
usethis::use_package( "purrr" )
usethis::use_package( "furrr" )
usethis::use_package( "ggplot2" )
usethis::use_package( "patchwork" )
usethis::use_package( "systemfonts" )
usethis::use_package( "sysfonts" )
usethis::use_package( "extrafont" )
usethis::use_package( "gfonts" )
usethis::use_package( "showtext" )
usethis::use_package( "ggtext" )
usethis::use_package( "tidyr" )
usethis::use_package( "ggiraph" )
usethis::use_package( "ggbump" )
usethis::use_package( "RCurl" )
usethis::use_package( "gt" )
usethis::use_package( "htmltools" )
usethis::use_package( "future" )
usethis::use_package( "shiny" )
usethis::use_package( "shinycustomloader" )
usethis::use_package( "shinyjs" )
usethis::use_pipe()
devtools::document()


## Add modules ----
## Create a module infrastructure in R/ #On tweakera peut-être la taille des plots et graphiques après coup
golem::add_module( name = "shot_chart_plotter" ) # Name of the module
golem::add_module( name = "shot_chart_selector" ) # Name of the module
golem::add_module( name = "scoring_perf_plotter" ) # Name of the module
golem::add_module( name = "scoring_perf_selector" ) # Name of the module
golem::add_module( name = "player_rank_plotter" ) # Name of the module
golem::add_module( name = "player_rank_selector" ) # Name of the module
golem::add_module( name = "players_table_plotter" ) # Name of the module
golem::add_module( name = "players_table_selector" ) # Same inputs as player_rank, no duplicate?
golem::add_module( name = "net_rating_plotter" ) # Name of the module
golem::add_module( name = "net_rating_selector" ) # Only season input
golem::add_module( name = "off_eff_plotter" ) # Name of the module
golem::add_module( name = "off_eff_selector" ) # Name of the module
golem::add_module( name = "win_correlation_plotter" ) # Name of the module
golem::add_module( name = "win_correlation_selector" ) # Name of the module
golem::add_module( name = "teams_table_plotter" ) # Name of the module
golem::add_module( name = "teams_table_selector" ) # Name of the module
golem::add_module( name = "season_recap_plotter" ) # Name of the module
golem::add_module( name = "season_recap_selector" ) # Only season input

### Adding player inputs - On verra pour du javascript, pour le moment top 200 BPM
### Adding a button to launch


## Add helper functions ----
## Creates ftc_* and utils_*
golem::add_fct( "plot_shot_chart" )
golem::add_fct( "plot_player_rank" )
golem::add_fct( "plot_scoring_perf" )
golem::add_fct( "plot_players_table" )
golem::add_fct( "plot_net_rating" )
golem::add_fct( "plot_off_eff" )
golem::add_fct( "plot_win_correlation" )
golem::add_fct( "plot_teams_table" )
golem::add_fct( "plot_season_recap" )
golem::add_utils( "theming" )
golem::add_utils( "inputs" )

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file( "script" )
golem::add_js_handler( "handlers" )
golem::add_css_file( "custom" )

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw( name = "my_dataset", open = FALSE )

## Tests ----
## Add one line by test you want to create
usethis::use_test( "app" )

# Documentation

## Vignette ----
usethis::use_vignette("nbadashboardr")
devtools::build_vignettes()

## Code coverage ----
## (You'll need GitHub there)
usethis::use_github()
usethis::use_travis()
usethis::use_appveyor()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")

