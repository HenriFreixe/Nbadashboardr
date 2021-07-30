#' inputs
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

scope_seasons <- function() {

  seasons <- get_award("asgmvp")$season %>%
    stringr::str_sub(end = 4) %>%
    as.integer()

  seasons <- seasons[seasons>2000]

  seasons_df <- tibble::tibble(pre = seasons, post = seasons +1) %>%
    dplyr::mutate(scope = as.character(glue::glue("{pre}-{stringr::str_sub(post,start = 3)}")))


    return(seasons_df$scope %>% unique())

}




scope_teams <- function(season = "2020-21", current = TRUE, type = "regular"){

  df <- get_team_standings(season)

if (current) {
teams <- df$team_name
} else {
  teams <- df$former_team_name
}

if (type == "regular") {
return(teams)
} else { if (type == "average") {
  teams_average <- c("global",teams)
  names(teams_average) <- c("League Average",teams)
  return(teams_average)
} else {
  teams_no <- c("global",teams)
  names(teams_no) <- c("No Specific Team Highlighted",teams)
  return(teams_no)
}
}
}


scope_players <- function(season = "2020-21") {

  df <- get_bpm_join(season)

  return(df$player_name)

}


scope_variables <- function(plot_type) {

  player_rank <- c("pts","reb","ast","blk","stl","bpm")
  names(player_rank) <- c("Points per Game","Rebounds per Game","Assists per Game","Blocks per Game","Steals per Game","Box Plus-Minus")

  player_table <- c("pts","reb","ast","blk","stl","bpm")
  names(player_table) <- c("Points per Game","Rebounds per Game","Assists per Game","Blocks per Game","Steals per Game","Box Plus-Minus")

  win_pct <- c("salary", "off_eff", "def_eff")
  names(win_pct) <- c("Salary", "Offensive Efficiency", "Defensive Efficiency")

if (plot_type == 'player_rank') {
  return(player_rank)
}else {
  if (plot_type == 'player_table') {
return(player_table)
  }else {
    return(win_pct)
  }
}
}
