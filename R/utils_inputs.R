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
} else {
  return(c("global",teams))
}

}
