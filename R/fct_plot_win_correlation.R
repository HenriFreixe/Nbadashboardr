### Get winning percentage per season


# teams_standings

get_team_standings <- function(season = "2020-21") {

  .headers  <- headers()
  url_team_stats <- function(season) {
    glue::glue("https://stats.nba.com/stats/leaguestandingsv3?LeagueID=00&Season={season}&SeasonType=Regular%20Season")
  }

  res <- httr::GET(url = url_team_stats(season), httr::add_headers(.headers=.headers))
  json_resp <- jsonlite::fromJSON(httr::content(res, "text"))

  team_stats <- data.frame(json_resp$resultSets$rowSet[1]) %>% dplyr::as_tibble()
  colnames(team_stats) <- json_resp$resultSets[["headers"]][[1]]

  team_stats <- team_stats %>%
    janitor::clean_names() %>%
    dplyr::mutate(team_name = glue::glue("{team_city} {team_name}") %>% as.character(),
                  win_pct = as.numeric(win_pct)) %>%
    dplyr::select(team_id,team_name,conference,win_pct) %>%
    dplyr::mutate(season = season,
                  team_name = dplyr::if_else(team_name == "LA Clippers","Los Angeles Clippers",team_name),
                  team_name = dplyr::if_else(team_name == "New Orleans/Oklahoma City Hornets", "New Orleans Hornets",team_name),
                  former_team_name = team_name,
                  team_name = dplyr::if_else(team_name == "New Orleans Hornets","New Orleans Pelicans",team_name),
                  team_name = dplyr::if_else(team_name == "New Jersey Nets","Brooklyn Nets",team_name),
                  team_name = dplyr::if_else(team_name == "Charlotte Bobcats","Charlotte Hornets",team_name),
                  team_name = dplyr::if_else(team_name == "Seattle SuperSonics","Oklahoma City Thunder",team_name))
  return(team_stats)
}


get_standings_range <- function(start_season = "2011-12",end_season = "2020-21") {

  future::plan(future::multisession(workers = future::availableCores()))
  c(start_season,end_season) %>%
    furrr::future_map_dfr(get_team_standings)

}


get_salaries <- function(season = "2020-21") {

  pre_season_reformat <- stringr::str_sub(season,end = 4) %>% as.integer()

  season_reformat <- glue::glue("{pre_season_reformat}-{pre_season_reformat+1}/")

  season_reformat <- if (season_reformat == "2020-2021/") {""} else {
    season_reformat
  }

  team_names <- get_team_traditional(season) %>%
    dplyr::select(team_name) %>%
    dplyr::mutate(team_city = remove_last_name(team_name),
                  team_city = dplyr::if_else(team_city == "Portland Trail","Portland",team_city),
                  team_city = dplyr::if_else(team_name == "Los Angeles Lakers","LA Lakers",team_city),
                  team_city = dplyr::if_else(team_name == "Los Angeles Clippers","LA Clippers",team_city),
                  team_nickname = get_last_name(team_name),
                  team_nickname = dplyr::if_else(team_nickname =="Blazers","Trail Blazers",team_nickname))

  df <- (xml2::read_html(glue::glue("https://hoopshype.com/salaries/{season_reformat}")) %>%
           rvest::html_nodes(css = "table") %>%
           rvest::html_table(trim = TRUE))[[1]]

  colnames(df) <- df %>% head(1)

  df <- df %>%
    janitor::clean_names() %>%
    dplyr::slice(2:dplyr::n()) %>%
    dplyr::select(2,3)

  colnames(df) <- c("team_name","salary")


  df %>%
    dplyr::mutate(season = season,
                  salary = stringr::str_sub(salary, start = 2) %>% stringr::str_remove_all(",") %>% as.numeric()) %>%
    dplyr::left_join(team_names, by = c("team_name" = "team_city")) %>%
    dplyr::select(team_name = team_name.y, salary, season)
}

### Bump Data

get_bump_data <- function(season = "2020-21", team = 'global', variable = 'salary') {

  if (variable == 'salary') { df <- get_salaries(season)
  } else {
    if (variable == 'off_rating') { df <- bballref_efficiency_season(season)
    }else {
      if (variable == 'def_rating') { df <- bballref_def_efficiency_season(season)
      }else {
        return("This variable is not correct")
      }
    }}


  bump_data <- get_team_standings(season) %>%
    dplyr::left_join(df, by = c("team_name")) %>%
    dplyr::select(-season.y) %>%
    dplyr::select(season = season.x, dplyr::everything()) %>%
    dplyr::mutate(rank_win_pct = rank(dplyr::desc(win_pct), ties.method = 'first'),
                  rank_variable = rank(desc(.data[[variable]]), ties.method = 'first')) %>%
    dplyr::mutate(variable_pct = .data[[variable]] / max(.data[[variable]])*max(win_pct),
                  selected = dplyr::if_else(team_name == team, 'yes','no'),
                  color = dplyr::if_else(selected == "yes","#CBA049",court_themes('lines'))) %>%
    janitor::clean_names()

  if (variable == 'def_rating') {
    bump_data <- bump_data %>%
      mutate(rank_variable = rank(dplyr::desc(rank_variable)))
  }

  return(bump_data)
}

plot_bump_chart <- function(season = '2020-21',team = 'global', variable = 'salary') {


  df <- get_bump_data(season, team, variable)


  x <- c(seq(-1.55,-0.75,by =.2), seq(0.75, 1.55, by =.2))
  y <- 1:(dplyr::n_distinct(df$rank_win_pct)+1)

  linerange <- tidyr::crossing(x,y)

  if (variable == 'salary') {
    title_end <- 'roster salary'
    subtitle_end <- 'highest roster salary'
    column_label <- 'salary'

    prefix_label <- "$"
    suffix_label <- 'M'
    scale_label <- 1/1000000

  }else {
    if (variable == 'off_rating') {
      title_end <- 'offensive efficiency'
      subtitle_end <- 'best offensive efficiency'
      column_label <- 'offensive efficiency'

      prefix_label <- ""
      suffix_label <- " pts"
      scale_label <- 1
    }else {
      title_end <- 'defensive efficiency'
      subtitle_end <- 'best defensive efficiency'
      column_label <- 'defensive efficiency'

      prefix_label <- ""
      suffix_label <- " pts"
      scale_label <- 1
    }
  }


  subtitles_second_line <- if (team == 'global') {
    ''
  }else {
    glue::glue("The <span style = 'color:#CBA049;font-size:18pt;'>{team}</span> ranked <span style = 'font-size:18pt;'>{df %>% dplyr::filter(team_name == team) %>% dplyr::pull(rank_win_pct) %>% scales::ordinal()}</span> in Win % while having the <span style = 'font-size:18pt;'>{df %>% dplyr::filter(team_name ==team) %>% dplyr::pull(rank_variable) %>% scales::ordinal()}</span> {subtitle_end}")
  }

  plot <- df %>%
    ggplot2::ggplot() +
    ggbump::geom_sigmoid(ggplot2::aes(x = -.3,
                                      xend = .3,
                                      y = rank_win_pct,
                                      yend = rank_variable,
                                      group = team_name,
                                      color = rank_win_pct),
                         smooth = 6,
                         size = 1.5,
                         alpha = .45) +
    ggplot2::geom_point(ggplot2::aes(x = -.3,
                                     y = rank_win_pct,
                                     color = rank_win_pct),
                        shape = "|",
                        size = 4) +
    ggplot2::geom_point(ggplot2::aes(x = .3,
                                     y = rank_variable,
                                     color = rank_win_pct),
                        shape = "|",
                        size = 4) +
    ggplot2::geom_segment(ggplot2::aes(x = -.55,
                                       xend = -.55 - win_pct ,
                                       y = rank_win_pct,
                                       yend = rank_win_pct,
                                       color = rank_win_pct),
                          size = 5) +
    ggplot2::geom_segment(ggplot2::aes(x = .55,
                                       xend = .55 + variable_pct,
                                       y = rank_variable,yend = rank_variable,
                                       color = rank_win_pct),
                          size = 5) +
    ggplot2::geom_linerange(
      data = linerange,
      ggplot2::aes(x = x, ymin = y - .85, ymax = y - .15),
      color = court_themes('court'),
      size = .5
    ) +
    ggtext::geom_richtext(ggplot2::aes(x = -.55,
                                       y = -1,
                                       label = "Teams ranked by Win%"),
                          family = "Kiwi Maru",
                          color = court_themes('lines'),
                          hjust = 1,
                          fill = NA,
                          size = 6,
                          label.color = NA) +
    ggtext::geom_richtext(ggplot2::aes(x = .55,
                                       y = -1,
                                       label = glue::glue("Teams ranked by {column_label}")),
                          family = "Kiwi Maru",
                          color = court_themes('lines'),
                          hjust = 0,
                          size = 6,
                          fill = NA,
                          label.color = NA) +
    ggplot2::labs(title = glue::glue("Teams win percentage<br> compared with their {title_end} <span style = 'color :#CBA049;'>| {season}</span>"),
                  subtitle = glue::glue("Are <span style='color:#F6C5A3;font-size:18pt;'>high performing teams</span> likelier to spend a lot on their roster salary than <span style='color:#BC1B56;font-size:18pt;'>low-performers</span> ?<br>{subtitles_second_line}"),
                  caption = glue::glue("Visualisation by Henri Freixe • Sources : Nba.com, Basketball-reference.com, Hoopshype.com")) +
    ggplot2::scale_y_continuous(trans = 'reverse') +
    ggplot2::scale_x_continuous(limits = c(-1.75,1.75)) +
    viridis::scale_color_viridis(option = "F",
                                 direction = -1,
                                 begin =.3,
                                 end = .9,
                                 guide = 'none') +
    #scico::scale_color_scico(palette = 'lajolla') +
    ggnewscale::new_scale_color() +
    ggtext::geom_richtext(ggplot2::aes(x = -.425,
                                       y = rank_win_pct,
                                       color = color,
                                       size = color,
                                       label = scales::percent(win_pct,accuracy = .1)),
                          family = "Kiwi Maru",
                          hjust = 0.5,
                          fill = NA,
                          label.color = NA) +
    ggtext::geom_richtext(ggplot2::aes(x = .425,
                                       y = rank_variable,
                                       color = color,
                                       size = color,
                                       label = scales::percent(.data[[variable]], prefix = prefix_label,suffix = suffix_label, scale = scale_label, accuracy = .1)),
                          family = "Kiwi Maru",
                          hjust = .5,
                          fill = NA,
                          label.color = NA) +
    ggtext::geom_richtext(ggplot2::aes(x = -.55 - win_pct -.05,
                                       y = rank_win_pct,
                                       color = color,
                                       size = color,
                                       label = team_name),
                          family = "Kiwi Maru",
                          hjust = 1,
                          fill = NA,
                          label.color = NA) +
    ggtext::geom_richtext(ggplot2::aes(x = .55 + variable_pct + .05,
                                       y = rank_variable,
                                       color = color,
                                       label = team_name,
                                       size = color),
                          family = "Kiwi Maru",
                          hjust = 0,
                          fill = NA,
                          label.color = NA) +
    ggplot2::scale_color_identity(guide = 'none') +
    ggplot2::scale_size_manual(values = c(4,4.5), guide = 'none') +
    ggplot2::guides(color = FALSE) +
    ggplot2::theme_minimal(base_size = 22) +
    theme_dark_cap() +
    ggplot2::theme(plot.title = ggtext::element_markdown(hjust = .5,
                                                         margin = ggplot2::margin(t = 20)),
                   plot.subtitle = ggtext::element_markdown(family = "Kiwi Maru",
                                                            hjust = .5,
                                                            margin = ggplot2::margin(t = 20,
                                                                                     b=5),
                                                            size = 14,
                                                            lineheight = 1.5),
                   plot.caption = ggtext::element_markdown(size = 9,
                                                           margin = ggplot2::margin(t = 20,
                                                                                    b = 10)),
                   panel.grid = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   legend.background = ggplot2::element_blank())

  plot_with_logo <- plot + patchwork::inset_element(team_logo(team, width = 70), left = 0.025, top = 1.175, right = 0.150, bottom = 1.060)
  return(plot_with_logo)

}
