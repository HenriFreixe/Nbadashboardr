# teams_net_rating

get_team_traditional <- function(season = "2020-21") {
  ### Without All star Selections
  .headers  <- headers()

  url_team_stats <- function(season) {
    glue::glue("https://stats.nba.com/stats/leaguedashteamstats?Conference=&DateFrom=&DateTo=&Division=&GameScope=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season={season}&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision=")
  }

  res <- httr::GET(url = url_team_stats(season), httr::add_headers(.headers=.headers))
  json_resp <- jsonlite::fromJSON(httr::content(res, "text"))

  team_stats <- data.frame(json_resp$resultSets$rowSet[1]) %>% dplyr::as_tibble()
  colnames(team_stats) <- json_resp$resultSets[["headers"]][[1]]

  team_stats <- team_stats %>%
    janitor::clean_names() %>%
    dplyr::mutate(across("gp":"cfid",as.numeric)) %>%
    dplyr::left_join(get_traditional_stats() %>%
                       dplyr::select(team_abbreviation,team_id) %>%
                       dplyr::distinct(),by = c("team_id")) %>%
    dplyr::mutate(logo = get_team_logo(team_abbreviation),
                  team_name = dplyr::if_else(team_name == "LA Clippers","Los Angeles Clippers",team_name),
                  team_name = dplyr::if_else(team_name == "New Orleans/Oklahoma City Hornets", "New Orleans Hornets",team_name),
                  former_team_name = team_name,
                  team_name = dplyr::if_else(team_name == "New Orleans Hornets","New Orleans Pelicans",team_name),
                  team_name = dplyr::if_else(team_name == "New Jersey Nets","Brooklyn Nets",team_name),
                  team_name = dplyr::if_else(team_name == "Charlotte Bobcats","Charlotte Hornets",team_name),
                  team_name = dplyr::if_else(team_name == "Seattle SuperSonics","Oklahoma City Thunder",team_name)
    )

  return(team_stats)
}

get_team_traditional_selections <- function(season = "2020-21") {
  ### With All star Selections
  .headers  <- headers()

  url_team_stats <- function(season) {
    glue::glue("https://stats.nba.com/stats/leaguedashteamstats?Conference=&DateFrom=&DateTo=&Division=&GameScope=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season={season}&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision=")
  }

  res <- httr::GET(url = url_team_stats(season), httr::add_headers(.headers=.headers))
  json_resp <- jsonlite::fromJSON(httr::content(res, "text"))

  team_stats <- data.frame(json_resp$resultSets$rowSet[1]) %>% dplyr::as_tibble()
  colnames(team_stats) <- json_resp$resultSets[["headers"]][[1]]

  team_stats <- team_stats %>%
    janitor::clean_names() %>%
    dplyr::mutate(across("gp":"cfid",as.numeric)) %>%
    dplyr::left_join(get_traditional_stats() %>%
                       dplyr::select(team_abbreviation,team_id) %>%
                       dplyr::distinct(),by = c("team_id")) %>%
    dplyr::mutate(logo = get_team_logo(team_abbreviation),
                  team_name = dplyr::if_else(team_name == "LA Clippers","Los Angeles Clippers",team_name),
                  team_name = dplyr::if_else(team_name == "New Orleans/Oklahoma City Hornets", "New Orleans Hornets",team_name),
                  former_team_name = team_name,
                  team_name = dplyr::if_else(team_name == "New Orleans Hornets","New Orleans Pelicans",team_name),
                  team_name = dplyr::if_else(team_name == "New Jersey Nets","Brooklyn Nets",team_name),
                  team_name = dplyr::if_else(team_name == "Charlotte Bobcats","Charlotte Hornets",team_name),
                  team_name = dplyr::if_else(team_name == "Seattle SuperSonics","Oklahoma City Thunder",team_name)
    ) %>%
    dplyr::left_join(get_all_stars(season) %>%
                       dplyr::group_by(team_name) %>%
                       dplyr::summarise(selections = dplyr::n()), by=c("team_name")) %>%
    dplyr::mutate(selections = dplyr::replace_na(selections,0))

  return(team_stats)
}



team_join <- function() {


  team_name <- c("Atlanta Hawks",
                 "Boston Celtics",
                 "Brooklyn Nets",
                 "Charlotte Hornets",
                 "Chicago Bulls",
                 "Cleveland Cavaliers",
                 "Dallas Mavericks",
                 "Denver Nuggets",
                 "Detroit Pistons",
                 "Golden State Warriors",
                 "Houston Rockets",
                 "Indiana Pacers",
                 "Los Angeles Clippers",
                 "Los Angeles Lakers",
                 "Memphis Grizzlies",
                 "Miami Heat",
                 "Milwaukee Bucks",
                 "Minnesota Timberwolves",
                 "New Orleans Pelicans",
                 "New York Knicks",
                 "Oklahoma City Thunder",
                 "Orlando Magic",
                 "Philadelphia 76ers",
                 "Phoenix Suns",
                 "Portland Trail Blazers",
                 "Sacramento Kings",
                 "San Antonio Spurs",
                 "Toronto Raptors",
                 "Utah Jazz",
                 "Washington Wizards")

  team_id <- c(1610612737L, 1610612738L, 1610612751L, 1610612766L, 1610612741L, 1610612739L, 1610612742L, 1610612743L, 1610612765L, 1610612744L, 1610612745L, 1610612754L, 1610612746L, 1610612747L, 1610612763L, 1610612748L, 1610612749L, 1610612750L, 1610612740L, 1610612752L, 1610612760L, 1610612753L, 1610612755L, 1610612756L, 1610612757L, 1610612758L, 1610612759L, 1610612761L, 1610612762L, 1610612764L)

  logo <- c("https://a1.espncdn.com/combiner/i?img=/i/teamlogos/nba/500/scoreboard/ATL.png", "https://a1.espncdn.com/combiner/i?img=/i/teamlogos/nba/500/scoreboard/BOS.png", "https://a1.espncdn.com/combiner/i?img=/i/teamlogos/nba/500/scoreboard/BKN.png", "https://a1.espncdn.com/combiner/i?img=/i/teamlogos/nba/500/scoreboard/CHA.png", "https://a1.espncdn.com/combiner/i?img=/i/teamlogos/nba/500/scoreboard/CHI.png", "https://a1.espncdn.com/combiner/i?img=/i/teamlogos/nba/500/scoreboard/CLE.png", "https://a1.espncdn.com/combiner/i?img=/i/teamlogos/nba/500/scoreboard/DAL.png", "https://a1.espncdn.com/combiner/i?img=/i/teamlogos/nba/500/scoreboard/DEN.png", "https://a1.espncdn.com/combiner/i?img=/i/teamlogos/nba/500/scoreboard/DET.png", "https://a1.espncdn.com/combiner/i?img=/i/teamlogos/nba/500/scoreboard/GSW.png", "https://a1.espncdn.com/combiner/i?img=/i/teamlogos/nba/500/scoreboard/HOU.png", "https://a1.espncdn.com/combiner/i?img=/i/teamlogos/nba/500/scoreboard/IND.png", "https://a1.espncdn.com/combiner/i?img=/i/teamlogos/nba/500/scoreboard/LAC.png", "https://a1.espncdn.com/combiner/i?img=/i/teamlogos/nba/500/scoreboard/LAL.png", "https://a1.espncdn.com/combiner/i?img=/i/teamlogos/nba/500/scoreboard/MEM.png", "https://a1.espncdn.com/combiner/i?img=/i/teamlogos/nba/500/scoreboard/MIA.png", "https://a1.espncdn.com/combiner/i?img=/i/teamlogos/nba/500/scoreboard/MIL.png", "https://a1.espncdn.com/combiner/i?img=/i/teamlogos/nba/500/scoreboard/MIN.png", "https://a1.espncdn.com/combiner/i?img=/i/teamlogos/nba/500/scoreboard/NO.png", "https://a1.espncdn.com/combiner/i?img=/i/teamlogos/nba/500/scoreboard/NYK.png", "https://a1.espncdn.com/combiner/i?img=/i/teamlogos/nba/500/scoreboard/OKC.png", "https://a1.espncdn.com/combiner/i?img=/i/teamlogos/nba/500/scoreboard/ORL.png", "https://a1.espncdn.com/combiner/i?img=/i/teamlogos/nba/500/scoreboard/PHI.png", "https://a1.espncdn.com/combiner/i?img=/i/teamlogos/nba/500/scoreboard/PHX.png", "https://a1.espncdn.com/combiner/i?img=/i/teamlogos/nba/500/scoreboard/POR.png", "https://a1.espncdn.com/combiner/i?img=/i/teamlogos/nba/500/scoreboard/SAC.png", "https://a1.espncdn.com/combiner/i?img=/i/teamlogos/nba/500/scoreboard/SAS.png", "https://a1.espncdn.com/combiner/i?img=/i/teamlogos/nba/500/scoreboard/TOR.png", "https://a1.espncdn.com/combiner/i?img=/i/teamlogos/nba/500/scoreboard/UTH.png", "https://a1.espncdn.com/combiner/i?img=/i/teamlogos/nba/500/scoreboard/WAS.png")


  return(dplyr::tibble(team_name, team_id = as.character(team_id), logo))

}


get_team_advanced <- function(season = "2020-21") {
  ### Without All star Selections

  .headers  <- headers()

  url_team_stats <- function(season) {
    glue::glue("https://stats.nba.com/stats/leaguedashteamstats?Conference=&DateFrom=&DateTo=&Division=&GameScope=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Advanced&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season={season}&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision=")
  }

  res <- httr::GET(url = url_team_stats(season), httr::add_headers(.headers=.headers))
  json_resp <- jsonlite::fromJSON(httr::content(res, "text"))

  team_stats <- data.frame(json_resp$resultSets$rowSet[1]) %>% dplyr::as_tibble()
  colnames(team_stats) <- json_resp$resultSets[["headers"]][[1]]

  team_stats <- team_stats %>%
    janitor::clean_names() %>%
    dplyr::mutate(across("gp":"cfid",as.numeric)) %>%
    dplyr::left_join(get_traditional_stats() %>%
                       dplyr::select(team_abbreviation,team_id) %>%
                       dplyr::distinct(),by = c("team_id")) %>%
    dplyr::mutate(logo = get_team_logo(team_abbreviation),
                  season = season,
                  team_name = dplyr::if_else(team_name == "New Orleans/Oklahoma City Hornets", "New Orleans Hornets",team_name),
                  team_name = dplyr::if_else(team_name == "LA Clippers","Los Angeles Clippers",team_name),
                  former_team_name = team_name,
                  team_name = dplyr::if_else(team_name == "New Orleans Hornets","New Orleans Pelicans",team_name),
                  team_name = dplyr::if_else(team_name == "New Jersey Nets","Brooklyn Nets",team_name),
                  team_name = dplyr::if_else(team_name == "Charlotte Bobcats","Charlotte Hornets",team_name),
                  team_name = dplyr::if_else(team_name == "Seattle SuperSonics","Oklahoma City Thunder",team_name)
    )

  return(team_stats)
}

get_team_advanced_selections <- function(season = "2020-21") {
  ### With All star Selections

  .headers  <- headers()

  url_team_stats <- function(season) {
    glue::glue("https://stats.nba.com/stats/leaguedashteamstats?Conference=&DateFrom=&DateTo=&Division=&GameScope=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Advanced&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season={season}&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision=")
  }

  res <- httr::GET(url = url_team_stats(season), httr::add_headers(.headers=.headers))
  json_resp <- jsonlite::fromJSON(httr::content(res, "text"))

  team_stats <- data.frame(json_resp$resultSets$rowSet[1]) %>% dplyr::as_tibble()
  colnames(team_stats) <- json_resp$resultSets[["headers"]][[1]]

  team_stats <- team_stats %>%
    janitor::clean_names() %>%
    dplyr::mutate(across("gp":"cfid",as.numeric)) %>%
    dplyr::left_join(get_traditional_stats() %>%
                       dplyr::select(team_abbreviation,team_id) %>%
                       dplyr::distinct(),by = c("team_id")) %>%
    dplyr::mutate(logo = get_team_logo(team_abbreviation),
                  season = season,
                  team_name = dplyr::if_else(team_name == "New Orleans/Oklahoma City Hornets", "New Orleans Hornets",team_name),
                  team_name = dplyr::if_else(team_name == "LA Clippers","Los Angeles Clippers",team_name),
                  former_team_name = team_name,
                  team_name = dplyr::if_else(team_name == "New Orleans Hornets","New Orleans Pelicans",team_name),
                  team_name = dplyr::if_else(team_name == "New Jersey Nets","Brooklyn Nets",team_name),
                  team_name = dplyr::if_else(team_name == "Charlotte Bobcats","Charlotte Hornets",team_name),
                  team_name = dplyr::if_else(team_name == "Seattle SuperSonics","Oklahoma City Thunder",team_name)
    ) %>%
    dplyr::left_join(get_all_stars(season) %>%
                       dplyr::group_by(team_name) %>%
                       dplyr::summarise(selections = dplyr::n()), by=c("team_name")) %>%
    dplyr::mutate(selections = replace_na(selections,0))


  return(team_stats)
}

get_champion <- function(.season = "all") {

  df <- (xml2::read_html("https://www.basketball-reference.com/playoffs/") %>%
           rvest::html_nodes(css = "table") %>%
           rvest::html_table(trim = TRUE))[[1]] %>%
    janitor::clean_names() %>%
    dplyr::select(1,3)

  colnames(df) <- df %>% head(1)

  df <- df %>%
    dplyr::slice(2:dplyr::n()) %>%
    janitor::clean_names() %>%
    dplyr::mutate(post_year = stringr::str_sub(year,start = 3),
                  year = as.integer(year),
                  pre_year = year-1,
                  pre_year = as.character(pre_year),
                  season = glue::glue("{pre_year}-{post_year}")) %>%
    dplyr::select(season,champion) %>%
    dplyr::mutate_all(as.character) %>%
    dplyr::filter(season != "NA-")


  if(.season =="all") {
    return(df) } else {
      if(.season %in% (df %>% dplyr::pull(season))) {
        return(df %>% dplyr::filter(season == .season) %>% dplyr::pull(champion))}
      else return("No champion, on this specific year !") }
}


## Subtitles for net rating plot


get_subtitles_champion <- function(season = "2020-21") {
  dplyr::if_else(get_champion(season) == '',
                 glue::glue("The {season} NBA season has yet to crown an <span style='color:#CBA049;font-size:18pt;'>NBA Champion.</span>"),
                 glue::glue("<span style='color:#CBA049;font-size:18pt;'>The {get_champion(season)}</span> won the NBA Championship in {season}."))
}


## Get Team Advanced Interactive

get_team_advanced_interactive <- function(season = "2020-21") {

  get_team_advanced(season) %>%
    dplyr::select(team_id:w_pct,off_rating,def_rating,net_rating,logo) %>%
    dplyr::mutate(mean_off = mean(off_rating),
                  mean_def = mean(def_rating),
                  positive_teams = dplyr::if_else(net_rating >= 0,
                                                  ggplot2::alpha("#BA5568",0.3),
                                                  ggplot2::alpha("#aaccdd",0.3)),
                  champion = dplyr::if_else(team_name == get_champion(season),"#CBA049","transparent"),
                  fill_box = dplyr::if_else(net_rating >= 0,
                                            ggplot2::alpha("#8f4351",0.95),
                                            ggplot2::alpha("#6e818a",0.95)),
                  border_box = dplyr::if_else(team_name == get_champion(season),"#CBA049","#303030")) %>%
    dplyr::mutate(net_rating_display = as.character(dplyr::if_else(net_rating>0,glue::glue("+{net_rating}"),glue::glue("{net_rating}"))),
                  off_rating_rank = dplyr::min_rank(dplyr::desc(off_rating)) %>% scales::ordinal(),
                  def_rating_rank = dplyr::min_rank(def_rating) %>% scales::ordinal(),
                  net_rating_rank = dplyr::min_rank(dplyr::desc(net_rating)) %>% scales::ordinal()) %>%
    dplyr::mutate(tooltip_label = glue::glue("<div style = 'font-family:Kiwi Maru;color:white;padding:5px;border-radius:4px;border-style:solid;border-color:{champion};border-width:2px;background-color:{fill_box};'><strong style = 'font-size:10pt'>{team_name}</strong><hr style='margin-top:5px;margin-bottom:1px;border:0;height:0;border-top:1px solid rgba(0,0,0,0.1);border-bottom:1px solid rgba(255,255,255,0.3);'/><span style = 'font-size:10pt'>Offensive efficiency : {off_rating} ({off_rating_rank})<br>Defensive efficiency : {def_rating} ({def_rating_rank}) <br>Net efficiency : {net_rating_display} ({net_rating_rank})</span></div>"))
}


## Efficiency interactive plot (ggiraph)

plot_teams_efficiency_interactive <- function(season = "2020-21") {

  df <- get_team_advanced_interactive(season)

  off_rating <- df$off_rating
  def_rating <- df$def_rating
  mean_rating <- mean(mean(off_rating),mean(def_rating))

  max_ratings <- dplyr::tibble(off = c((min(off_rating) - mean_rating) %>% abs(),(max(off_rating) - mean_rating) %>% abs()),
                               def = c((max(def_rating) - mean_rating) %>% abs(),(min(def_rating) - mean_rating) %>% abs()))

  plot <- df %>%
    ggplot2::ggplot() +
    ggplot2::geom_hline(ggplot2::aes(yintercept = mean_def),
                        color = court_themes('lines'),
                        linetype = 'dashed',
                        alpha = .35) +
    ggplot2::geom_vline(aes(xintercept = mean_off),
                        color = court_themes('lines'),
                        linetype = 'dashed',
                        alpha = .35) +
    ggplot2::annotate(geom = "text",
                      x = mean(off_rating),
                      y = (mean(def_rating) - max(max_ratings))*.99,
                      label = "High Defensive Efficiency",
                      family = "Kiwi Maru",
                      size = 12,
                      alpha = .75,
                      color = "grey10") +
    ggplot2::annotate(geom = "text",
                      x = (mean(off_rating) + max(max_ratings))*1.01,
                      y = mean(def_rating),
                      label = "High Offensive Efficiency",
                      family = "Kiwi Maru",
                      size = 12,
                      alpha = .75,
                      angle = 90,
                      color = "grey10") +
    ggplot2::annotate(geom = "text",
                      x = (mean(off_rating) - max(max_ratings))*0.99,
                      y = mean(def_rating),
                      label = "Low Offensive Efficiency",
                      family = "Kiwi Maru",
                      size = 12,
                      alpha = .75,
                      angle = 90,
                      color = "grey10") +
    ggplot2::annotate(geom = "text",
                      x = mean(off_rating),
                      y = (mean(def_rating) + max(max_ratings))*1.01,
                      label = "Low Defensive Efficiency",
                      family = "Kiwi Maru",
                      size = 12,
                      alpha = .75,
                      color = "grey10") +
    # ggplot2::geom_point(aes(x = off_rating,
    #                         y = def_rating,
    #                         fill = positive_teams,alpha(positive_teams,0.1)
    #                         color = champion),
    #                     shape = 21,
    #                     stroke = 2,
    #                     size = 30) +
    ggtext::geom_richtext(ggplot2::aes(x = off_rating,
                                       y = def_rating,
                                       label = link_to_img(logo)),
                          fill = NA,
                          label.color = NA,
                          label.padding = grid::unit(rep(0,4),"pt")) +
    ggiraph::geom_point_interactive(ggplot2::aes(x = off_rating,
                                                 y = def_rating,
                                                 tooltip = tooltip_label,
                                                 data_id = team_name,
                                                 fill = positive_teams,
                                                 color = champion),
                                    shape = 21,
                                    stroke = 2,
                                    size = 30) +
    ggplot2::scale_y_reverse(limits = c((mean(def_rating) + max(max_ratings))*1.01,
                                        (mean(def_rating) - max(max_ratings))*.99)) +
    ggplot2::labs(title = glue::glue("Teams offensive and defensive efficiency <span style='color:#CBA049;'>| {season}</span> ",),
                  subtitle = glue::glue("Efficiency corresponds to the number of points scored / conceded by a team every 100 possessions.<br>
        <span style='color:#BA5568;font-size:18pt;'>Teams with positive efficiency</span> are reliably more valuable than <span style='color:#AACCDD;font-size:18pt;'>teams with negative efficiency.</span><br>
        {get_subtitles_champion(season)}"),
                  caption = glue::glue("Visualisation by Henri Freixe • Sources : Nba.com, Basketball-reference.com")) +
    ggplot2::coord_cartesian(xlim = c((mean(off_rating) - max(max_ratings))*.99,
                                      (mean(off_rating) + max(max_ratings))*1.01)) +
    ggplot2::scale_color_identity() +
    ggplot2::scale_fill_identity() +
    ggplot2::theme_minimal(base_size = 22) +
    theme_dark_cap() +
    ggplot2::theme(plot.margin = ggplot2::margin(t = 5.5, r= 5.5, b = 0, l = 5.5, unit = "pt"),
                   plot.title=ggtext::element_markdown(hjust = 0.5,
                                                       margin = ggplot2::margin(t = 20)),
                   plot.title.position = "plot",
                   plot.subtitle = ggtext::element_markdown(size = 14,
                                                            margin = ggplot2::margin(t = 20,
                                                                                     b=20),
                                                            lineheight = 1,
                                                            hjust=0.5),
                   plot.caption = ggtext::element_markdown(size = 9,
                                                           margin = ggplot2::margin(t = 20,
                                                                                    b = 10)),
                   panel.grid = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank())

  ggiraph::girafe(ggobj = plot,
                  width_svg = 12,
                  height_svg = 12,
                  options = list(ggiraph::opts_tooltip(css="background-color:transparent"),
                                 ggiraph::opts_hover(css = "fill:red;")))

}
