## Data

#Geom_point

#On récupère les Datas !

get_per_poss_stats <- function(season = "2020-21") {

  .headers  <- headers()

  url_traditional_stats <- function(season) {
    glue::glue("https://stats.nba.com/stats/leaguedashplayerstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=Per100Possessions&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season={season}&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision=&Weight=")
  }

  res <- httr::GET(url = url_traditional_stats(season), httr::add_headers(.headers=.headers))
  json_resp <- jsonlite::fromJSON(httr::content(res, "text"))

  stats_data <- data.frame(json_resp$resultSets$rowSet[1]) %>% dplyr::as_tibble()
  colnames(stats_data) <- json_resp$resultSets[["headers"]][[1]]

  stats_data <- stats_data %>%
    janitor::clean_names() %>%
    dplyr::mutate(dplyr::across("age":"cfid",as.numeric)) %>%
    dplyr::mutate(fg2m = fgm - fg3m,
                  fg2a = fga - fg3a,
                  fg2_pct = fg2m / fg2a) %>%
    dplyr::mutate(team_abbreviation = dplyr::if_else(team_abbreviation == "UTA","UTH",dplyr::if_else(team_abbreviation == "NOP","NO",team_abbreviation)))

  return(stats_data)
}

## Player Advanced Data

get_player_advanced <- function(season = "2020-21") {
  .headers  <- headers()

  url_traditional_stats <- function(season) {
    glue::glue("https://stats.nba.com/stats/leaguedashplayerstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&MeasureType=Advanced&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season={season}&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision=&Weight=")
  }

  res <- httr::GET(url = url_traditional_stats(season), httr::add_headers(.headers=.headers))
  json_resp <- jsonlite::fromJSON(httr::content(res, "text"))

  stats_data <- data.frame(json_resp$resultSets$rowSet[1]) %>% dplyr::as_tibble()
  colnames(stats_data) <- json_resp$resultSets[["headers"]][[1]]

  stats_data <- stats_data %>%
    janitor::clean_names() %>%
    dplyr::mutate(dplyr::across("age":"cfid",as.numeric)) %>%
    dplyr::mutate(team_abbreviation = dplyr::if_else(team_abbreviation == "UTA","UTH",dplyr::if_else(team_abbreviation == "NOP","NO",team_abbreviation)))

  return(stats_data)
}


# True Shooting Percentage Average

get_mean_ts_pct <- function(season = "2020-21") {

  df <- xml2::read_html(glue::glue('https://widgets.sports-reference.com/wg.fcgi?css=1&site=bbr&url=%2Fleagues%2FNBA_{stringr::str_sub(season,end = -4) %>% as.numeric()+1}.html&div=div_advanced-team')) %>%
    rvest::html_element("table") %>%
    rvest::html_table(trim = TRUE)

  colnames(df) <- df %>% head(1)
  df <- df %>%
    janitor::clean_names() %>%
    dplyr::pull(ts_percent) %>%
    tail(1)

  return(as.numeric(df))
}

# Player Position ?


get_scoring_rate <- function(season = "2020-21", team = 'global') {

  if (stringr::str_sub(season, end = 4) %>% as.integer() >= 2015) {
    get_per_poss_stats(season) %>%
      dplyr::left_join(get_player_advanced(season) %>%
                         dplyr::select(player_name,ts_pct,poss), by = c("player_name")) %>%
      dplyr::filter(poss > max(poss, na.rm = TRUE)*.4) %>%
      dplyr::mutate(mean_ts_pct = get_mean_ts_pct(season),
                    rel_ts_pct = ts_pct - mean_ts_pct,
                    pts_75 = pts*.75) %>%
      dplyr::arrange(dplyr::desc(pts)) %>%
      head(100) %>%
      dplyr::left_join(get_team_traditional(season) %>% dplyr::select(team_id,team_name), by = c("team_id")) %>%
      dplyr::mutate(selected_team = dplyr::if_else(team_name == team, 'yes','no')) %>%
      dplyr::mutate(image = glue::glue("https://ak-static.cms.nba.com/wp-content/uploads/headshots/nba/latest/260x190/{player_id}.png"),
                    player_label = glue::glue("<div style = 'box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2);max-width: 150px;margin: auto;padding-bottom:5px;text-align: center;font-family: {court_themes('font')};background-color:#1A1A1A'><img src= {image} style = 'width : 100%'><span style = 'display: block; margin-top: 0.67em; margin-bottom: 0.67em; margin-left: 0; margin-right: 0; font-weight: bold;color:#BFBFBF;font-size:14px'>{player_name}</span><span style = 'color:grey;font-size:10px;display: block; margin-top: 1em; margin-bottom: 1em; margin-left: 0; margin-right: 0;'>{team_name}</span><span style = 'color:#BFBFBF;font-size:10px;display: block; margin-top: 1em; margin-bottom: 1em; margin-left: 0; margin-right: 0;'><span style = 'font-weight :bold'>{round(pts_75,digits =1)} pts</span> per 75 poss.<br><span style = 'font-weight :bold'>{scales::percent(ts_pct,accuracy = .1)}</span> TS perc.</span></div>"))

  }else {

    df <- tidyr::tibble(player_name = c("Jerry Stackhouse", "Antawn Jamison","Ray Allen", "Richard Hamilton", "Juwan Howard","Lamar Odom","Shawn Marion","Steve Nash" , "Larry Hughes","Rashard Lewis","Jason Kidd","Morris Peterson","Jermaine O'Neal","Chauncey Billups","Chucky Atkins","Kurt Thomas","Michael Redd","Quentin Richardson","Jamaal Magloire","Corey Maggette","Jason Richardson","Andrei Kirilenko","Stephen Jackson","Al Harrington","Flip Murray","Carlos Arroyo","Raja Bell","Brian Cardinal","Carlos Boozer","Mehmet Okur","Vladimir Radmanovic","DeShawn Stevenson","Mike James","Mickael Pietrus","Michael Sweetney","Eddie House","Nate Robinson","Gerald Wallace","Brian Cook","Derek Fisher","Chris Wilcox","Sebastian Telfair","Chris Mihm","Brandon Roy","Hakim Warrick","Matt Carroll","Willie Green","Jannero Pargo","Travis Outlaw","Craig Smith","Al Thornton","Francisco Garcia","Linas Kleiza","Ronnie Brewer","Ryan Gomes","Andrew Bynum","Andray Blatche","Luke Ridnour","Jonny Flynn","Sam Young","Shannon Brown","Byron Mullens","Shaquille O'Neal","Joel Przybilla","Tyrus Thomas","Ronny Turiaf","Andris Biedrins","Chris Duhon","Brendan Haywood","Darko Milicic","Reggie Evans"),
                        espn_id = c(802,385,9,294,351,617,510,592,356,469,429,656,615,63,26,846,692,703,498,497,1018,434,378,308,1777,1055,49,130,1703,1014,1016,808,1051,2173,2175,348,2782,1026,1998,246,1731,2417,549,3027,2794,2211,2004,1821,2015,3031,3237,2755,2770,2991,2757,2748,2746,1985,3985,4020,2992,4005,614,682,3032,2789,2427,2377,1000,2171,1828))
    no_picture_players_list <- c("David Robinson", "Rik Smits", "Glenn Robinson","Glen Rice","Vin Baker", "Tracy Murray", "Maurice Taylor","Isaiah Rider","Michael Finley", "Tim Hardaway", "Kevin Willis","Tom Gugliotta","Arvydas Sabonis", "Johnny Newman", "Alan Henderson","John Starks","Jerry Stackhouse", "Ray Allen", "Matt Geiger","Clifford Robinson","Bryant Reeves", "Rex Chapman", "Hakeem Olajuwon","John Wallace","Larry Johnson", "Rony Seikaly", "Christian Laettner"  ,"Ike Austin","Lamond Murray", "Chris Gatling", "Derek Strong","Juwan Howard","Gary Trent", "Detlef Schrempf", "Tony Delk","Dana Barros","Ron Mercer", "Shandon Anderson", "Sam Mitchell","Blue Edwards","Armen Gilliam", "Jamal Mashburn", "LaPhonso Ellis","Eddie Johnson","Donyell Marshall", "John Stockton", "Sam Mack","Terry Porter","Mark Price", "Latrell Sprewell", "Vernon Maxwell","Larry Hughes","Othella Harrington", "Terry Cummings", "Lawrence Funderburke","Tony Massenburg","Antawn Jamison", "Chauncey Billups", "Clar. Weatherspoon"  ,"Jason Kidd","Rick Fox", "Kenny Anderson", "Dee Brown","Tyrone Nesby","Bobby Phills", "Cedric Ceballos", "John Amaechi","Chucky Atkins","Corey Maggette", "Lamar Odom", "Austin Croshere","Rashard Lewis","Bryon Russell", "Tariq Abdul-Wahad", "Erick Strickland","Monty Williams","Richard Hamilton", "Shawn Marion", "Steve Nash","Courtney Alexander","Marcus Fizer", "Morris Peterson", "Lindsey Hunter","Jermaine O'Neal","Antonio Davis", "Kurt Thomas", "Michael Redd","Quentin Richardson","Bobby Jackson", "Ricky Davis", "Jamaal Magloire","Malik Rose","Lee Nailon", "Stromile Swift", "Lucious Harris","Jason Richardson","Andrei Kirilenko", "Aaron McKie","Jeff McInnis","Matt Harpring","Dajuan Wagner", "Desmond Mason", "Stephen Jackson","Rodney White","Al Harrington", "Predrag Drobnjak", "Alvin Williams","Flip Murray","Carlos Arroyo", "Raja Bell", "Brian Cardinal","Carlos Boozer","Juan Dixon", "Mehmet Okur", "Speedy Claxton","Vladimir Radmanovic","Slava Medvedenko", "DeShawn Stevenson", "Mike James","Mickael Pietrus","Dan Dickau", "Michael Sweetney", "Eddie House","Primoz Brezec","Nate Robinson", "Gerald Wallace", "Brian Cook","Derek Fisher","Chris Wilcox", "Sebastian Telfair", "Chris Mihm","Brandon Roy","Hakim Warrick", "Matt Carroll", "Willie Green","Jannero Pargo","Travis Outlaw", "Rashad McCants", "Craig Smith","Al Thornton","Francisco Garcia", "Linas Kleiza", "Ronnie Brewer","Ryan Gomes","Andrew Bynum", "Andray Blatche", "Luke Ridnour","Jonny Flynn","Sam Young", "Shannon Brown", "Byron Mullens","Mark Jackson","Eric Snow")

    get_per_poss_stats(season) %>%
      dplyr::left_join(get_player_advanced(season) %>%
                         dplyr::select(player_name,ts_pct,poss), by = c("player_name")) %>%
      dplyr::filter(poss > max(poss, na.rm = TRUE)*.4) %>%
      dplyr::mutate(mean_ts_pct = get_mean_ts_pct(season),
                    rel_ts_pct = ts_pct - mean_ts_pct,
                    pts_75 = pts*.75) %>%
      dplyr::arrange(desc(pts)) %>%
      head(100) %>%
      dplyr::left_join(get_team_traditional(season) %>% dplyr::select(team_id,team_name), by = c("team_id")) %>%
      dplyr::mutate(selected_team = dplyr::if_else(team_name == team, 'yes','no')) %>%
      dplyr::left_join(df, by = c("player_name")) %>%
      dplyr::mutate(espn_id = tidyr::replace_na(espn_id,0)) %>%
      dplyr::mutate(pre_image = glue::glue("https://ak-static.cms.nba.com/wp-content/uploads/headshots/nba/latest/260x190/{player_id}.png"),
                    nba_logo ="https://a.espncdn.com/combiner/i?img=/i/teamlogos/leagues/500/nba.png",
                    espn_picture = dplyr::if_else(espn_id == 0,
                                                  'no picture',
                                                  as.character(get_picture_espn(espn_id))),
                    image = dplyr::if_else(espn_picture != 'no picture',
                                           espn_picture,
                                           dplyr::if_else(player_name %in% no_picture_players_list,
                                                          nba_logo,
                                                          as.character(pre_image))),
                    player_label = glue::glue("<div style = 'box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2);max-width: 150px;margin: auto;padding-bottom:5px;text-align: center;font-family: {court_themes('font')};background-color:#1A1A1A'><img src= {image} style = 'width : 100%'><h1 style = 'color:#BFBFBF;font-size:14px'>{player_name}</h1><p style = 'color:grey;font-size:10px'>{team_name}</p><p style = 'color:#BFBFBF;font-size:10px;'><span style = 'font-weight :bold'>{round(pts_75,digits =1)} pts</span> per 75 poss.<br><span style = 'font-weight :bold'>{scales::percent(ts_pct,accuracy = .1)}</span> TS perc.</p></div>"))

  }



}

plotless_scoring_rate <- function(season = "2020-21", team = "global") {
  df <- get_scoring_rate(season,team)
  eff_color <- '#de425b'

  pre_xlim <- max(abs(min(df$rel_ts_pct)),abs(max(df$rel_ts_pct)))
  xlim <- c(-pre_xlim,pre_xlim)
  ylim <- c(min(df$pts_75)*.95,max(df$pts_75)*1.05)

  subtitles_third_line <- if (team == 'global') {
    glue::glue("<span style = 'font-size:18pt'>{df %>% dplyr::filter(rel_ts_pct>=0) %>% dplyr::pull(player_id) %>% length()}</span> of the 100 most prolific scorers had <span style ='color:{eff_color};font-size:18pt'>above avg. efficiency</span>")
  }else {
    glue::glue("The <span style = 'color:#CBA049;font-size:18pt;'>{team}</span> had <span style = 'font-size:18pt;color:#CBA049'>{df %>% dplyr::filter(selected_team == 'yes') %>% dplyr::pull(player_id) %>% length()}</span> of the 100 most prolific scorers in their roster,<br> <span style = 'font-size:18pt;color:#CBA049'>{df %>% dplyr::filter(selected_team == 'yes') %>% dplyr::filter(rel_ts_pct>=0) %>% dplyr::pull(player_id) %>% length()}</span> of which had <span style ='font-size:18pt;color:{eff_color}'>above avg. efficiency</span>")
  }

  plot <-  df %>%
    ggplot2::ggplot() +
    ggplot2::geom_vline(ggplot2::aes(xintercept = 0),
                        color = court_themes('lines'),
                        linetype = 'dashed',
                        alpha = .35) +
    ggplot2::annotate(geom = "text",
                      x = xlim[2],
                      y = (ylim[1]+ylim[2])*.5,
                      label = "Higher Efficiency",
                      family = court_themes('font'),
                      size = 12,
                      alpha = .75,
                      angle = 90,
                      color = "grey10") +
    ggplot2::annotate(geom = "text",
                      x = xlim[1],
                      y = (ylim[1]+ylim[2])*.5,
                      label = "Lower Efficiency",
                      family = court_themes('font'),
                      size = 12,
                      alpha = .75,
                      angle = 90,
                      color = "grey10") +
    ggplot2::annotate(geom = "text",
                      x = (xlim[1]+xlim[2])*.5,
                      y = ylim[1],
                      label = "Lower Volume",
                      family = court_themes('font'),
                      size = 12,
                      alpha = .75,
                      color = "grey10") +
    ggplot2::annotate(geom = "text",
                      x = (xlim[1]+xlim[2])*.5,
                      y = ylim[2],
                      label = "Higher Volume",
                      family = court_themes('font'),
                      size = 12,
                      alpha = .75,
                      color = "grey10") +
    ggiraph::geom_point_interactive(ggplot2::aes(x = rel_ts_pct,
                                                 y = pts_75,
                                                 data_id = player_id,
                                                 size = pts_75,
                                                 tooltip = player_label,
                                                 fill = rel_ts_pct,
                                                 color = selected_team),
                                    shape = 21,
                                    stroke = 1.5) +
    ggplot2::labs(title = glue::glue("Top 100 scorers performance breakdown<br>volume vs. efficiency trade-off<span style = 'color:#CBA049;'> | {season}</span>"),
                  subtitle = glue::glue("True Shooting % is a measure of efficiency that compounds all shot types<br>{subtitles_third_line}"),
                  y = "Scoring rate (pts per 75 poss.)",
                  x = "Relative True Shooting %",
                  caption = glue::glue("Visualisation by Henri Freixe • Sources : Nba.com, Basketball-reference.com")) +
    ggplot2::coord_cartesian(xlim = xlim,
                             ylim = ylim) +
    ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    ggplot2::scale_y_continuous(sec.axis = ggplot2::dup_axis()) +
    ggplot2::scale_size_continuous(range = c(10,15)) +
    ggplot2::scale_color_manual(values = c("yes" = "#CBA049","no"= "grey10")) +
    ggplot2::scale_fill_gradient2(low = ggplot2::alpha('#223c8f',.75),
                                  mid = ggplot2::alpha('#c6c6c6',.75),
                                  high = ggplot2::alpha('#de425b',.75),
                                  midpoint = 0,) +
    #viridis::scale_fill_viridis(option = "F",
    #                               direction = -1,
    #                               begin =.3,
    #                               end = .7,
    #                               alpha = .75,
    #                               guide = 'none') +
    #scico::scale_fill_scico(alpha = .75,
    #                        palette = 'vikO',
    #                        begin = .3,alpha,
    #                        end = .9) +
    ggplot2::guides(fill = FALSE,
                    size = FALSE,
                    color = FALSE) +
    ggplot2::theme_minimal(base_size = 22) +
    theme_dark_cap() +
    ggplot2::theme(plot.title = ggtext::element_markdown(hjust = .5,
                                                         margin = ggplot2::margin(t = 20)),
                   plot.subtitle = ggtext::element_markdown(hjust=.5,
                                                            size = 14,
                                                            margin = ggplot2::margin(t = 15,
                                                                                     b=15),
                                                            lineheight = 1),
                   plot.caption = ggtext::element_markdown(size = 9,
                                                           margin = ggplot2::margin(t = 20,
                                                                                    b = 10)),
                   panel.grid = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(size = 12, color = court_themes('lines')),
                   axis.text.x = ggplot2::element_text(size = 12, color = court_themes('lines')),
                   axis.title.x =  ggplot2::element_text(size = 12, color = court_themes('lines')),
                   axis.title.y =  ggplot2::element_text(size = 12, color = court_themes('lines')))

  return(plot)

}


plot_scoring_rate <- function(season = "2020-21",team = "global") {

  future::plan(future::multisession)

  a <- furrr::future_invoke_map(.f = list(plotless_scoring_rate,team_logo),.x = list(c(season,team),team))

  #future::futureAssign("team_logo",team_logo(team))
  #team_logo <- team_logo(team)

  plot_with_logo <- (a[[1]] + patchwork::inset_element(a[[2]], left = 0, top = 1.195, right = 0.09  , bottom = 1.1)) & ggplot2::theme(plot.background = ggplot2::element_rect(fill = court_themes('court'), color = court_themes('court')))

}
#  ggiraph::girafe(ggobj = plot_with_logo,
#                  width_svg = 12,
#                  height_svg = 12,
#                  options = list(ggiraph::opts_tooltip(css="background-color:transparent"),
#                                 ggiraph::opts_hover(css = "fill:red;"),
#                                 ggiraph::opts_toolbar(saveaspng = FALSE),
#                                 ggiraph::opts_sizing(rescale = FALSE)))
#
#}

