get_finals_stats_n_game <- function(season = "2019-20", n) {

  .headers  <- headers()

  url_finals_stats <- function(season) {
    glue::glue("https://stats.nba.com/stats/leaguedashteamstats?Conference=&DateFrom=&DateTo=&Division=&GameScope=&GameSegment=&LastNGames={n}&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=4&PaceAdjust=N&PerMode=Totals&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season={season}&SeasonSegment=&SeasonType=Playoffs&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision=")
  }

  res <- httr::GET(url = url_finals_stats(season), httr::add_headers(.headers=.headers))
  json_resp <- jsonlite::fromJSON(httr::content(res, "text"))

  stats_data <- data.frame(json_resp$resultSets$rowSet[1]) %>% as_tibble()
  colnames(stats_data) <- json_resp$resultSets[["headers"]][[1]]

  stats_data <- stats_data %>%
    janitor::clean_names() %>%
    dplyr::select(team_id,team_name,w,l,pts) %>%
    dplyr::mutate(dplyr::across("w":"pts",as.numeric))
  return(stats_data)

}

get_finals_stats <- function(season = "2019-20") {

  games <- c(1:(get_finals_stats_n_game(season,n = 7) %>%
                  dplyr::summarise(games = sum(w)) %>%
                  dplyr::pull(games)))
  future::plan(future::multisession)
  games %>%
    furrr::future_map_dfr(.f = get_finals_stats_n_game, season = season) %>%
    dplyr::group_by(team_name) %>%
    dplyr::rename(cum_w = w, cum_l = l, cum_pts = pts) %>%
    dplyr::mutate(w = cum_w - dplyr::lag(cum_w, default = 0),
                  l = cum_l - dplyr::lag(cum_l, default = 0),
                  pts = cum_pts - dplyr::lag(cum_pts, default = 0),
                  reverse_game = dplyr::row_number()) %>%
    dplyr::arrange(dplyr::desc(reverse_game)) %>%
    dplyr::mutate(game = dplyr::row_number()) %>%
    dplyr::select(team_id, team_name, w, l , pts, game) %>%
    dplyr::ungroup()

}

## From Player Name to Team Name

get_team_from_player <- function(player, season = "2020-21") {

  get_player_advanced(season) %>%
    dplyr::filter(player_name == player) %>%
    dplyr::select(team_id) %>%
    dplyr::left_join(get_team_traditional(season) %>% dplyr::select(team_id, team_name), by = c("team_id")) %>%
    dplyr::pull(team_name)
}


plot_finals <- function(season = "2019-20") {

  conf_df <- get_team_standings(season) %>%
    dplyr::select(team_name,conference, former_team_name)

  df <- get_finals_stats(season) %>%
    dplyr::left_join(conf_df, by = c("team_name" = "former_team_name")) %>%
    dplyr::mutate(conf_position = dplyr::if_else(conference == "West",-1,1),
                  win_position = w*conf_position,
                  start_position = conf_position*.3)

  stats <- get_award_stats(award = 'fmvp',.season = season) %>%
    dplyr::mutate(team_name = purrr::map(.x = player_name, .f = get_team_from_player, season = season))

  team_recap_df <- df %>%
    dplyr::group_by(conference, team_id, team_name, conf_position) %>%
    dplyr::summarise(n = sum(w)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(x = 0.5*conf_position,
                  y = 0.6,
                  color = dplyr::if_else(conference == "West","#2E4EB8","#de425b"),
                  label = glue::glue("<img src= {purrr::map(.x = team_id, .f = get_team_logo_from_team_id)} width = '70'><br><br><span style = 'color:#ccb076;font-weight:bold;font-size:24px'>{team_name}</span><br><br><span style = 'color:{color};font-size:55px;'>{n}</span>"))


  df %>% ggplot2::ggplot() +
    ggplot2::geom_segment(data = df %>% dplyr::filter(w == 1),
                          ggplot2::aes(y = game,
                                       yend = game,
                                       x = start_position,
                                       xend = win_position),
                          linetype = 2,
                          color = "#1A1A1A") +
    ggplot2::geom_point(data = df %>% dplyr::filter(w == 1),
                        ggplot2::aes(y = game,
                                     x = win_position,
                                     fill = conference),
                        color = "#1A1A1A",
                        size = 7,
                        shape = 21,
                        stroke = 0.5,
                        alpha = .9) +
    ggplot2::geom_point(data = df %>% dplyr::filter(conference == "West"),
                        ggplot2::aes(x = -.22,
                                     y = game,
                                     size = pts),
                        fill = ggplot2::alpha("#2E4EB8",0.35),
                        color = "#1A1A1A",
                        stroke = 0.5,
                        shape = 21) +
    ggplot2::geom_point(data = df %>% dplyr::filter(conference == "East"),
                        ggplot2::aes(x = .22,
                                     y = game,
                                     size = pts),
                        fill = ggplot2::alpha("#de425b",0.35),
                        color = "#1A1A1A",
                        stroke = 0.5,
                        shape = 21) +
    ggplot2::geom_text(data = df %>% dplyr::filter(conference == "West"),
                       ggplot2::aes(x = -.22,
                                    y = game,
                                    label = pts),
                       family = "Kiwi Maru",
                       size = 4,
                       color = "#999999") +
    ggplot2::geom_text(data = df %>% dplyr::filter(conference == "East"),
                       ggplot2::aes(x = .22,
                                    y = game,
                                    label = pts),
                       family = "Kiwi Maru",
                       size = 4.5,
                       color = "#999999") +
    ggplot2::geom_text(data = df,
                       ggplot2::aes(x = 0,
                                    y = game,
                                    label = glue::glue("Game {game}")),
                       family = "Kiwi Maru",
                       colour = "#1A1A1A",
                       size = 7) +
    #Finals MVP Label----------
  ggtext::geom_richtext(mapping = ggplot2::aes(x = 0,
                                               y = df %>% dplyr::arrange(dplyr::desc(game)) %>% head(1) %>% dplyr::pull(game) + 1.75),
                        label = glue::glue("<span style = 'color:#ccb076;font-weight:bold;font-size:22px'>Finals Most Valuable Player</span><br><span style = 'color:#999999'>{stats$team_name}</span>"),
                        family = "Kiwi Maru",
                        fill = NA,
                        label.color = NA,
                        lineheight = 1.75,
                        label.padding = grid::unit(rep(8,4),"pt"),
                        vjust = 0) +
    ggtext::geom_richtext(mapping = ggplot2::aes(x = 0,
                                                 y = df %>% dplyr::arrange(dplyr::desc(game)) %>% head(1) %>% dplyr::pull(game) + 1.75),
                          label = glue::glue("<img src= {get_player_picture(stats %>% dplyr::pull(player_name), season = season)} width = '145'><br><br><span style = 'display: block; font-weight: bold;color:#999999;font-size:22px;text-align:center;'>{stats %>% dplyr::pull(player_name) %>% remove_last_name() }<br>{stats %>% dplyr::pull(player_name) %>% get_last_name() }</span><br><br><span style = 'color:#777777;font-size:14px;display: block;text-align:center;line-height:18px;'><span style = 'font-size :18px'>{stats %>% dplyr::pull(pts)}</span> pts<br style = 'line-height:150%'><span style = 'font-size :18px;line-height:18px;'>{stats %>% dplyr::pull(ast)}</span> assists<br><span style = 'font-size :18px;line-height:18px;'>{stats %>% dplyr::pull(reb)}</span> rebounds</span><br>"),
                          family = "Kiwi Maru",
                          fill = "#1F1F1F",
                          label.color = NA,
                          lineheight = 0.5,
                          label.padding = grid::unit(c(9,4,4,4),"pt"),
                          label.r = unit(.25, "lines"),
                          vjust = 1) +
    #Teams recap Label----------
  ggtext::geom_richtext(data = team_recap_df,
                        mapping = ggplot2::aes(x = x,
                                               y = y,
                                               label = label),
                        family = "Kiwi Maru",
                        fill = NA,
                        label.color = NA,
                        label.padding = grid::unit(rep(4,4),"pt"),
                        vjust = 0,
                        hjust = 0.5) +
    #Scales and Labs --------
  ggplot2::scale_size_continuous(range = c(12,17),
                                 guide = FALSE) +
    ggplot2::scale_y_reverse(limits = c(df %>% dplyr::arrange(dplyr::desc(game)) %>% head(1) %>% dplyr::pull(game) + 6.5 ,-2.5)) +
    ggplot2::scale_x_continuous(limits = c(-1,1)) +
    ggplot2::scale_fill_manual(values = c("West" = "#2E4EB8","East" = "#de425b")) +
    ggplot2::guides(fill = FALSE) +
    ggplot2::labs(title = glue::glue("NBA Season Summary <span style = color:#CBA049;>| {season}</span>"),
                  subtitle = glue::glue("<span style = 'color:#CBA049;font-size:22pt;font-weight:bold'>NBA Finals</span><br>The <span style = color:#ccb076;>{get_champion(.season = season)}</span> won the NBA Championship"),
                  y = "",
                  x = "") +
    #Themes --------
  ggplot2::theme_minimal(base_size = 22) +
    theme_dark_cap() +
    ggplot2::theme(plot.title = ggtext::element_markdown(hjust = .5,
                                                         size = 24,
                                                         margin = ggplot2::margin(t = 20,
                                                                                  b = 0)),
                   plot.subtitle = ggtext::element_markdown(hjust=.5,
                                                            size = 16,
                                                            margin = ggplot2::margin(t = 15,
                                                                                     b = 25),
                                                            lineheight = 1),
                   plot.caption = ggtext::element_markdown(size = 9,
                                                           margin = ggplot2::margin(t = 20,
                                                                                    b = 10)),
                   axis.text.y = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_blank(),
                   axis.title.y = ggtext::element_markdown(size = 12,color = court_themes('lines')),
                   panel.grid = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(),
                   legend.background = ggplot2::element_blank())

}

plot_season_awards <- function(season = "2018-19") {

  awards <- c("mvp","dpoy","mip","smoy","roy")
  award_names <- c("Most Valuable Player", "Defensive Player of the Year", "Most Improved Player", "Sixth Man of the Year","Rookie of the Year")

  future::plan(future::multisession(workers = future::availableCores()))

  stats_tb <- awards %>%
    furrr::future_map_dfr(get_award_stats, .season = season) %>%
    dplyr::mutate(player_picture = furrr::future_map(.x = player_name,.f = get_player_picture, season = season),
                  award_name = award_names,
                  team_name = furrr::future_map(.x = player_name, .f = get_team_from_player, season = season)) %>%
    tidyr::unnest(cols = c(player_picture, team_name))

  df <- stats_tb %>%
    dplyr::mutate(label_player = dplyr::if_else(award == "dpoy",
                                                glue::glue("<img src= {player_picture} width = '145'><br><br><span style = 'display: block; font-weight: bold;color:#999999;font-size:22px;text-align:center;'>{player_name %>% remove_last_name() }<br>{player_name %>% get_last_name()}</span><br><br><span style = 'color:#777777;font-size:14px;display: block;text-align:center;line-height:18px;'><span style = 'font-size :18px'>{reb}</span> rebounds<br style = 'line-height:150%'><span style = 'font-size :18px;line-height:18px;'>{blk}</span> blocks<br><span style = 'font-size :18px;line-height:18px;'>{stl}</span> steals</span><br>"),
                                                glue::glue("<img src= {player_picture} width = '145'><br><br><span style = 'display: block; font-weight: bold;color:#999999;font-size:22px;text-align:center;'>{player_name %>% remove_last_name()  }<br>{player_name %>% get_last_name()}</span><br><br><span style = 'color:#777777;font-size:14px;display: block;text-align:center;line-height:18px;'><span style = 'font-size :18px'>{pts}</span> pts<br style = 'line-height:150%'><span style = 'font-size :18px;line-height:18px;'>{ast}</span> assists<br><span style = 'font-size :18px;line-height:18px;'>{reb}</span> rebounds</span><br>")),
                  label_award = glue::glue("<span style = 'color:#ccb076;font-weight:bold;font-size:20px'>{award_name}</span><br><span style = 'color:#999999'>{team_name}</span>"),
                  y = dplyr::if_else(award %in% c("mvp", "dpoy"), 1.75,-.75),
                  x = dplyr::if_else(award == "mip", 0,
                                     dplyr::if_else(award == "mvp", -.75,
                                                    dplyr::if_else(award == "dpoy", .75,
                                                                   dplyr::if_else(award == "smoy", -1.5, 1.5)))
                  ))

  df %>%
    ggplot2::ggplot() +
    ggtext::geom_richtext(mapping = ggplot2::aes(x = x,
                                                 y = y,
                                                 label = label_award),
                          family = "Kiwi Maru",
                          fill = NA,
                          label.color = NA,
                          label.padding = grid::unit(rep(4,4),"pt"),
                          vjust = 0,
                          hjust = 0.5) +
    ggtext::geom_richtext(mapping = ggplot2::aes(x = x,
                                                 y = y,
                                                 label = label_player),
                          family = "Kiwi Maru",
                          fill = "#1F1F1F",
                          label.color = NA,
                          lineheight = 0.5,
                          label.padding = grid::unit(c(9,4,4,4),"pt"),
                          label.r = unit(.25, "lines"),
                          vjust = 1)  +
    #Scales and Labs --------

  ggplot2::scale_y_continuous(limits = c(-2.6,1.9)) +
    ggplot2::scale_x_continuous(limits = c(-2,2)) +
    ggplot2::labs(subtitle = glue::glue("<span style = 'color:#CBA049;font-size:22pt;font-weight:bold'>NBA Regular Season Awards</span><br><span style = color:#ccb076;>{get_award('mvp',.season = season)}</span> was the NBA Most Valuable Player"),
                  y = "",
                  x = "") +
    #Themes --------
  ggplot2::theme_minimal(base_size = 22) +
    theme_dark_cap() +
    ggplot2::theme(plot.title = ggtext::element_markdown(hjust = .5,
                                                         size = 24,
                                                         margin = ggplot2::margin(t = 20,
                                                                                  b = 0)),
                   plot.subtitle = ggtext::element_markdown(hjust=.5,
                                                            size = 16,
                                                            margin = ggplot2::margin(t = 15,
                                                                                     b = 20),
                                                            lineheight = 1),
                   plot.caption = ggtext::element_markdown(size = 9,
                                                           margin = ggplot2::margin(t = 20,
                                                                                    b = 10)),
                   axis.text.y = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_blank(),
                   axis.title.y = ggtext::element_markdown(size = 12,color = court_themes('lines')),
                   panel.grid = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(),
                   legend.background = ggplot2::element_blank())
  #ON VA PAS FACETTER, ON VA AJOUTER LES X ET LES Y SUR NOTRE TIBBLE.

}


plot_all_stars <- function(season = "2018-19") {

  trad_stats <- get_traditional_stats(season) %>%
    dplyr::select(player_name, pts, reb, ast)

  position_color <- tidyr::tibble(pos = c("PG","SG","SF","PF","C"),
                                  col = RColorBrewer::brewer.pal(n = 5, "Set3"))

  df <- get_all_stars(season) %>%
    dplyr::left_join(get_bpm(season) %>% dplyr::select(player_name,bpm,pos), by = c("player_name")) %>%
    dplyr::left_join(trad_stats, by = c("player_name")) %>%
    dplyr::group_by(pos) %>%
    dplyr::arrange(dplyr::desc(bpm), .by_group = TRUE) %>%
    dplyr::left_join(position_color, by = c("pos")) %>%
    dplyr::mutate(n = dplyr::row_number()-1,
                  y = as.integer(n/3),
                  x = n %% 3,
                  pos = purrr::map(.x = pos, .f = get_position) %>% factor(levels = c("Point Guard",
                                                                                      "Shooting Guard",
                                                                                      "Small Forward",
                                                                                      "Power Forward",
                                                                                      "Center"))) %>%
    dplyr::arrange(pos) %>%
    dplyr::mutate(label_pos = glue::glue("<span style = 'color:{col}'>{pos}s</span>"),
                  label_pos = forcats::fct_inorder(label_pos),
                  player_label = as.character(glue::glue("<span style = 'color:#999999;font-size:13px'>{remove_last_name(player_name)}</span><br><span style = 'color:#999999;font-size:16px'>{get_last_name(player_name)}</span>"))) %>%
    dplyr::left_join(get_traditional_stats(season) %>%
                       dplyr::select(player_id,player_name), by = c("player_name"))
  #         tooltip_label = glue::glue("<div style = 'box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2);max-width: #150px;margin: auto;padding-bottom:5px;text-align: center;font-family: Kiwi #Maru;background-color:#1A1A1A'><img src=  style = 'width : 100%'><span style = 'display: block; #margin-top: 0.67em; margin-bottom: 0.67em; margin-left: 0; margin-right: 0; font-weight: #bold;color:#BFBFBF;font-size:14px'>{player_name}</span><span style = 'color:grey;font-size:10px;display: #block; margin-top: 1em; margin-bottom: 1em; margin-left: 0; margin-right: 0;'>{team_name}</span><span #style = 'color:#BFBFBF;font-size:10px;display: block; margin-top: 1em; margin-bottom: 1em; margin-left: #0; margin-right: 0;'><span style = 'font-weight :bold'>{round(pts,digits =1)}</span> pts per #game<br><span style = 'font-weight :bold'>{round(ast,digits =1)}</span> assists per game<br><span style #= 'font-weight :bold'>{round(reb,digits =1)}</span> rebounds per game</span></div>"))


  if (stringr::str_sub(season, end = 4) %>% as.integer() >= 2015) {
    df <- df %>%
      dplyr::mutate(image = glue::glue("https://ak-static.cms.nba.com/wp-content/uploads/headshots/nba/latest/260x190/{player_id}.png"))
  } else {
    image_df <- tidyr::tibble(player_name = c("Jerry Stackhouse", "Antawn Jamison","Ray Allen", "Richard Hamilton", "Juwan Howard","Lamar Odom","Shawn Marion","Steve Nash" , "Larry Hughes","Rashard Lewis","Jason Kidd","Morris Peterson","Jermaine O'Neal","Chauncey Billups","Chucky Atkins","Kurt Thomas","Michael Redd","Quentin Richardson","Jamaal Magloire","Corey Maggette","Jason Richardson","Andrei Kirilenko","Stephen Jackson","Al Harrington","Flip Murray","Carlos Arroyo","Raja Bell","Brian Cardinal","Carlos Boozer","Mehmet Okur","Vladimir Radmanovic","DeShawn Stevenson","Mike James","Mickael Pietrus","Michael Sweetney","Eddie House","Nate Robinson","Gerald Wallace","Brian Cook","Derek Fisher","Chris Wilcox","Sebastian Telfair","Chris Mihm","Brandon Roy","Hakim Warrick","Matt Carroll","Willie Green","Jannero Pargo","Travis Outlaw","Craig Smith","Al Thornton","Francisco Garcia","Linas Kleiza","Ronnie Brewer","Ryan Gomes","Andrew Bynum","Andray Blatche","Luke Ridnour","Jonny Flynn","Sam Young","Shannon Brown","Byron Mullens","Shaquille O'Neal","Joel Przybilla","Tyrus Thomas","Ronny Turiaf","Andris Biedrins","Chris Duhon","Brendan Haywood","Darko Milicic","Reggie Evans"),
                              espn_id = c(802,385,9,294,351,617,510,592,356,469,429,656,615,63,26,846,692,703,498,497,1018,434,378,308,1777,1055,49,130,1703,1014,1016,808,1051,2173,2175,348,2782,1026,1998,246,1731,2417,549,3027,2794,2211,2004,1821,2015,3031,3237,2755,2770,2991,2757,2748,2746,1985,3985,4020,2992,4005,614,682,3032,2789,2427,2377,1000,2171,1828))
    no_picture_players_list <- c("David Robinson", "Rik Smits", "Glenn Robinson","Glen Rice","Vin Baker", "Tracy Murray", "Maurice Taylor","Isaiah Rider","Michael Finley", "Tim Hardaway", "Kevin Willis","Tom Gugliotta","Arvydas Sabonis", "Johnny Newman", "Alan Henderson","John Starks","Jerry Stackhouse", "Ray Allen", "Matt Geiger","Clifford Robinson","Bryant Reeves", "Rex Chapman", "Hakeem Olajuwon","John Wallace","Larry Johnson", "Rony Seikaly", "Christian Laettner"  ,"Ike Austin","Lamond Murray", "Chris Gatling", "Derek Strong","Juwan Howard","Gary Trent", "Detlef Schrempf", "Tony Delk","Dana Barros","Ron Mercer", "Shandon Anderson", "Sam Mitchell","Blue Edwards","Armen Gilliam", "Jamal Mashburn", "LaPhonso Ellis","Eddie Johnson","Donyell Marshall", "John Stockton", "Sam Mack","Terry Porter","Mark Price", "Latrell Sprewell", "Vernon Maxwell","Larry Hughes","Othella Harrington", "Terry Cummings", "Lawrence Funderburke","Tony Massenburg","Antawn Jamison", "Chauncey Billups", "Clar. Weatherspoon"  ,"Jason Kidd","Rick Fox", "Kenny Anderson", "Dee Brown","Tyrone Nesby","Bobby Phills", "Cedric Ceballos", "John Amaechi","Chucky Atkins","Corey Maggette", "Lamar Odom", "Austin Croshere","Rashard Lewis","Bryon Russell", "Tariq Abdul-Wahad", "Erick Strickland","Monty Williams","Richard Hamilton", "Shawn Marion", "Steve Nash","Courtney Alexander","Marcus Fizer", "Morris Peterson", "Lindsey Hunter","Jermaine O'Neal","Antonio Davis", "Kurt Thomas", "Michael Redd","Quentin Richardson","Bobby Jackson", "Ricky Davis", "Jamaal Magloire","Malik Rose","Lee Nailon", "Stromile Swift", "Lucious Harris","Jason Richardson","Andrei Kirilenko", "Aaron McKie","Jeff McInnis","Matt Harpring","Dajuan Wagner", "Desmond Mason", "Stephen Jackson","Rodney White","Al Harrington", "Predrag Drobnjak", "Alvin Williams","Flip Murray","Carlos Arroyo", "Raja Bell", "Brian Cardinal","Carlos Boozer","Juan Dixon", "Mehmet Okur", "Speedy Claxton","Vladimir Radmanovic","Slava Medvedenko", "DeShawn Stevenson", "Mike James","Mickael Pietrus","Dan Dickau", "Michael Sweetney", "Eddie House","Primoz Brezec","Nate Robinson", "Gerald Wallace", "Brian Cook","Derek Fisher","Chris Wilcox", "Sebastian Telfair", "Chris Mihm","Brandon Roy","Hakim Warrick", "Matt Carroll", "Willie Green","Jannero Pargo","Travis Outlaw", "Rashad McCants", "Craig Smith","Al Thornton","Francisco Garcia", "Linas Kleiza", "Ronnie Brewer","Ryan Gomes","Andrew Bynum", "Andray Blatche", "Luke Ridnour","Jonny Flynn","Sam Young", "Shannon Brown", "Byron Mullens","Mark Jackson","Eric Snow")

    df <- df %>%
      dplyr::mutate(pre_image = glue::glue("https://ak-static.cms.nba.com/wp-content/uploads/headshots/nba/latest/260x190/{player_id}.png")) %>%
      dplyr::left_join(image_df, by = c("player_name")) %>%
      dplyr::mutate(espn_id = tidyr::replace_na(espn_id,0),
                    nba_logo ="https://a.espncdn.com/combiner/i?img=/i/teamlogos/leagues/500/nba.png",
                    espn_picture = dplyr::if_else(espn_id == 0,
                                                  'no picture',
                                                  as.character(get_picture_espn(espn_id))),
                    image = dplyr::if_else(espn_picture != 'no picture',
                                           espn_picture,
                                           dplyr::if_else(player_name %in% no_picture_players_list,
                                                          nba_logo,
                                                          as.character(pre_image))))
  }


  if (get_award(award = "asgmvp", .season = season) %>% length() == 1) {
    subtitle <- glue::glue("<span style = 'color:#CBA049;font-size:22pt;font-weight:bold'>NBA All-Stars</span><br><span style = 'color:{df %>% filter(player_name == get_award(award = 'asgmvp',.season = season)) %>% pull(col)}'>{get_award(award = 'asgmvp',.season = season)}</span> was the NBA All-Star Game Most Valuable Player")

  } else{

    subtitle <- glue::glue("<span style = 'color:#CBA049;font-size:22pt;font-weight:bold'>NBA All-Star Game Selected Players</span><br><span style = 'color:{df %>% filter(player_name == get_award(award = 'asgmvp',.season = season)[1]) %>% pull(col)}'>{get_award(award = 'asgmvp',.season = season)[1]}</span> and <span style = 'color:{df %>% filter(player_name == get_award(award = 'asgmvp',.season = season)[2]) %>% pull(col)}'>{get_award(award = 'asgmvp',.season = season)[2]}</span> were<br> the NBA All-Star Game Most Valuable Players")
  }


  df <- df %>%
    dplyr::mutate(tooltip_label = glue::glue("<div style = 'box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2);max-width: 150px;margin: auto;padding-bottom:5px;text-align: center;font-family: Kiwi Maru;background-color:#1A1A1A'><img src= {image} style = 'width : 100%'><span style = 'display: block; margin-top: 0.67em; margin-bottom: 0.67em; margin-left: 0; margin-right: 0; font-weight: bold;color:#BFBFBF;font-size:14px'>{player_name}</span><span style = 'color:grey;font-size:10px;display: block; margin-top: 1em; margin-bottom: 1em; margin-left: 0; margin-right: 0;'>{team_name}</span><span style = 'color:#BFBFBF;font-size:10px;display: block; margin-top: 1em; margin-bottom: 1em; margin-left: 0; margin-right: 0;'><span style = 'font-weight :bold'>{round(pts,digits =1)}</span> pts per game<br><span style = 'font-weight :bold'>{round(ast,digits =1)}</span> assists per game<br><span style = 'font-weight :bold'>{round(reb,digits =1)}</span> rebounds per game</span></div>"))

  plot <- df %>%
    ggplot2::ggplot() +
    ggiraph::geom_point_interactive(ggplot2::aes(x = x,
                                                 y = y,
                                                 fill = pos,
                                                 data_id = player_id,
                                                 tooltip = tooltip_label),
                                    shape = 21,
                                    color = "#262626",
                                    size = 13,
                                    stroke = 1.5,
                                    alpha = 0.9) +
    ggtext::geom_richtext(ggplot2::aes(x = x,
                                       y = y,
                                       label = player_label),
                          family = "Kiwi Maru",
                          fill = NA,
                          label.color = NA,
                          label.padding = grid::unit(rep(0,4),"pt"),
                          nudge_y = .45) +
    ggplot2::labs(y = "",
                  x = "",
                  subtitle = subtitle,
                  caption = glue::glue("Visualisation by Henri Freixe • Sources : Nba.com, Basketball-reference.com, Basketball.realgm.com,Espn.com")) +
    ggplot2::scale_fill_manual(values = RColorBrewer::brewer.pal(n = 5, "Set3"),
                               guide = FALSE) +
    ggplot2::scale_x_continuous(limits = c(-.5,2.5)) +
    ggplot2::scale_y_reverse(limits = c(2.25,-.5)) +
    ggplot2::facet_wrap(ggplot2::vars(label_pos),
                        nrow = 3) +
    ggplot2::theme_minimal(base_size = 22) +
    theme_dark_cap() +
    ggplot2::theme(plot.title = ggtext::element_markdown(hjust = .5,
                                                         size = 24,
                                                         margin = ggplot2::margin(t = 20,
                                                                                  b = 0)),
                   plot.subtitle = ggtext::element_markdown(hjust=.5,
                                                            size = 16,
                                                            margin = ggplot2::margin(t = 15,
                                                                                     b = 15),
                                                            lineheight = 1),
                   plot.caption = ggtext::element_markdown(size = 9,
                                                           margin = ggplot2::margin(t = 20,
                                                                                    b = 10)),
                   axis.text.y = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_blank(),
                   axis.title.y = ggtext::element_markdown(size = 12,
                                                           color = court_themes('lines')),
                   strip.text = ggtext::element_markdown(size = 18),
                   panel.grid = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(),
                   legend.background = ggplot2::element_blank())

}



plot_season_recap <- function(season = "2018-19") {

  future::plan(future::multisession)

  A %<-% plot_finals("2018-19") %globals% structure(TRUE, add =c("get_last_name","remove_last_name","get_team_logo_from_team_id","get_player_picture","get_player_picture_fromid","get_champion"))
  B %<-% plot_season_awards("2018-19") %globals% structure(TRUE, add =c("get_last_name","remove_last_name","get_award"))
  C %<-% plot_all_stars("2018-19") %globals% structure(TRUE, add =c("get_last_name","remove_last_name"))

  plot <- ((A/B/C) +
             patchwork::plot_layout(heights = c(6,5,6))) &
    theme(plot.background = ggplot2::element_rect(fill = court_themes('court'),color = court_themes('court')))


  ggiraph::girafe(ggobj = plot,
                  width_svg = 10,
                  height_svg = 34,
                  options = list(ggiraph::opts_tooltip(css="background-color:transparent"),
                                 ggiraph::opts_hover(css = "fill:red;")))


}
