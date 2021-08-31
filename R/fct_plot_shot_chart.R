## From Player Name to Last Name / from team Name to nickname

get_last_name <- function(x) {
  stringr::str_extract(x, '[^ ]+$')

}

remove_last_name <- function(x) {
  stringr::str_remove(x, '[^ ]+$') %>% trimws()

}

#players_2001_02 <- get_scoring_rate(season = "2001-02") %>%
#  dplyr::mutate(url_exists = RCurl::url.exists(image)) %>%
#  dplyr::filter(url_exists == FALSE) %>%
#  dplyr::pull(player_name)
#
#espn_player_pictures <- tibble(player_name = c("Jerry Stackhouse", "Antawn Jamison","Ray Allen", "Richard Hamilton", "Juwan Howard","Lamar Odom","Shawn Marion","Steve Nash" , "Larry Hughes","Rashard Lewis","Jason Kidd","Morris Peterson","Jermaine O'Neal","Chauncey Billups","Chucky Atkins","Kurt Thomas","Michael Redd","Quentin Richardson","Jamaal Magloire","Corey Maggette","Jason Richardson","Andrei Kirilenko","Stephen Jackson","Al Harrington","Flip Murray","Carlos Arroyo","Raja Bell","Brian Cardinal","Carlos Boozer","Mehmet Okur","Vladimir Radmanovic","DeShawn Stevenson","Mike James","Mickael Pietrus","Michael Sweetney","Eddie House","Nate Robinson","Gerald Wallace","Brian Cook","Derek Fisher","Chris Wilcox","Sebastian Telfair","Chris Mihm","Brandon Roy","Hakim Warrick","Matt Carroll","Willie Green","Jannero Pargo","Travis Outlaw","Craig Smith","Al Thornton","Francisco Garcia","Linas Kleiza","Ronnie Brewer","Ryan Gomes","Andrew Bynum","Andray Blatche","Luke Ridnour","Jonny Flynn","Sam Young","Shannon Brown","Byron Mullens","Shaquille O'Neal"),
#                               espn_id = c(802,385,9,294,351,617,510,592,356,469,429,656,615,63,26,846,692,703,498,497,1018,434,378,308,1777,1055,49,130,1703,1014,1016,808,1051,2173,2175,348,2782,1026,1998,246,1731,2417,549,3027,2794,2211,2004,1821,2015,3031,3237,2755,2770,2991,2757,2748,2746,1985,3985,4020,2992,4005,614))
#
#no_picture_players_list <- c("David Robinson", "Rik Smits", "Glenn Robinson","Glen Rice","Vin Baker", "Tracy Murray", "Maurice Taylor","Isaiah Rider","Michael Finley", "Tim Hardaway", "Kevin Willis","Tom Gugliotta","Arvydas Sabonis", "Johnny Newman", "Alan Henderson","John Starks","Jerry Stackhouse", "Ray Allen", "Matt Geiger","Clifford Robinson","Bryant Reeves", "Rex Chapman", "Hakeem Olajuwon","John Wallace","Larry Johnson", "Rony Seikaly", "Christian Laettner"  ,"Ike Austin","Lamond Murray", "Chris Gatling", "Derek Strong","Juwan Howard","Gary Trent", "Detlef Schrempf", "Tony Delk","Dana Barros","Ron Mercer", "Shandon Anderson", "Sam Mitchell","Blue Edwards","Armen Gilliam", "Jamal Mashburn", "LaPhonso Ellis","Eddie Johnson","Donyell Marshall", "John Stockton", "Sam Mack","Terry Porter","Mark Price", "Latrell Sprewell", "Vernon Maxwell","Larry Hughes","Othella Harrington", "Terry Cummings", "Lawrence Funderburke","Tony Massenburg","Antawn Jamison", "Chauncey Billups", "Clar. Weatherspoon"  ,"Jason Kidd","Rick Fox", "Kenny Anderson", "Dee Brown","Tyrone Nesby","Bobby Phills", "Cedric Ceballos", "John Amaechi","Chucky Atkins","Corey Maggette", "Lamar Odom", "Austin Croshere","Rashard Lewis","Bryon Russell", "Tariq Abdul-Wahad", "Erick Strickland","Monty Williams","Richard Hamilton", "Shawn Marion", "Steve Nash","Courtney Alexander","Marcus Fizer", "Morris Peterson", "Lindsey Hunter","Jermaine O'Neal","Antonio Davis", "Kurt Thomas", "Michael Redd","Quentin Richardson","Bobby Jackson", "Ricky Davis", "Jamaal Magloire","Malik Rose","Lee Nailon", "Stromile Swift", "Lucious Harris","Jason Richardson","Andrei Kirilenko", "Aaron McKie","Jeff McInnis","Matt Harpring","Dajuan Wagner", "Desmond Mason", "Stephen Jackson","Rodney White","Al Harrington", "Predrag Drobnjak", "Alvin Williams","Flip Murray","Carlos Arroyo", "Raja Bell", "Brian Cardinal","Carlos Boozer","Juan Dixon", "Mehmet Okur", "Speedy Claxton","Vladimir Radmanovic","Slava Medvedenko", "DeShawn Stevenson", "Mike James","Mickael Pietrus","Dan Dickau", "Michael Sweetney", "Eddie House","Primoz Brezec","Nate Robinson", "Gerald Wallace", "Brian Cook","Derek Fisher","Chris Wilcox", "Sebastian Telfair", "Chris Mihm","Brandon Roy","Hakim Warrick", "Matt Carroll", "Willie Green","Jannero Pargo","Travis Outlaw", "Rashad McCants", "Craig Smith","Al Thornton","Francisco Garcia", "Linas Kleiza", "Ronnie Brewer","Ryan Gomes","Andrew Bynum", "Andray Blatche", "Luke Ridnour","Jonny Flynn","Sam Young", "Shannon Brown", "Byron Mullens")


## Team logos and player pictures - NBA API
get_player_picture_fromid <- function(player_id) {

  if (RCurl::url.exists(glue::glue("https://ak-static.cms.nba.com/wp-content/uploads/headshots/nba/latest/260x190/{player_id}.png"))) {
    picture <- glue::glue("https://ak-static.cms.nba.com/wp-content/uploads/headshots/nba/latest/260x190/{player_id}.png")
  } else { picture <- "https://a.espncdn.com/combiner/i?img=/i/teamlogos/leagues/500/nba.png"}

  return(picture)
}

get_player_picture <- function(player,season = "2020-21") {

  df <- dplyr::tibble(player_name = c("Jerry Stackhouse", "Antawn Jamison","Ray Allen", "Richard Hamilton", "Juwan Howard","Lamar Odom","Shawn Marion","Steve Nash" , "Larry Hughes","Rashard Lewis","Jason Kidd","Morris Peterson","Jermaine O'Neal","Chauncey Billups","Chucky Atkins","Kurt Thomas","Michael Redd","Quentin Richardson","Jamaal Magloire","Corey Maggette","Jason Richardson","Andrei Kirilenko","Stephen Jackson","Al Harrington","Flip Murray","Carlos Arroyo","Raja Bell","Brian Cardinal","Carlos Boozer","Mehmet Okur","Vladimir Radmanovic","DeShawn Stevenson","Mike James","Mickael Pietrus","Michael Sweetney","Eddie House","Nate Robinson","Gerald Wallace","Brian Cook","Derek Fisher","Chris Wilcox","Sebastian Telfair","Chris Mihm","Brandon Roy","Hakim Warrick","Matt Carroll","Willie Green","Jannero Pargo","Travis Outlaw","Craig Smith","Al Thornton","Francisco Garcia","Linas Kleiza","Ronnie Brewer","Ryan Gomes","Andrew Bynum","Andray Blatche","Luke Ridnour","Jonny Flynn","Sam Young","Shannon Brown","Byron Mullens","Shaquille O'Neal"),
                      espn_id = c(802,385,9,294,351,617,510,592,356,469,429,656,615,63,26,846,692,703,498,497,1018,434,378,308,1777,1055,49,130,1703,1014,1016,808,1051,2173,2175,348,2782,1026,1998,246,1731,2417,549,3027,2794,2211,2004,1821,2015,3031,3237,2755,2770,2991,2757,2748,2746,1985,3985,4020,2992,4005,614))

  if (player %in% df$player_name) {
    picture <- df %>% dplyr::filter(player_name == player) %>% dplyr::pull(espn_id) %>% get_picture_espn()
  }else {
    picture <- get_traditional_stats(season) %>%
      dplyr::filter(player_name == player) %>%
      dplyr::pull(player_id) %>%
      get_player_picture_fromid()
  }
  return(picture)
}


get_team_logo <- function(team_abbr) {
  glue::glue("https://a1.espncdn.com/combiner/i?img=/i/teamlogos/nba/500/scoreboard/{team_abbr}.png")
}


get_team_logo_from_team_id <- function(id) {

  get_team_traditional() %>%
    dplyr::filter(team_id == id) %>%
    dplyr::pull(logo) %>%
    unique()
}



get_team_logo_from_player_id <- function(id,season = "2020-21") {
  get_traditional_stats(season) %>%
    dplyr::filter(player_id == id) %>%
    dplyr::pull(team_id) %>%
    get_team_logo_from_team_id()
}


get_team_logo_from_player <- function(player,season = "2020-21") {
  get_traditional_stats(season) %>%
    dplyr::filter(player_name == player) %>%
    dplyr::pull(team_id) %>%
    get_team_logo_from_team_id()
}
## Player pictures - ESPN API

get_picture_espn <- function(player_id) {
  glue::glue("https://a.espncdn.com/combiner/i?img=/i/headshots/nba/players/full/{player_id}.png")
}
#Maintenant la question c'est comment on récupère ces id ?



### ID to abbreviation tibble()


get_abbr_from_id <- function(id) {
  ## This helps us to get the current logo for teams existing under a different name in the past
  get_team_traditional() %>%
    dplyr::filter(team_id == id) %>%
    dplyr::pull(team_abbreviation)
}

link_to_img <- function(x, width = 50, alt = "NBA Player") {
  glue::glue("<img src='{x}' alt = '{alt}' width='{width}'/>")
}

link_to_img_css <- function(x, width = 50) {
  glue::glue("<img src='{x}' width='{width}' style ='text-align: center;'/>")
}


# Data Querying and Wrangling

headers <- function() {
  headers <- c(
    `Connection` = 'keep-alive',
    `Accept` = 'application/json, text/plain, */*',
    `x-nba-stats-token` = 'true',
    `X-NewRelic-ID` = 'VQECWF5UChAHUlNTBwgBVw==',
    `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/78.0.3904.87 Safari/537.36',
    `x-nba-stats-origin` = 'stats',
    `Sec-Fetch-Site` = 'same-origin',
    `Sec-Fetch-Mode` = 'cors',
    `Referer` = 'https://stats.nba.com/players/leaguedashplayerbiostats/',
    `Accept-Encoding` = 'gzip, deflate, br',
    `Accept-Language` = 'en-US,en;q=0.9')

  return(headers)
}


get_traditional_stats <- function(season = "2020-21") {

  .headers  <- headers()

  url_traditional_stats <- function(season) {
    glue::glue("https://stats.nba.com/stats/leaguedashplayerstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season={season}&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision=&Weight=")
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

get_average_shot_data <- function(season = "2020-21") {

  # Player ID variable removed with number 1 arbitrarily, in case a problem comes up

  .headers  <- headers()

  url_shot_data <- function(season) {

    glue::glue("https://stats.nba.com/stats/shotchartdetail?AheadBehind=&CFID=33&CFPARAMS={season}&ClutchTime=&Conference=&ContextFilter=&ContextMeasure=FGA&DateFrom=&DateTo=&Division=&EndPeriod=10&EndRange=28800&GROUP_ID=&GameEventID=&GameID=&GameSegment=&GroupID=&GroupMode=&GroupQuantity=5&LastNGames=0&LeagueID=00&Location=&Month=0&OnOff=&OpponentTeamID=0&Outcome=&PORound=0&Period=0&PlayerID=1&PlayerID1=&PlayerID2=&PlayerID3=&PlayerID4=&PlayerID5=&PlayerPosition=&PointDiff=&Position=&RangeType=0&RookieYear=&Season={season}&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StartPeriod=1&StartRange=0&StarterBench=&TeamID=0&VsConference=&VsDivision=&VsPlayerID1=&VsPlayerID2=&VsPlayerID3=&VsPlayerID4=&VsPlayerID5=&VsTeamID=")

  }

  res <- httr::GET(url = url_shot_data(season), httr::add_headers(.headers=.headers))
  json_resp <- jsonlite::fromJSON(httr::content(res, "text"))


  av_data <- data.frame(json_resp$resultSets$rowSet[2]) %>% dplyr::as_tibble()
  colnames(av_data) <- json_resp$resultSets[["headers"]][[2]]

  av_data <- av_data %>%
    janitor::clean_names() %>%
    dplyr::mutate(dplyr::across("fga":"fgm",as.integer),
                  fg_pct = as.numeric(fg_pct)) %>%
    dplyr::mutate(shot_zone = glue::glue("{shot_zone_basic}{shot_zone_area}{shot_zone_range}"))

  return(av_data)
}


get_shot_data <- function(player_id,season = "2020-21") {

  .headers  <- headers()

  url_shot_data <- function(player_id,season) {

    glue::glue("https://stats.nba.com/stats/shotchartdetail?AheadBehind=&CFID=33&CFPARAMS={season}&ClutchTime=&Conference=&ContextFilter=&ContextMeasure=FGA&DateFrom=&DateTo=&Division=&EndPeriod=10&EndRange=28800&GROUP_ID=&GameEventID=&GameID=&GameSegment=&GroupID=&GroupMode=&GroupQuantity=5&LastNGames=0&LeagueID=00&Location=&Month=0&OnOff=&OpponentTeamID=0&Outcome=&PORound=0&Period=0&PlayerID={player_id}&PlayerID1=&PlayerID2=&PlayerID3=&PlayerID4=&PlayerID5=&PlayerPosition=&PointDiff=&Position=&RangeType=0&RookieYear=&Season={season}&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StartPeriod=1&StartRange=0&StarterBench=&TeamID=0&VsConference=&VsDivision=&VsPlayerID1=&VsPlayerID2=&VsPlayerID3=&VsPlayerID4=&VsPlayerID5=&VsTeamID=")

  }


  res <- httr::GET(url = url_shot_data(player_id,season), httr::add_headers(.headers=.headers))
  json_resp <- jsonlite::fromJSON(httr::content(res, "text"))


  shot_data <- data.frame(json_resp$resultSets$rowSet[1]) %>% dplyr::as_tibble()
  colnames(shot_data) <- json_resp$resultSets[["headers"]][[1]]

  shot_data <- shot_data %>%
    janitor::clean_names() %>%
    dplyr::mutate(dplyr::across(c("period","minutes_remaining","seconds_remaining","shot_distance","loc_x","loc_y","shot_attempted_flag","shot_made_flag"),as.integer),
                  game_date = lubridate::ymd(game_date)) %>%
    dplyr::mutate(loc_x = loc_x/10,
                  loc_y = loc_y/10 + 5.25) %>%
    dplyr::mutate(shot_zone = glue::glue("{shot_zone_basic}{shot_zone_area}{shot_zone_range}"))

  av <- get_average_shot_data(season = season) %>%
    dplyr::select("fga_league" = "fga","fgm_league" = "fgm","fg_pct_league" = "fg_pct","shot_zone")


  shot_data <- shot_data %>%
    dplyr::left_join(av, by = c("shot_zone")) %>%
    dplyr::group_by(shot_zone) %>%
    dplyr::mutate(fga = sum(shot_attempted_flag),
                  fgm = sum(shot_made_flag),
                  fg_pct = fgm/fga,
                  av_loc_x = mean(loc_x),
                  av_loc_y = mean(loc_y)) %>%
    dplyr::ungroup(shot_zone) %>%
    dplyr::mutate(zone_efficiency = dplyr::if_else((fg_pct - fg_pct_league) > 0.1, "very high",
                                                   dplyr::if_else((fg_pct - fg_pct_league) %>% dplyr::between(0.06,0.1),"high",
                                                                  dplyr::if_else((fg_pct - fg_pct_league) %>% dplyr::between(0.02,0.06),"high",
                                                                                 dplyr::if_else((fg_pct - fg_pct_league) %>% dplyr::between(-0.02,0.02),"high",
                                                                                                dplyr::if_else((fg_pct - fg_pct_league) %>% dplyr::between(-0.02,-0.06),"high",
                                                                                                               dplyr::if_else((fg_pct - fg_pct_league) < -0.1,"very low","low")))))))

  return(shot_data)
}

get_sum_shot_data <- function(player_id, season = "2020-21") {

  .headers  <- headers()

  url_shot_data <- function(player_id,season) {

    glue::glue("https://stats.nba.com/stats/shotchartdetail?AheadBehind=&CFID=33&CFPARAMS={season}&ClutchTime=&Conference=&ContextFilter=&ContextMeasure=FGA&DateFrom=&DateTo=&Division=&EndPeriod=10&EndRange=28800&GROUP_ID=&GameEventID=&GameID=&GameSegment=&GroupID=&GroupMode=&GroupQuantity=5&LastNGames=0&LeagueID=00&Location=&Month=0&OnOff=&OpponentTeamID=0&Outcome=&PORound=0&Period=0&PlayerID={player_id}&PlayerID1=&PlayerID2=&PlayerID3=&PlayerID4=&PlayerID5=&PlayerPosition=&PointDiff=&Position=&RangeType=0&RookieYear=&Season={season}&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StartPeriod=1&StartRange=0&StarterBench=&TeamID=0&VsConference=&VsDivision=&VsPlayerID1=&VsPlayerID2=&VsPlayerID3=&VsPlayerID4=&VsPlayerID5=&VsTeamID=")

  }


  res <- httr::GET(url = url_shot_data(player_id,season), httr::add_headers(.headers=.headers))
  json_resp <- jsonlite::fromJSON(httr::content(res, "text"))


  shot_data <- data.frame(json_resp$resultSets$rowSet[1]) %>% dplyr::as_tibble()
  colnames(shot_data) <- json_resp$resultSets[["headers"]][[1]]

  shot_data <- shot_data %>%
    janitor::clean_names() %>%
    dplyr::mutate(dplyr::across(c("period","minutes_remaining","seconds_remaining","shot_distance","loc_x","loc_y","shot_attempted_flag","shot_made_flag"),as.integer),
                  game_date = lubridate::ymd(game_date)) %>%
    dplyr::mutate(loc_x = loc_x/10,
                  loc_y = loc_y/10 + 5.25) %>%
    dplyr::mutate(shot_zone = glue::glue("{shot_zone_basic}{shot_zone_area}{shot_zone_range}"))

  av <- get_average_shot_data(season = season) %>%
    dplyr::select("fga_league" = "fga","fgm_league" = "fgm","fg_pct_league" = "fg_pct","shot_zone")


  shot_data <- shot_data %>%
    dplyr::left_join(av, by = c("shot_zone")) %>%
    dplyr::group_by(shot_zone) %>%
    dplyr::summarise(fga = sum(shot_attempted_flag),
                     fgm = sum(shot_made_flag),
                     fg_pct = fgm/fga,
                     av_loc_x = mean(loc_x),
                     av_loc_y = mean(loc_y),
                     fga_league,
                     fgm_league,
                     fg_pct_league,
                     shot_zone_basic,
                     shot_zone_area,
                     shot_zone_range) %>%
    dplyr::ungroup(shot_zone) %>%
    unique() %>%
    dplyr::mutate(zone_efficiency = dplyr::if_else((fg_pct - fg_pct_league) > 0.1, "very high",
                                                   dplyr::if_else((fg_pct - fg_pct_league) %>% dplyr::between(0.06,0.1),"high",
                                                                  dplyr::if_else((fg_pct - fg_pct_league) %>% dplyr::between(0.02,0.06),"high",
                                                                                 dplyr::if_else((fg_pct - fg_pct_league) %>% dplyr::between(-0.02,0.02),"high",
                                                                                                dplyr::if_else((fg_pct - fg_pct_league) %>% dplyr::between(-0.02,-0.06),"high",
                                                                                                               dplyr::if_else((fg_pct - fg_pct_league) < -0.1,"very low","low")))))))
  return(shot_data)

}

get_shot_data_player <- function(player,season = "2020-21", summary = TRUE) {

  if(summary==TRUE) {get_sum_shot_data(player_id = get_traditional_stats(season) %>% dplyr::filter(player_name == player) %>% dplyr::pull(player_id),season=season)} else
  {get_shot_data(player_id = get_traditional_stats(season) %>% dplyr::filter(player_name == player) %>% dplyr::pull(player_id),season=season)}
}


get_shot_data_player2 <- function(player,season = "2020-21", f = get_sum_shot_data) {

  f(player_id = get_traditional_stats(season) %>% dplyr::filter(player_name == player) %>% dplyr::pull(player_id),season=season)

}


# Adding the subtitles

average_2pct3pct <- function(season = "2020-21") {

  get_average_shot_data(season) %>%
    dplyr::mutate(type = dplyr::if_else(shot_zone_basic %in% c("Mid-Range","Restricted Area","In The Paint (Non-RA)"),"2ps","3ps")) %>%
    dplyr::group_by(type) %>%
    dplyr::summarise(fga = sum(fga),
                     fgm = sum(fgm)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(fg_pct = fgm/fga) %>%
    tidyr::pivot_wider(values_from = c(fga,fgm,fg_pct),
                       names_from = type) %>%
    dplyr::select("fg2_pct_league" = fg_pct_2ps,"fg3_pct_league" = fg_pct_3ps)

}

get_subtitles_tbl <- function(player,season = "2020-21") {
  color_palette <- get_color_tibble('palette')
  color_palette_tbl <- get_color_tibble('tibble')

  get_traditional_stats(season) %>%
    dplyr::filter(player_name == player) %>%
    dplyr::bind_cols(average_2pct3pct(season)) %>%
    dplyr::mutate(efficiency_fg2 = dplyr::if_else((fg2_pct - fg2_pct_league) > 0.1, "very high",
                                                  dplyr::if_else((fg2_pct-fg2_pct_league) %>% dplyr::between(0.06,0.1),"high",
                                                                 dplyr::if_else((fg2_pct-fg2_pct_league) %>% dplyr::between(0.02,0.06),"medium high",
                                                                                dplyr::if_else((fg2_pct-fg2_pct_league) %>% dplyr::between(-0.02,0.02),"medium",
                                                                                               dplyr::if_else((fg2_pct-fg2_pct_league) %>% dplyr::between(-0.02,-0.06),"medium low",
                                                                                                              dplyr::if_else((fg2_pct - fg2_pct_league) < -0.1,"very low","low")))))),
                  efficiency_fg3 = dplyr::if_else((fg3_pct - fg3_pct_league) > 0.1, "very high",
                                                  dplyr::if_else((fg3_pct - fg3_pct_league) %>% dplyr::between(0.06,0.1),"high",
                                                                 dplyr::if_else((fg3_pct - fg3_pct_league) %>% dplyr::between(0.02,0.06),"medium high",
                                                                                dplyr::if_else((fg3_pct - fg3_pct_league) %>% dplyr::between(-0.02,0.02),"medium",
                                                                                               dplyr::if_else((fg3_pct - fg3_pct_league) %>% dplyr::between(-0.02,-0.06),"medium low",
                                                                                                              dplyr::if_else((fg3_pct - fg3_pct_league) < -0.1,"very low","low"))))))) %>%
    dplyr::left_join(color_palette_tbl, by = c("efficiency_fg2" = "rowname")) %>%
    dplyr::left_join(color_palette_tbl, by = c("efficiency_fg3" = "rowname")) %>%
    dplyr::rename("color_fg2" = "color_palette.x",
                  "color_fg3" = "color_palette.y")
}

## Shot chart plotting function

# Court characteristics

## Se rappeler de récupérer le code initial si je veux reformer
## les lignes de court au global

circle_points = function(center = c(0, 0), radius = 1, npoints = 360) {
  angles = seq(0, 2 * pi, length.out = npoints)
  return(dplyr::tibble(x = center[1] + radius * cos(angles),
                       y = center[2] + radius * sin(angles)))
}


court_points <- function() {
  width <-  50
  height <-  94 / 2
  key_height <-  19
  inner_key_width <- 12
  outer_key_width <- 16
  backboard_width <- 6
  backboard_offset <- 4
  neck_length <- 0.5
  hoop_radius <- 0.75
  hoop_center_y <- backboard_offset + neck_length + hoop_radius
  three_point_radius <- 23.75
  three_point_side_radius <- 22
  three_point_side_height <- 14

  court_points = dplyr::tibble(
    x = c(width / 2, width / 2, -width / 2, -width / 2, width / 2),
    y = c(0, 0, 0, 0, 0),
    desc = "perimeter"
  )

  court_points = dplyr::bind_rows(court_points , dplyr::tibble(
    x = c(outer_key_width / 2, outer_key_width / 2, -outer_key_width / 2, -outer_key_width / 2),
    y = c(0, key_height, key_height, 0),
    desc = "outer_key"
  ))

  court_points = dplyr::bind_rows(court_points , dplyr::tibble(
    x = c(-backboard_width / 2, backboard_width / 2),
    y = c(backboard_offset, backboard_offset),
    desc = "backboard"
  ))

  court_points = dplyr::bind_rows(court_points , dplyr::tibble(
    x = c(0, 0), y = c(backboard_offset, backboard_offset + neck_length), desc = "neck"
  ))

  foul_circle = circle_points(center = c(0, key_height), radius = inner_key_width / 2)

  foul_circle_top = dplyr::filter(foul_circle, y > key_height) %>%
    dplyr::mutate(desc = "foul_circle_top")

  foul_circle_bottom = dplyr::filter(foul_circle, y < key_height) %>%
    dplyr::mutate(
      angle = atan((y - key_height) / x) * 180 / pi,
      angle_group = floor((angle - 5.625) / 11.25),
      desc = paste0("foul_circle_bottom_", angle_group)
    ) %>%
    dplyr::filter(angle_group %% 2 == 0) %>%
    dplyr::select(x, y, desc)

  hoop = circle_points(center = c(0, hoop_center_y), radius = hoop_radius) %>%
    dplyr::mutate(desc = "hoop")

  restricted = circle_points(center = c(0, hoop_center_y), radius = 4) %>%
    dplyr::filter(y >= hoop_center_y) %>%
    dplyr::mutate(desc = "restricted")

  three_point_circle = circle_points(center = c(0, hoop_center_y), radius = three_point_radius) %>%
    dplyr::filter(y >= three_point_side_height, y >= hoop_center_y)

  three_point_line = dplyr::tibble(
    x = c(three_point_side_radius, three_point_side_radius, three_point_circle$x, -three_point_side_radius, -three_point_side_radius),
    y = c(0, three_point_side_height, three_point_circle$y, three_point_side_height, 0),
    desc = "three_point_line"
  )

  court_points = dplyr::bind_rows(
    court_points,
    foul_circle_top,
    foul_circle_bottom,
    hoop,
    restricted,
    three_point_line
  )

  return(court_points)
}


court_layout <- function() {

  .court_points <- court_points()

  ggplot2::ggplot() +
    ggplot2::geom_path(
      data = .court_points,
      ggplot2::aes(x = x, y = y, group = desc),
      color = court_themes('lines')
    ) +
    ggplot2::coord_fixed(ylim = c(0, 45), xlim = c(-25, 25)) +
    ggplot2::theme_minimal(base_size = 22) +
    theme_dark_cap() +
    ggplot2::theme(
      plot.margin = ggplot2::margin(t = 5.5, r= 5.5, b = -10, l = 5.5, unit = "pt"),
      plot.title=ggtext::element_markdown(hjust = 0.5,
                                          margin = ggplot2::margin(t = 20)),
      plot.title.position = "plot",
      plot.subtitle = ggtext::element_markdown(size = 14,
                                               margin = ggplot2::margin(t = 10,
                                                                        b=0),
                                               hjust=0.5),
      panel.grid = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank())


}

plot_court <- function(player,
                       season = "2020-21",
                       summary = TRUE) {

  height <-  94 / 2

  # Shooting efficiency Color palette

  color_palette <- get_color_tibble('palette')

  color_palette_tbl <- get_color_tibble('tibble')

  .court_layout <- court_layout()
  # Dataset

  df <- get_shot_data_player(player, season, summary) %>%
    dplyr::mutate(gp = get_traditional_stats(season) %>% dplyr::filter(player_name == player) %>% dplyr::pull(gp)) %>%
    dplyr::select(gp, everything()) %>%
    dplyr::mutate(fga = fga/gp *6)

  shot_chart <-  .court_layout +
    ggplot2::geom_point(data = df,
                        mapping = ggplot2::aes(x = -av_loc_x,
                                               y = av_loc_y,
                                               fill = zone_efficiency,
                                               size = fga),
                        shape = 21,
                        stroke = 1,
                        color = "grey5") +
    ggtext::geom_richtext(mapping = ggplot2::aes(x = -20,
                                                 y = height*0.9,
                                                 label = get_team_logo_from_player(player,season) %>% link_to_img(width = 80)),
                          fill = NA,
                          label.color = NA,
                          label.padding = grid::unit(rep(0,4),"pt")) +
    ggtext::geom_richtext(mapping = ggplot2::aes(x = 0,
                                                 y = height*0.85,
                                                 label = get_player_picture(player,season) %>% link_to_img(width = 150)),
                          fill = NA,
                          label.color = NA,
                          label.padding = grid::unit(rep(0,4),"pt")) +
    ggplot2::labs(title = glue::glue("{player} Shooting Profile <span style='color:#CBA049;'>| {season}</span>" ),
                  subtitle = glue::glue("{get_last_name(player)} scored <span style='color:#CBA049;font-size:18pt;'>{get_subtitles_tbl(player,season) %>% dplyr::pull(pts)}</span> points per game,<br> on <span style='color:{get_subtitles_tbl(player,season) %>% dplyr::pull(color_fg2)};font-size:18pt;'>{get_subtitles_tbl(player,season) %>% dplyr::pull(fg2_pct) %>% round(digits = 3)*100}%</span> from 2 point shots and <span style='color:{get_subtitles_tbl(player,season) %>% dplyr::pull(color_fg3)};font-size:18pt;'>{get_subtitles_tbl(player,season) %>% dplyr::pull(fg3_pct)%>% round(digits = 3)*100}%</span> from threes")) +
    ggplot2::guides(fill = "none",
                    size = "none") +
    ggplot2::scale_size_identity() +
    ggplot2::scale_fill_manual(values = color_palette) # %globals% structure(TRUE, add = c("get_last_name","get_subtitles_tbl","average_2pct3pct","color_palette_tbl"))


  # Accuracy Legend
  accuracy_legend <-  tibble::rownames_to_column(data.frame(color_palette)) %>%
    dplyr::select("attribute" = "rowname","color" = "color_palette") %>%
    dplyr::mutate(x = dplyr::row_number()*0.25) %>%
    ggplot2::ggplot() +
    ggplot2::geom_point(ggplot2::aes(x = x,
                                     y = 1,
                                     fill = color),
                        shape = 21,
                        color = "grey5",
                        stroke = 1,
                        size = 10) +
    ggtext::geom_richtext(data = tibble::rownames_to_column(data.frame(color_palette)) %>%
                            dplyr::select("attribute" = "rowname","color" = "color_palette") %>%
                            dplyr::mutate(x = dplyr::row_number()*0.25) %>% dplyr::slice(1) %>%
                            dplyr::mutate(label = "Above<br>Avg."),
                          ggplot2::aes(x = x,
                                       y = 1,
                                       label = label,
                                       color = color),
                          fill = NA,
                          family = court_themes('font'),
                          label.color = NA,
                          nudge_x = -0.25) +
    ggtext::geom_richtext(data = tibble::rownames_to_column(data.frame(color_palette)) %>%
                            dplyr::select("attribute" = "rowname","color" = "color_palette") %>%
                            dplyr::mutate(x = dplyr::row_number()*0.25) %>% dplyr::slice(dplyr::n()) %>%
                            dplyr::mutate(label = "Below<br>Avg."),
                          ggplot2::aes(x = x,
                                       y = 1,
                                       label = label,
                                       color = color),
                          fill = NA,
                          family = court_themes('font'),
                          label.color = NA,
                          nudge_x = 0.25) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_color_identity() +
    ggplot2::coord_cartesian(xlim = c(-0.125,2.125)) +
    ggplot2::labs(title = "Shooting Accuracy by location") +
    theme_dark_cap() +
    ggplot2::theme(plot.margin = ggplot2::margin(t = -10, r = 5.5, l = 5.5, b = 5.5, unit = "pt"),
                   plot.title = ggplot2::element_text(hjust = .5,
                                                      margin = ggplot2::margin(t=0,b = 0)),
                   panel.grid = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   legend.background = ggplot2::element_blank())

  # Shooting Volume Legend


  volume_legend <-  tibble::tibble(label = c("High","Medium","Low"), size = c(15,9,3)) %>%
    dplyr::mutate(x = dplyr::row_number()*0.25) %>%
    ggplot2::ggplot() +
    ggplot2::geom_point(ggplot2::aes(x = x,
                                     y = 1,
                                     size = size),
                        fill = NA,
                        color = "grey5",
                        shape = 21,
                        stroke = 1) +
    ggplot2::geom_text(data = tibble::tibble(label = c("High","Medium","Low"), size = c(15,9,3)) %>%
                            dplyr::mutate(x = dplyr::row_number()*0.25) %>% dplyr::slice(1),
                          mapping = ggplot2::aes(x = x,
                                                 y = 1,
                                                 label = label),
                          color = court_themes('lines'),
                          family = court_themes('font'),
                          nudge_x = -0.25) +
    ggplot2::geom_text(data = tibble::tibble(label = c("High","Medium","Low"), size = c(15,9,3)) %>%
                            dplyr::mutate(x = dplyr::row_number()*0.25) %>% dplyr::slice(dplyr::n()),
                          mapping = ggplot2::aes(x = x,
                                                 y = 1,
                                                 label = label),
                          color = court_themes('lines'),
                          family = court_themes('font'),
                          nudge_x = 0.25) +
    ggplot2::scale_size_identity() +
    ggplot2::coord_cartesian(xlim = c(-.125,1.125)) +
    ggplot2::labs(title = "Shooting frequency by location") +
    theme_dark_cap() +
    ggplot2::theme(plot.margin = ggplot2::margin(t = -10, r = 5.5, l = 5.5, b = 5.5, unit = "pt"),
                   plot.title = ggplot2::element_text(hjust = .5,
                                             margin = ggplot2::margin(t=0,b = 0)),
                   panel.grid = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   legend.background = ggplot2::element_blank())

  legend <- accuracy_legend + volume_legend + patchwork::plot_layout(widths = c(1.2,0.8))

  plot <- (shot_chart / legend + patchwork::plot_layout(heights = c(1,.075))) +
    patchwork::plot_annotation(caption = glue::glue("Inspiration from Kirk Goldsberry's and Owen Philipp's shot charts<br>Visualisation by Henri Freixe • Sources : Nba.com")) & ggplot2::theme(plot.background = ggplot2::element_rect(fill = court_themes('court'),color = court_themes('court')),
                                                                                                                                                                                       plot.margin = ggplot2::margin(t=0,r=0,b=5.5,l=0,unit = "pt"),
                                                                                                                                                                                       plot.caption = ggtext::element_markdown(size = 9,
                                                                                                                                                                                                                               family = court_themes("font"),
                                                                                                                                                                                                                               color = court_themes("lines"),
                                                                                                                                                                                                                               margin = ggplot2::margin(t = 5,
                                                                                                                                                                                                                                                        b = 20,
                                                                                                                                                                                                                                                        r = 30)))
}
#  ggiraph::girafe(ggobj = plot,
#                  width_svg = 12,
#                  height_svg = 12,
#                  options = list(ggiraph::opts_toolbar(saveaspng = FALSE),
#                                 ggiraph::opts_sizing(rescale = FALSE)))
#}



# Awards function

get_award <- function(award = "mvp", .season = "all") {

  conv <- dplyr::tibble(prize = c("mvp","dpoy","roy","mip","smoy","fmvp","asgmvp"),
                        id = c(33,39,35,36,40,43,53))

  df <- (xml2::read_html(glue::glue("http://www.espn.com/nba/history/awards/_/id/{conv %>% dplyr::filter(prize == award) %>% dplyr::pull(id)}")) %>%
           rvest::html_nodes(css="table") %>%
           rvest::html_table(header=TRUE))[[1]]

  colnames(df) <- df %>% head(1)

  df <- df %>%
    dplyr::slice(2:dplyr::n()) %>%
    janitor::clean_names() %>%
    dplyr::select(year,player) %>%
    dplyr::mutate(post_year = stringr::str_sub(year,start = 3),
                  year = as.integer(year),
                  pre_year = year-1,
                  pre_year = as.character(pre_year),
                  season = glue::glue("{pre_year}-{post_year}")) %>%
    dplyr::select(season,player) %>%
    dplyr::mutate_all(as.character) %>%
    dplyr::mutate(season = dplyr::na_if(season,"NA-")) %>%
    tidyr::fill(season)

  if(.season =="all") {
    return(df) } else {
      if(.season %in% (df %>% dplyr::pull(season))) {
        return(df %>% dplyr::filter(season == .season) %>% dplyr::pull(player))}
      else return("No award winner, on this specific year !") }
}
# Award Stats function

get_award_stats <- function(award = "fmvp", .season = "2019-20") {

  conv <- dplyr::tibble(prize = c("mvp","dpoy","roy","mip","smoy","fmvp"),
                        translation = c("mvp","dpoy","roy","mip","smoy","finals_mvp"))

  df <- (xml2::read_html(glue::glue("https://www.basketball-reference.com/awards/{conv %>% dplyr::filter(prize == award) %>% dplyr::pull(translation)}.html")) %>%
           rvest::html_nodes(css="table") %>%
           rvest::html_table(header=TRUE))[[1]]

  colnames(df) <- df %>% head(1)

  if (award == "dpoy") {

    df <- df %>%
      dplyr::slice(2:dplyr::n()) %>%
      janitor::clean_names() %>%
      dplyr::filter(season == .season) %>%
      dplyr::select(player_name = player,team = tm, reb = trb, blk, stl) %>%
      dplyr::mutate(dplyr::across(reb:stl,as.numeric)) %>%
      dplyr::mutate(award = award)

  } else {

    df <- df %>%
      dplyr::slice(2:dplyr::n()) %>%
      janitor::clean_names() %>%
      dplyr::filter(season == .season) %>%
      dplyr::select(player_name = player, team = tm, pts, reb = trb, ast) %>%
      dplyr::mutate(dplyr::across(pts:ast,as.numeric)) %>%
      dplyr::mutate(award = award)

  }

  df <- df %>% dplyr::mutate(player_name = stringr::str_replace_all(player_name,"ć","c") %>%
                               stringr::str_replace_all("č" ,"c") %>%
                               stringr::str_replace_all("á" ,"a") %>%
                               stringr::str_replace_all("ā" ,"a") %>%
                               stringr::str_replace_all("š" ,"s") %>%
                               stringr::str_replace_all("ý" ,"y") %>%
                               stringr::str_replace_all("é" ,"e") %>%
                               stringr::str_replace_all("ū" ,"u") %>%
                               stringr::str_replace_all("ž" ,"z") %>%
                               stringr::str_replace_all("ņ" ,"n") %>%
                               stringr::str_replace_all("Š" ,"S") %>%
                               stringr::str_replace_all("İ" ,"I") %>%
                               stringr::str_replace_all("Č" ,"C") %>%
                               stringr::str_replace_all("ò" ,"o") %>%
                               stringr::str_replace_all("ģ" ,"g") %>%
                               stringr::str_replace_all("í" ,"i") %>%
                               stringr::str_replace_all("ö" ,"o") %>%
                               stringr::str_replace_all("ó" ,"o") %>%
                               stringr::str_replace_all("Ž" ,"Z") %>%
                               stringr::str_replace_all("è" ,"e") %>%
                               stringr::str_replace_all("Á" ,"A") %>%
                               stringr::str_replace_all("ê" ,"e") %>%
                               stringr::str_replace_all("Ö" ,"O") %>%
                               stringr::str_replace_all("ş" ,"s") %>%
                               stringr::str_replace_all("ı" ,"i") %>%
                               stringr::str_replace_all("ç" ,"c") %>%
                               stringr::str_replace_all("ã" ,"a") %>%
                               stringr::str_replace_all("ß" ,"ss") %>%
                               stringr::str_replace_all("\\*" ,"") %>%
                               stringr::str_replace_all("ü" ,"u") %>%
                               stringr::str_replace_all("ğ" ,"g") %>%
                               stringr::str_replace_all("í" ,"i") %>%
                               stringr::str_replace_all("ë" ,"e") %>%
                               stringr::str_replace_all("ô" ,"o") %>%
                               stringr::str_replace_all("ï" ,"i") %>%
                               stringr::str_replace_all("ú" ,"u") %>%
                               stringr::str_replace_all("ř" ,"r") %>%
                               stringr::str_replace_all("ń" ,"n") %>%
                               stringr::str_replace_all("Ó" ,"O") %>%
                               stringr::str_replace_all("ä" ,"a") %>%
                               stringr::str_replace_all("ș" ,"s"),
                             player_name = dplyr::if_else(player_name == "Brian Bowen","Brian Bowen II",
                                                          dplyr::if_else(player_name == "Charlie Brown","Charlie Brown Jr.",
                                                                         dplyr::if_else(player_name == "Danuel House","Danuel House Jr.",
                                                                                        dplyr::if_else(player_name == "Frank Mason III","Frank Mason",
                                                                                                       dplyr::if_else(player_name == "Harry Giles","Harry Giles III",
                                                                                                                      dplyr::if_else(player_name == "J.J. Redick","JJ Redick",
                                                                                                                                     dplyr::if_else(player_name == "James Ennis","James Ennis III",
                                                                                                                                                    dplyr::if_else(player_name == "Karim Many","Karim Mane",
                                                                                                                                                                   dplyr::if_else(player_name == "Kevin Knox","Kevin Knox II",
                                                                                                                                                                                  dplyr::if_else(player_name == "Lonnie Walker", "Lonnie Walker IV",
                                                                                                                                                                                                 dplyr::if_else(player_name == "Marcus Morris","Marcus Morris Sr.",
                                                                                                                                                                                                                dplyr::if_else(player_name == "Otto Porter","Otto Porter Jr.",
                                                                                                                                                                                                                               dplyr::if_else(player_name == "Robert Williams","Robert Williams III",
                                                                                                                                                                                                                                              dplyr::if_else(player_name == "Robert Woodard","Robert Woodard II",
                                                                                                                                                                                                                                                             dplyr::if_else(player_name == "Sviatoslav Mykhailiuk","Svi Mykhailiuk",
                                                                                                                                                                                                                                                                            dplyr::if_else(player_name == "Wesley Iwundu","Wes Iwundu",
                                                                                                                                                                                                                                                                                           dplyr::if_else(player_name == "Xavier Tillman Sr.","Xavier Tillman",
                                                                                                                                                                                                                                                                                                          dplyr::if_else(player_name == "Juan Hernangomez","Juancho Hernangomez",
                                                                                                                                                                                                                                                                                                                         dplyr::if_else(player_name == "B.J. Johnson","BJ Johnson",
                                                                                                                                                                                                                                                                                                                                        dplyr::if_else(player_name == "C.J. Miles","CJ Miles",
                                                                                                                                                                                                                                                                                                                                                       dplyr::if_else(player_name == "Derrick Walton","Derrick Walton Jr.",
                                                                                                                                                                                                                                                                                                                                                                      dplyr::if_else(player_name == "J.R. Smith","JR Smith",
                                                                                                                                                                                                                                                                                                                                                                                     dplyr::if_else(player_name == "Melvin Frazier","Melvin Frazier Jr.",
                                                                                                                                                                                                                                                                                                                                                                                                    dplyr::if_else(player_name == "Michael Frazier","Michael Frazier II",
                                                                                                                                                                                                                                                                                                                                                                                                                   dplyr::if_else(player_name == "Zach Norvell","Zach Norvell Jr.",
                                                                                                                                                                                                                                                                                                                                                                                                                                  dplyr::if_else(player_name == "D.J. Stephens","DJ Stephens",
                                                                                                                                                                                                                                                                                                                                                                                                                                                 dplyr::if_else(player_name == "Mitch Creek","Mitchell Creek",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                dplyr::if_else(player_name == "R.J. Hunter","RJ Hunter",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                               dplyr::if_else(player_name == "Vince Edwards","Vincent Edwards",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              dplyr::if_else(player_name == "Wade Baldwin","Wade Baldwin IV", dplyr::if_else(player_name == "Andrew White", "Andrew White III",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             dplyr::if_else(player_name == "Johnny O'Bryant","Johnny O'Bryant III",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            dplyr::if_else(player_name == "Matt Williams","Matt Williams Jr.",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           dplyr::if_else(player_name == "Vince Hunter","Vincent Hunter",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          dplyr::if_else(player_name == "A.J. Hammons","AJ Hammons",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         dplyr::if_else(player_name == "K.J. McDaniels", "KJ McDaniels",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        dplyr::if_else(player_name == "J.J. Hickson","JJ Hickson",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       dplyr::if_else(player_name == "J.J. O'Brien", "JJ O'Brien",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      dplyr::if_else(player_name == "P.J. Hairston", "PJ Hairston",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     dplyr::if_else(player_name == "A.J. Price", "AJ Price",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    dplyr::if_else(player_name == "Glen Rice Jr.","Glen Rice",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   dplyr::if_else(player_name == "Jeff Taylor", "Jeffery Taylor",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  dplyr::if_else(player_name == "Perry Jones","Perry Jones III",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 dplyr::if_else(player_name == "D.J. White","DJ White",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                dplyr::if_else(player_name =="Hamady N'Diaye","Hamady Ndiaye",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               dplyr::if_else(player_name == "Roger Mason", "Roger Mason Jr.",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              dplyr::if_else(player_name == "Vitor Luiz Faverani", "Vitor Faverani",
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             dplyr::if_else(player_name == "D.J. Mbenga","DJ Mbenga",player_name))))))))))))))))))))))))))))))))))))))))))))))))) %>%
    dplyr::mutate(player_name = dplyr::if_else(player_name == "Eugene Jeter", "Pooh Jeter",
                                               dplyr::if_else(player_name == "Sun Yue", "Sun Sun",
                                                              dplyr::if_else(player_name == "Ronald Murray", "Flip Murray",
                                                                             dplyr::if_else(player_name == "D.J. Strawberry", "DJ Strawberry",
                                                                                            dplyr::if_else(player_name == "Clarence Weatherspoon","Clar. Weatherspoon",
                                                                                                           dplyr::if_else(player_name == "Ha Seung-Jin", "Ha Ha",
                                                                                                                          dplyr::if_else(player_name == "Ibo Kutluay","Ibrahim Kutluay",
                                                                                                                                         dplyr::if_else(player_name == "Mamadou N'Diaye","Mamadou N'diaye",
                                                                                                                                                        dplyr::if_else(player_name == "Mike Sweetney","Michael Sweetney",
                                                                                                                                                                       dplyr::if_else(player_name == "Stanislav Medvedenko","Slava Medvedenko",
                                                                                                                                                                                      dplyr::if_else(player_name == "Steve Smith", "Steven Smith",
                                                                                                                                                                                                     dplyr::if_else(player_name == "Wang Zhizhi", "Wang Zhi-zhi",
                                                                                                                                                                                                                    dplyr::if_else(player_name == "Boniface N'Dong","Boniface Ndong",
                                                                                                                                                                                                                                   dplyr::if_else(player_name == "Isaac Austin","Ike Austin",
                                                                                                                                                                                                                                                  dplyr::if_else(player_name == "Isaac Fontaine","Ike Fontaine",
                                                                                                                                                                                                                                                                 dplyr::if_else(player_name == "Norm Richardson","Norman Richardson",
                                                                                                                                                                                                                                                                                dplyr::if_else(player_name == "Charles Jones", "Charles R. Jones",
                                                                                                                                                                                                                                                                                               dplyr::if_else(player_name == "Kiwane Lemorris Garris", "Kiwane Garris", player_name)))))))))))))))))))


  return(df)
}
