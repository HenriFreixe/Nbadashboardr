get_bpm <- function(season = "2020-21") {


  df <- xml2::read_html(glue::glue('https://www.basketball-reference.com/leagues/NBA_{stringr::str_sub(season,end = -4) %>% as.numeric()+1}_advanced.html#advanced_stats::27')) %>%
    rvest::html_element("table") %>%
    rvest::html_table(trim = TRUE) %>%
    janitor::clean_names() %>%
    dplyr::select(player_name = player,team_abbr = tm,obpm,dbpm,bpm,pos) %>%
    dplyr::mutate(player_name = stringr::str_replace_all(player_name,"ć","c") %>%
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
                                                                                                                                                                                                                                                                                               dplyr::if_else(player_name == "Kiwane Lemorris Garris", "Kiwane Garris", player_name))))))))))))))))))) %>%
    dplyr::group_by(player_name) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(dplyr::across(c('obpm' : 'bpm'),as.numeric)) %>%
    tidyr::drop_na() %>%
    dplyr::select(player_name,obpm,dbpm,bpm,pos) %>%
    dplyr::mutate(pos = stringr::str_split(pos,"-"),
                  pos = purrr::map(pos,1)) %>%
    tidyr::unnest(pos) %>%
    dplyr::mutate(pos = dplyr::if_else(player_name == "James Harden","SG",pos))


  return(df)
}



get_bpm_join <- function(season = "2020-21") {

  if (stringr::str_sub(season, end = 4) %>% as.integer() >= 2015) {
    get_traditional_stats(season) %>%
      dplyr::left_join(get_bpm(season), by = c("player_name")) %>%
      dplyr::mutate(total_min = min*gp) %>%
      dplyr::arrange(dplyr::desc(total_min)) %>%
      head(200) %>%
      dplyr::arrange(dplyr::desc(bpm)) %>%
      dplyr::mutate(image = glue::glue("https://ak-static.cms.nba.com/wp-content/uploads/headshots/nba/latest/260x190/{player_id}.png"))
  }else {

    df <- tidyr::tibble(player_name = c("Jerry Stackhouse", "Antawn Jamison","Ray Allen", "Richard Hamilton", "Juwan Howard","Lamar Odom","Shawn Marion","Steve Nash" , "Larry Hughes","Rashard Lewis","Jason Kidd","Morris Peterson","Jermaine O'Neal","Chauncey Billups","Chucky Atkins","Kurt Thomas","Michael Redd","Quentin Richardson","Jamaal Magloire","Corey Maggette","Jason Richardson","Andrei Kirilenko","Stephen Jackson","Al Harrington","Flip Murray","Carlos Arroyo","Raja Bell","Brian Cardinal","Carlos Boozer","Mehmet Okur","Vladimir Radmanovic","DeShawn Stevenson","Mike James","Mickael Pietrus","Michael Sweetney","Eddie House","Nate Robinson","Gerald Wallace","Brian Cook","Derek Fisher","Chris Wilcox","Sebastian Telfair","Chris Mihm","Brandon Roy","Hakim Warrick","Matt Carroll","Willie Green","Jannero Pargo","Travis Outlaw","Craig Smith","Al Thornton","Francisco Garcia","Linas Kleiza","Ronnie Brewer","Ryan Gomes","Andrew Bynum","Andray Blatche","Luke Ridnour","Jonny Flynn","Sam Young","Shannon Brown","Byron Mullens","Shaquille O'Neal","Joel Przybilla","Tyrus Thomas","Ronny Turiaf","Andris Biedrins","Chris Duhon","Brendan Haywood","Darko Milicic","Reggie Evans"),
                        espn_id = c(802,385,9,294,351,617,510,592,356,469,429,656,615,63,26,846,692,703,498,497,1018,434,378,308,1777,1055,49,130,1703,1014,1016,808,1051,2173,2175,348,2782,1026,1998,246,1731,2417,549,3027,2794,2211,2004,1821,2015,3031,3237,2755,2770,2991,2757,2748,2746,1985,3985,4020,2992,4005,614,682,3032,2789,2427,2377,1000,2171,1828))
    no_picture_players_list <- c("David Robinson", "Rik Smits", "Glenn Robinson","Glen Rice","Vin Baker", "Tracy Murray", "Maurice Taylor","Isaiah Rider","Michael Finley", "Tim Hardaway", "Kevin Willis","Tom Gugliotta","Arvydas Sabonis", "Johnny Newman", "Alan Henderson","John Starks","Jerry Stackhouse", "Ray Allen", "Matt Geiger","Clifford Robinson","Bryant Reeves", "Rex Chapman", "Hakeem Olajuwon","John Wallace","Larry Johnson", "Rony Seikaly", "Christian Laettner"  ,"Ike Austin","Lamond Murray", "Chris Gatling", "Derek Strong","Juwan Howard","Gary Trent", "Detlef Schrempf", "Tony Delk","Dana Barros","Ron Mercer", "Shandon Anderson", "Sam Mitchell","Blue Edwards","Armen Gilliam", "Jamal Mashburn", "LaPhonso Ellis","Eddie Johnson","Donyell Marshall", "John Stockton", "Sam Mack","Terry Porter","Mark Price", "Latrell Sprewell", "Vernon Maxwell","Larry Hughes","Othella Harrington", "Terry Cummings", "Lawrence Funderburke","Tony Massenburg","Antawn Jamison", "Chauncey Billups", "Clar. Weatherspoon"  ,"Jason Kidd","Rick Fox", "Kenny Anderson", "Dee Brown","Tyrone Nesby","Bobby Phills", "Cedric Ceballos", "John Amaechi","Chucky Atkins","Corey Maggette", "Lamar Odom", "Austin Croshere","Rashard Lewis","Bryon Russell", "Tariq Abdul-Wahad", "Erick Strickland","Monty Williams","Richard Hamilton", "Shawn Marion", "Steve Nash","Courtney Alexander","Marcus Fizer", "Morris Peterson", "Lindsey Hunter","Jermaine O'Neal","Antonio Davis", "Kurt Thomas", "Michael Redd","Quentin Richardson","Bobby Jackson", "Ricky Davis", "Jamaal Magloire","Malik Rose","Lee Nailon", "Stromile Swift", "Lucious Harris","Jason Richardson","Andrei Kirilenko", "Aaron McKie","Jeff McInnis","Matt Harpring","Dajuan Wagner", "Desmond Mason", "Stephen Jackson","Rodney White","Al Harrington", "Predrag Drobnjak", "Alvin Williams","Flip Murray","Carlos Arroyo", "Raja Bell", "Brian Cardinal","Carlos Boozer","Juan Dixon", "Mehmet Okur", "Speedy Claxton","Vladimir Radmanovic","Slava Medvedenko", "DeShawn Stevenson", "Mike James","Mickael Pietrus","Dan Dickau", "Michael Sweetney", "Eddie House","Primoz Brezec","Nate Robinson", "Gerald Wallace", "Brian Cook","Derek Fisher","Chris Wilcox", "Sebastian Telfair", "Chris Mihm","Brandon Roy","Hakim Warrick", "Matt Carroll", "Willie Green","Jannero Pargo","Travis Outlaw", "Rashad McCants", "Craig Smith","Al Thornton","Francisco Garcia", "Linas Kleiza", "Ronnie Brewer","Ryan Gomes","Andrew Bynum", "Andray Blatche", "Luke Ridnour","Jonny Flynn","Sam Young", "Shannon Brown", "Byron Mullens","Mark Jackson","Eric Snow")

    get_traditional_stats(season) %>%
      dplyr::left_join(get_bpm(season), by = c("player_name")) %>%
      dplyr::mutate(total_min = min*gp) %>%
      dplyr::arrange(dplyr::desc(total_min)) %>%
      head(200) %>%
      dplyr::arrange(dplyr::desc(bpm)) %>%
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
                                                          as.character(pre_image))))
  }
}



plot_player_ranking_interactive <- function(season = "2020-21", variable = "bpm"){

  df <- get_bpm_join(season) %>%
    dplyr::arrange(dplyr::desc(.data[[variable]])) %>%
    head(10) %>%
    dplyr::left_join(get_team_traditional(season) %>% dplyr::select(team_id,team_name), by = c("team_id")) %>%
    dplyr::mutate(label = link_to_img(image, width = 95, alt = player_name),
           tooltip_label = glue::glue("<div style = 'box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2);max-width: 150px;margin: auto;padding-bottom:5px;text-align: center;font-family: {court_themes('court')};background-color:#1A1A1A'><img src= {image} style = 'width : 100%'><span style = 'display: block; margin-top: 0.67em; margin-bottom: 0.67em; margin-left: 0; margin-right: 0; font-weight: bold;color:#BFBFBF;font-size:14px'>{player_name}</span><span style = 'color:grey;font-size:10px;display: block; margin-top: 1em; margin-bottom: 1em; margin-left: 0; margin-right: 0;'>{team_name}</span><span style = 'color:#BFBFBF;font-size:10px;display: block; margin-top: 1em; margin-bottom: 1em; margin-left: 0; margin-right: 0;'><span style = 'font-weight :bold'>{round(pts,digits =1)}</span> pts per game<br><span style = 'font-weight :bold'>{round(ast,digits =1)}</span> assists per game<br><span style = 'font-weight :bold'>{round(reb,digits =1)}</span> rebounds per game<br><span style = 'font-weight :bold'>{round(stl,digits =1)}</span> steals per game<br><span style = 'font-weight :bold'>{round(blk,digits =1)}</span> blocks per game</span></div>")) %>%
    dplyr::mutate(label = forcats::fct_inorder(label),
                  color = dplyr::if_else(dplyr::row_number() == 1,"#CBA049","grey10"),
                  color2 = dplyr::if_else(dplyr::row_number() == 1,"#CBA049",court_themes('lines'))) #%>%


  if (variable == "bpm") {
    df <- df %>% dplyr::mutate(max_bpm = max(bpm),
                               max_obpm = max(obpm),
                               max_dbpm = max(dbpm),
                               min_bpm = min(bpm),
                               min_obpm = min(obpm),
                               min_dbpm = min(dbpm))
  } else {
    if (variable == "reb") {
      df <- df %>%
        dplyr::mutate(dbpm = dreb,
                      obpm = oreb)
    } else {
      df <- df %>%
        dplyr::mutate(dbpm = 0,
                      obpm =0)
    }

  }


  if (variable == "bpm") {

    label_size <- 12
    point_size <- 7.5
    point_stroke <- 1
    line_color <- "grey10"
    label_all_time_pos <- 10
    label_mvp_pos <- 8

    font_size_superscript <- '16pt'

    pre_title <- "Most Valuable Players in the League (BPM)"

    subtitle <- glue::glue("Box Plus Minus is an all-in-one metric that estimates a player's overall contribution<br>It is made of an <span style =' color:#de425b;font-size:18pt'> Offensive Component </span> and a <span style =' color:#2E4EB8;font-size:18pt'>Defensive Component</span><br><span style =' color:#CBA049;font-size:18pt'>{df %>% head(1) %>% dplyr::pull(player_name)}</span> led the league in BPM with a <span style =' color:#CBA049;font-size:18pt'>{df %>% head(1) %>% dplyr::pull(.data[[variable]])}</span> mark")

    y_label <- "Box Plus Minus (BPM)"

    footnote <- "(1) As per Basketball Reference's estimates<br>"

    label_size2 <- 0

    nudge_label <- 0


    y_up_bk <- if (max(df$max_bpm %>% head(1),df$max_obpm %>% head(1),df$max_dbpm %>% head(1)) > 10) {
      ceiling((max(df$max_bpm %>% head(1),df$max_obpm %>% head(1),df$max_dbpm %>% head(1))/2))*2
    }else {
      10
    }

    y_up_lim <- if (max(df$max_bpm %>% head(1),df$max_obpm %>% head(1),df$max_dbpm %>% head(1)) <= 10) {
      11
    } else {
      ceiling((max(df$max_bpm %>% head(1),df$max_obpm %>% head(1),df$max_dbpm %>% head(1)))/2)*2
    }
    y_breaks <- seq(0,y_up_bk,2)
    y_limits <- c(NA,y_up_lim)

  } else {
    if (variable == "pts") {

      label_size <- 0
      point_size <- 0
      point_stroke <- 0
      line_color <- "transparent"
      label_all_time_pos <- 0
      label_mvp_pos <- 0

      font_size_superscript <- '0pt'

      pre_title <- "Most prolific <span style =' color:#de425b'>Scorers</span> in the League (Points per Game)"

      subtitle <- glue::glue("<span style =' color:#CBA049;font-size:18pt'>{df %>% head(1) %>% dplyr::pull(player_name)}</span> led the league in scoring with <span style =' color:#CBA049;font-size:18pt'>{df %>% head(1) %>% dplyr::pull(.data[[variable]])}</span> pts per game")

      y_label <- "Points per Game"

      footnote <- ""

      label_size2 <- 6

      nudge_label <- .5

      y_breaks <- seq(0,ceiling((df %>% head(1) %>% dplyr::pull(pts))/5)*5,5)
      y_limits <- c(NA,NA)

    } else {
      if (variable == "ast") {

        label_size <- 0
        point_size <- 0
        point_stroke <- 0
        line_color <- "transparent"
        label_all_time_pos <- 0
        label_mvp_pos <- 0

        font_size_superscript <- '0pt'

        pre_title <- "Most prolific <span style ='color:#de425b'>Passers</span> in the League (Assists per Game)"

        subtitle <- glue::glue("<span style =' color:#CBA049;font-size:18pt'>{df %>% head(1) %>% dplyr::pull(player_name)}</span> led the league in passing with <span style =' color:#CBA049;font-size:18pt'>{df %>% head(1) %>% dplyr::pull(.data[[variable]])}</span> assists per game")

        y_label <- "Assists per Game"

        footnote <- ""

        label_size2 <- 6

        nudge_label <- .2

        y_breaks <- seq(0,ceiling((df %>% head(1) %>% dplyr::pull(ast))/2)*2,2)
        y_limits <- c(NA,NA)


      }else {
        if (variable == "reb") {

          label_size <- 0
          point_size <- 7.5
          point_stroke <- 1
          line_color <- "transparent"
          label_all_time_pos <- 0
          label_mvp_pos <- 0

          font_size_superscript <- '0pt'

          pre_title <- "Most prolific Rebounders in the League (Rebounds per Game)"

          subtitle <- glue::glue("Circles represent <span style =' color:#de425b;font-size:18pt'> Offensive Rebounds </span> and <span style =' color:#2E4EB8;font-size:18pt'>Defensive Rebounds</span><br><span style =' color:#CBA049;font-size:18pt'>{df %>% head(1) %>% dplyr::pull(player_name)}</span> led the league in rebounding with <span style =' color:#CBA049;font-size:18pt'>{df %>% head(1) %>% dplyr::pull(.data[[variable]])}</span> rebounds per game")

          y_label <- "Rebounds per Game"

          footnote <- ""

          label_size2 <- 6

          nudge_label <- .2

          y_breaks <- seq(0,ceiling((df %>% head(1) %>% dplyr::pull(reb))/2)*2,2)
          y_limits <- c(NA,NA)

        }else {
          if (variable == "stl") {

            label_size <- 0
            point_size <- 0
            point_stroke <- 0
            line_color <- "transparent"
            label_all_time_pos <- 0
            label_mvp_pos <- 0

            font_size_superscript <- '0pt'

            pre_title <- "Most prolific <span style ='color:#2E4EB8'>Intercepters</span> in the League (Steals per Game)"

            subtitle <- glue::glue("<span style =' color:#CBA049;font-size:18pt'>{df %>% head(1) %>% dplyr::pull(player_name)}</span> led the league in steals with <span style =' color:#CBA049;font-size:18pt'>{df %>% head(1) %>% dplyr::pull(.data[[variable]])}</span> steals per game")

            y_label <- "Steals per Game"

            footnote <- ""

            label_size2 <- 6

            nudge_label <- .05

            y_breaks <- seq(0,ceiling((df %>% head(1) %>% dplyr::pull(stl))/.5)*.5,.5)
            y_limits <- c(NA,NA)

          }else {
            if (variable =="blk") {

              label_size <- 0
              point_size <- 0
              point_stroke <- 0
              line_color <- "transparent"
              label_all_time_pos <- 0
              label_mvp_pos <- 0

              font_size_superscript <- '0pt'

              pre_title <- "Most prolific <span style ='color:#2E4EB8'>Rim Protectors</span> in the League (Blocks per Game)"

              subtitle <- glue::glue("<span style =' color:#CBA049;font-size:18pt'>{df %>% head(1) %>% dplyr::pull(player_name)}</span> led the league in blocked shots with <span style =' color:#CBA049;font-size:18pt'>{df %>% head(1) %>% dplyr::pull(.data[[variable]])}</span> blocks per game")

              y_label <- "Blocks per Game"

              footnote <- ""

              label_size2 <- 6

              nudge_label <- .05

              y_breaks <- seq(0,ceiling((df %>% head(1) %>% dplyr::pull(blk))/.5)*.5,.5)
              y_limits <- c(NA,NA)
            }
          }
        }


      }}}


  plot <- df %>%
    ggplot2::ggplot() +
    ggplot2::geom_hline(yintercept = label_all_time_pos, color = line_color, linetype = 3) +
    ggtext::geom_richtext(x = 5.5,
                          y = label_all_time_pos,
                          family = court_themes('font'),
                          label = glue::glue("All-Time Level Season<sup style = font-size:{font_size_superscript}>(1)</sup>"),
                          size = label_size,
                          alpha = .75,
                          color = "grey15",
                          vjust = 0,
                          fill = NA,
                          label.color = NA,
                          label.padding = grid::unit(rep(0,4),"pt")) +
    ggplot2::geom_hline(yintercept = label_mvp_pos , color = line_color, linetype = 3) +
    ggtext::geom_richtext(x = 5.5,
                          y = label_mvp_pos,
                          family = court_themes('font'),
                          label = glue::glue("MVP Level Season<sup style = font-size:{font_size_superscript}>(1)</sup>"),
                          size = label_size,
                          alpha = .75,
                          color = "grey15",
                          vjust = 0,
                          fill = NA,
                          label.color = NA,
                          label.padding = grid::unit(rep(0,4),"pt")) +
    ggiraph::geom_col_interactive(ggplot2::aes(x = label,
                                               y = .data[[variable]],
                                               color = color,
                                               tooltip = tooltip_label,
                                               data_id = player_name
    ),
    width = .5,
    alpha = .35,
    linetype = 2) +
    ggplot2::geom_hline(yintercept = 0, color = 'grey5', linetype = 1, size = 1) +
    ggplot2::geom_point(ggplot2::aes(x = label,
                                     y = dbpm),
                        shape = 21,
                        size = point_size,
                        color = "grey10",
                        stroke = point_stroke,
                        fill = ggplot2::alpha('#223c8f',1)) +
    ggplot2::geom_point(ggplot2::aes(x = label,
                                     y = obpm),
                        shape = 21,
                        color = "grey10",
                        size = point_size,
                        stroke = point_stroke,
                        fill = ggplot2::alpha('#de425b',1)) +
    ggtext::geom_richtext(ggplot2::aes(x = label,
                                       y = .data[[variable]],
                                       color = color2,
                                       label = .data[[variable]]),
                          nudge_y = nudge_label,
                          size = label_size2,
                          family = court_themes('font'),
                          vjust = 0,
                          fill = NA,
                          label.color = NA,
                          label.padding = grid::unit(rep(0,4),"pt")) +
    ggplot2::labs(title = glue::glue("{pre_title} <span style = 'color:#CBA049'>| {season}</span>"),
                  subtitle = subtitle,
                  x = "",
                  y = y_label,
                  caption = glue::glue("{footnote}Visualisation by Henri Freixe • Sources : Nba.com, Basketball-reference.com")) +
    ggplot2::scale_color_identity() +
    ggplot2::scale_y_continuous(breaks = y_breaks,
                                limits = y_limits) +
    ggplot2::theme_minimal(base_size = 22) +
    theme_dark_cap() +
    ggplot2::theme(plot.title = ggtext::element_markdown(hjust = .5,
                                                         margin = ggplot2::margin(t = 20)),
                   plot.subtitle = ggtext::element_markdown(hjust=.5,
                                                            size = 16,
                                                            margin = ggplot2::margin(t = 15,
                                                                                     b = 15),
                                                            lineheight = 1),
                   plot.caption = ggtext::element_markdown(size = 9,
                                                           margin = ggplot2::margin(t = 20,
                                                                                    b = 10)),
                   axis.text.y = ggplot2::element_text(size = 12, color = court_themes('lines')),
                   axis.text.x = ggtext::element_markdown(size = 12,color = court_themes('lines'),
                                                          margin = ggplot2::margin(t = -20)),
                   axis.title.x = ggplot2::element_blank(),
                   axis.title.y = ggtext::element_markdown(size = 12,color = court_themes('lines')),
                   panel.grid = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(),
                   legend.background = ggplot2::element_blank())

}
#  ggiraph::girafe(ggobj = plot,
#                  width_svg = 16,
#                  height_svg = 12,
#                  options = list(ggiraph::opts_tooltip(css="background-color:transparent"),
#                                 ggiraph::opts_hover(css = "fill:red;"),
#                                 ggiraph::opts_toolbar(saveaspng = FALSE),
#                                 ggiraph::opts_sizing(rescale = FALSE)))
#
#}


