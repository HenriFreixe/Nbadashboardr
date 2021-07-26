get_all_stars <- function(season = "2020-21") {
  pre_season_reformat <- stringr::str_sub(season,end = 4) %>% as.integer()

  season_reformat <- glue::glue("{pre_season_reformat+1}")

  pre_df <- xml2::read_html(glue::glue("https://basketball.realgm.com/nba/allstar/game/rosters/{season_reformat}")) %>%
    rvest::html_nodes(css = "table") %>%
    rvest::html_table(trim = TRUE)

  len <- length(pre_df)

  df1 <- pre_df[[len-1]]
  df2 <- pre_df[[len]]

  df <- dplyr::bind_rows(df1,df2) %>%
    janitor::clean_names() %>%
    dplyr::select(player_name = player,team_name = team) %>%
    dplyr::mutate(former_team_name = team_name,
                  team_name = dplyr::if_else(team_name == "New Orleans Hornets","New Orleans Pelicans",team_name),
                  team_name = dplyr::if_else(team_name == "Philadelphia Sixers","Philadelphia 76ers",team_name),
                  team_name = dplyr::if_else(team_name == "New Jersey Nets","Brooklyn Nets",team_name),
                  team_name = dplyr::if_else(team_name == "Charlotte Bobcats","Charlotte Hornets",team_name),
                  team_name = dplyr::if_else(team_name == "Seattle SuperSonics","Oklahoma City Thunder",team_name)) %>%
    dplyr::mutate(all_star = 1)

  return(df)

}


get_playoff_teams <- function(season = "2020-21") {

  if (season == "2021-22") {
    ""
  }else {
    pre_season_reformat <- stringr::str_sub(season,end = 4) %>% as.integer()

    season_reformat <- glue::glue("{pre_season_reformat+1}")


    df <- (xml2::read_html(glue::glue("https://www.basketball-reference.com/playoffs/NBA_{season_reformat}.html")) %>%
             rvest::html_nodes(css = "table") %>%
             rvest::html_table(trim = TRUE))[[17]] %>%
      janitor::clean_names() %>%
      dplyr::pull(team)

    return(df)
  }


}



get_prev_standings <- function(season = "2020-21", conf = "both") {

  prev_season_begin <- stringr::str_sub(season,end = 4) %>% as.integer()-1
  prev_season_end <- stringr::str_sub(glue::glue('20{stringr::str_sub(season,start = 6)}') %>% as.integer()-1,start = 3)


  df <- get_team_standings(season = as.character(glue::glue("{prev_season_begin}-{prev_season_end}")))

  if (conf == "both") {
    df %>%
      dplyr::mutate(rank = rank(dplyr::desc(win_pct), ties.method = "first")) %>%
      dplyr::select(team_name, previous_rank = rank)
  } else {
    df %>%
      dplyr::filter(conference == conf) %>%
      dplyr::mutate(rank = rank(dplyr::desc(win_pct), ties.method = "first")) %>%
      dplyr::select(team_name, previous_rank = rank)
  }

}



stars <- function(x){
  if (x == 0) {
    ""
  } else {
    if (x==1) {
      "<span style = 'color:#404040'>&#9733;</span>"
    }else {
      if (x==2) {
        "<span style = 'color:#404040'>&#9733;&#9733;</span>"
      }else {
        if (x == 3) {
          "<span style = 'color:#404040'>&#9733;&#9733;&#9733;</span>"
        }else {
          if(x == 4){
            "<span style = 'color:#404040'>&#9733;&#9733;&#9733;&#9733;</span>"
          }else {
            if (x == 5) {
              "<span style = 'color:#404040'>&#9733;&#9733;&#9733;&#9733;&#9733;</span>"
            }
          }
        }
      }
    }}


}


plot_teams_table <- function(season = "2019-20", conf = "both") {

  lead_player <- get_traditional_stats(season) %>%
    dplyr::mutate(total_min = min*gp) %>%
    dplyr::arrange(dplyr::desc(total_min)) %>%
    head(200) %>%
    dplyr::group_by(team_id) %>%
    dplyr::arrange(dplyr::desc(pts),.by_group = TRUE) %>%
    dplyr::slice_head(n=1) %>%
    dplyr::ungroup() %>%
    dplyr::select(player_id,player_name,team_id,pts,ast,reb,blk,stl) %>%
    dplyr::mutate(image = glue::glue("https://ak-static.cms.nba.com/wp-content/uploads/headshots/nba/latest/260x190/{player_id}.png")) %>%
    dplyr::select(player_name,team_id,image,pts)

  previous_ranking <- get_prev_standings(season, conf)

  playoffs <- get_playoff_teams(season)
  champion <- get_champion(.season = season)
  conf_df <- get_team_standings(season) %>%
    dplyr::select(team_name,conference)


  if (conf == "West") {
    conference_color <- "#de425b"
  }

  if (conf == "East") {
    conference_color <- "#2E4EB8"

  }


  if (conf == "both") {
    future::plan(future::multisession(workers = future::availableCores()))

    table_data <- get_team_advanced_selections(season) %>%
      dplyr::left_join(conf_df, by = c("team_name")) %>%
      dplyr::left_join(previous_ranking, by = c("team_name")) %>%
      dplyr::mutate(playoffs = dplyr::if_else(team_name %in% playoffs,glue::glue("<span style = 'color:#404040'>✓</span>"),""),
                    champion = dplyr::if_else(team_name %in% champion,glue::glue("<span style = 'color:#404040'>{fontawesome::fa('trophy')}</span>"),"")) %>%
      dplyr::left_join(lead_player, by = c("team_id")) %>%
      dplyr::mutate(team_name = glue::glue("<b style = 'font-size:14px'>{get_last_name(team_name)}</b> <span style = 'font-size:9px;color:grey'>{w}-{l}</span>"),
                    rank = rank(dplyr::desc(w_pct), ties.method = 'first' ),
                    selections = furrr::future_map(.x = selections,.f = stars),
                    player_name = glue::glue("<div style = 'line-height:100%'><b style = 'font-size:14px'>{get_last_name(player_name)}</b></div>  <div style = 'line-height:100%'><span style = 'font-size:12px;color:grey'>{pts} pts per game</span></div> "),
                    rank_change = -(rank-previous_rank),
                    ## change difference to previous year
                    rank_change = dplyr::case_when(
                      rank_change > 0 ~ glue::glue("<span style='color:#02af74;'>&uarr;+{rank_change}</span>"),
                      rank_change < 0 ~ glue::glue("<span style='color:#fc7042;'>&darr;{rank_change}</span>"),
                      rank_change == 0 ~ glue::glue("<span style='color:#b3b3b3;'>&hybull;</span>"))) %>%
      dplyr::arrange(rank) %>%
      dplyr::select(rank, rank_change,logo,team_name,conference, w_pct,off_rating,def_rating,net_rating,selections,image,player_name,playoffs,champion)

    title <- glue::glue("**Season Overview <span style = 'color:#CBA049'>| {season}</span>**")



  } else {
    future::plan(future::multisession(workers = future::availableCores()))

    table_data <- get_team_advanced_selections(season) %>%
      dplyr::left_join(conf_df, by = c("team_name")) %>%
      dplyr::left_join(previous_ranking, by = c("team_name")) %>%
      dplyr::filter(conference == conf) %>%
      dplyr::mutate(playoffs = dplyr::if_else(team_name %in% playoffs,glue::glue("<span style = 'color: #404040'>✓</span>"),""),
                    champion = dplyr::if_else(team_name %in% champion,glue::glue("<span style = 'color:#404040'>{fontawesome::fa('trophy')}</span>"),"")) %>%
      dplyr::left_join(lead_player, by = c("team_id")) %>%
      dplyr::mutate(team_name = glue::glue("<div style = 'line-height:100%'><b style = 'font-size:14px'>{get_last_name(team_name)}</b> <span style = 'font-size:12px'>{remove_last_name(team_name)}</span></div> <div style = 'line-height:100%'><span style = 'font-size:12px;color:grey'>{w}-{l}</span></div>"),
                    rank = rank(dplyr::desc(w_pct), ties.method = 'first' ),
                    selections = furrr::future_map(.x = selections,.f = stars),
                    player_name = glue::glue("<div style = 'line-height:100%'><b style = 'font-size:14px'>{get_last_name(player_name)}</b></div>  <div style = 'line-height:100%'><span style = 'font-size:12px;color:grey'>{pts} pts per game</span></div> "),
                    rank_change = -(rank-previous_rank),
                    ## change difference to previous year
                    rank_change = dplyr::case_when(
                      rank_change > 0 ~ glue::glue("<span style='color:#02af74;'>&uarr;+{rank_change}</span>"),
                      rank_change < 0 ~ glue::glue("<span style='color:#fc7042;'>&darr;{rank_change}</span>"),
                      rank_change == 0 ~ glue::glue("<span style='color:#b3b3b3;'>&hybull;</span>"))) %>%
      dplyr::arrange(rank) %>%
      dplyr::select(rank, rank_change,logo,team_name,conference,w_pct,off_rating,def_rating,net_rating,selections,image,player_name,playoffs,champion)

    title <- glue::glue("**Season Overview <span style = 'color: {conference_color}'>{conf}ern Conference</span> <span style = 'color:#CBA049'>| {season}</span>**")

  }



  table <- table_data %>%
    gt::gt() %>%
    ## COLORS --------------------------------------------------------------------
  gt::data_color(columns = gt::vars(w_pct,off_rating),
                 colors = scales::col_numeric(
                   palette = rev(c("#35b0ab",
                                   "#59bbac",
                                   "#76c7ad",
                                   "#92d1b1",
                                   "#abdcb6",
                                   "#c4e6bd",
                                   "#dcf1c6",
                                   "#f2fbd2")),
                   domain = NULL
                 )) %>%
    gt::data_color(columns = gt::vars(def_rating),
                   colors = scales::col_numeric(c("#35b0ab",
                                                  "#59bbac",
                                                  "#76c7ad",
                                                  "#92d1b1",
                                                  "#abdcb6",
                                                  "#c4e6bd",
                                                  "#dcf1c6",
                                                  "#f2fbd2"),
                                                domain = NULL
                   )) %>%
    gt::data_color(columns = gt::vars(playoffs),
                   colors = scales::col_factor(
                     palette = c(court_themes('bg_table_markdown'),"#fad684"),
                     domain = NULL
                   )) %>%
    gt::data_color(columns = gt::vars(conference),
                   apply_to = "text",
                   colors = scales::col_factor(
                     palette = c(court_themes('bg_table_markdown'),"#de425b"),
                     domain = NULL
                   )) %>%
    gt::data_color(columns = gt::vars(champion),
                   colors = scales::col_factor(
                     palette = c(court_themes('bg_table_markdown'),"#fad684"),
                     domain = c("",glue::glue("<span style = 'color:#404040'>{fontawesome::fa('trophy')}</span>"))
                   )) %>%
    ## IMAGES --------------------------------------------------------------------
  gt::text_transform(locations = gt::cells_body(gt::vars(logo,image)),
                     fn = function(x) {
                       gt::web_image(url = x, height = gt::px(35))
                     }) %>%
    ## OTHERS --------------------------------------------------------------------
  gt::fmt_markdown(columns = c("team_name","selections","player_name","playoffs","champion","rank_change")) %>%
    gt::fmt_percent(columns = c("w_pct"), decimals = 1) %>%
    gt::fmt(columns = c("net_rating"),
            fns = function(x){
              dplyr::if_else(x>0, glue::glue("<span style = 'color:#02af74'>+{x}</span>"),
                      dplyr::if_else(x ==0, glue::glue("<span style = 'color:#b3b3b3'>=</span>"),glue::glue("<span style = 'color:#fc7042'>{x}</span>")))
            }) %>%
    gt::opt_table_font(font = list(
      gt::google_font("Manrope"),
      gt::google_font("Kiwi Maru"),
      gt::google_font("Montserrat"),
      gt::google_font("Lato"),
      gt::google_font("Roboto Condensed")
    ))%>%
    ## Fonts and text formatting
    gt::tab_style(
      style = list(gt::cell_text(align = "center",weight = "bold", v_align = "bottom",size = "small")),
      locations = list(gt::cells_column_labels(gt::everything()))
    ) %>%
    gt::tab_style(
      style = list(gt::cell_text(align = "left", v_align = "bottom")),
      locations = list(gt::cells_column_labels(c("team_name","player_name")))
    ) %>%
    gt::tab_style(
      style = list(gt::cell_text(align = "left")),
      locations = list(gt::cells_body(c("team_name","player_name")))) %>%
    gt::tab_style(
      style = list(gt::cell_text(align = "center", size = "small")),
      locations = list(gt::cells_body(c("selections")))) %>%
    gt::tab_style(
      style = list(gt::cell_text(align = "center", size = "small", font = "Kiwi Maru")),
      locations = list(gt::cells_body(c("conference","net_rating","w_pct","off_rating","def_rating")))) %>%
    gt::tab_style(
      style = list(gt::cell_text(align = "center", size = "small", font = "Kiwi Maru", weight = "bold")),
      locations = list(gt::cells_body(c("net_rating")))) %>%
    gt::tab_style(
      style = list(gt::cell_text(align = "center", color = "grey", size = "x-small")),
      locations = list(gt::cells_body(c("rank")))) %>%
    gt::tab_style(
      style = list(gt::cell_text(align = "center", font = "Manrope", color = "#404040")),
      locations = list(gt::cells_title(groups = "title"))) %>%
    gt::tab_style(
      style = list(gt::cell_text(align = "center", font = "Manrope", color = "#404040")),
      locations = list(gt::cells_title(groups = "subtitle"))) %>%
    gt::tab_style(
      style = list(gt::cell_text(font = "Manrope", size = "x-small", align = "center")),
      location = list(gt::cells_body(columns = gt::vars(rank_change)))
    ) %>%
    gt::tab_style(
      style = list(
        gt::cell_fill(color = "#fad684")
      ),
      locations = gt::cells_body(
        columns = gt::vars(selections),
        rows = selections != ""
      )) %>%
    ## Tab Spanners
    gt::tab_spanner(label = gt::md("**TEAM RESULTS**"),
                    columns = gt::vars(w_pct,off_rating,def_rating,net_rating)) %>%
    gt::tab_spanner(label = gt::md("**ROSTER STANDOUTS**"),
                    columns = gt::vars(selections,image,player_name)) %>%
    gt::tab_spanner(label = gt::md("**POSTSEASON**"),
                    columns = gt::vars(playoffs,champion)) %>%
    ## Borders formatting
    gt::tab_style(
      style = list(
        gt::cell_borders(
          sides = "left",
          color = "#0D0D0D",
          weight = gt::px(2)
        )
      ),
      locations = list(
        gt::cells_body(
          columns = gt::vars(w_pct,selections,playoffs)
        )
      )
    ) %>%
    gt::tab_style(
      style = list(
        gt::cell_borders(
          sides = "bottom",
          color = "#0D0D0D",
          weight = gt::px(2)
        )
      ),
      locations = list(
        gt::cells_column_labels(
          columns = gt::everything()
        )
      )
    ) %>%
    ## Column Labels
    gt::cols_label(rank = "",
                   rank_change = "",
                   logo = "",
                   team_name = gt::html("<div style = 'line-height:100%'>Team</div><div style = 'line-height:100%'><span style='font-size:8pt;color:grey;font-weight:400;'>Record</span></div>"),
                   conference = "Conference",
                   w_pct = gt::html("Win Percentage"),
                   off_rating = "Offensive Efficiency",
                   def_rating = "Defensive Efficiency",
                   net_rating = gt::html("Net Rating"),
                   selections = "All-Stars",
                   image = "",
                   player_name = "Leading Scorer",
                   playoffs = "Playoffs",
                   champion = "Champion") %>%
    ## Column Widths
    gt::cols_width(
      gt::vars(rank)~gt::px(30),
      gt::vars(rank_change)~gt::px(30),
      gt::vars(logo)~gt::px(50),
      gt::vars(team_name)~gt::px(200),
      gt::vars(conference)~gt::px(80),
      gt::vars(w_pct)~gt::px(80),
      gt::vars(off_rating)~gt::px(80),
      gt::vars(def_rating)~gt::px(80),
      gt::vars(net_rating)~gt::px(75),
      gt::vars(selections)~gt::px(75),
      gt::vars(image)~gt::px(65),
      gt::vars(player_name)~gt::px(175),
      gt::vars(playoffs)~gt::px(75),
      gt::vars(champion)~gt::px(75)
    ) %>%
    gt::tab_footnote(
      footnote = gt::md(glue::glue("Efficiency corresponds to the number of points scored / allowed every 100 possessions")),
      locations = gt::cells_column_labels(columns = gt::vars(off_rating,def_rating))
    ) %>%
    gt::tab_footnote(
      footnote = gt::md(glue::glue("Number of players to participate in the {season} NBA All-Star Game")),
      locations = gt::cells_column_labels(columns = gt::vars(selections))
    ) %>%

    gt::tab_options(table.border.top.color = "transparent",
                    table.border.bottom.color = "transparent",
                    table.border.bottom.width = gt::px(2),
                    column_labels.border.top.width = gt::px(2),
                    column_labels.border.bottom.width = gt::px(2),
                    column_labels.border.bottom.color = "#0D0D0D",
                    heading.title.font.size = 18,
                    heading.subtitle.font.size = 14,
                    heading.title.font.weight = "bold",
                    heading.subtitle.font.weight = "bold",
                    source_notes.font.size = 10,
                    footnotes.font.size = 10,
                    column_labels.border.top.color = "#0D0D0D",
                    data_row.padding = gt::px(3),
                    heading.border.bottom.color = "#a1a1a1",
                    table_body.border.bottom.color = "#a1a1a1",
                    table_body.hlines.color = "#a1a1a1",
                    table.background.color = court_themes('bg_table_markdown')) %>%
    gt::tab_source_note(gt::md("**<span style = 'color:#404040'>Visualisation by Henri Freixe • Sources : Nba.com, Basketball-reference.com, Basketball.realgm.com,Espn.com</span>**")) %>%
    gt::tab_header(
      title = gt::md(title),
      subtitle = gt::md("**Top 8 teams in each Conference to qualify for the playoffs**")
    )

  if (season == "2020-21") {
    table <- table %>% gt::tab_footnote(
      footnote = gt::md(glue::glue("{season} NBA Season has yet to crown an NBA Champion")),
      locations = cgt::ells_column_labels(columns = gt::vars(playoffs,champion)))

  }else {
    table <- table
  }
  if (conf == "both") {
    table <- table %>%
      gt::cols_hide(columns = gt::vars(rank_change)) %>%
      gt::cols_width(gt::vars(team_name)~gt::px(150))
  }else {
    table <- table %>% gt::cols_hide(columns = gt::vars(conference))
  }

  return(table)
}
