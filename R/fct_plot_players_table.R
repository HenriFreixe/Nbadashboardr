get_position <- function(x) {
  dplyr::case_when(
    x == "PG" ~ "Point Guard",
    x == "SG" ~ "Shooting Guard",
    x == "SF" ~ "Small Forward",
    x == "PF" ~ "Power Forward",
    x == "C"  ~ "Center"
  )
}
variable_name <- function(x){
  dplyr::case_when(
    x == "bpm" ~ "Box Plus-Minus",
    x == "obpm" ~ "Offensive Box Plus-Minus",
    x == "dbpm" ~ "Defensive Box Plus-Minus",
    x == "pts" ~ "Points per Game",
    x == "ast"  ~ "Assists per Game",
    x == "reb"  ~ "Rebounds per Game",
    x == "blk"  ~ "Blocks per Game",
    x == "stl"  ~ "Steals per Game",
    x == "fg3_pct"  ~ "3 Point Shooting",
    x == "ft_pct"  ~ "Free-Throw Shooting",
  )


}

plot_players_table <- function(season = "2020-21", variable = "bpm") {

  playoffs <- get_playoff_teams(season)
  champion <- get_champion(season)
  asgdf <- get_all_stars(season) %>% dplyr::select(player_name,all_star)
  team_join <- team_join()

  table_data <- get_bpm_join(season) %>%
    dplyr::mutate(fg2m = fgm - fg3m,
                  fg2a = fga - fg3a,
                  fg2_pct = fg2m / fg2a) %>%
    dplyr::select(image,player_name,pos,pts,ast,reb,blk,stl,bpm,obpm,dbpm,team_id,fg_pct,fg2_pct,fg3m,fg3_pct,ft_pct) %>%
    dplyr::left_join(asgdf, by = c("player_name")) %>%
    dplyr::mutate(all_star = tidyr::replace_na(all_star,0),
                  fg3_pct = dplyr::if_else(fg3m <= .2,0,fg3_pct),
                  fg3_pct = dplyr::na_if(fg3_pct,0)) %>%
    dplyr::left_join(team_join, by = c("team_id")) %>%
    dplyr::mutate(playoffs = dplyr::if_else(team_name %in% playoffs,glue::glue("<span style = 'color:#404040'>✓</span>"),""),
                  champion = dplyr::if_else(team_name %in% champion,glue::glue("<span style = 'color:#404040'>{fontawesome::fa('trophy')}</span>"),""),
                  mvp = dplyr::if_else(player_name == get_award("mvp",season),glue::glue("<span style = 'color:#404040'>{fontawesome::fa('medal')}</span>"),""),
                  dpoy = dplyr::if_else(player_name == get_award("dpoy",season),glue::glue("<span style = 'color:#404040'>{fontawesome::fa('shield-alt')}</span>"),""),
                  fmvp = dplyr::if_else(player_name == get_award("fmvp",season),glue::glue("<span style = 'color:#404040'>{fontawesome::fa('medal')}</span>"),"")) %>%
    dplyr::mutate(rank = dplyr::row_number(),
                  all_star = dplyr::if_else(all_star == 1, "<span style = 'color:#404040'>&#9733;</span>",""),
                  player_name = glue::glue("<div style = 'line-height:100%'><b style = 'font-size:14px'>{player_name}</b></div>  <div style = 'line-height:100%'><span style = 'font-size:12px;color:grey'>{get_position(pos)}</span></div> "),
                  team_name = glue::glue("<div style = 'line-height:100%'><b style = 'font-size:14px'>{get_last_name(team_name)}</b></div> <div style = 'line-height:100%'><span style = 'font-size:12px;color:grey'>{remove_last_name(team_name)}</div>")) %>%
    dplyr::select(rank,image, player_name,pts,ast,reb,blk,stl,fg2_pct,fg3_pct,ft_pct,mvp,dpoy,all_star,bpm,obpm,dbpm,logo,team_name,playoffs,champion,fmvp) %>%
    dplyr::arrange(dplyr::desc(.data[[variable]])) %>%
    head(25)

  table <- table_data %>%
    gt::gt() %>%
    ## COLORS --------------------------------------------------------------------
  gt::data_color(columns = gt::vars(pts,ast,reb,blk,stl),
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

    gt::data_color(columns = gt::vars(fg2_pct,fg3_pct,ft_pct),
                   colors = scales::col_numeric(
                     palette = rev(c("#de425b",
                                     "#eb5f57",
                                     "#f47c55",
                                     "#fa9758",
                                     "#feb261",
                                     "#ffcc70",
                                     "#ffe584",
                                     "#fffe9e")
                     ),
                     domain = NULL
                   )) %>%
    gt::data_color(columns = gt::vars(champion),
                   colors = scales::col_factor(
                     palette = c(court_themes('bg_table_markdown'),"#fad684"),
                     domain = c("",glue::glue("<span style = 'color:#404040'>{fontawesome::fa('trophy')}</span>"))
                   )) %>%
    gt::data_color(columns = gt::vars(playoffs),
                   colors = scales::col_factor(
                     palette = c(court_themes('bg_table_markdown'),"#fad684"),
                     domain = NULL
                   )) %>%
    gt::data_color(columns = gt::vars(fmvp),
                   colors = scales::col_factor(
                     palette = c(court_themes('bg_table_markdown'),"#fad684"),
                     domain = c("",glue::glue("<span style = 'color:#404040'>{fontawesome::fa('medal')}</span>"))
                   )) %>%
    gt::data_color(columns = gt::vars(mvp),
                   colors = scales::col_factor(
                     palette = c(court_themes('bg_table_markdown'),"#ffc391"),
                     domain = c("",glue::glue("<span style = 'color:#404040'>{fontawesome::fa('medal')}</span>"))
                   )) %>%
    gt::data_color(columns = gt::vars(dpoy),
                   colors = scales::col_factor(
                     palette = c(court_themes('bg_table_markdown'),"#ffc391"),
                     domain = c("",glue::glue("<span style = 'color:#404040'>{fontawesome::fa('shield-alt')}</span>"))
                   )) %>%
    gt::data_color(columns = gt::vars(all_star),
                   colors = scales::col_factor(
                     palette = c(court_themes('bg_table_markdown'),"#ffc391"),
                     domain = c("","<span style = 'color:#404040'>&#9733;</span>"))
    ) %>%
    ## IMAGES --------------------------------------------------------------------
  gt::text_transform(locations = gt::cells_body(gt::vars(logo,image)),
                     fn = function(x) {
                       gt::web_image(url = x, height = gt::px(35))
                     }) %>%
    ## Fonts and text formatting -------------------------------------------------
  gt::tab_style(
    style = list(gt::cell_text(align = "center",weight = "bold", v_align = "bottom",size = "small")),
    locations = list(gt::cells_column_labels(gt::everything()))
  ) %>%
    gt::tab_style(
      style = list(gt::cell_text(align = "left", v_align = "bottom")),
      locations = list(gt::cells_column_labels(c("team_name","player_name")))
    ) %>%
    gt::tab_style(style = list(gt::cell_text(align = "left")),
                  locations = list(gt::cells_body(c("team_name","player_name")))) %>%
    gt::tab_style(style = list(gt::cell_text(align = "center", size = "small")),
                  locations = list(gt::cells_body(c("mvp","dpoy","all_star","champion","fmvp")))) %>%
    gt::tab_style(style = list(gt::cell_text(align = "center", size = "small", font = "Kiwi Maru")),
                  locations = list(gt::cells_body(c("bpm","obpm","dbpm","pts","ast","reb","blk","stl","fg2_pct","fg3_pct","ft_pct")))) %>%
    gt::tab_style(style = list(gt::cell_text(align = "center", size = "small", font = "Kiwi Maru")),
                  locations = list(gt::cells_body(c("pts","ast","reb","blk","stl")))) %>%
    gt::tab_style(style = list(gt::cell_text(weight = "bold")),
                  locations = list(gt::cells_body(c(variable)))) %>%
    gt::tab_style(style = list(gt::cell_text(align = "center", color = "grey", size = "x-small")),
                  locations = list(gt::cells_body(c("rank")))) %>%
    gt::tab_style(
      style = list(gt::cell_text(align = "center", font = "Manrope", color = "#404040")),
      locations = list(gt::cells_title(groups = "title"))) %>%
    gt::tab_style(
      style = list(gt::cell_text(align = "center", font = "Manrope", color = "#404040")),
      locations = list(gt::cells_title(groups = "subtitle"))) %>%
    ## Transparent fill for NA Values -------------------------------------------
  gt::tab_style(
    style = list(gt::cell_fill(color = court_themes('bg_table_markdown'))),
    locations = list(gt::cells_body(columns = c("fg3_pct"),
                                    rows = is.na(fg3_pct))))%>%
    ## OTHERS --------------------------------------------------------------------
  gt::fmt_markdown(columns = c("all_star","player_name","team_name","playoffs","champion","fmvp","mvp","dpoy")) %>%
    gt::fmt_percent(columns = c("fg2_pct","fg3_pct","ft_pct"), decimals = 1) %>%
    gt::fmt_missing(columns = c("fg3_pct"), missing_text = "–") %>%
    gt::fmt(columns = c("bpm","obpm","dbpm"),
            fns = function(x){
              dplyr::if_else(x>0, glue::glue("<span style = 'color:#02af74'>+{x}</span>"),
                             if_else(x ==0, glue::glue("<span style = 'color:#b3b3b3'>=</span>"),glue::glue("<span style = 'color:#fc7042'>{x}</span>")))
            }) %>%
    gt::opt_table_font(font = list(
      gt::google_font("Manrope"),
      gt::google_font("Kiwi Maru"),
      gt::google_font("Montserrat"),
      gt::google_font("Lato"),
      gt::google_font("Roboto Condensed")
    )) %>%
    ## Tab Spanners ----------------------------------------------------------------
  gt::tab_spanner(label = gt::md("**PER GAME STATISTICS**"),
                  columns = gt::vars(pts,ast,reb,blk,stl)) %>%
    gt::tab_spanner(label = gt::md("**SHOOTING ACCURACY**"),
                    columns = gt::vars(fg2_pct,fg3_pct,ft_pct)) %>%
    gt::tab_spanner(label = gt::md("**AWARDS**"),
                    columns = gt::vars(mvp,dpoy,all_star)) %>%
    gt::tab_spanner(label = gt::md("**ALL-IN-ONE METRICS**"),
                    columns = gt::vars(bpm,obpm,dbpm)) %>%
    gt::tab_spanner(label = gt::md("**POSTSEASON**"),
                    columns = gt::vars(logo,team_name,playoffs,champion,fmvp)) %>%
    ## Borders formatting ----------------------------------------------------------
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
        columns = gt::vars(pts,fg2_pct,mvp,bpm,logo)
      )
    )
  ) %>%
    ## Footnotes -------------------------------------------------------------------
  gt::tab_footnote(
    footnote = gt::md(glue::glue("Shooting Percentage among players shooting at least 0.2 Three point shots per Game")),
    locations = gt::cells_column_labels(columns = gt::vars(fg3_pct))
  ) %>%
    gt::tab_footnote(
      footnote = gt::md(glue::glue("Free Throw Shooting")),
      locations = gt::cells_column_labels(columns = gt::vars(ft_pct))
    ) %>%
    gt::tab_footnote(
      footnote = gt::md(glue::glue("Defensive Player of the Year")),
      locations = gt::cells_column_labels(columns = gt::vars(dpoy))
    ) %>%
    gt::tab_footnote(
      footnote = gt::md(glue::glue("Box Plus Minus estimates a player's overall contribution")),
      locations = gt::cells_column_labels(columns = gt::vars(bpm,obpm,dbpm))
    ) %>%
    ## Column Labels ---------------------------------------------------------------
  gt::cols_label(rank = "",
                 image = "",
                 player_name = gt::html("<div style = 'line-height:100%'>Player</div>  <div style = 'line-height:100%'><span style='font-size:12px;color:grey;font-weight:400;'>Position</span></div>"),
                 pts = "Points",
                 ast = "Assists",
                 reb = "Rebounds",
                 blk = "Blocks",
                 stl = "Steals",
                 fg2_pct = "2 Point Shooting",
                 fg3_pct ="3 Point Shooting",
                 ft_pct = "FT Shooting",
                 mvp = "MVP",
                 dpoy = "DPOY",
                 all_star = "All-Star",
                 bpm = "BPM",
                 obpm = "Offensive BPM",
                 dbpm = "Defensive BPM",
                 logo = "",
                 team_name = "Team",
                 playoffs = "Playoffs",
                 champion = "Champion",
                 fmvp = "Finals MVP") %>%
    ## Column Widths
    gt::cols_width(
      gt::vars(rank)~gt::px(30),
      gt::vars(image)~gt::px(50),
      gt::vars(logo)~gt::px(50),
      gt::vars(player_name)~gt::px(200),
      gt::vars(pts)~gt::px(70),
      gt::vars(ast)~gt::px(70),
      gt::vars(reb)~gt::px(70),
      gt::vars(blk)~gt::px(70),
      gt::vars(stl)~gt::px(70),
      gt::vars(fg2_pct)~gt::px(70),
      gt::vars(fg3_pct)~gt::px(70),
      gt::vars(ft_pct)~gt::px(70),
      gt::vars(mvp)~gt::px(60),
      gt::vars(dpoy)~gt::px(60),
      gt::vars(all_star)~gt::px(60),
      gt::vars(bpm)~gt::px(70),
      gt::vars(obpm)~gt::px(70),
      gt::vars(dbpm)~gt::px(70),
      gt::vars(logo)~gt::px(50),
      gt::vars(team_name)~gt::px(115),
      gt::vars(playoffs)~gt::px(70),
      gt::vars(champion)~gt::px(70),
      gt::vars(fmvp)~gt::px(70)
    ) %>%
    ##Options --------------------------------------------------------------------
  gt::tab_options(table.border.top.color = "transparent",
                  table.border.bottom.color = "transparent",
                  table.border.bottom.width = gt::px(2),
                  column_labels.border.top.width = gt::px(2),
                  column_labels.border.bottom.width = gt::px(2),
                  column_labels.border.bottom.color = "#0D0D0D",
                  heading.title.font.size = 18,
                  heading.subtitle.font.size = 14,
                  source_notes.font.size = 10,
                  footnotes.font.size = 10,
                  column_labels.border.top.color = "#0D0D0D",
                  heading.title.font.weight = "bold",
                  heading.subtitle.font.weight = "bold",
                  data_row.padding = px(3),
                  heading.border.bottom.color = "#a1a1a1",
                  table_body.border.bottom.color = "#a1a1a1",
                  table_body.hlines.color = "#a1a1a1",
                  table.background.color = court_themes('bg_table_markdown')) %>%
    gt::tab_source_note(gt::md("**<span style = 'color:#404040'>Visualisation by Henri Freixe • Sources : Nba.com, Basketball-reference.com, Espn.com</span>**"))%>%
    gt::tab_header(
      title = gt::md(glue::glue("**Player-oriented Season Overview <span style = color:#CBA049>| {season}</span>**")),
      subtitle = gt::md(glue::glue("**Ranking of the 25 best players in <span style = color:#CBA049>{variable_name(variable)}</span>**"))
    )

  if (season == "2020-21") {
    table <- table %>% gt::tab_footnote(
      footnote = gt::md(glue::glue("{season} NBA season has yet to unveil Awards and Posteason outcomes")),
      locations = gt::cells_column_spanners(spanners = c(gt::md("**POSTSEASON**"),gt::md("**AWARDS**"))))
  }

  return(table)

}
