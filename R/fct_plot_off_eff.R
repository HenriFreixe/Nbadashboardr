#### Team shot locations by zone


get_team_shot_zone <- function(season = "2020-21") {

  .headers  <- headers()

  url_team_stats <- function(season) {
    glue::glue("https://stats.nba.com/stats/leaguedashteamshotlocations?Conference=&DateFrom=&DateTo=&DistanceRange=By+Zone&Division=&GameScope=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=Totals&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season={season}&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision=")
  }

  res <- httr::GET(url = url_team_stats(season), httr::add_headers(.headers=.headers))
  json_resp <- jsonlite::fromJSON(httr::content(res, "text"))

  team_stats <- data.frame(json_resp$resultSets$rowSet) %>% dplyr::as_tibble()
  colnames(team_stats) <- c('team_id','team_name','fgm_ra','fga_ra','fg_pct_ra','fgm_paintnra','fga_paintnra','fg_pct_paintnra','fgm_mid','fga_mid','fg_pct_mid','fgm_lc3','fga_lc3','fg_pct_lc3','fgm_rc3','fga_rc3','fg_pct_rc3','fgm_atb3','fga_atb3','fg_pct_atb3')

  team_stats <- team_stats %>%
    janitor::clean_names() %>%
    dplyr::mutate(across("fgm_ra":"fg_pct_atb3",as.numeric)) %>%
    dplyr::mutate(fgm_rim = fgm_paintnra + fgm_ra,
                  fga_rim = fga_paintnra + fga_ra,
                  fg_pct_rim = fgm_rim/fga_rim,
                  fgm3 = fgm_lc3 + fgm_rc3 + fgm_atb3,
                  fga3 = fga_lc3 + fga_rc3 + fga_atb3,
                  fg_pct3 = fgm3/fga3) %>%
    dplyr::select(team_id,team_name,fgm_rim,fga_rim,fg_pct_rim,fgm_mid,fga_mid,fg_pct_mid,fgm3,fga3,fg_pct3) %>%
    dplyr::mutate(team_name = dplyr::if_else(team_name == "LA Clippers","Los Angeles Clippers",team_name),
                  team_name = dplyr::if_else(team_name == "New Orleans/Oklahoma City Hornets", "New Orleans Hornets",team_name),
                  former_team_name = team_name,
                  team_name = dplyr::if_else(team_name == "New Orleans Hornets","New Orleans Pelicans",team_name),
                  team_name = dplyr::if_else(team_name == "New Jersey Nets","Brooklyn Nets",team_name),
                  team_name = dplyr::if_else(team_name == "Charlotte Bobcats","Charlotte Hornets",team_name),
                  team_name = dplyr::if_else(team_name == "Seattle SuperSonics","Oklahoma City Thunder",team_name))

  return(team_stats)


}


### Shooting splits per team per year

get_team_shoot_split <- function(season = "2020-21") {

  get_team_shot_zone(season) %>%
    dplyr::select(team_id,team_name,fga_rim,fga_mid,fga3) %>%
    dplyr::mutate(total = fga_rim + fga_mid + fga3,
                  share_rim = fga_rim/total,
                  share_mid = fga_mid/total,
                  share_3 = fga3/total,
                  season = season)

}

get_all_seasons <- function(start_season = "2011-12",end_season = "2020-21") {

  start_year = start_season %>%
    stringr::str_sub(end=-4) %>%
    as.integer()

  end_year = end_season %>%
    stringr::str_sub(end=-4) %>%
    as.integer()

  all_seasons <-  dplyr::tibble(pre_year = seq(start_year,end_year),
                                post_year = seq(start_year,end_year)+1) %>%
    dplyr::mutate(season = glue::glue("{pre_year}-{stringr::str_sub(post_year,start=3)}") %>%
                    as.character) %>%
    dplyr::pull(season)

  return(all_seasons)
}

get_all_seas_team_shoot_split <- function(start_season = "2011-12", end_season = "2020-21") {

  future::plan(future::multisession(workers = future::availableCores()))

  get_all_seasons(start_season,end_season) %>%
    furrr::future_map_dfr(get_team_shoot_split)
}

team_split <- function(start_season = "2011-12", end_season = "2020-21",team = "global") {

  df <- get_all_seas_team_shoot_split(start_season, end_season)

  if (team == "global") {return(df %>%
                                  dplyr::group_by(season) %>%
                                  dplyr::summarise(fga_rim = sum(fga_rim),
                                                   fga_mid = sum(fga_mid),
                                                   fga3 = sum(fga3),
                                                   total = sum(total),
                                                   share_rim = fga_rim/total,
                                                   share_mid = fga_mid/total,
                                                   share_3 = fga3/total))}
  else { return(df %>% dplyr::filter(team_name == team)) }

}



### Average efficiency

av_efficiency_range <- function(start_season = "2011-12",end_season =  "2020-21") {
  df <- (xml2::read_html("https://www.basketball-reference.com/leagues/NBA_stats_per_game.html") %>%
           rvest::html_nodes(css = "table") %>%
           rvest::html_table(trim = TRUE))[[1]]


  colnames(df) <- df %>% head(1)

  df %>%
    janitor::clean_names() %>%
    dplyr::filter(season %in% get_all_seasons(start_season,end_season)) %>%
    dplyr::select(season,'off_rating' = o_rtg) %>%
    dplyr::mutate(off_rating = as.numeric(off_rating) )

}


### Min, max, selected efficiency => Fonction trop longue...

hltd_off_rating <- function(start_season = "2011-12", end_season = "2020-21",team = "global") {

  future::plan(future::multisession(workers = future::availableCores()))

  if (team == "global") {return(get_all_seasons(start_season,end_season) %>%
                                  furrr::future_map_dfr(get_team_advanced) %>%
                                  dplyr::select(team_name,season,off_rating) %>%
                                  dplyr::group_by(season) %>%
                                  dplyr::mutate(min = dplyr::if_else(off_rating == min(off_rating),"yes","no"),
                                                max = dplyr::if_else(off_rating == max(off_rating),"yes","no")) %>%
                                  dplyr::ungroup() %>%
                                  dplyr::filter(min == "yes"| max =="yes"))}
  else {return(get_all_seasons(start_season,end_season) %>%
                 furrr::future_map_dfr(get_team_advanced) %>%
                 dplyr::select(team_name,season,off_rating) %>%
                 dplyr::group_by(season) %>%
                 dplyr::mutate(min = dplyr::if_else(off_rating == min(off_rating),"yes","no"),
                               max = dplyr::if_else(off_rating == max(off_rating),"yes","no")) %>%
                 dplyr::ungroup() %>%
                 dplyr::filter(min == "yes"| max=="yes"| team_name == team) %>%
                 dplyr::mutate(selected = if_else(team_name==team,"yes","no")))}

}

### BBall reference test hltd_off_rating


bballref_efficiency_season <- function(season = "2020-21") {

  df <- xml2::read_html(glue::glue("https://www.basketball-reference.com/leagues/NBA_{stringr::str_sub(season,end = -4) %>% as.numeric()+1}_ratings.html")) %>%
    rvest::html_element("table") %>%
    rvest::html_table(trim = TRUE)

  colnames(df) <- df %>% head(1)
  df <- df %>%
    janitor::clean_names() %>%
    dplyr::select(team_name = team,off_rating = o_rtg)

  df %>%
    dplyr::slice(2:n()) %>%
    dplyr::mutate(off_rating = as.numeric(off_rating),
                  season = season)
}

bballref_def_efficiency_season <- function(season = "2020-21") {

  df <- xml2::read_html(glue::glue("https://www.basketball-reference.com/leagues/NBA_{stringr::str_sub(season,end = -4) %>% as.numeric()+1}_ratings.html")) %>%
    rvest::html_element("table") %>%
    rvest::html_table(trim = TRUE)

  colnames(df) <- df %>% head(1)
  df <- df %>%
    janitor::clean_names() %>%
    dplyr::select(team_name = team,def_rating = d_rtg)

  df %>%
    dplyr::slice(2:n()) %>%
    dplyr::mutate(def_rating = as.numeric(def_rating),
                  season = season)
}

bbal_ref_rating <- function(start_season = "2010-11", end_season = "2020-21",team="global") {

  future::plan(future::multisession(workers = future::availableCores()))

  get_all_seasons(start_season,end_season) %>%
    furrr::future_map_dfr(bballref_efficiency_season) %>%
    dplyr::group_by(season) %>%
    dplyr::mutate(min = dplyr::if_else(off_rating == min(off_rating),"yes","no"),
                  max = dplyr::if_else(off_rating == max(off_rating),"yes","no")) %>%
    dplyr::filter(min == "yes" | max == "yes" | team_name == team) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(selected = dplyr::if_else(team_name == team,'yes','no'))

}


### Interactive version of the above plot

off_rating_evo_interactive <- function(start_season = '2011-12', end_season = '2020-21' , team = 'global') {

  ## Dataset
  ratings <- bbal_ref_rating(start_season, end_season, team) %>%
    dplyr::left_join(av_efficiency_range(start_season, end_season), by = c("season")) %>%
    dplyr::rename('off_rating' = 'off_rating.x', 'av_off_rating' = 'off_rating.y') %>%
    dplyr::mutate(fill = dplyr::if_else(min =='yes','#2E4EB8','#de425b'),
                  tooltip_label = glue::glue("<div style ='font-family:Kiwi Maru;'><span style = 'font-size:10pt'>{team_name}</span><hr style='margin-top:5px;margin-bottom:1px;border:0;height:0;border-top:1px solid rgba(0,0,0,0.1);border-bottom:1px solid rgba(255,255,255,0.3);'/><span style = 'font-size:10pt'>Offensive efficiency : {round(off_rating,digits = 1)}</span></div>"))

  ##Dataset geom_line

  line_ratings_team <- if (team =='global') {ratings %>% dplyr::filter(selected =="no")}
  else {ratings %>% dplyr::filter(selected == "yes")}



  ##Color and subtitles management

  color_titles <- dplyr::if_else(team =='global',
                                 court_themes('lines'),
                                 '#CBA049')
  team_name_titles <- dplyr::if_else(team =='global',
                                     'League Average',
                                     team)

  font_size_titles <- dplyr::if_else(team =='global',
                                     '14pt',
                                     '18pt')

  alpha_size_team <- dplyr::if_else(team =='global',
                                    0,
                                    .75)

  pre_efficiency <- if (team =='global') {ratings %>% dplyr::pull(av_off_rating) %>% head(1)}
  else {ratings %>% dplyr::filter(selected =="yes") %>% dplyr::pull(off_rating) %>% head(1)}

  post_efficiency <- if (team =='global') {ratings %>% dplyr::pull(av_off_rating) %>% tail(1)}
  else {ratings %>% dplyr::filter(selected =="yes") %>% dplyr::pull(off_rating) %>% tail(1)}


  plot <- ratings %>%
    ggplot2::ggplot() +
    ggplot2::geom_segment(ggplot2::aes(x = season,xend = season,
                                       y=av_off_rating, yend = off_rating),
                          color = "grey5",
                          linetype = 2,
                          size = .5) +
    ggplot2::geom_line(ggplot2::aes(x = season, y = av_off_rating, group = 1),
                       color = court_themes('lines'),
                       size = if_else(team == 'global',.75,0.25)) +
    ggplot2::geom_line(data = line_ratings_team,
                       ggplot2::aes(x = season, y = off_rating, group = 1),
                       color = '#CAB17D',
                       size = if_else(team == 'global',0,0.75),
                       alpha = alpha_size_team) +
    ggplot2::geom_point(data = ratings %>% dplyr::filter(min == "yes" | max == "yes"),
                        ggplot2::aes(x = season,
                                     y = off_rating,
                                     fill = fill),
                        color = "#0D0D0D",
                        shape = 21,
                        size = 7,
                        stroke = 1) +
    ggiraph::geom_point_interactive(data = ratings %>% dplyr::filter(min == "yes" | max == "yes"),
                                    ggplot2::aes(x = season,
                                                 y = off_rating,
                                                 fill = ggplot2::alpha(fill,.2),
                                                 data_id = team_name,
                                                 tooltip = tooltip_label),
                                    color = ggplot2::alpha("#0D0D0D",.2),
                                    shape = 21,
                                    size = 7,
                                    stroke = 1) +
    ggtext::geom_richtext(aes(x = 7.5,
                              y = 97.5),
                          label ="<span style = 'font-size:12pt;color:#ccb076;'>Trading off long 2-point shots<br> for 3-point shots helped boost <br>Offensive Efficiency in the 2010s</span>",
                          family = 'Kiwi Maru',
                          fill = NA,
                          label.color = NA,
                          label.padding = grid::unit(rep(0,4),"pt")) +
    ggplot2::guides(color = FALSE, fill = FALSE) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_color_identity() +
    ggplot2::labs(title = glue::glue("<span style = 'color:{color_titles};'>{team_name_titles}</span> Offensive Efficiency and <br> shooting split evolution <span style = 'color:#CBA049;'>| {glue::glue('{stringr::str_sub(start_season,end = 4)}-{stringr::str_sub(end_season, start = 6)}')}</span>"),
                  subtitle = glue::glue("<span style = 'color:{color_titles};font-size:{font_size_titles}'>{(team_name_titles)}</span> Offensive Efficiency went from <span style = 'font-size:18pt;'>{round(pre_efficiency, digits = 1)}</span> to <span style = 'font-size:18pt;'>{round(post_efficiency,digits = 1)}</span> pts per 100 possessions <br> Points represent <span style = 'color:#de425b;font-size:18pt'> season-highs </span> and <span style = 'color:#2E4EB8;font-size:18pt'>season-lows</span>")) +
    ggplot2::theme_minimal(base_size = 22) +
    theme_dark_cap() +
    ggplot2::theme(plot.title = ggtext::element_markdown(hjust=.5,
                                                         margin = ggplot2::margin(t = 20)),
                   plot.subtitle = ggtext::element_markdown(hjust=.5,
                                                            size = 14,
                                                            margin = ggplot2::margin(t = 15,
                                                                                     b=15)),
                   panel.grid = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(size = 12,color = court_themes('lines')),
                   axis.text.x = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank())

}

### Second plot, stacked bars

shot_frequency <- function(start_season = '2011-12',end_season ='2020-21' ,team = 'global') {

  fill_mid <- dplyr::if_else(team == "global","#767676",'#CAB17D')
  fill_3 <- dplyr::if_else(team == "global","#B7B7B7","#CBA049")
  dataset2 <- team_split(start_season, end_season, team) %>%
    tidyr::pivot_longer(cols = dplyr::contains("share"),names_to = 'shot_distance',values_to = 'share') %>%
    dplyr::select(season,shot_distance,share) %>%
    dplyr::filter(shot_distance %in% c("share_mid","share_3")) %>%
    dplyr::mutate(fill = dplyr::if_else(shot_distance == "share_mid",fill_mid,fill_3))

  dataset2 %>%
    ggplot2::ggplot() +
    ggplot2::geom_col(ggplot2::aes(x = season,
                                   y = share,
                                   group = fill,
                                   fill = fill),
                      position = "dodge",
                      color = "grey20",
                      width = .75) +
    ggplot2::labs(subtitle = glue::glue("Three point shots went from <span style = 'font-size:18pt'>{dataset2 %>% filter(shot_distance == 'share_3') %>% head(1) %>%  pull(share) %>% scales::percent() }</span> to <span style = 'font-size:18pt'>{dataset2 %>% filter(shot_distance == 'share_3') %>% tail(1) %>% pull(share) %>% scales::percent()}</span><br> of all<span style = 'color:#CBA049;font-size:18pt'>{if_else(team =='global','',as.character(glue::glue(' {get_last_name(team)}')))}</span> shots taken in the time span"),
                  caption = glue::glue("Visualisation by Henri Freixe • Sources : Nba.com, Basketball-reference.com")) +
    ggplot2::scale_fill_identity(guide = "legend",
                                 labels = c("Mid-range<br>shooting frequency","3 point<br>shooting frequency")) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(),
                                limits = c(0,.6)) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1,
                                                 label.position = 'bottom',
                                                 title = NULL,
                                                 keywidth = 8,
                                                 keyheight = 1)) +
    ggplot2::theme_minimal(base_size = 22) +
    theme_dark_cap() +
    ggplot2::theme(plot.subtitle = ggtext::element_markdown(hjust = .5,
                                                            size = 14,
                                                            margin = ggplot2::margin(0,0,0,0)),
                   plot.caption = ggtext::element_markdown(size = 9,
                                                           margin = ggplot2::margin(t = 20,
                                                                                    b = 10)),
                   panel.grid = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(),
                   axis.text = ggplot2::element_text(size = 12,
                                                     color = court_themes('lines')),
                   axis.title = ggplot2::element_blank(),
                   legend.position = c(.5,.8),
                   legend.text = ggtext::element_markdown(size = 12))

}



### Team logo

team_logo <- function(team = 'global', width = 50) {

  if (team == 'global') {
    ggplot2::ggplot() +
      theme_dark_cap() +
      ggplot2::theme(plot.background = ggplot2::element_rect(fill = "transparent",color = "transparent"),
                     panel.background = ggplot2::element_rect(fill = "transparent", color = "transparent"),
                     panel.grid = ggplot2::element_blank(),
                     panel.border = ggplot2::element_blank(),
                     axis.text = ggplot2::element_blank(),
                     axis.title = ggplot2::element_blank(),
                     axis.ticks = ggplot2::element_blank(),
                     legend.background = ggplot2::element_blank()) } else {
                       ggplot2::ggplot() +
                         ggtext::geom_richtext(ggplot2::aes(x = .5,
                                                            y = .5,
                                                            label = get_team_traditional() %>% dplyr::filter(team_name == team) %>% dplyr::pull(logo) %>% link_to_img(width)),
                                               fill = NA,
                                               color = NA,
                                               label.color = NA,
                                               label.padding = grid::unit(rep(0,4),"pt")
                         ) +
                         ggplot2::coord_cartesian() +
                         theme_dark_cap() +
                         ggplot2::theme(plot.background = element_rect(fill = "transparent",color = "transparent"),
                                        panel.background = ggplot2::element_rect(fill = "transparent", color = "transparent"),
                                        panel.grid = ggplot2::element_blank(),
                                        panel.border = ggplot2::element_blank(),
                                        axis.text = ggplot2::element_blank(),
                                        axis.title = ggplot2::element_blank(),
                                        axis.ticks = ggplot2::element_blank(),
                                        legend.background = ggplot2::element_blank())
                     }

}


### Plot regroupment Interactive

plot_off_evo_interactive <- function(start_season = "2011-12",end_season = "2020-21" ,team = 'global') {


  future::plan(future::multisession)
  future::`%globals%`(future::`%<-%`(A,off_rating_evo_interactive(start_season, end_season, team)),structure(TRUE, add = c("get_last_name")))
  future::`%globals%`(future::`%<-%`(B,shot_frequency(start_season,end_season,team)),structure(TRUE, add = c("get_last_name")))

  future::`%<-%`(team_logo,team_logo(team))


  plot <- (A / B) & ggplot2::theme(plot.background = ggplot2::element_rect(fill = court_themes('court'),color = court_themes('court')))

  plot_with_logo <- plot + patchwork::inset_element(team_logo, left = -0.075, top = 2.45, right = 0.125, bottom = 2.25)

  ggiraph::girafe(ggobj = plot_with_logo,
                  width_svg = 12,
                  height_svg = 12,
                  options = list(ggiraph::opts_tooltip(use_fill = TRUE),
                                 ggiraph::opts_hover(css = "fill:red;")))

}
