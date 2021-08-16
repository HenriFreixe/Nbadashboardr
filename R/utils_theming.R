# Theming functions

court_themes <- function(var) {

  df <- tibble::tibble(court = 'grey20',
                       lines = '#999999',
                       font = 'Kiwi Maru',
                       bg_table_markdown = "#E8E8E8")
  df %>% dplyr::pull(var)
}

theme_dark_cap <- function(font_family = court_themes('font'), text_color = court_themes('lines'), plot_background = court_themes('court')) {
  ggplot2::theme(text = ggplot2::element_text(family = font_family,
                                              color = text_color),
                 plot.background = ggplot2::element_rect(fill = plot_background, color = plot_background),
                 panel.background = ggplot2::element_rect(fill = plot_background, color = plot_background)) }


### Color Palette

get_color_tibble <- function(var) {

  color_palette <- c("very high" = "#F51D05",
                     "high" = "#ED5B21",
                     "medium high" = "#F08622",
                     "medium" = "#F5CE58",
                     "medium low" = "#CDEFF7",
                     "low" = "#A4DFF5",
                     "very low" = "#5EB4D6")

  color_palette_tbl <- tibble::rownames_to_column(data.frame(color_palette)) %>%
    tibble::as_tibble()

  if (var == 'palette') {
    return(color_palette)
  } else {
    return(color_palette_tbl)
  }
}

## Not Run (downloading Kiwi Maru and Manrope fonts and CSS within project,
## slower than having both fonts installed anyway)

#if(!dir.exists("fonts")) dir.create("fonts")
#gfonts::setup_font(id = "kiwi-maru", output_dir = "fonts",
#           variants = "regular", prefer_local_source = FALSE)
#setup_font(id = "manrope", output_dir = "fonts",
#           variants = "regular", prefer_local_source = FALSE)


rcolor_to_hex <- function(color) {
  rgb <- col2rgb(color)
  rgb(rgb[1],rgb[2],rgb[3],maxColorValue = 255)
}
