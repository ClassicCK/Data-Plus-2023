library(ggthemes)
library(ggplot2)
library(stringr)
library(stringi)

#Creating Custom Theme 
theme_twoseventyeight <- theme_fivethirtyeight()

theme_twoseventyeight <- theme_twoseventyeight +
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = 'white'),
        text = element_text(family = "Arial"),
        plot.caption = element_text(size = rel(0.8), hjust=c(0,1)),
        axis.text = element_text(size = rel(0.8)),
        axis.title = element_text(size = rel(1.1)),
        legend.text = element_text(size = rel(0.8)),
        legend.title = element_text(size = rel(1.1), face = "bold"),
        plot.title = element_text(size = rel(1.5), face = "bold", hjust = 0),
        plot.subtitle = element_text(size = rel(1.2), hjust = 0),
        strip.text = element_text(size = rel(0.8), face = "bold"),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)))

#Creating Custom Pallete
twoseventyeight_color <- function(...) {
  twoseventyeight_colors <- c(
    `pink`     = "#E44EAE",
    `orange` = "#E4844E",
    `gray`     = "#999999",
    `red`    = "#e44e63",
    `purple`   = "#844EE4",
    `green` = "#4EE484",
    `blue` = "#4e63e4",
    `yellow` = "#e4cf4e",
    `black` = "#222222")
  cols <- c(...)
  if (is.null(cols))
    return (twoseventyeight_colors)
  twoseventyeight_colors[cols]
}
twoseventyeight_palette <- function(palette = "main", ...) {
  twoseventyeight_palettes <- list(
    `main` = twoseventyeight_color("purple", "orange", "green", "pink"),
    `political` = twoseventyeight_color("blue", "red", "yellow"),
    `single` = twoseventyeight_color("purple", "gray")
  )
  twoseventyeight_palettes[[palette]]
}
palette_gen <- function(palette = "main", direction = 1) {
  function(n) {
    if (n > length(twoseventyeight_palette(palette)))
      warning("Not enough colors in this palette!")
    else {
      all_colors <- twoseventyeight_palette(palette)
      all_colors <- unname(unlist(all_colors))
      all_colors <- if (direction >= 0) all_colors else rev(all_colors)
      color_list <- all_colors[1:n]
    }
  }
}
palette_gen_c <- function(palette = "main", direction = 1, ...) {
  pal <- twoseventyeight_palette(palette)
  pal <- if (direction >= 0) pal else rev(pal)
  colorRampPalette(pal, ...)
}
scale_fill_278 <- function(palette = "main", direction = 1, ...) {
  ggplot2::discrete_scale(
    "fill", "278",
    palette_gen(palette, direction),
    ...
  )
}
scale_colour_278 <- function(palette = "main", direction = 1, ...) {
  ggplot2::discrete_scale(
    "colour", "278",
    palette_gen(palette, direction),
    ...
  )
}
scale_color_278_c <- function(palette = "main", direction = 1, ...) {
  pal <- palette_gen_c(palette = palette, direction = direction)
  scale_color_gradientn(colors = pal(256), ...)
}

