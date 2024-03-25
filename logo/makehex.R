library(hexSticker)

s_without_title <- sticker(
    subplot = "heart_cuff.png",
    s_x = 1.0,
    s_y = 0.90,
    s_width = 0.6,
    s_height = 0.6,
    package = '',
    p_x = 1,
    p_y = 1.6,
    p_color = "#111111",
    p_family = "serif",
    p_fontface = "plain",
    p_size = 18,
    h_size = 1.2,
    h_fill = "#009F96",# "#A2AAAD",
    h_color = "#000000",
    spotlight = TRUE,
    l_x = 1.5,
    l_y = 0.5,
    l_width = 3,
    l_height = 3,
    l_alpha = 0.1,
    url = "",
    u_x = 1,
    u_y = 0.08,
    u_color = "black",
    u_family = "Aller_Rg",
    u_size = 1.5,
    u_angle = 30,
    white_around_sticker = FALSE,
    filename = "../man/figures/pedbplogo_sanstext.png",
    asp = 1,
    dpi = 300
  )
plot(s_without_title)

s_with_title <- sticker(
    subplot = "heart_cuff.png",
    s_x = 1.0,
    s_y = 0.80,
    s_width = 0.6,
    s_height = 0.6,
    package = 'pedbp',
    p_x = 1,
    p_y = 1.6,
    p_color = "#111111",
    p_family = "serif",
    p_fontface = "plain",
    p_size = 18,
    h_size = 1.2,
    h_fill = "#009F96",# "#A2AAAD",
    h_color = "#000000",
    spotlight = TRUE,
    l_x = 1.5,
    l_y = 0.5,
    l_width = 3,
    l_height = 3,
    l_alpha = 0.1,
    url = "",
    u_x = 1,
    u_y = 0.08,
    u_color = "black",
    u_family = "Aller_Rg",
    u_size = 1.5,
    u_angle = 30,
    white_around_sticker = FALSE,
    filename = "../man/figures/pedbplogo.png",
    asp = 1,
    dpi = 300
  )
plot(s_with_title)

# Wall-of-software versions
library(png)
heart_cuff <- png::readPNG("heart_cuff.png")
heart_cuff_grob <- grid::rasterGrob(heart_cuff, interpolate = TRUE)
theta <- pi * seq(1/6, 2 + 1/6, by = 1/3)

# 1200 x 1200 without text
hexagon <- data.frame(x = 9.1 * cos(theta), y = 9.1 * sin(theta))

wall_of_software_1200x1200 <-
  ggplot2::ggplot() +
  ggplot2::theme_void() +
  ggplot2::geom_rect(fill = "#009F96", mapping = ggplot2::aes(xmin = -10, xmax = 10, ymin = -10, ymax = 10)) +
  ggplot2::annotation_custom(heart_cuff_grob, xmin = -6, xmax = 6, ymin = -6 - 0.5, ymax = 6 - 0.5) +
  ggplot2::geom_point() +
#  ggplot2::geom_polygon(data = hexagon, mapping = ggplot2::aes(x = x, y = y), alpha = 0.2) +
#  ggplot2::geom_point(data = hexagon, mapping = ggplot2::aes(x = x, y = y), alpha = 0.2)  +
#  ggplot2::geom_path(data = hexagon, mapping = ggplot2::aes(x = x, y = y), alpha = 0.2)  +
  ggplot2::theme(
    plot.margin = ggplot2::unit(c(-1, -1, -1, -1), "cm")
                 )

ggplot2::ggsave(plot = wall_of_software_1200x1200, filename = "wall_of_software_1200x1200.png", width = 1200, height = 1200, units = "px")

# 1200 x 1200 with text
hexagon <- data.frame(x = 9.1 * cos(theta), y = 9.1 * sin(theta))

wall_of_software_with_title_1200x1200 <-
  ggplot2::ggplot() +
  ggplot2::theme_void() +
  ggplot2::geom_rect(fill = "#009F96", mapping = ggplot2::aes(xmin = -10, xmax = 10, ymin = -10, ymax = 10)) +
  ggplot2::geom_text(mapping = ggplot2::aes(x = 0, y = 5.5, label = "pedbp")
                     , family = "serif"
                     , size = 32
                     ) +
  ggplot2::annotation_custom(heart_cuff_grob, xmin = -5.5, xmax = 5.5, ymin = -5.5 - 1.5, ymax = 5.5 - 1.5) +
  ggplot2::geom_point() +
#  ggplot2::geom_polygon(data = hexagon, mapping = ggplot2::aes(x = x, y = y), alpha = 0.2) +
#  ggplot2::geom_point(data = hexagon, mapping = ggplot2::aes(x = x, y = y), alpha = 0.2)  +
#  ggplot2::geom_path(data = hexagon, mapping = ggplot2::aes(x = x, y = y), alpha = 0.2)  +
  ggplot2::theme(
    plot.margin = ggplot2::unit(c(-1, -1, -1, -1), "cm")
                 )

ggplot2::ggsave(plot = wall_of_software_with_title_1200x1200
                , filename = "wall_of_software_with_title_1200x1200.png"
                , width = 1200, height = 1200, units = "px")

system(command = "convert wall_of_software_1200x1200.png -resize 600x600 wall_of_software_600x600.png")
system(command = "convert wall_of_software_with_title_1200x1200.png -resize 600x600 wall_of_software_with_title_600x600.png")
