library(hexSticker)

s <- sticker(
    subplot = "heart_cuff.png",
    s_x = 1.3,
    s_y = 0.65,
    s_width = 1.5,
    s_height = 1.5,
    package = 'pedbp',
    p_x = 1,
    p_y = 1.7,
    p_color = "#111111",
    p_family = "serif",
    p_fontface = "plain",
    p_size = 18,
    h_size = 1.2,
    h_fill = "#A2AAAD",
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

plot(s)
