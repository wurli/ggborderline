devtools::load_all(".")

library(ggplot2)

set.seed(213)

plot_data <- data.frame(
  x = rep(1:10, 2),
  y = cumsum(runif(20, -1, 1)),
  colour = rep(letters[1:2], each = 10)
)

sticker_plot <- ggplot(plot_data, aes(x, y, colour = colour)) +
  geom_borderline(size = 1, border_size = .5, lineend = "round") +
  theme_minimal() +
  labs(x = NULL, y = NULL) +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    panel.grid.major = element_line(colour = "grey70", size = 0.1),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "grey60")
  )

hexSticker::sticker(
  sticker_plot,
  package = "ggborderline",
  s_x = 1, s_y = 0.8, s_width = 1.5, s_height = 0.7,
  p_size = 6.5,
  p_color = colorspace::darken("steelblue", .7),
  h_fill = "grey98",
  h_color = colorspace::darken("steelblue", .8),
  url = "github.com/wurli/ggborderline",
  filename = "man/figures/logo.png",
  dpi = 120
)

