# utils.R

format_axes <- function(decimals) {
  function(x) format(x, nsmall = decimals, scientific = FALSE, digits = 1)
}

library(ggplot2)
blank_plot <- ggplot(data.frame(x = 0.5, y = 0.5), aes(x, y)) +
  geom_text(label = "No Data", size = 24) +
  theme_void()
