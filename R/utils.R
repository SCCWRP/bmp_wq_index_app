# utils.R
round2 <- function(x, digits) {
  posneg = sign(x)
  z = abs(x)*10^digits
  z = z + 0.5 + sqrt(.Machine$double.eps)
  z = trunc(z)
  z = z/10^digits
  z*posneg
}

format_axes <- function(decimals) {
  function(x) format(x, nsmall = decimals, scientific = FALSE, digits = 1)
}

blank_plot <- ggplot(data.frame(x = 0.5, y = 0.5), aes(x, y)) +
  geom_text(label = "No Data", size = 24) +
  theme_void()
