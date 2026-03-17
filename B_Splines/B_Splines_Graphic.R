library(splines)
library(ggplot2)

# x grid
x <- seq(1, 100, length.out = 500)

# B-spline basis: 6 df, cubic
B <- bs(x, df = 6, degree = 3, intercept = TRUE)

# reshape for ggplot
df <- data.frame(
  x = rep(x, ncol(B)),
  value = as.vector(B),
  basis = factor(rep(seq_len(ncol(B)), each = length(x)))
)

ggplot(df, aes(x = x, y = value, color = basis)) +
  geom_line(linewidth = 1.1) +
  labs(
    title = "B-Spline Basis Functions",
    subtitle = "6 basis functions, cubic splines",
    x = "Time",
    y = "Basis value",
    color = "Basis"
  ) +
  theme_classic(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray30"),
    legend.position = "right",
    legend.title = element_text(face = "bold")
  )