# =========================================================
# WORMS DATA: RAW TIME SERIES, B-SPLINES, AND FOURIER
# =========================================================

library(ggplot2)
library(tidyverse)
library(splines)

# =========================================================
# ---- load data ----
# =========================================================
train <- read.table("/Users/toastymac/Desktop/Academia/Coursework/Stat 255 - Multivariate/Time Series/Worms/Worms_TRAIN.tsv")
test  <- read.table("/Users/toastymac/Desktop/Academia/Coursework/Stat 255 - Multivariate/Time Series/Worms/Worms_TEST.tsv")

dat <- rbind(train, test)

# labels and time series
y <- factor(dat[, 1])
X <- as.matrix(dat[, -1])

n <- nrow(X)
Tlen <- ncol(X)

# =========================================================
# 1. RAW TIME SERIES K-MEANS
# =========================================================

set.seed(1)
km_raw <- kmeans(X, centers = 5, nstart = 50)

# ---- contingency table ----
tab_raw <- table(cluster = km_raw$cluster, label = y)
tab_raw

# ---- visualize cluster centroids ----
matplot(
  t(km_raw$centers),
  type = "l", lty = 1,
  main = "Worms: K-means Cluster Centroids (Raw Time Series)",
  xlab = "Time", ylab = "Value"
)
legend("topright", legend = paste("Cluster", 1:5), lty = 1, col = 1:5, bty = "n")

# ---- reshape data for plotting ----
df_long_raw <- data.frame(
  series_id = rep(seq_len(n), each = Tlen),
  class = rep(y, each = Tlen),
  time = rep(seq_len(Tlen), times = n),
  value = as.vector(t(X))
)

# ---- plot signals by true class ----
ggplot(df_long_raw, aes(x = time, y = value, group = series_id)) +
  geom_line(color = "grey70", alpha = 0.10, linewidth = 0.25) +
  facet_wrap(~ class, ncol = 5) +
  labs(
    title = "Worms Signals by True Class",
    x = "Time",
    y = "Signal"
  ) +
  theme_minimal(base_size = 10, base_family = "serif")

# ---- map each cluster to its majority true label ----
cluster_to_label_raw <- apply(tab_raw, 1, function(x) names(which.max(x)))
pred_raw <- factor(cluster_to_label_raw[km_raw$cluster], levels = levels(y))
is_correct_raw <- pred_raw == y

# ---- long data with prediction correctness ----
df_long_raw_pred <- data.frame(
  series_id = rep(seq_len(n), each = Tlen),
  class = rep(y, each = Tlen),
  pred = rep(pred_raw, each = Tlen),
  correct = rep(is_correct_raw, each = Tlen),
  time = rep(seq_len(Tlen), times = n),
  value = as.vector(t(X))
)

df_long_raw_pred$cluster <- factor(km_raw$cluster[df_long_raw_pred$series_id])

# ---- plot signals by true class, colored by raw cluster ----
ggplot(df_long_raw_pred,
       aes(x = time, y = value, group = series_id, color = cluster)) +
  geom_line(alpha = 0.12, linewidth = 0.25) +
  facet_wrap(~ class, ncol = 5) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Worms Signals by True Class, Colored by Raw K-means Cluster",
    x = "Time",
    y = "Signal",
    color = "Cluster"
  ) +
  theme_minimal(base_size = 10, base_family = "serif") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(size = 9, face = "bold", family = "serif"),
    plot.title = element_text(size = 11, face = "bold", family = "serif", hjust = 0.5),
    axis.title = element_text(size = 9, family = "serif"),
    axis.text = element_text(size = 8, family = "serif"),
    legend.position = "bottom",
    legend.title = element_text(size = 9, face = "bold", family = "serif"),
    legend.text = element_text(size = 8, family = "serif")
  ) +
  guides(
    color = guide_legend(
      override.aes = list(linewidth = 3, alpha = 1)
    )
  )

# =========================================================
# 2. B-SPLINE K-MEANS
# =========================================================

# ---- B-spline basis ----
n_basis <- 6
degree <- 3

x_grid <- seq(0, 1, length.out = Tlen)
knots <- seq(0, 1, length.out = n_basis - 2)

B <- bs(x_grid, knots = knots, degree = degree)[, 1:n_basis]

# ---- visualize basis functions ----
B %>%
  as.data.frame() %>%
  mutate(time = row_number()) %>%
  pivot_longer(cols = -time, names_to = "basis", values_to = "value") %>%
  ggplot(aes(x = time, y = value, color = basis)) +
  geom_line(linewidth = 0.9) +
  labs(
    title = "B-spline Basis Functions",
    x = "Time",
    y = "Basis Value",
    color = "Basis"
  ) +
  theme_minimal(base_size = 11, base_family = "serif")

# ---- compute B-spline coefficients ----
BtB_inv <- solve(t(B) %*% B)
Bcoef <- X %*% B %*% BtB_inv

# ---- k-means on B-spline coefficients ----
set.seed(1)
km_bs <- kmeans(Bcoef, centers = 5, nstart = 50)

# ---- contingency table ----
tab_bs <- table(cluster = km_bs$cluster, label = y)
tab_bs

# ---- map cluster to majority true label ----
cluster_to_label_bs <- apply(tab_bs, 1, function(x) names(which.max(x)))
pred_bs <- factor(cluster_to_label_bs[km_bs$cluster], levels = levels(y))
is_correct_bs <- pred_bs == y

# ---- long dataframe for plotting original signals ----
df_long_bs <- data.frame(
  series_id = rep(seq_len(n), each = Tlen),
  class = rep(y, each = Tlen),
  cluster = rep(factor(km_bs$cluster), each = Tlen),
  pred = rep(pred_bs, each = Tlen),
  correct = rep(is_correct_bs, each = Tlen),
  time = rep(seq_len(Tlen), times = n),
  value = as.vector(t(X))
)

# ---- plot signals by true class ----
ggplot(df_long_bs, aes(x = time, y = value, group = series_id)) +
  geom_line(color = "grey70", alpha = 0.10, linewidth = 0.25) +
  facet_wrap(~ class, ncol = 5) +
  labs(
    title = "Worms Signals by True Class",
    x = "Time",
    y = "Signal"
  ) +
  theme_minimal(base_size = 10, base_family = "serif")

# ---- plot signals by true class, colored by B-spline cluster ----
ggplot(df_long_bs,
       aes(x = time, y = value, group = series_id, color = cluster)) +
  geom_line(alpha = 0.12, linewidth = 0.25) +
  facet_wrap(~ class, ncol = 5) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Worms Signals by True Class, Colored by B-spline Cluster",
    x = "Time",
    y = "Signal",
    color = "Cluster"
  ) +
  theme_minimal(base_size = 10, base_family = "serif") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(size = 9, face = "bold", family = "serif"),
    plot.title = element_text(size = 11, face = "bold", family = "serif", hjust = 0.5),
    axis.title = element_text(size = 9, family = "serif"),
    axis.text = element_text(size = 8, family = "serif"),
    legend.position = "bottom",
    legend.title = element_text(size = 9, face = "bold", family = "serif"),
    legend.text = element_text(size = 8, family = "serif")
  ) +
  guides(
    color = guide_legend(
      override.aes = list(linewidth = 3, alpha = 1)
    )
  )

# ---- cluster centroids in coefficient space, reconstructed to signal space ----
centroid_curves_bs <- km_bs$centers %*% t(B)

centroid_df_bs <- data.frame(
  time = rep(seq_len(Tlen), times = 5),
  value = as.vector(t(centroid_curves_bs)),
  cluster = factor(rep(1:5, each = Tlen))
)

ggplot(centroid_df_bs, aes(x = time, y = value, color = cluster)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Reconstructed Cluster Centroids from B-spline Coefficients",
    x = "Time",
    y = "Signal",
    color = "Cluster"
  ) +
  theme_minimal(base_size = 11, base_family = "serif")

# =========================================================
# 3. FOURIER K-MEANS
# =========================================================

# ---- Fourier transform features ----
fftX <- t(fft(t(X)))     # row-wise FFT
fft_mod <- Mod(fftX)

# keep only first half of spectrum
n_freq <- floor(Tlen / 2) + 1
fourier_features <- fft_mod[, 1:n_freq]

# double non-DC/non-Nyquist terms for amplitude-style scaling
if (Tlen %% 2 == 0) {
  if (n_freq > 2) {
    fourier_features[, 2:(n_freq - 1)] <- 2 * fourier_features[, 2:(n_freq - 1)]
  }
} else {
  if (n_freq > 1) {
    fourier_features[, 2:n_freq] <- 2 * fourier_features[, 2:n_freq]
  }
}

# standardize features before k-means
fourier_features <- scale(fourier_features)

# ---- k-means on Fourier features ----
set.seed(1)
km_fourier <- kmeans(fourier_features, centers = 5, nstart = 50)

# ---- contingency table ----
tab_fourier <- table(cluster = km_fourier$cluster, label = y)
tab_fourier

# ---- map each cluster to majority true label ----
cluster_to_label_fourier <- apply(tab_fourier, 1, function(x) names(which.max(x)))
pred_fourier <- factor(cluster_to_label_fourier[km_fourier$cluster], levels = levels(y))
is_correct_fourier <- pred_fourier == y

# ---- long dataframe for plotting original signals ----
df_long_fourier <- data.frame(
  series_id = rep(seq_len(n), each = Tlen),
  class = rep(y, each = Tlen),
  cluster = rep(factor(km_fourier$cluster), each = Tlen),
  pred = rep(pred_fourier, each = Tlen),
  correct = rep(is_correct_fourier, each = Tlen),
  time = rep(seq_len(Tlen), times = n),
  value = as.vector(t(X))
)

# ---- plot signals by true class ----
ggplot(df_long_fourier, aes(x = time, y = value, group = series_id)) +
  geom_line(color = "grey70", alpha = 0.10, linewidth = 0.25) +
  facet_wrap(~ class, ncol = 5) +
  labs(
    title = "Worms Signals by True Class",
    x = "Time",
    y = "Signal"
  ) +
  theme_minimal(base_size = 10, base_family = "serif")

# ---- plot signals by true class, colored by Fourier cluster ----
ggplot(df_long_fourier,
       aes(x = time, y = value, group = series_id, color = cluster)) +
  geom_line(alpha = 0.12, linewidth = 0.25) +
  facet_wrap(~ class, ncol = 5) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Worms Signals by True Class, Colored by Fourier Cluster",
    x = "Time",
    y = "Signal",
    color = "Cluster"
  ) +
  theme_minimal(base_size = 10, base_family = "serif") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(size = 9, face = "bold", family = "serif"),
    plot.title = element_text(size = 11, face = "bold", family = "serif", hjust = 0.5),
    axis.title = element_text(size = 9, family = "serif"),
    axis.text = element_text(size = 8, family = "serif"),
    legend.position = "bottom",
    legend.title = element_text(size = 9, face = "bold", family = "serif"),
    legend.text = element_text(size = 8, family = "serif")
  ) +
  guides(
    color = guide_legend(
      override.aes = list(linewidth = 3, alpha = 1)
    )
  )

# ---- reconstruct cluster centroids from Fourier space ----
centroid_fft <- matrix(0 + 0i, nrow = 5, ncol = Tlen)

for (j in 1:5) {
  idx <- which(km_fourier$cluster == j)
  centroid_fft[j, ] <- colMeans(fftX[idx, , drop = FALSE])
}

centroid_curves_fourier <- t(
  apply(centroid_fft, 1, function(z) Re(fft(z, inverse = TRUE) / length(z)))
)

centroid_df_fourier <- data.frame(
  time = rep(seq_len(Tlen), times = 5),
  value = as.vector(t(centroid_curves_fourier)),
  cluster = factor(rep(1:5, each = Tlen))
)

ggplot(centroid_df_fourier, aes(x = time, y = value, color = cluster)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Reconstructed Cluster Centroids from Fourier Features",
    x = "Time",
    y = "Signal",
    color = "Cluster"
  ) +
  theme_minimal(base_size = 11, base_family = "serif")

