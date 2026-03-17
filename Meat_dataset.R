# ---- load data ----
train <- read.table("/Users/toastymac/Desktop/Academia/Coursework/Stat 255 - Multivariate/Time Series/Meat/Meat_TRAIN.tsv")
test  <- read.table("/Users/toastymac/Desktop/Academia/Coursework/Stat 255 - Multivariate/Time Series/Meat/Meat_TEST.tsv")

# combine datasets
dat <- rbind(train, test)

# separate labels and time series
y <- dat[, 1]
X <- dat[, -1]

# make class a factor if helpful for plotting
y <- factor(y)

# ---- run k-means on raw time series ----
set.seed(1)
clusters <- kmeans(X, centers = 3, nstart = 50)

# ---- contingency table ----
tab <- table(cluster = clusters$cluster, label = y)
tab

# ---- visualize cluster centroids ----
matplot(
  t(clusters$centers),
  type = "l", lty = 1,
  main = "K-means cluster centroids",
  xlab = "Time", ylab = "Value"
)
legend("topright", legend = paste("Cluster", 1:3), lty = 1, col = 1:3, bty = "n")

library(ggplot2)

# ---- reshape data for plotting ----
df_long <- data.frame(
  series_id = rep(seq_len(nrow(X)), each = ncol(X)),
  class = rep(y, each = ncol(X)),
  time = rep(seq_len(ncol(X)), times = nrow(X)),
  value = as.vector(t(as.matrix(X)))
)

# ---- plot signals by true class ----
ggplot(df_long, aes(x = time, y = value, group = series_id)) +
  geom_line(color = "grey70", alpha = 0.10, linewidth = 0.25) +
  facet_wrap(~ class, ncol = 3) +
  labs(
    title = "Meat Signals by True Class",
    x = "Time",
    y = "Signal"
  ) +
  theme_minimal(base_size = 10, base_family = "serif")

# ---- map each cluster to its majority true label ----
tab <- table(cluster = clusters$cluster, label = y)
cluster_to_label <- apply(tab, 1, function(x) names(which.max(x)))

pred <- factor(cluster_to_label[clusters$cluster], levels = levels(y))
is_correct <- pred == y

# ---- long data with prediction correctness ----
df_long_pred <- data.frame(
  series_id = rep(seq_len(nrow(X)), each = ncol(X)),
  class = rep(y, each = ncol(X)),
  pred = rep(pred, each = ncol(X)),
  correct = rep(is_correct, each = ncol(X)),
  time = rep(seq_len(ncol(X)), times = nrow(X)),
  value = as.vector(t(as.matrix(X)))
)

df_long_pred$cluster <- factor(clusters$cluster[df_long_pred$series_id])

ggplot(df_long_pred,
       aes(x = time, y = value, group = series_id, color = cluster)) +
  geom_line(alpha = 0.12, linewidth = 0.25) +
  facet_wrap(~ class, ncol = 3) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Meat Signals by True Class",
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

#---

library(splines)
library(tidyverse)

# ---- load data ----
train <- read.table("/Users/toastymac/Desktop/Academia/Coursework/Stat 255 - Multivariate/Time Series/Meat/Meat_TRAIN.tsv")
test  <- read.table("/Users/toastymac/Desktop/Academia/Coursework/Stat 255 - Multivariate/Time Series/Meat/Meat_TEST.tsv")
df <- rbind(train, test)

# ---- split labels and signals ----
y <- factor(df[, 1])         # true class
X <- as.matrix(df[, -1])     # time series matrix
n <- nrow(X)
Tlen <- ncol(X)

# ---- B-spline basis ----
n_basis <- 6
degree <- 3

x <- seq(0, 1, length.out = Tlen)
knots <- seq(0, 1, length.out = n_basis - 2)

B <- bs(x, knots = knots, degree = degree)[, 1:n_basis]

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
    y = "Basis value",
    color = "Basis"
  ) +
  theme_minimal(base_size = 11, base_family = "serif")

# ---- compute B-spline coefficients ----
# each row of Bcoef contains coefficients for one signal
BtB_inv <- solve(t(B) %*% B)
Bcoef <- X %*% B %*% BtB_inv

# equivalently: t(solve(t(B)%*%B) %*% t(B) %*% t(X))
# but this matrix form is cleaner/faster

# ---- k-means on B-spline coefficients ----
set.seed(1)
km <- kmeans(Bcoef, centers = 3, nstart = 50)

# ---- contingency table ----
tab <- table(cluster = km$cluster, label = y)
tab

# ---- map cluster to majority true label ----
cluster_to_label <- apply(tab, 1, function(x) names(which.max(x)))
pred <- factor(cluster_to_label[km$cluster], levels = levels(y))
is_correct <- pred == y

# ---- long dataframe for original signals ----
df_long <- data.frame(
  series_id = rep(seq_len(n), each = Tlen),
  class = rep(y, each = Tlen),
  cluster = rep(factor(km$cluster), each = Tlen),
  pred = rep(pred, each = Tlen),
  correct = rep(is_correct, each = Tlen),
  time = rep(seq_len(Tlen), times = n),
  value = as.vector(t(X))
)

# ---- plot signals by true class ----
ggplot(df_long, aes(x = time, y = value, group = series_id)) +
  geom_line(color = "grey70", alpha = 0.10, linewidth = 0.25) +
  facet_wrap(~ class, ncol = 3) +
  labs(
    title = "Signals by True Class",
    x = "Time",
    y = "Signal"
  ) +
  theme_minimal(base_size = 10, base_family = "serif")

# ---- plot signals by true class, colored by B-spline cluster ----
ggplot(df_long,
       aes(x = time, y = value, group = series_id, color = cluster)) +
  geom_line(alpha = 0.12, linewidth = 0.25) +
  facet_wrap(~ class, ncol = 3) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Signals by True Class, Colored by B-spline Cluster",
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
centroid_curves <- km$centers %*% t(B)

centroid_df <- data.frame(
  time = rep(seq_len(Tlen), times = 3),
  value = as.vector(t(centroid_curves)),
  cluster = factor(rep(1:3, each = Tlen))
)

ggplot(centroid_df, aes(x = time, y = value, color = cluster)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Reconstructed Cluster Centroids from B-spline Coefficients",
    x = "Time",
    y = "Signal",
    color = "Cluster"
  ) +
  theme_minimal(base_size = 11, base_family = "serif")

##

library(tidyverse)

# ---- load data ----
train <- read.table("/Users/toastymac/Desktop/Academia/Coursework/Stat 255 - Multivariate/Time Series/Meat/Meat_TRAIN.tsv")
test  <- read.table("/Users/toastymac/Desktop/Academia/Coursework/Stat 255 - Multivariate/Time Series/Meat/Meat_TEST.tsv")
df <- rbind(train, test)

# ---- split labels and signals ----
y <- factor(df[, 1])         # true class
X <- as.matrix(df[, -1])     # time series matrix
n <- nrow(X)
Tlen <- ncol(X)

# ---- Fourier transform features ----
fftX <- t(fft(t(X)))         # row-wise FFT
fft_mod <- Mod(fftX)

# double non-DC frequencies
if (Tlen > 1) {
  fft_mod[, 2:Tlen] <- 2 * fft_mod[, 2:Tlen]
}

# keep only first half of spectrum (real-valued signal symmetry)
n_freq <- floor(Tlen / 2)
fourier_features <- fft_mod[, 1:n_freq]

# ---- k-means on Fourier features ----
set.seed(1)
km <- kmeans(fourier_features, centers = 3, nstart = 50)

# ---- contingency table ----
tab <- table(cluster = km$cluster, label = y)
tab

# ---- map each cluster to majority true label ----
cluster_to_label <- apply(tab, 1, function(x) names(which.max(x)))
pred <- factor(cluster_to_label[km$cluster], levels = levels(y))
is_correct <- pred == y

# ---- long dataframe for plotting original signals ----
df_long <- data.frame(
  series_id = rep(seq_len(n), each = Tlen),
  class = rep(y, each = Tlen),
  cluster = rep(factor(km$cluster), each = Tlen),
  pred = rep(pred, each = Tlen),
  correct = rep(is_correct, each = Tlen),
  time = rep(seq_len(Tlen), times = n),
  value = as.vector(t(X))
)

# ---- plot signals by true class ----
ggplot(df_long, aes(x = time, y = value, group = series_id)) +
  geom_line(color = "grey70", alpha = 0.10, linewidth = 0.25) +
  facet_wrap(~ class, ncol = 3) +
  labs(
    title = "Signals by True Class",
    x = "Time",
    y = "Signal"
  ) +
  theme_minimal(base_size = 10, base_family = "serif")

# ---- plot signals by true class, colored by Fourier cluster ----
ggplot(df_long,
       aes(x = time, y = value, group = series_id, color = cluster)) +
  geom_line(alpha = 0.12, linewidth = 0.25) +
  facet_wrap(~ class, ncol = 3) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Signals by True Class, Colored by Fourier Cluster",
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
centroid_fft <- matrix(0+0i, nrow = 3, ncol = Tlen)

# use mean complex FFT within each cluster
for (j in 1:3) {
  idx <- which(km$cluster == j)
  centroid_fft[j, ] <- colMeans(fftX[idx, , drop = FALSE])
}

centroid_curves <- t(apply(centroid_fft, 1, function(z) Re(fft(z, inverse = TRUE) / length(z))))

centroid_df <- data.frame(
  time = rep(seq_len(Tlen), times = 3),
  value = as.vector(t(centroid_curves)),
  cluster = factor(rep(1:3, each = Tlen))
)

ggplot(centroid_df, aes(x = time, y = value, color = cluster)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Reconstructed Cluster Centroids from Fourier Features",
    x = "Time",
    y = "Signal",
    color = "Cluster"
  ) +
  theme_minimal(base_size = 11, base_family = "serif")
