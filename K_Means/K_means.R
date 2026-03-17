# ---- load data ----
train <- read.table("/Users/toastymac/Desktop/Academia/Coursework/Stat 255 - Multivariate/Time Series/ECG5000/ECG5000_TEST.tsv")
test  <- read.table("/Users/toastymac/Desktop/Academia/Coursework/Stat 255 - Multivariate/Time Series/ECG5000/ECG5000_TRAIN.tsv")

# combine datasets
dat <- rbind(train, test)

# separate labels and time series
y <- dat[,1]
X <- dat[,-1]

# ---- run k-means on raw time series ----
set.seed(1)
clusters <- kmeans(X, centers = 5, nstart = 50)

# ---- contingency table ----
tab <- table(cluster = clusters$cluster, label = y)
tab

# ---- visualize cluster centroids ----
matplot(t(clusters$centers), type="l", lty=1,
        main="K-means cluster centroids",
        xlab="Time", ylab="Value")

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
  facet_wrap(~ class, ncol = 5) +
  labs(
    title = "ECG5000 Signals by Class",
    x = "Time",
    y = "Signal"
  ) +
  theme_minimal(base_size = 10, base_family = "serif")

# ---- map each cluster to its majority true label ----
tab <- table(cluster = clusters$cluster, label = y)
cluster_to_label <- apply(tab, 1, function(x) as.numeric(names(which.max(x))))

pred <- cluster_to_label[clusters$cluster]
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
  facet_wrap(~ class, ncol = 5) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "ECG5000 Signals by True Class",
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
