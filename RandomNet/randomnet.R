#RandomNet Output:
clusters <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1) + 1
train <- read.table("Coffee_TRAIN.tsv")
test <- read.table("Coffee_TEST.tsv")
df <- rbind(train, test)

y <- df[,1]
X <- df[,-1]
library(ggplot2)

# Df to long for plotting
df_long <- data.frame(
		series_id = rep(seq_len(nrow(X)), each=ncol(X)),
		class = rep(y, each=ncol(X)),
		time = rep(seq_len(ncol(X)), times=nrow(X)),
		value = as.vector(t(as.matrix(X))),
		cluster = factor(rep(clusters, each=ncol(X)))
)

# Random clusters plot
ggplot(df_long, aes(x=time, y=value, group=series_id, color=cluster)) +
		geom_line(linewidth=0.25) +
		facet_wrap(~ class, ncol=2) +
		labs(
			 title="Coffee Beans by Class",
			 x = "Time",
			 y = "Signal"
		) +
		theme_minimal(base_size=10, base_family="serif") +
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

ggsave("randomnet_clusters.jpg", width=5, height=5, units="in")

df1 <- df[df$cluster==1,]
df2 <- df[df$cluster==2,]

par(mfrow=c(2,1))
matplot(t(df1[,2:287]), type='l', col=df1$V1+1)
matplot(t(df2[,2:287]), type='l', col=df2$V1+1)

table(df$cluster, df$V1)


