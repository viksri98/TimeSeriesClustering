#RandomNet Output:
clusters <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1) + 1
train <- read.table("Coffee_TRAIN.tsv")
test <- read.table("Coffee_TEST.tsv")
df <- rbind(train, test)

y <- df[,1]
X <- df[,-1]
library(ggplot2)

df_long <- data.frame(
		series_id = rep(seq_len(nrow(X)), each=ncol(X)),
		class = rep(y, each=ncol(X)),
		time = rep(seq_len(ncol(X)), times=nrow(X)),
		value = as.vector(t(as.matrix(X))),
		cluster = rep(clusters, each=ncol(X))
)

ggplot(df_long, aes(x=time, y=value, group=series_id, color=cluster)) +
		geom_line(linewidth=0.25) +
		facet_wrap(~ class, ncol=2) +
		labs(
			 title="Coffee Beans by Class",
			 x = "Time",
			 y = "Signal"
		) +
		theme_minimal(base_size=10, base_family="serif")

df1 <- df[df$cluster==1,]
df2 <- df[df$cluster==2,]

par(mfrow=c(2,1))
matplot(t(df1[,2:287]), type='l', col=df1$V1+1)
matplot(t(df2[,2:287]), type='l', col=df2$V1+1)

table(df$cluster, df$V1)


