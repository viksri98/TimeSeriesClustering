#RandomNet Output:
clusters <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
train <- read.table("Coffee_TRAIN.tsv")
test <- read.table("Coffee_TEST.tsv")
df <- rbind(train, test)
df$cluster <- clusters + 1

df1 <- df[df$cluster==1,]
df2 <- df[df$cluster==2,]

par(mfrow=c(2,1))
matplot(t(df1[,2:287]), type='l', col=df1$V1+1)
matplot(t(df2[,2:287]), type='l', col=df2$V1+1)

table(df$cluster, df$V1)


