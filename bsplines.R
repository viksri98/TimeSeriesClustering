library(splines)
library(tidyverse)

n.knots <- 6
x <- seq(0,1,length.out=100)
knots <- seq(0,1,length.out=n.knots-2)

B <- bs(x, knots=knots, degree=3)[,1:n.knots]

B%>%
  as.data.frame()%>%
  mutate(rw=row_number())%>%
  gather(key='key',value='value',`1`:`6`)%>%
  ggplot(aes(x=rw,y=value,col=key))+geom_line(aes(group=key))+
  labs(title='B-Spline Basis with 6 knots and 3rd-Degree Polynomials')

B <- bs(seq(0,1,length.out=140), knots=knots, degree=3)[,1:n.knots]
df <- read.table("ECG5000_TRAIN.tsv")
par(mfrow=c(5,1))
plot(t(df[100,2:141]), type='l', ylab="1")
plot(t(df[2628,2:141]), type='l', ylab="2")
plot(t(df[2999,2:141]), type='l', ylab="3")
plot(t(df[2666,2:141]), type='l', ylab="4")
plot(t(df[2814,2:141]), type='l', ylab="5")

X <- df[,2:141]
Bcoef <- matrix(0, dim(X)[1], n.knots)
for(i in 1:dim(X)[1]){
		Bcoef[i,] <- solve(t(B)%*%B)%*%t(B)%*%t(as.matrix(X[i,]))
}
k <- kmeans(as.data.frame(Bcoef), 5, nstart=10)
df$cluster <- k$cluster

df1 <- df[df$cluster==1,]
df2 <- df[df$cluster==2,]
df3 <- df[df$cluster==3,]
df4 <- df[df$cluster==4,]
df5 <- df[df$cluster==5,]

par(mfrow=c(5,1))
matplot(t(df1[,2:141], type='l', col=df1$V1))
matplot(t(df2[,2:141], type='l', col=df2$V1))
matplot(t(df3[,2:141], type='l', col=df3$V1))
matplot(t(df4[,2:141], type='l', col=df4$V1))
matplot(t(df5[,2:141], type='l', col=df5$V1))

c(rnorm(1400, B[,2], .1), rnorm(1400, B[,4], .1)) |> matrix(ncol=140, byrow=T) -> data
matplot(t(data, type='l'))
Bcoef <- matrix(0, dim(data)[1], n.knots)
for(i in 1:dim(data)[1]){
		Bcoef[i,] <- solve(t(B)%*%B)%*%t(B)%*%as.matrix(data[i,])
}
k <- kmeans(as.data.frame(Bcoef), 2, nstart=10)

