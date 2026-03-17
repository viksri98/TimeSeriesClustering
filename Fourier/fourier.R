acq.freq <- 100
time <- 6
ts <- seq(0, time-1/acq.freq, 1/acq.freq)
f.0 <- 1/time
dc.component <- 1
component.freqs <- c(3, 7, 10)
component.strength <- c(1.5, .5, .75)

f <- function(t, w){
		dc.component + sum(component.strength * sin(component.freqs*w*t))
}

plot.fourier <- function(fourier.series, f.0, ts){
		w <- 2 * pi * f.0
		trajectory <- sapply(ts, function(t) fourier.series(t,w))
		plot(ts, trajectory, type='l', xlab='time', ylab='f(t)')
		abline(h=0,lty=3)
}

plot.fourier(f, f.0, ts=ts)

     
plot.frequency.spectrum <- function(X.k, xlimits=c(0,length(X.k))) {
  plot.data  <- cbind(0:(length(X.k)-1), Mod(X.k))

  # TODO: why this scaling is necessary?
  plot.data[2:length(X.k),2] <- 2*plot.data[2:length(X.k),2] 
  
  plot(plot.data, t="h", lwd=2, main="", 
       xlab="Frequency (Hz)", ylab="Strength", 
       xlim=xlimits, ylim=c(0,max(Mod(plot.data[,2]))))
}

# Plot the i-th harmonic
# Xk: the frequencies computed by the FFt
#  i: which harmonic
# ts: the sampling time points
# acq.freq: the acquisition rate
plot.harmonic <- function(Xk, i, ts, acq.freq, color="red") {
  Xk.h <- rep(0,length(Xk))
  Xk.h[i+1] <- Xk[i+1] # i-th harmonic
  harmonic.trajectory <- get.trajectory(Xk.h, ts, acq.freq=acq.freq)
  points(ts, harmonic.trajectory, type="l", col=color)
}

w <- 2 * pi * f.0
trajectory <- sapply(ts, function(t) f(t,w))
Xk <- fft(trajectory)
plot.frequency.spectrum(Xk, xlimits=c(0,20))

fftX <- Mod(t(fft(t(X))))
fftX[,2:140] <- 2 * fftX[,2:140]

k <- kmeans(as.data.frame(fftX), 5, nstart=10)
df$cluster <- k$cluster

df1 <- df[df$cluster==1,]
df2 <- df[df$cluster==2,]
df3 <- df[df$cluster==3,]
df4 <- df[df$cluster==4,]
df5 <- df[df$cluster==5,]

par(mfrow=c(5,1))
matplot(t(df1[,2:141]), type='l', col=df1$V1)
matplot(t(df2[,2:141]), type='l', col=df2$V1)
matplot(t(df3[,2:141]), type='l', col=df3$V1)
matplot(t(df4[,2:141]), type='l', col=df4$V1)
matplot(t(df5[,2:141]), type='l', col=df5$V1)

table(df$cluster, df$V1)

##
