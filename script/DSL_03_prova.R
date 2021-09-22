
library(xts)
library(forecast)
library(smooth)
library(fpp2)
library(ggplot2)
library(dplyr)

u6_daily_ma$seriestorica <- xts(u6_daily_ma$ENERGIA,u6_daily_ma$DATA)

# Stima del trend con finestre di media mobile di varie ampiezze ----------

fltr <- c(1/2, rep(1, times = 30), 1/2)/31
u6_trend <- stats::filter(as.vector(u6_daily_ma$seriestorica), filter = fltr, method = "convo", 
                          sides = 2)
par(mfrow=c(2,1))
plot.ts(u6_trend, ylab = "Trend", cex = 1,)

plot.ts(as.vector(u6_daily_ma$seriestorica))

## proviamo con una finestra minore

fltr <- c(1/2, rep(1, times = 365), 1/2)/366
u6_trend <- stats::filter(as.vector(u6_daily_ma$seriestorica), filter = fltr, method = "convo", 
                          sides = 2)
par(mfrow=c(2,1))
plot.ts(u6_trend, ylab = "Trend", cex = 1)

plot.ts(as.vector(u6_daily_ma$seriestorica))

## Infine una via di mezzo

fltr <- c(1/2, rep(1, times = 90), 1/2)/91
u6_trend <- stats::filter(as.vector(u6_daily_ma$seriestorica), filter = fltr, method = "convo", 
                          sides = 2)
par(mfrow=c(2,1))
plot.ts(u6_trend, ylab = "Trend", cex = 1)

plot.ts(as.vector(u6_daily_ma$seriestorica))

# Varie stime di stagionalità ---------------------------------------------
u6_seas <- as.vector(u6_daily_ma$seriestorica) - as.vector(u6_trend)

par(mfrow=c(1,1))

plot.ts(u6_seas, ylab = "Seasonal effect", xlab = "", cex = 1)+
  abline(v=c(365,730),col='red',lty='dashed')
  

## stagionalità ottenuta seguendo la guida, decisamente confusa

## stagionalità con aggregazione mensile e giornaliera

## length of ts
ll <- length(u6_seas)
## frequency (ie, 12)
ff <- 52 ## trend settimanale
## number of periods (years); %/% is integer division
periods <- ll%/%ff
## index of cumulative week
index <- seq(1, ll, by = ff) - 1
## get mean by week
mm <- numeric(ff)
for (i in 1:ff) {
  mm[i] <- mean(u6_seas[index + i], na.rm = TRUE)
}
## subtract mean to make overall mean = 0
mm <- mm - mean(mm)

plot.ts(mm, ylab = "Seasonal effect", xlab = "Week", cex = 1)

## length of ts
ll <- length(u6_seas)
## frequency (ie, 12)
ff <- 36 ## trend mensile
## number of periods (years); %/% is integer division
periods <- ll%/%ff
## index of cumulative Month
index <- seq(1, ll, by = ff) - 1
## get mean by Month
mm <- numeric(ff)
for (i in 1:ff) {
  mm[i] <- mean(u6_seas[index + i], na.rm = TRUE)
}
## subtract mean to make overall mean = 0
mm <- mm - mean(mm)

plot.ts(mm, ylab = "Seasonal effect", xlab = "Month", cex = 1)

# Decompose e rumore bianco -----------------------------------------------

## ottengo il rumore bianco, il trend e la stagionalità con decompose

## con msts indico vari livelli di stagionalità e questi influenzano la stima della serie

## stagionalità settimanale e annuale, additiva

x<-msts(u6_daily_ma$ENERGIA, seasonal.periods=c(7,365),  start=2018)
xd<-decompose(x, type = "additive")  
plot(xd, yax.flip = TRUE)

## stagionalità giornaliera, settimanale e annuale

x<-msts(u6_daily_ma$ENERGIA, seasonal.periods=c(7,365),  start=2018)
xd<-decompose(x, type = "multiplicative")  
plot(xd, yax.flip = TRUE)
