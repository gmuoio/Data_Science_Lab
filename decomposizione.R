## https://nwfsc-timeseries.github.io/atsa-labs/sec-tslab-decomposition-of-time-series.html

library(xts)
library(forecast)
library(smooth)
library(fpp2)
library(ggplot2)
library(dplyr)

u6$seriestorica<-xts(u6$CONSUMO_ATTIVA_PRELEVATA, u6$timestamp)

# Stima del trend con finestre di media mobile di varie ampiezze ----------

fltr <- c(1/2, rep(1, times = 10000), 1/2)/10001
u6_trend <- stats::filter(as.vector(u6$seriestorica), filter = fltr, method = "convo", 
                          sides = 2)
par(mfrow=c(2,1))
plot.ts(u6_trend, ylab = "Trend", cex = 1, ylim=c(0,250))

plot.ts(as.vector(u6$seriestorica), ylim=c(0,250))

## proviamo con una finestra minore

fltr <- c(1/2, rep(1, times = 1000), 1/2)/1001
u6_trend <- stats::filter(as.vector(u6$seriestorica), filter = fltr, method = "convo", 
                          sides = 2)
par(mfrow=c(2,1))
plot.ts(u6_trend, ylab = "Trend", xlab="index", cex = 1, ylim=c(0,250))

plot.ts(as.vector(u6$seriestorica), ylab="serie storica", xlab="index", ylim=c(0,250))

## Infine una via di mezzo

fltr <- c(1/2, rep(1, times = 5000), 1/2)/5001
u6_trend <- stats::filter(as.vector(u6$seriestorica), filter = fltr, method = "convo", 
                          sides = 2)
par(mfrow=c(2,1))
plot.ts(u6_trend, ylab = "Trend", cex = 1, ylim=c(0,250))

plot.ts(as.vector(u6$seriestorica), ylim=c(0,250))

# Varie stime di stagionalità ---------------------------------------------

u6_seas <- as.vector(u6$seriestorica)- as.vector(u6_trend)

par(mfrow=c(1,1))

plot.ts(u6_seas, ylab = "Seasonal effect", xlab = "", cex = 1) 
## stagionalità ottenuta seguendo la guida, decisamente confusa

## stagionalità con aggregazione mensile e giornaliera

## length of ts
ll <- length(u6_seas)
## frequency (ie, 12)
ff <- 1096 ## trend giornaliero
## number of periods (years); %/% is integer division
periods <- ll%/%ff
## index of cumulative Day
index <- seq(1, ll, by = ff) - 1
## get mean by Day
mm <- numeric(ff)
for (i in 1:ff) {
  mm[i] <- mean(u6_seas[index + i], na.rm = TRUE)
}
## subtract mean to make overall mean = 0
mm <- mm - mean(mm)

plot.ts(mm, ylab = "Seasonal effect", xlab = "Day", cex = 1)

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

## Quanto segue non credo sia proprio corretto ma può essere informativo
## andamento medio all'interno di un periodo di tempo

## andamento all'interno della giornata

day_seas<-u6 %>% group_by(ORA)%>% summarise(media=mean(CONSUMO_ATTIVA_PRELEVATA))
day_seas$ORA<-as.numeric(day_seas$ORA)/10000
plot(day_seas$ORA,day_seas$media , type="l", xlab = "Ora", ylab="consumo")

## andamento all'interno della settimana

u6$weekday<-weekdays(u6$DATA)
week_seas<-u6 %>% group_by(weekday)%>% summarise(media=mean(CONSUMO_ATTIVA_PRELEVATA))
week_seas$weekday<-factor(week_seas$weekday, c("lunedì","martedì","mercoledì","giovedì","venerdì","sabato","domenica"))
week_seas<-week_seas[order(week_seas$weekday),]
qplot(week_seas$weekday, week_seas$media, geom="line",group=1)

## andamento all'interno dell'anno

u6$month<-months.Date(u6$DATA)
month_seas<-u6 %>% group_by(month)%>% summarise(media=mean(CONSUMO_ATTIVA_PRELEVATA))
month_seas$month<-factor(month_seas$month, c("gennaio","febbraio","marzo","aprile","maggio","giugno","luglio",
                                             "agosto","settembre","ottobre","novembre","dicembre"))
month_seas<-month_seas[order(month_seas$month),]
qplot(month_seas$month, month_seas$media, geom="line",group=1)

# Decompose e rumore bianco -----------------------------------------------

## ottengo il rumore bianco, il trend e la stagionalità con decompose. 
## con msts indico vari livelli di stagionalità e questi influenzano la stima della serie

## stagionalità giornaliera e settimanale

x<- msts(u6$CONSUMO_ATTIVA_PRELEVATA, seasonal.periods=c(96,96*7),  start=2018)
xd<-decompose(x, type = "additive")  
plot(xd, yax.flip = TRUE)

## stagionalità giornaliera, settimanale e annuale

x<- msts(u6$CONSUMO_ATTIVA_PRELEVATA, seasonal.periods=c(96,96*7, 96*365),  start=2018)
xd<-decompose(x, type = "additive")  
plot(xd, yax.flip = TRUE)


# Analogamente per l'u1 ---------------------------------------------------

u1$seriestorica<-xts(u1$CONSUMO_ATTIVA_PRELEVATA, u1$timestamp)

# Stima del trend con finestre di media mobile di varie ampiezze ----------

fltr <- c(1/2, rep(1, times = 10000), 1/2)/10001
u1_trend <- stats::filter(as.vector(u1$seriestorica), filter = fltr, method = "convo", 
                          sides = 2)
par(mfrow=c(2,1))
plot.ts(u1_trend, ylab = "Trend", xlab="index", cex = 1, ylim=c(0,200))

plot.ts(as.vector(u1$seriestorica), ylab="serie storica", xlab="index", ylim=c(0,200))


## proviamo con una finestra minore

fltr <- c(1/2, rep(1, times = 1000), 1/2)/1001
u1_trend <- stats::filter(as.vector(u1$seriestorica), filter = fltr, method = "convo", 
                          sides = 2)
par(mfrow=c(2,1))
plot.ts(u1_trend, ylab = "Trend", cex = 1, ylim=c(0,200))

plot.ts(as.vector(u1$seriestorica), ylim=c(0,200))

## Infine una via di mezzo

fltr <- c(1/2, rep(1, times = 5000), 1/2)/5001
u1_trend <- stats::filter(as.vector(u1$seriestorica), filter = fltr, method = "convo", 
                          sides = 2)
par(mfrow=c(2,1))
plot.ts(u1_trend, ylab = "Trend", cex = 1, ylim=c(0,200))

plot.ts(as.vector(u1$seriestorica), ylim=c(0,200))

# Varie stime di stagionalità ---------------------------------------------

u1_seas <- as.vector(u1$seriestorica)- as.vector(u1_trend)

par(mfrow=c(1,1))

plot.ts(u1_seas, ylab = "Seasonal effect", xlab = "", cex = 1) 
## stagionalità ottenuta seguendo la guida, decisamente confusa

## stagionalità con aggregazione mensile e giornaliera

## length of ts
ll <- length(u1_seas)
## frequency (ie, 12)
ff <- 1096 ## trend giornaliero
## number of periods (years); %/% is integer division
periods <- ll%/%ff
## index of cumulative Day
index <- seq(1, ll, by = ff) - 1
## get mean by Day
mm <- numeric(ff)
for (i in 1:ff) {
  mm[i] <- mean(u1_seas[index + i], na.rm = TRUE)
}
## subtract mean to make overall mean = 0
mm <- mm - mean(mm)

plot.ts(mm, ylab = "Seasonal effect", xlab = "Day", cex = 1)

## length of ts
ll <- length(u1_seas)
## frequency (ie, 12)
ff <- 36 ## trend mensile
## number of periods (years); %/% is integer division
periods <- ll%/%ff
## index of cumulative Month
index <- seq(1, ll, by = ff) - 1
## get mean by Month
mm <- numeric(ff)
for (i in 1:ff) {
  mm[i] <- mean(u1_seas[index + i], na.rm = TRUE)
}
## subtract mean to make overall mean = 0
mm <- mm - mean(mm)

plot.ts(mm, ylab = "Seasonal effect", xlab = "Month", cex = 1)
## trend decisamente diverso rispetto all'u6

## Quanto segue non credo sia proprio corretto ma può essere informativo
## andamento medio all'interno di un periodo di tempo

## andamento all'interno della giornata

day_seas<-u1 %>% group_by(ORA)%>% summarise(media=mean(CONSUMO_ATTIVA_PRELEVATA))
day_seas$ORA<-as.numeric(day_seas$ORA)/10000
plot(day_seas$ORA,day_seas$media , type="l", xlab = "Ora", ylab="consumo")

## andamento all'interno della settimana

u1$weekday<-weekdays(as.Date(u1$DATA))
week_seas<-u1 %>% group_by(weekday)%>% summarise(media=mean(CONSUMO_ATTIVA_PRELEVATA))
week_seas$weekday<-factor(week_seas$weekday, levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
week_seas<-week_seas[order(week_seas$weekday),]
qplot(week_seas$weekday, week_seas$media, geom="line",group=1)

## andamento all'interno dell'anno

u1$month<-months.Date(u1$DATA)
month_seas<-u1 %>% group_by(month)%>% summarise(media=mean(CONSUMO_ATTIVA_PRELEVATA))
month_seas$month<-factor(month_seas$month, c("gennaio","febbraio","marzo","aprile","maggio","giugno","luglio",
                                             "agosto","settembre","ottobre","novembre","dicembre"))
month_seas<-month_seas[order(month_seas$month),]
qplot(month_seas$month, month_seas$media, geom="line",group=1)

# Decompose e rumore bianco -----------------------------------------------

## ottengo il rumore bianco, il trend e la stagionalità con decompose. 
## con msts indico vari livelli di stagionalità e questi influenzano la stima della serie

## stagionalità giornaliera e settimanale

x<- msts(u1$CONSUMO_ATTIVA_PRELEVATA, seasonal.periods=c(96,96*7),  start=2018)
xd<-decompose(x, type = "additive")  
plot(xd, yax.flip = TRUE)

## stagionalità giornaliera, settimanale e annuale

x<- msts(u1$CONSUMO_ATTIVA_PRELEVATA, seasonal.periods=c(96,96*7, 96*365),  start=2018)
xd<-decompose(x, type = "additive")  
plot(xd, yax.flip = TRUE)

