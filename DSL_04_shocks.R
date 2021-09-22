library(forecast)
library(TSA)
library(EnvStats)
library(MASS)
library(tseries)
library(dplyr)
library(xts)
library(DataCombine)
library(DescTools)

# cose più utili riga 460

mod_d_u1<-auto.arima(u1_timeseries)
summary(mod_d_u1)

a<-u1_daily[u1_daily$DATA<"2020-03-07",] 
a$covid=0

b<-u1_daily[u1_daily$DATA>="2020-03-07",] 
b$covid=1

u1_daily<-rbind(a,b)
u1_daily$covid<-as.factor(u1_daily$covid)

arimax1<-arimax(u1_timeseries, order = c(2, 1, 1), xreg = u1_daily$covid)
arimax1

res<-arimax1$residuals
par(mfrow=c(2,2),cex.axis=1, cex.lab=1, cex.main=1, fg="white",
    col.axis="white", col.lab="white", col.main="white", bg="black")
acf(res, ci.col="white")
pacf(res, ci.col="white")
hist(res)
qqnorm(res)
qqline(res)
shapiro.test(res)

## insomma
## destagionalizziamo i dati per togliere l'autocorrelazione

fltr <- c(1/2, rep(1, times = 50), 1/2)/51
u1_dailytrend <- stats::filter(as.vector(u1_timeseries), filter = fltr, method = "convo", 
                               sides = 2)
par(mfrow=c(2,1))
plot.ts(u1_dailytrend, ylab = "Trend", cex = 1)

plot.ts(as.vector(u1_timeseries))

arimax2<-arimax(u1_dailytrend, order = c(2, 1, 1), xreg = u1_daily$covid)
arimax2

res<-arimax2$residuals
res<-na.omit(res)
par(mfrow=c(2,2),cex.axis=1, cex.lab=1, cex.main=1, fg="white",
    col.axis="white", col.lab="white", col.main="white", bg="black")
acf(res, ci.col="white")
pacf(res, ci.col="white")
hist(res)
qqnorm(res)
qqline(res)
shapiro.test(res)

## niente da fare

arimax3<-arimax(u1_timeseries, order = c(2, 1, 1), seasonal=list(order=c(4,0,4)), xreg = u1_daily$covid)
arimax3

res<-arimax3$residuals
par(mfrow=c(2,2),cex.axis=1, cex.lab=1, cex.main=1, fg="white",
    col.axis="white", col.lab="white", col.main="white", bg="black")
acf(res, ci.col="white")
pacf(res, ci.col="white")
hist(res)
qqnorm(res)
qqline(res)
shapiro.test(res)

## lavoriamo sugli outliers

fit <- nnetar(tsclean(u1_timeseries),p=7)
fit_boxcox<-BoxCox(fit$x, lambda=1.5)
plot(fit_boxcox)
mod_d2_u1<-auto.arima(fit_boxcox)
summary(mod_d2_u1)

arimax4<-arimax(fit_boxcox, order = c(1, 1, 2), seasonal=list(order=c(1,0,1),period = 7), xreg = u1_daily$covid)
arimax4
res<-arimax4$residuals
par(mfrow=c(2,2),cex.axis=1, cex.lab=1, cex.main=1, fg="white",
    col.axis="white", col.lab="white", col.main="white", bg="black")
acf(res, ci.col="white")
pacf(res, ci.col="white")
hist(res)
qqnorm(res)
qqline(res)
shapiro.test(res)
ks.test(x=res, y=pnorm)

par(mfrow=c(1,1))
fitted(arimax4)
plot(fitted(arimax4),arimax4$residuals)
fit_boxcox<-as.ts(fit_boxcox)
modarima<-Arima(
  fit_boxcox,
  order = c(1, 1, 2),
  seasonal = c(7, 0, 7))
summary(modarima)

McLeod.Li.test(modarima, gof.lag=7)


# Pattume -----------------------------------------------------------------

hist(u1_daily$ENERGIA)
shapiro.test(u1_daily$ENERGIA)

u1_daily_norm<-(u1_daily$ENERGIA-mean(u1_daily$ENERGIA))/sd(u1_daily$ENERGIA)
hist((u1_daily$ENERGIA-mean(u1_daily$ENERGIA))/sd(u1_daily$ENERGIA))
u1_daily_norm[u1_daily_norm< (-3)]
u1_daily_norm[943]<-NA
u1_daily_norm<-na.omit(u1_daily_norm)
hist(u1_daily_norm)
shapiro.test(u1_daily_norm)


logaritmo<-(log(u1_daily$ENERGIA))
logaritmo[943]<-NA
logaritmo<-na.omit(logaritmo)
logaritmo[1000:length(logaritmo)]
shapiro.test(logaritmo)

hist(fit_boxcox)
shapiro.test(fit_boxcox)

a<-u1_daily[u1_daily$DATA<"2020-03-07",] 
a$covidcol="No"

b<-u1_daily[(u1_daily$DATA>="2020-03-07" & u1_daily$DATA<"2020-05-18")
            | (u1_daily$DATA>="2020-11-07" & u1_daily$DATA<"2020-11-29")
            | (u1_daily$DATA>="2020-12-24") ,] 
b$covidcol="Rosso"

c<-u1_daily[(u1_daily$DATA>="2020-11-29" & u1_daily$DATA<"2020-12-13"),] 
c$covidcol="Arancio"

d<-u1_daily[(u1_daily$DATA>="2020-05-18" & u1_daily$DATA<"2020-11-07")
            | (u1_daily$DATA>="2020-12-13" & u1_daily$DATA<"2020-12-24") ,] 
d$covidcol="Giallo"

u1_daily<-rbind(a,b,c,d)
u1_daily$covid<-as.factor(u1_daily$covid)
u1_daily$covidcol<-as.factor(u1_daily$covidcol)
u1_daily<-u1_daily[order(u1_daily$DATA),]

fit <- nnetar(tsclean(u1_timeseries),p=7)
fit_boxcox<-BoxCox(fit$x, lambda=1.5)
plot(fit_boxcox)
mod_d2_u1<-auto.arima(fit_boxcox)
summary(mod_d2_u1)

arimax5<-arimax(as.numeric(fit_boxcox), order = c(1, 1, 2), seasonal=list(order=c(1,0,1),period = 7), 
                xreg = as.numeric(u1_daily$covidcol))
arimax5
res<-arimax5$residuals
par(mfrow=c(2,2),cex.axis=1, cex.lab=1, cex.main=1, fg="white",
    col.axis="white", col.lab="white", col.main="white", bg="black")
acf(res, ci.col="white")
pacf(res, ci.col="white")
hist(res)
qqnorm(res)
qqline(res)
shapiro.test(res)
ks.test(x=res, y=pnorm)

a<-u1[u1$DATA<"2020-03-07",] 
a$covid=0

b<-u1[u1$DATA>="2020-03-07",] 
b$covid=1

u1<-rbind(a,b)
u1$h<-substr(u1$ORA, start = 1, stop = 2)

u1_h <- u1 %>% 
  select(POD,DATA,h,CONSUMO_ATTIVA_PRELEVATA,covid) %>% 
  mutate(kWh=CONSUMO_ATTIVA_PRELEVATA*0.25) %>% 
  group_by(POD,DATA,h) %>% 
  summarise(ENERGIA=sum(kWh), covid=mean(covid)) %>% 
  ungroup() %>% 
  as.data.frame()

str(u1_h)


u1_h$POD <- as.factor(u1_h$POD)
u1_h$DATA <- as.Date(u1_h$DATA,format="%Y-%m-%d")
u1_h$WEEKDAY <- as.factor(weekdays(u1_h$DATA))
u1_h$WEEKDAY <- factor(u1_h$WEEKDAY , levels=c("Monday", "Tuesday", 
                                               "Wednesday","Thursday",
                                               "Friday","Saturday","Sunday"))

u1_h <- arrange(u1_h,DATA)

summary(u1_h)
u1_h$timestamp<-as.POSIXct(strptime(paste(u1_h$DATA , u1_h$h), format = "%Y-%m-%d %H"))
u1h_timeseries <- xts(u1_h$ENERGIA,u1_h$timestamp)


fit <- nnetar(tsclean(u1h_timeseries),p=24)
fit_boxcox<-BoxCox(fit$x, lambda="auto")
plot(fit_boxcox)
mod_d2_u1<-auto.arima(fit_boxcox)
summary(mod_d2_u1)

#seasonal=list(order=c(1,0,1),period = 24
arimax7<-arimax(fit_boxcox, order = c(1, 1, 5), xreg = u1_h$covid)
arimax7

res<-arimax7$residuals
par(mfrow=c(2,2),cex.axis=1, cex.lab=1, cex.main=1, fg="white",
    col.axis="white", col.lab="white", col.main="white", bg="black")
acf(res, ci.col="white")
pacf(res, ci.col="white")
hist(res)
qqnorm(res)
qqline(res)
shapiro.test(res)
ks.test(x=res, y=pnorm)

## boh proviamo a fare le cose a mano fatte male

mu_a<-mean(a$CONSUMO_ATTIVA_PRELEVATA); mu_a
t.test(a$CONSUMO_ATTIVA_PRELEVATA)$"conf.int"
mu_b<-mean(b$CONSUMO_ATTIVA_PRELEVATA); mu_b
t.test(b$CONSUMO_ATTIVA_PRELEVATA)$"conf.int"

## si ok stra bello ma sta cosa la devi modellare


# Un barlume di speranza ---------------------------------------------------


fit <- nnetar(tsclean(u1_timeseries),p=7)
fit_ts<-as.ts(fit$x)
tslm1<-tslm(fit_ts ~ u1_daily$covid, lambda = 1.5)
summary(tslm1)

res<-tslm1$residuals
par(mfrow=c(2,2),cex.axis=1, cex.lab=1, cex.main=1, fg="white",
    col.axis="white", col.lab="white", col.main="white", bg="black")
acf(res, ci.col="white")
pacf(res, ci.col="white")
hist(res)
qqnorm(res)
qqline(res)
shapiro.test(res)

## correggiamo come in hartangel

u1_daily$u_hat<-tslm1$residuals
u1_daily<-slide(data=u1_daily, Var="u_hat", TimeVar = "DATA", NewVar = "u_hat_lag")

aux<-lm(u_hat ~ u_hat_lag,u1_daily)
summary(aux)

rho<-aux$coefficients[2]
u1_daily<-slide(data=u1_daily, Var="covid", TimeVar = "DATA", NewVar = "covid_lag")
u1_daily<-slide(data=u1_daily, Var="ENERGIA", TimeVar = "DATA", NewVar = "ENERGIA_lag")

u1_daily$ENERGIA_t<-u1_daily$ENERGIA-rho*u1_daily$ENERGIA_lag
u1_daily$interc_t<-1-rho

u1_timeseries_t<-u1_daily$ENERGIA_t
fit <- nnetar(tsclean(u1_timeseries_t),p=7)
fit_ts2<-as.ts(fit$x, p=7)

tslm2<-lm(fit_ts2 ~ 0+u1_daily$interc_t+covid_lag ,u1_daily)
summary(tslm2)

res<-tslm2$residuals
par(mfrow=c(2,2),cex.axis=1, cex.lab=1, cex.main=1, fg="white",
    col.axis="white", col.lab="white", col.main="white", bg="black")
acf(res, ci.col="white")
pacf(res, ci.col="white")
hist(res)
qqnorm(res)
qqline(res)
shapiro.test(res)

appo_auto<-auto.arima(fit_ts2)
summary(appo_auto)

res<-appo_auto$residuals
par(mfrow=c(2,2),cex.axis=1, cex.lab=1, cex.main=1, fg="white",
    col.axis="white", col.lab="white", col.main="white", bg="black")
acf(res, ci.col="white")
pacf(res, ci.col="white")
hist(res)
qqnorm(res)
qqline(res)
shapiro.test(res)


appo_arima<-arima(fit_ts2, order = c(1, 1, 3),
                  seasonal = list(order = c(1, 0, 1), period = 7), xreg = u1_daily$covid)
appo_arima

res<-appo_arima$residuals
par(mfrow=c(2,2),cex.axis=1, cex.lab=1, cex.main=1, fg="white",
    col.axis="white", col.lab="white", col.main="white", bg="black")
acf(res, ci.col="white")
pacf(res, ci.col="white")
hist(res)
qqnorm(res)
qqline(res)
shapiro.test(res)

#non va tslm3

tslm3<-tslm(fitted(appo_arima) ~ u1_daily$interc_t+u1_daily$covid_lag)

summary(tslm3)

fitted(appo_arima)
res<-tslm3$residuals[2:1096]
par(mfrow=c(2,2),cex.axis=1, cex.lab=1, cex.main=1, fg="white",
    col.axis="white", col.lab="white", col.main="white", bg="black")
acf(res, ci.col="white")
pacf(res, ci.col="white")
hist(res)
qqnorm(res)
qqline(res)
shapiro.test(res)

## decompose

x<- msts(u1_daily$ENERGIA, seasonal.periods=c(1*7), ts.frequency = 365.33,  start=2018)
xd<-decompose(x, type = "additive")  
plot(xd, yax.flip = TRUE)
ts_no_s<-x - xd$seasonal

tslm4<-tslm(ts_no_s ~ covid ,u1_daily)
summary(tslm4)

res<-tslm4$residuals[2:1096]
par(mfrow=c(2,2),cex.axis=1, cex.lab=1, cex.main=1, fg="white",
    col.axis="white", col.lab="white", col.main="white", bg="black")
acf(res, ci.col="white")
pacf(res, ci.col="white")
hist(res)
qqnorm(res)
qqline(res)
shapiro.test(res)

## correzione autocorrelazione

u1_daily$u_hat<-tslm4$residuals
u1_daily<-slide(data=u1_daily, Var="u_hat", TimeVar = "DATA", NewVar = "u_hat_lag")

aux<-lm(u_hat ~ u_hat_lag,u1_daily)
summary(aux)

rho<-aux$coefficients[2]
u1_daily<-slide(data=u1_daily, Var="covid", TimeVar = "DATA", NewVar = "covid_lag")
u1_daily<-slide(data=u1_daily, Var="ENERGIA", TimeVar = "DATA", NewVar = "ENERGIA_lag")

u1_daily$ENERGIA_t<-u1_daily$ENERGIA-rho*u1_daily$ENERGIA_lag
u1_daily$interc_t<-1-rho

u1_timeseries_t<-u1_daily$ENERGIA_t
fit <- nnetar(tsclean(u1_timeseries_t),p=7)

x<- msts(fit$x, seasonal.periods=7, ts.frequency = 365.33,  start=2018)
xd<-decompose(x, type = "additive")  
plot(xd, yax.flip = TRUE)
ts_no_s<-x - xd$seasonal

tslm5<-tslm(ts_no_s ~ 0+u1_daily$interc_t+covid_lag ,u1_daily)
summary(tslm5)

res<-tslm5$residuals[2:1096]
par(mfrow=c(2,2),cex.axis=1, cex.lab=1, cex.main=1, fg="white",
    col.axis="white", col.lab="white", col.main="white", bg="black")
acf(res, ci.col="white")
pacf(res, ci.col="white")
hist(res)
qqnorm(res)
qqline(res)
shapiro.test(res)

## ultima spiaggia

u1_daily$week<-strftime(u1_daily$DATA,"%V")
u1_daily$year<-strftime(u1_daily$DATA,"%Y")

u1_weekly<-u1_daily%>%
  group_by(year, week)%>%
  summarize(energia = mean(ENERGIA), Covid = Mode(covid))
x<- ts(u1_weekly$energia, frequency = 52,  start=2018)
fit <- nnetar(tsclean(x))
lambda<-BoxCox.lambda(fit$x, method = "guerrero")
fit_boxcox<-BoxCox(fit$x, lambda=lambda)
plot(fit_boxcox)


tslm6<-tslm(x ~ Covid ,u1_weekly)
summary(tslm6)

res<-tslm6$residuals
par(mfrow=c(2,2),cex.axis=1, cex.lab=1, cex.main=1, fg="white",
    col.axis="white", col.lab="white", col.main="white", bg="black")
acf(res, ci.col="white")
pacf(res, ci.col="white")
hist(res)
qqnorm(res)
qqline(res)
shapiro.test(res)

plot(fit_boxcox)
lines(ksmooth(time(fit_boxcox), fit_boxcox, "normal", bandwidth = 0.4), lwd=2, col=4)


## correzione autocorrelazione

u1_weekly<-as.data.frame(u1_weekly)
u1_weekly$u_hat<-tslm6$residuals
u1_weekly<-slide(data=u1_weekly, Var="u_hat", NewVar = "u_hat_lag" )

aux<-lm(u_hat ~ u_hat_lag,u1_weekly)
summary(aux)

rho<-aux$coefficients[2]
u1_weekly<-slide(data=u1_weekly, Var="Covid", NewVar = "Covid_lag")
u1_weekly<-slide(data=u1_weekly, Var="energia", NewVar = "ENERGIA_lag")

u1_weekly$energia_t<-u1_weekly$energia-rho*u1_weekly$ENERGIA_lag
u1_weekly$interc_t<-1-rho

x<- ts(u1_weekly$energia_t, frequency = 52,  start=2018)

fit <- nnetar(tsclean(x))
lambda<-BoxCox.lambda(fit$x, method = "guerrero")
fit_boxcox<-BoxCox(fit$x, lambda=lambda)
plot(fit_boxcox)

tslm7<-tslm(fit_boxcox ~ 0+u1_weekly$interc_t+Covid_lag ,u1_weekly)
summary(tslm7)

res<-tslm7$residuals[2:157]
par(mfrow=c(2,2),cex.axis=1, cex.lab=1, cex.main=1, fg="white",
    col.axis="white", col.lab="white", col.main="white", bg="black")
acf(res, ci.col="white")
pacf(res, ci.col="white")
hist(res)
qqnorm(res)
qqline(res)
shapiro.test(res)


# Riassunto con solo le cose utili ----------------------------------------

par(mfrow=c(1,1))

mod_d_u1<-auto.arima(u1_timeseries)
summary(mod_d_u1)

a<-u1_daily[u1_daily$DATA<"2020-03-07",] 
a$covid=0

b<-u1_daily[u1_daily$DATA>="2020-03-07",] 
b$covid=1

u1_daily<-rbind(a,b)
u1_daily$covid<-as.factor(u1_daily$covid)

u1_daily$week<-strftime(u1_daily$DATA,"%V")
u1_daily$year<-strftime(u1_daily$DATA,"%Y")

u1_weekly<-u1_daily%>%
  group_by(year, week)%>%
  summarize(energia = mean(ENERGIA), Covid = Mode(covid))

x<- ts(u1_weekly$energia, frequency = 52,  start=2018)
fit <- nnetar(tsclean(x))
lambda<-BoxCox.lambda(fit$x, method = "loglik")
fit_boxcox<-BoxCox(fit$x, lambda=lambda)
plot(fit_boxcox)


tslm6<-tslm(x ~ trend+season+Covid, u1_weekly)
summary(tslm6)

res<-tslm6$residuals
par(mfrow=c(2,2),cex.axis=1, cex.lab=1, cex.main=1, fg="white",
    col.axis="white", col.lab="white", col.main="white", bg="black")
acf(res, ci.col="white")
pacf(res, ci.col="white")
hist(res)
qqnorm(res)
qqline(res)
shapiro.test(res)

train_weekly_u1 <- u1_weekly %>% head(144)
test_weekly_u1 <- u1_weekly %>% tail(13)

x<- ts(train_weekly_u1$energia, frequency = 52,  start=2018)
tslm8 <- tslm(x ~ trend+season+Covid,train_weekly_u1)
summary(tslm8)

res<-tslm8$residuals
par(mfrow=c(2,2),cex.axis=1, cex.lab=1, cex.main=1, fg="white",
    col.axis="white", col.lab="white", col.main="white", bg="black")
acf(res, ci.col="white")
pacf(res, ci.col="white")
hist(res)
qqnorm(res)
qqline(res)
shapiro.test(res)

#forecast

forecast(tslm8,newdata = data.frame(Covid=test_weekly_u1$Covid))
test_weekly_u1
