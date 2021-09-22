# Nuova decomposizione di dati giornalieri in modo tale da prevedere

# mettere nuove library qua ed eventualmente aggiornare il file library.R

# la decomposizione di u1 e u6 avverrà separatamente per non 
# creare confusione

head(u6_daily_ma) #dati u6
head(u1_daily) #dati u1

#partiamo da u6

# Modello additivo
# y(t) = Level + Trend + Seasonality + Noise

# Modello moltiplicativo
# y(t) = Level * Trend * Seasonality * Noise

# If the seasonality and residual components are independent of the trend, 
# then you have an additive series. If the seasonality and residual components 
# are in fact dependent, meaning they fluctuate on trend, then you have a 
# multiplicative series. 

#prima decomposizione

#frequenza giornaliera (freq=365)

u6_ts_daily_365 <- ts(u6_daily_ma$ENERGIA, start = c(2018,1), frequency = 365.25)

u6_ts_daily_365

frequency(u6_ts_daily_365)

plot(u6_ts_daily_365)

#plotto le componenti ricavate

#additivo

plot(decompose(u6_ts_daily_365, type = "additive"))

autoplot(decompose(u6_ts_daily_365, type = "additive")) + xlab("") + ylab("") + 
  ggtitle("Decomposition plot of the Time-Series") + 
  theme(plot.title = element_text(hjust = 0.5))

plot(stl(u6_ts_daily_365, s.window = "periodic"))

#moltiplicativo

plot(decompose(u6_ts_daily_365, type = "multiplicative"))

autoplot(decompose(u6_ts_daily_365, type = "multiplicative")) + xlab("") + ylab("") + 
  ggtitle("Decomposition plot of the Time-Series") + 
  theme(plot.title = element_text(hjust = 0.5))

#seasonal adjustement

#additivo

u6_ts_daily_365_add <- decompose(u6_ts_daily_365, "additive")
u6_ts_daily_365_add_adj <- u6_ts_daily_365-u6_ts_daily_365_add$seasonal
plot(u6_ts_daily_365_add_adj)

#moltiplicativo

u6_ts_daily_365_mult <- decompose(u6_ts_daily_365, "additive")
u6_ts_daily_365_mult_adj <- u6_ts_daily_365-u6_ts_daily_365_mult$seasonal
plot(u6_ts_daily_365_mult_adj)

#forecast

autoplot(stlf(u6_ts_daily_365, method = "arima")) + xlab("") + ylab("") + 
  ggtitle("Forecast of the Time-Series using `stlf' function") + 
  theme(plot.title = element_text(hjust = 0.5))

#modello ets

etsmodel = ets(u6_daily_ma$seriestorica, model = 'ZZZ')
etsmodel

# Plotting the model vs original

plot(as.ts(u6_daily_ma$seriestorica), lwd = 3)
lines(etsmodel$fitted, col = "red")

autoplot(as.ts(u6_daily_ma$seriestorica), size = 1.5) + xlab("") + ylab("") + ggtitle("Time-Series") + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none") + 
  autolayer(etsmodel$fitted)

plot(forecast(etsmodel, h = 12))

# Multi seasonal decomposition (daily and weekly)
u6_ts_daily_ms <- msts(u6_daily_ma$ENERGIA, seasonal.periods = c(7,365.25))
u6_ts_daily_ms_dec_add <- decompose(x, type = "additive")  
plot(u6_ts_daily_ms_dec, yax.flip = TRUE)

#ACF 
ggtsdisplay(u6_daily_ma$seriestorica,
            plot.type = "partial",
            main = "ACF & PACF plot for Time-Series",
            smooth = TRUE) 

#dickey fuller test
adf.test(u6_daily_ma$seriestorica)

# auto.arima

arima_u6_daily <- auto.arima(u6_daily_ma$seriestorica,
                             stepwise = F,
                             approximation = F,
                             seasonal = T)
summary(arima_u6_daily)

arima_u6_fore <- forecast(arima_u6_daily, h = 30)
plot(arima_u6_fore)

