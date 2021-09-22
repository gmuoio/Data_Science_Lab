#operazioni preliminari

##### library utilizzate
library(stringr)
library(forecast)
library(xts)
library(imputeTS)
library(readr)
library(ggplot2)
library(forecast)
library(MLmetrics)
library(fpp2)
library(TTR)
library(dplyr)
library(tidyr)
library(imputeTS)
library(naniar)
library(astsa)
library(zoo)
library(gridExtra)

# Unione dataset ----------------------------------------------------------

u1<-rbind(u1_2018 , u1_2019 , u1_2020)
u1$DATA[35035:35045] ## controllo cambio di anno, ok!

u6<-rbind(u6_2018 , u6_2019 , u6_2020)
u6$DATA[35035:35045] ## controllo cambio di anno, ok!

# Creazione timestamps ----------------------------------------------------

u1$data2<-as.character(u1$DATA) ## cambio il tipo delle variabili


##costruisco ora nel formato 00:00:00

for (i in seq(1,length(u1$ORA))){
  while (nchar(u1$ORA[i])<6) {
    u1$ORA[i]<-paste("0",u1$ORA[i],sep="")
  }
  if (u1$ORA[i] == "01e+05") u1$ORA[i]="100000"
  if (u1$ORA[i] == "02e+05") u1$ORA[i]="200000"
}

u1$ora2<-paste(substr(u1$ORA, start = 1, stop = 2), substr(u1$ORA, start = 3, stop = 4), substr(u1$ORA, start = 5, stop = 6), sep = ":")

##genero il timestamp
u1$timestamp<-as.POSIXct(strptime(paste(u1$data2 , u1$ora2), format = "%Y-%m-%d %H:%M:%S"))


u1$data2<-as.character(u1$DATA) ## cambio il tipo delle variabili


##costruisco ora nel formato 00:00:00

for (i in seq(1,length(u1$ORA))){
  while (nchar(u1$ORA[i])<6) {
    u1$ORA[i]<-paste("0",u1$ORA[i],sep="")
  }
  if (u1$ORA[i] == "01e+05") u1$ORA[i]="100000"
  if (u1$ORA[i] == "02e+05") u1$ORA[i]="200000"
}

u1$ora2<-paste(substr(u1$ORA, start = 1, stop = 2), substr(u1$ORA, start = 3, stop = 4), substr(u1$ORA, start = 5, stop = 6), sep = ":")

##genero il timestamp
u1$timestamp<-as.POSIXct(strptime(paste(u1$data2 , u1$ora2), format = "%Y-%m-%d %H:%M:%S"))

u1$data2<-as.character(u1$DATA) ## cambio il tipo delle variabili


##costruisco ora nel formato 00:00:00

for (i in seq(1,length(u1$ORA))){
  while (nchar(u1$ORA[i])<6) {
    u1$ORA[i]<-paste("0",u1$ORA[i],sep="")
  }
  if (u1$ORA[i] == "01e+05") u1$ORA[i]="100000"
  if (u1$ORA[i] == "02e+05") u1$ORA[i]="200000"
}

u1$ora2<-paste(substr(u1$ORA, start = 1, stop = 2), substr(u1$ORA, start = 3, stop = 4), substr(u1$ORA, start = 5, stop = 6), sep = ":")

##genero il timestamp
u1$timestamp<-as.POSIXct(strptime(paste(u1$data2 , u1$ora2), format = "%Y-%m-%d %H:%M:%S"))

## ripeto per u6 
u6$data2<-as.character(u6$DATA) ## cambio il tipo delle variabili

##costruisco ora nel formato 00:00:00

for (i in seq(1,length(u6$ORA))){
  while (nchar(u6$ORA[i])<6) {
    u6$ORA[i]<-paste("0",u6$ORA[i],sep="")
  }
  if (u6$ORA[i] == "01e+05") u6$ORA[i]="100000"
  if (u6$ORA[i] == "02e+05") u6$ORA[i]="200000"
}

u6$ora2<-paste(substr(u6$ORA, start = 1, stop = 2), substr(u6$ORA, start = 3, stop = 4), substr(u6$ORA, start = 5, stop = 6), sep = ":")

##genero il timestamp
u6$timestamp<-as.POSIXct(strptime(paste(u6$data2 , u6$ora2), format = "%Y-%m-%d %H:%M:%S"))

##elimino le variabili di appoggio

u1$ora2<-NULL
u1$data2<-NULL
u6$ora2<-NULL
u6$data2<-NULL

# Primo grafico -----------------------------------------------------------
par(mfrow=c(2,1))

plot(u1$timestamp, u1$CONSUMO_ATTIVA_PRELEVATA , xlab = "timestamp", ylab = "potenza media prelevata", type = "l")
## eh insomma fa un po' male agli occhi questo grafico, ci sono troppe variazioni locali, ci lavoreremo...
#plot(u1$timestamp, u1$CONSUMO_REATTIVA_INDUTTIVA_PRELEVATA , xlab = "timestamp", ylab = "potenza media prelevata", type = "l")
## simile al precedente

plot(u6$timestamp, u6$CONSUMO_ATTIVA_PRELEVATA , xlab = "timestamp", ylab = "potenza media prelevata", type = "l")
## Questo sembra più bello, si nota decisamente il lockdown di Marzo 2020 
#plot(u6$timestamp, u6$CONSUMO_REATTIVA_INDUTTIVA_PRELEVATA , xlab = "timestamp", ylab = "potenza media prelevata", type = "l")
## anche questo simile al precedente

par(mfrow=c(1,1))

#PRE-PROCESSING E ANALISI ESPLORATIVA

# aggregazione giornaliera
# edificio u1

u1_daily <- u1 %>% 
  select(POD,DATA,CONSUMO_ATTIVA_PRELEVATA) %>% 
  mutate(kWh=CONSUMO_ATTIVA_PRELEVATA*0.25) %>% 
  group_by(POD,DATA) %>% 
  summarise(ENERGIA=sum(kWh)) %>% 
  ungroup() %>% 
  as.data.frame()

str(u1_daily)

u1_daily$POD <- as.factor(u1_daily$POD)
u1_daily$DATA <- as.Date(u1_daily$DATA,format="%Y-%m-%d")
u1_daily$WEEKDAY <- as.factor(weekdays(u1_daily$DATA))
u1_daily$WEEKDAY <- factor(u1_daily$WEEKDAY , levels=c("Monday", "Tuesday", 
                                                       "Wednesday","Thursday",
                                                       "Friday","Saturday","Sunday"))

u1_daily <- arrange(u1_daily,DATA)

summary(u1_daily)

#boxplot
ggplot(u1_daily) +
  aes(x = "", y = ENERGIA) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

#boxplot condizionato
ggplot(u1_daily) +
  aes(x = WEEKDAY, y = ENERGIA) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

#valutare barplot condizionato ai weekdays, guardare progetto di marketing

#cerchiamo gli outlier
out_u1 <- boxplot.stats(u1_daily$ENERGIA)$out
out_u1_idx <- which(u1_daily$ENERGIA %in% c(out_u1))
length(out_u1)

#33 outlier
u1_daily[out_u1_idx,]

#2018: outlier alti a luglio (lungo tutto il mese) e bassi a dicembre (intorno a natale)
#2019: outlier alti a fine giugno e fine luglio, no dicembre
#2020: solo 2 outlier, 21 giugno al ribasso e 31 luglio nullo (blackout milano)

# histogram
ggplot(u1_daily, aes(x=ENERGIA)) +
  geom_histogram(binwidth=200, colour="black", fill="white")

# Histogram overlaid with kernel density curve
ggplot(u1_daily, aes(x=ENERGIA)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=200,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")  # Overlay with transparent density plot

u1_timeseries <- xts(u1_daily$ENERGIA,u1_daily$DATA)

plot(u1_timeseries)

# aggregazione giornaliera
# edificio u6

u6_daily <- u6 %>% 
  select(POD,DATA,CONSUMO_ATTIVA_PRELEVATA) %>% 
  mutate(kWh=CONSUMO_ATTIVA_PRELEVATA*0.25) %>% 
  group_by(POD,DATA) %>% 
  summarise(ENERGIA=sum(kWh)) %>% 
  ungroup() %>% 
  as.data.frame()

str(u6_daily)

u6_daily$POD <- as.factor(u6_daily$POD)
u6_daily$DATA <- as.Date(u6_daily$DATA,format="%Y-%m-%d")
u6_daily$WEEKDAY <- as.factor(weekdays(u6_daily$DATA))
u6_daily$WEEKDAY <- factor(u6_daily$WEEKDAY , levels=c("Monday", "Tuesday", 
                                                       "Wednesday","Thursday",
                                                       "Friday","Saturday","Sunday"))

u6_daily <- arrange(u6_daily,DATA)
summary(u6_daily)

#boxplot
ggplot(u6_daily) +
  aes(x = "", y = ENERGIA) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

#boxplot condizionato
ggplot(u6_daily) +
  aes(x = WEEKDAY, y = ENERGIA) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

#cerchiamo gli outlier
out_u6 <- boxplot.stats(u6_daily$ENERGIA)$out
out_u6_idx <- which(u6_daily$ENERGIA %in% c(out_u6))
length(out_u6)
#19 outlier

u6_daily[out_u6_idx,]
#i dati di giugno 2020 hanno il POD di u1, quindi li escludo (solo POD U6)
subset(u6_daily[out_u6_idx,],POD!="IT012E00491869")

#gli outlier sono tutti bassi (al di sotto del baffo inferiore)
#2018: dati sparsi ma sono praticamente tutte domeniche, tranne 27 dicembre
#2019: solo 31 dicembre
#2020: capodanno, sabato santo e blackout 31 luglio

# histogram
ggplot(u6_daily, aes(x=ENERGIA)) +
  geom_histogram(binwidth=200, colour="black", fill="white")

# Histogram overlaid with kernel density curve
ggplot(u6_daily, aes(x=ENERGIA)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=200,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")  # Overlay with transparent density plot

#boxplot affiancati
aux <- rbind(cbind(u1_daily, EDIFICIO='U1'), cbind(u6_daily, EDIFICIO='U6'))

boxplot(aux$ENERGIA ~ aux$EDIFICIO, xlab='Edificio', ylab='Energia (kWh)')

#ANALISI DATI SBAGLIATI
u6_daily[u6_daily$POD=="IT012E00491869",]
#i dati sbagliati sono quelli di giugno 2020, che fanno riferimento a u1
#----> u6 giugno 2020 ha il pod di u1

u6_timeseries <- xts(u6_daily$ENERGIA,u6_daily$DATA)
plot(u6_timeseries)

#come risolvere giugno 2020 di u6?

#Opzione 1: omettere i dati o sostituire con NA

u6_daily_without <- subset(u6_daily, POD!="IT012E00491869")

u6_timeseries_without <- xts(u6_daily_without$ENERGIA,u6_daily_without$DATA)
plot(u6_timeseries_without)

#in caso di omissione i dati di fine maggio e inizio luglio vengono congiunti
#graficamente da una linea retta

u6_daily_NA <- u6_daily
u6_daily_NA$ENERGIA[u6_daily_NA$POD=="IT012E00491869"] <- NA

u6_timeseries_NA <- xts(u6_daily_NA$ENERGIA,u6_daily_NA$DATA)
plot(u6_timeseries_NA)

ggplot_na_distribution(u6_timeseries_NA)
ggplot_na_gapsize(u6_timeseries_NA)

#al contrario con gli NA viene proprio a mancare la corrispettiva finestra temporale

#Opzione 2: imputare l'informazione mancante, come? Provare più strade

# Utilizzo del pacchetto imputeTS, creato appositamente per i dati mancanti all'interno
# di serie temporali

#interpolazione lineare
u6_interpolation_linear <- na_interpolation(u6_timeseries_NA)
ggplot_na_imputations(u6_timeseries_NA,u6_interpolation_linear)

#decomposizione stagionale
u6_seadec <- na_seadec(u6_timeseries_NA, 
                       algorithm = 'kalman',
                       find_frequency=TRUE)
ggplot_na_imputations(u6_timeseries_NA,u6_seadec)

#split stagionale

u6_seasplit_kalman <- na_seasplit(u6_timeseries_NA, 
                                  algorithm = 'kalman',
                                  find_frequency=TRUE)
ggplot_na_imputations(u6_timeseries_NA,u6_seasplit_kalman)

u6_seasplit_interpolation <- na_seasplit(u6_timeseries_NA, 
                                         algorithm = 'interpolation',
                                         find_frequency=TRUE)
ggplot_na_imputations(u6_timeseries_NA,u6_seasplit_interpolation)

u6_seasplit_ma <- na_seasplit(u6_timeseries_NA, 
                              algorithm = 'ma',
                              find_frequency=TRUE)
ggplot_na_imputations(u6_timeseries_NA,u6_seasplit_ma)

#analisi esplorativa con dati imputati, seasplit ma
u6_daily_ma <- u6_daily
u6_daily_ma$ENERGIA <- as.numeric(u6_seasplit_ma)
u6_daily_ma$POD <- as.factor(rep("IT012E00491824",nrow(u6_daily_ma)))
summary(u6_daily_ma)

#boxplot
ggplot(u6_daily_ma) +
  aes(x = "", y = ENERGIA) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

#boxplot condizionato
ggplot(u6_daily_ma) +
  aes(x = WEEKDAY, y = ENERGIA) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

#cerchiamo gli outlier
out_u6_ma <- boxplot.stats(u6_daily_ma$ENERGIA)$out
out_u6_idx_ma <- which(u6_daily_ma$ENERGIA %in% c(out_u6_ma))
length(out_u6_ma)
#16 outlier

u6_daily_ma[out_u6_idx_ma,]

#gli outlier sono tutti bassi (al di sotto del baffo inferiore)
#2018: dati sparsi ma sono praticamente tutte domeniche, tranne 27 dicembre
#2019: solo 27 e 31 dicembre
#2020: capodanno, sabato santo e blackout 31 luglio

# histogram
ggplot(u6_daily_ma, aes(x=ENERGIA)) +
  geom_histogram(binwidth=200, colour="black", fill="white")

# Histogram overlaid with kernel density curve
ggplot(u6_daily_ma, aes(x=ENERGIA)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=200,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")  # Overlay with transparent density plot

#boxplot affiancati
aux_ma <- rbind(cbind(u1_daily, EDIFICIO='U1'), cbind(u6_daily_ma, EDIFICIO='U6'))

boxplot(aux$ENERGIA ~ aux$EDIFICIO, xlab='Edificio', ylab='Energia (kWh)')

#prova filtro dati solo giugno
monthplot(u6_seasplit_ma)
?strptime
u6_daily_ma$month <- format(u6_daily_ma$DATA, "%m")
u6_daily_ma$WEEKDAY<-format(u6_daily_ma$DATA, "%u")

u6_daily_ma_june <- u6_daily_ma %>% 
  filter(month=="06" & WEEKDAY %in% c(1,2,3,4,5))

sp <-  ggplot(u6_daily_ma_june, aes(x = DATA, y = ENERGIA)) +
  geom_line(color = "blue") + 
  xlab(label = "Time") +
  ylab(label = "Energy") +
  ylim(500,4500)+
  facet_wrap( ~ format(DATA, "%Y"), scales = "free_x")
plot(sp)

u1_daily$month <- format(u1_daily$DATA, "%m")
u1_daily$WEEKDAY<-format(u1_daily$DATA, "%u")

u1_daily_june <- u1_daily %>% 
  filter(month=="06" & WEEKDAY %in% c(1,2,3,4,5))

u6_daily_ma_june$EDIFICIO <- "U6"
u1_daily_june$EDIFICIO <- "U1"

# Aggregazione per giorno e grafici-------------------------------------------------

load("C:/Users/Andrea/OneDrive/Desktop/DataScienceLAB/dati_meteo_aggregazione.RData")
df_meteo<-df_imputato

df_aux2 <- df_meteo %>% 
  group_by(data) %>% 
  summarize(Temperatura_aggregata=mean(Temperatura),
            Umidita_relativa_aggregata=mean(Umidita_relativa),
            Radiazione_globale_aggregata=mean(Radiazione_globale),
            Velocita_vento_aggregata=mean(Velocita_vento),
            Precipitazioni_aggregata=sum(Precipitazioni)) %>% 
  ungroup()

df_aux2$WEEKDAY<-format(df_aux2$data, "%u")
df_aux2_feriali<-df_aux2[!(df_aux2$WEEKDAY %in% c(6,7)) & format(df_aux2$data,"%m")=="06",]

bicocca_june <- rbind(u6_daily_ma_june,u1_daily_june)

sp2 <-  ggplot(u1_daily_june, aes(x = DATA, y = ENERGIA)) +
  geom_line(color = "blue") + 
  xlab(label = "Time") +
  ylab(label = "Energy") +
  ylim(500,4500)+
  facet_wrap( ~ format(DATA, "%Y"), scales = "free_x")
plot(sp2)
plot(sp)
grid.arrange(sp, sp2, nrow=2)

sp3 <-  ggplot(bicocca_june, aes(x = DATA, y = ENERGIA,colour=EDIFICIO)) +
  geom_line() + 
  xlab(label = "Time") +
  ylab(label = "Energy") +
  ylim(500,4500)+
  facet_wrap( ~ format(DATA, "%Y"), scales = "free_x")+
  theme(legend.position = "none")
plot(sp3)

sp4 <- ggplot(df_aux2_feriali, aes(x = data, y = Temperatura_aggregata)) +
  geom_line() + 
  xlab(label = "Time") +
  ylab(label = "Temperature") +
 # ylim(500,4500)+
  facet_wrap( ~ format(data, "%Y"), scales = "free_x")

#plot(sp3)
#plot(sp4)
grid.arrange(sp3, sp4, nrow=2)


 # Proviamo le serie intere senza domeniche e sabati ---------------------------------------


plot(ts(u6_daily_ma$ENERGIA[u6_daily_ma$WEEKDAY %in% c(1,2,3,4,5)]))
plot(ts(u1_daily$ENERGIA[u1_daily$WEEKDAY %in% c(1,2,3,4,5)]))
#plot(ts(df_aux2$Temperatura_aggregata))
