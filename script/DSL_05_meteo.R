library(dplyr)
library(sqldf)
library(mice)
library(forecast)
library(TSA)
library(EnvStats)
library(MASS)
library(tseries)
library(xts)
library(DataCombine)
library(DescTools)

# cambiare directory dati meteo e non cambiare i nomi dei files, creare delle cartelle dentro
# la directory rispettivamente 2018 2019 e 2020

meteo_dir <- '/Users/guglielmo/Desktop/DSLAB/raw_meteo/'

#temperatura

temp_2018 <- read.csv(paste0(meteo_dir,'RW_20210712101804_447153_4058_1.csv'))
temp_2019 <- read.csv(paste0(meteo_dir,'RW_20210712101807_447154_4058_1.csv'))
temp_2020 <- read.csv(paste0(meteo_dir,'RW_20210712102304_447156_4058_1.csv'))
temp <- rbind(temp_2018,temp_2019,temp_2020)
rm(temp_2018,temp_2019,temp_2020)

#umidità
um_2018 <- read.csv(paste0(meteo_dir,'RW_20210712101805_447153_4059_1.csv'))
um_2019 <- read.csv(paste0(meteo_dir,'RW_20210712101807_447154_4059_1.csv'))
um_2020 <- read.csv(paste0(meteo_dir,'RW_20210712102304_447156_4059_1.csv'))
um <- rbind(um_2018,um_2019,um_2020)
rm(um_2018,um_2019,um_2020)

#radiazione globale
rad_2018 <- read.csv(paste0(meteo_dir,'RW_20210712101805_447153_4060_1.csv'))
rad_2019 <- read.csv(paste0(meteo_dir,'RW_20210712101808_447154_4060_1.csv'))
rad_2020 <- read.csv(paste0(meteo_dir,'RW_20210712102305_447156_4060_1.csv'))
rad <- rbind(rad_2018,rad_2019,rad_2020)
rm(rad_2018,rad_2019,rad_2020)

#vento
vento_2018 <- read.csv(paste0(meteo_dir,'RW_20210712101806_447153_4064_1.csv'))
vento_2019 <- read.csv(paste0(meteo_dir,'RW_20210712101808_447154_4064_1.csv'))
vento_2020 <- read.csv(paste0(meteo_dir,'RW_20210712102305_447156_4064_1.csv'))
vento <- rbind(vento_2018,vento_2019,vento_2020)
rm(vento_2018,vento_2019,vento_2020)

#precipitazioni
prec_2018 <- read.csv(paste0(meteo_dir,'RW_20210712101806_447153_4065_4.csv'))
prec_2019 <- read.csv(paste0(meteo_dir,'RW_20210712101809_447154_4065_4.csv'))
prec_2020 <- read.csv(paste0(meteo_dir,'RW_20210712102306_447156_4065_4.csv'))
prec <- rbind(prec_2018,prec_2019,prec_2020)
rm(prec_2018,prec_2019,prec_2020)

#pre-processing
names(temp)[3] <- 'Temperatura'
names(rad)[3] <- 'Radiazione_globale'
names(um)[3] <- 'Umidita_relativa'
names(prec)[3] <- 'Precipitazioni'
names(vento)[3] <- 'Velocita_vento'

um$Id.Sensore <- NULL
temp$Id.Sensore <- NULL
rad$Id.Sensore <- NULL
prec$Id.Sensore <- NULL
vento$Id.Sensore <- NULL

#merge

df_1 <- merge(temp,um,by='Data.Ora')
df_2 <- merge(df_1,rad,by='Data.Ora')
df_3 <- merge(df_2,vento,by='Data.Ora')
df_meteo <- merge(df_3,prec,by='Data.Ora')
df_meteo$data<-substr(df_meteo$Data.Ora,1,10)
df_meteo$data<-as.Date(df_meteo$data, "%Y/%m/%d")
df_meteo$week<-format(df_meteo$data, "%V")
df_meteo$year<-format(df_meteo$data, "%G")

summary(df_meteo) #Si nota che ci sono degli NA riportati con -999

#andare a vedere outlier radiazione globale
#24 luglio 2020 6:00, pioggia torrenziale max 73.6mm

df_meteo$Temperatura<-ifelse(df_meteo$Temperatura==-999,NA,df_meteo$Temperatura)
df_meteo$Umidita_relativa<-ifelse(df_meteo$Umidita_relativa==-999,NA,df_meteo$Umidita_relativa)
df_meteo$Radiazione_globale<-ifelse(df_meteo$Radiazione_globale==-999,NA,df_meteo$Radiazione_globale)
df_meteo$Velocita_vento<-ifelse(df_meteo$Velocita_vento==-999,NA,df_meteo$Velocita_vento)
df_meteo$Precipitazioni<-ifelse(df_meteo$Precipitazioni==-999,NA,df_meteo$Precipitazioni)

summary(df_meteo)
sapply(df_meteo, function(x) sum(is.na(x))/nrow(df_meteo))

# Imputazione valori mancanti ----------------------------------------------

#ci mette un pò a girare
#imputato<-mice(df_meteo,m=1,maxit=1000,meth='pmm',seed=500)

imputato$imp$Temperatura
imputato$imp$Umidita_relativa
imputato$imp$Radiazione_globale
imputato$imp$Velocita_vento
imputato$imp$Precipitazioni

df_imputato<-complete(imputato,1)

#Aggregazione

df_meteo<-df_imputato

df_aux <- df_meteo %>% 
  group_by(year,week) %>% 
  summarize(Temperatura_aggregata=mean(Temperatura),
            Umidita_relativa_aggregata=mean(Umidita_relativa),
            Radiazione_globale_aggregata=mean(Radiazione_globale),
            Velocita_vento_aggregata=mean(Velocita_vento),
            Precipitazioni_aggregata=sum(Precipitazioni)) %>% 
  ungroup()

#Merge con u1_weekly (per i modelli)

meteo_aggregato<-merge(u1_weekly,df_aux, by=c("year","week"))
names(meteo_aggregato)

write.csv(meteo_aggregato,'aggregato.csv',row.names = FALSE)

#analizzo andamento energia giugno (solo lavorativi) e temperatura

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

#sp: grafico energia giugno tutti i giorni
sp <-  ggplot(u6_daily_ma_june, aes(x = DATA, y = ENERGIA)) +
  geom_line(color = "blue") + 
  xlab(label = "Time") +
  ylab(label = "Energy") +
  ylim(500,4500)+
  facet_wrap( ~ format(DATA, "%Y"), scales = "free_x")
plot(sp)

#sp2: grafico energia giugno senza weekend
sp2 <-  ggplot(u1_daily_june, aes(x = DATA, y = ENERGIA)) +
  geom_line(color = "blue") + 
  xlab(label = "Time") +
  ylab(label = "Energy") +
  ylim(500,4500)+
  facet_wrap( ~ format(DATA, "%Y"), scales = "free_x")
plot(sp2)
plot(sp)
dev.off()
grid.arrange(sp, sp2, nrow=2)


#
sp3 <-  ggplot(bicocca_june, aes(x = DATA, y = ENERGIA, colour=EDIFICIO)) +
  geom_line() + 
  xlab(label = "Time") +
  ylab(label = "Energy") +
  ylim(500,4500)+
  facet_wrap( ~ format(DATA, "%Y"), scales = "free_x")+
  theme(legend.position = "top")
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
u6_daily_ma$WEEKDAY<-format(u6_daily_ma$DATA, "%u")
u1_daily$WEEKDAY<-format(u1_daily$DATA, "%u")

plot(ts(u6_daily_ma$ENERGIA[u6_daily_ma$WEEKDAY %in% c(1,2,3,4,5)]))
plot(ts(u1_daily$ENERGIA[u1_daily$WEEKDAY %in% c(1,2,3,4,5)]))

#plot(ts(df_aux2$Temperatura_aggregata))
