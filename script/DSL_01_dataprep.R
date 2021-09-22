#### DSLab progetto 

rm(list = ls())

##### library utilizzate
library(stringr)
library(readxl)

#CUSTOM PATH DA CAMBIARE!!!!!!!!!!!!

#script dir
script_dir <- getwd()

#cambiate con il vostro path dei dati unzippati
data_dir <- '/Users/guglielmo/Desktop/DSLAB/Dati Energia'

#cambiate con il vostro path dove volete salvare i dati puliti (e scommentare ultime righe di codice)
save_dir <- '/Users/guglielmo/Desktop/DSLAB/data'

#funzioni personalizzate
commadot <- function(x){
  gsub(',','.',x)
}

#lo script gira su mac e linux, per windows sostituire le '/' nei paste0 con '\\' almeno credo

# dati u1 -------- 
data_u1 <- paste0(data_dir,'/',list.files(data_dir)[1])

data_u1_18 <- paste0(data_u1,'/',list.files(data_u1)[1])
data_u1_19 <- paste0(data_u1,'/',list.files(data_u1)[2])
data_u1_20 <- paste0(data_u1,'/',list.files(data_u1)[3])

# u1-2018 ---------
u1_2018 <- data.frame()
lista <- list.files(data_u1_18)
mesi_u1_18 <- paste0(c('Gennaio','Febbraio','Marzo','Aprile','Maggio','Giugno','Luglio','Agosto','Settembre',
                       'Ottobre','Novembre','Dicembre'),'2018')

for (m in mesi_u1_18){
  idx <- lista[str_detect(lista,m)]
  f <- paste0(data_u1_18,'/',idx)
  df <- read_excel(f)[,1:8]
  u1_2018 <- rbind(u1_2018,df)
}
lista
# u1-2019 --------
u1_2019 <- data.frame()
lista <- list.files(data_u1_19)
mesi_u1_19 <- paste0(c('Gennaio','Febbraio','Marzo','Aprile','Maggio','Giugno','Luglio','Agosto','Settembre',
                       'Ottobre','Novembre','dicembre'),'2019')

for (m in mesi_u1_19){
  idx <- lista[str_detect(lista,m)]
  for (i in idx){
    f <- paste0(data_u1_19,'/',i)
    if (m=='dicembre2019'){
      df <- read.csv(f,sep=';')[,1:8]
    } else {df <- read_excel(f)[,1:8]}
    u1_2019 <- rbind(u1_2019,df)
  }
}


# u1-2020 --------
u1_2020 <- data.frame()
lista <- list.files(data_u1_20)
#mesi_u1_20 <- lista[c(1,7,11,5,10,8,9,4,14,13,12,6)] usare i numeri sulla base dell'output lista (cambiano da pc a pc)
mesi_u1_20 <- c("Copia di U1_gennaio2020_14066_20200218142630_PDO_12883450152.csv","U1_febbraio2020_14848_20200318165340_PDO_12883450152.csv",          
                "U1_marzo2020_15115_20200406105853_PDO_12883450152.csv","U1_Aprile_2020_16325_20200513131423_PDO_12883450152.csv",   
                "U1_maggio_2020_16607_20200605122615_PDO_12883450152.csv","U1_giugno_17604_20200720130219_PDO_12883450152.csv",            
                "U1_Luglio_20200831112713541_IT012E00491869_20200701_20200731.csv","U1_ago_2020_20201202101351030_IT012E00491869_20200801_20200831.csv",
                "U1_sett_2020_20201202101338266_IT012E00491869_20200901_20200930.csv","U1_ott_2020_IT012E00491869_20201001_20201031.csv",                 
                "U1_nov_2020_20201202101319092_IT012E00491869_20201101_20201130.csv", "U1_DIC_2020_IT012E00491869.csv")
print(mesi_u1_20)
for (m in mesi_u1_20[1:6]){
  f <- paste0(data_u1_20,'/',m)
  df <- read.csv(f,sep=';')[,1:8]
  u1_2020 <- rbind(u1_2020,df)
}

for (m in mesi_u1_20[7:12]){
  f <- paste0(data_u1_20,'/',m)
  df <- read.csv(f,header = FALSE,skip = 1,sep = ';')[,1:8]
  colnames(df) <- colnames(u1_2020)
  u1_2020 <- rbind(u1_2020,df)
}

# dati u6 --------

data_u6<- paste0(data_dir,'/',list.files(data_dir)[2])

data_u6_18 <- paste0(data_u6,'/',list.files(data_u6)[1])
data_u6_19 <- paste0(data_u6,'/',list.files(data_u6)[2])
data_u6_20 <- paste0(data_u6,'/',list.files(data_u6)[3])

# u6-2018 ---------
u6_2018 <- data.frame()
lista <- list.files(data_u6_18)
mesi_u6_18 <- paste0(c('Gennaio','Febbraio','Marzo','Aprile','Maggio','Giugno','Luglio','Agosto','Settembre',
                       'Ottobre','Novembre','Dicembre'),'2018')

for (m in mesi_u6_18){
  idx <- lista[str_detect(lista,m)]
  for (i in idx){
    f <- paste0(data_u6_18,'/',i)
    df <- read_excel(f)[,1:8]
    u6_2018 <- rbind(u6_2018,df)
  }
}

# u6-2019 ----------
u6_2019 <- data.frame()
lista <- list.files(data_u6_19)
lista
#mesi_u6_19 <- as.vector(lista[c(5,4,10,2,3,9,6,7,1,12,11,14,13)]) usare i numeri sulla base dell'output lista (cambiano da pc a pc)
mesi_u6_19 <-c("Gennaio2019_11092_20191127155557_PDO_12883450152.xlsx","Febbraio2019_11093_20191127155557_PDO_12883450152.xlsx",                 
               "Marzo2019_11107_20191127155557_PDO_12883450152.xlsx","Aprile2019(1-29)_11094_20191127155558_PDO_12883450152.xlsx",            
               "Aprile2019(30)_11089_20191127155555_PDO_12883450152.xlsx","Maggio2019_11083_20191127155555_PDO_12883450152.xlsx",                   
               "Giugno2019_11084_20191127155556_PDO_12883450152.xlsx","Luglio2019_11090_20191127155556_PDO_12883450152_ELABORATO.xlsx",         
               "Agosto2019_11091_20191127155556_PDO_12883450152.xlsx","Settembre2019_11105_20191127155556_PDO_12883450152.xlsx",                
               "Ottobre2019_11106_20191127155556_PDO_12883450152.xlsx","U6_Novembre_2019_20201109120532475_IT012E00491824_20191101_20191130.csv",
               "U6_dicembre2019_14073_20200218144136_PDO_12883450152.csv")               

for (m in mesi_u6_19[1:11]){
  f <- paste0(data_u6_19,'/',m)
  if (m==mesi_u6_19[8]){
    df <- read_excel(f,col_types = c("text", "skip", "numeric", "numeric", "skip", "skip", 
                                     "numeric","numeric", "numeric", "text", "text"))
  }
  else {df <- read_excel(f)[,1:8]}
  u6_2019 <- rbind(u6_2019,df)
}

for (m in mesi_u6_19[12:13]){
  f <- paste0(data_u6_19,'/',m)
  df <- read.csv(f,header = FALSE,skip = 1,sep = ';')[,1:8]
  colnames(df) <- colnames(u6_2019)
  u6_2019 <- rbind(u6_2019,df)
}

# u6-2020 -------
u6_2020 <- data.frame()
lista <- list.files(data_u6_20)
#mesi_u6_20 <- lista[c(6,4,12,2,11,8,10,1,16,15,14,3)] usare i numeri sulla base dell'output lista (cambiano da pc a pc)
mesi_u6_20 <- c("U6_gennaio2020_14074_20200218144136_PDO_12883450152.csv","U6_febbraio2020_15629_20200417144935_PDO_12883450152.csv",          
                "U6_marxo_2020_15628_20200417144434_PDO_12883450152.csv","U6_Apr_2020_16230_20200511101230_PDO_12883450152.csv",              
                "U6_maggio_2020_16608_20200605122654_PDO_12883450152.csv","U6_giugno_17604_20200720130219_PDO_12883450152.csv",                
                "U6_luglio_20200831112745023_IT012E00491824_20200701_20200731.xlsx","U6_Agosto_20201109_114712332_IT012E00491824_20200801_20200831.csv", 
                "U6_sett_2020_IT012E00491824_20200901_20200930.csv","U6_Ottobre_20201109_114307412_IT012E00491824_20201001_20201031.csv",
                "U6_NOV_2020_IT012E00491824_20201101_20201130.csv","U6_Dic_20210_IT012E00491824.csv")  

for (m in mesi_u6_20[1:7]){
  f <- paste0(data_u6_20,'/',m)
  if (m==mesi_u6_20[7]){
    df <- read_excel(f,col_names = FALSE, col_types = c("text", 
                                                        "numeric", "skip", "skip", "skip", 
                                                        "skip", "skip", "numeric", 
                                                        "numeric", "text", "text", "numeric", 
                                                        "text"), skip = 2) #skip 2 guardare il file luglio per capire il perchè
    colnames(df) <- colnames(u6_2020)
  } else {df <- read.csv(f,sep=';')[,1:8]}
  u6_2020 <- rbind(u6_2020,df)
}
for (m in mesi_u6_20[8:12]){
  f <- paste0(data_u6_20,'/',m)
  df <- read.csv(f,header = FALSE,skip = 1,sep = ';')[,1:8]
  colnames(df) <- colnames(u6_2020)
  u6_2020 <- rbind(u6_2020,df)
}

rm(list = c('df','f','i','idx','lista','m'))

# data cleaning -----------

#teniamo conto del fatto che i giorni dovrebbero essere 365 (366 per il 2020 poichè bisestile)
#per ogni giorno ci dovrebbero poi essere 96 osservazioni: quarti d'ora * 24 ore = 4*24 = 96
#per 2018 e 2019: 96*365=35040
#per 2020: 96*366=35136

#u1_2018


dim(u1_2018) #numero di osservazioni giuste
dim(na.omit(u1_2018)) #nessun NA
length(unique(u1_2018$DATA)) #numero di giorni giusto

table(table(u1_2018$DATA)) #25 marzo e 28 ottobre da valutare
which(table(u1_2018$DATA)==92) #25 marzo (idx 84)
which(table(u1_2018$DATA)==100) #28 ottobre (idx 301)

#u1_2019

dim(u1_2019) #numero di osservazioni sbagliate
dim(na.omit(u1_2019)) #2 NA
u1_2019[complete.cases(u1_2019)==FALSE,] # probabilmente sono valori aggregati o simili, dunque si scartano

u1_2019 <- u1_2019[complete.cases(u1_2019),]
dim(u1_2019) #numero di osservazioni sbagliate, probabili duplicati
dim(na.omit(u1_2019)) #nessun NA

length(unique(u1_2019$DATA)) #numero di giorni giusto
table(table(u1_2019$DATA))
which(table(u1_2019$DATA)==92) #31 marzo (idx 90) cambio da solare a legale +1 ---> -4
which(table(u1_2019$DATA)==100) #27 ottobre (idx 300) cambio da legale a solare -1 ---> +4
which(table(u1_2019$DATA)==192) #30 aprile dà problemi

u1_2019 <- u1_2019[!duplicated(u1_2019),]
dim(u1_2019)
table(table(u1_2019$DATA))
#rimossi i duplicati del 30 aprile

#u1_2020

dim(u1_2020) #numero di osservazioni sbagliate
dim(na.omit(u1_2020)) #houston we got a problem
colSums(is.na(u1_2020)) #NA solo per potenza_massima, che non ci serve quindi si può ignorare al momento

length(unique(u1_2020$DATA)) #numero di giorni giusto
table(table(u1_2020$DATA)) #abbiamo un giorno di duplicati e nessun giorno da legale a solare, magari coincidono
which(table(u1_2020$DATA)==92) #29 marzo
which(table(u1_2020$DATA)==192) #25 ottobre

df <- u1_2020[u1_2020$DATA=='20201025',] #i dati ci sono due volte, vanno eliminati dunque i duplicati
duplicated(df) #non ci sono duplicati, situa problematica
View(df)
dim(df[!df$CONSUMO_ATTIVA_PRELEVATA==0,]) #si risolve togliendo le oss con consumo=0
rm(df)

u1_2020 <- u1_2020[!(u1_2020$DATA=='20201025' & u1_2020$CONSUMO_ATTIVA_PRELEVATA==0),]
dim(u1_2020)
table(table(u1_2020$DATA))

#u6_2018

dim(u6_2018) #numero di osservazioni giuste
dim(na.omit(u6_2018)) #nessun NA
length(unique(u6_2018$DATA)) #numero di giorni giusto

table(table(u6_2018$DATA))
which(table(u6_2018$DATA)==92) #25 marzo (idx 84)
which(table(u6_2018$DATA)==100) #28 ottobre (idx 301)

#u6_2019

dim(u6_2019) #numero di osservazioni giuste
dim(na.omit(u6_2019)) #NA problem
colSums(is.na(u6_2019)) #sempre potenza massima, che ignoriamo
length(unique(u6_2019$DATA)) #numero di giorni giusto

table(table(u6_2019$DATA)) 
which(table(u6_2019$DATA)==92) #31 marzo
which(table(u6_2019$DATA)==100) #27 ottonre

#u6_2020
dim(u6_2020) #numero di osservazioni sbagliate
dim(na.omit(u6_2020)) #NA problem
colSums(is.na(u6_2020)) #sempre potenza massima, che ignoriamo
length(unique(u6_2020$DATA))

table(table(u6_2020$DATA)) #abbiamo un giorno di duplicati e nessun giorno da legale a solare, magari coincidono
which(table(u6_2020$DATA)==92) #29 marzo
which(table(u6_2020$DATA)==192) #25 ottobre

df <- u6_2020[u6_2020$DATA=='20201025',] #i dati ci sono due volte, vanno eliminati dunque i duplicati
duplicated(df) #non ci sono duplicati, situa problematica
View(df)
dim(df[!df$CONSUMO_ATTIVA_PRELEVATA==0,]) #si risolve togliendo le oss con consumo=0
rm(df)

u6_2020 <- u6_2020[!(u6_2020$DATA=='20201025' & u6_2020$CONSUMO_ATTIVA_PRELEVATA==0),]
dim(u6_2020)
table(table(u6_2020$DATA))

# data preparation --------

# data types
# POD                                 DATA                                  ORA                        FL_ORA_LEGALE             CONSUMO_ATTIVA_PRELEVATA 
# "character"                         "Date"                            "numeric"                      "numeric"                            "numeric" 
# CONSUMO_REATTIVA_INDUTTIVA_PRELEVATA                      POTENZA_MASSIMA                            
# "numeric"                                                     "numeric"                         

#u1_2018
sapply(u1_2018,class)
u1_2018$POTENZA_MASSIMA <- as.numeric(u1_2018$POTENZA_MASSIMA)
u1_2018$DATA <- as.Date(as.character(u1_2018$DATA),format =  "%Y%m%d")
u1_2018$TIPO_DATO <- NULL
sapply(u1_2018,class)

#u1_2019
sapply(u1_2019,class)
u1_2019$POTENZA_MASSIMA <- as.numeric(u1_2019$POTENZA_MASSIMA)
u1_2019$CONSUMO_ATTIVA_PRELEVATA <- as.numeric(commadot(u1_2019$CONSUMO_ATTIVA_PRELEVATA))
u1_2019$CONSUMO_REATTIVA_INDUTTIVA_PRELEVATA <- as.numeric(commadot(u1_2019$CONSUMO_REATTIVA_INDUTTIVA_PRELEVATA))
u1_2019$POTENZA_MASSIMA <- as.numeric(u1_2019$POTENZA_MASSIMA)
u1_2019$DATA <- as.Date(as.character(u1_2019$DATA),format =  "%Y%m%d")
u1_2019$TIPO_DATO <- NULL

sapply(u1_2019,class)

#u1_2020
sapply(u1_2020,class)
u1_2020$POTENZA_MASSIMA <- as.numeric(u1_2020$POTENZA_MASSIMA)
u1_2020$CONSUMO_ATTIVA_PRELEVATA <- as.numeric(commadot(u1_2020$CONSUMO_ATTIVA_PRELEVATA))
u1_2020$CONSUMO_REATTIVA_INDUTTIVA_PRELEVATA <- as.numeric(commadot(u1_2020$CONSUMO_REATTIVA_INDUTTIVA_PRELEVATA))
u1_2020$POTENZA_MASSIMA <- as.numeric(u1_2020$POTENZA_MASSIMA)
u1_2020$DATA <- as.Date(as.character(u1_2020$DATA),format =  "%Y%m%d")
u1_2020$ORA <- as.numeric(u1_2020$ORA)
u1_2020$FL_ORA_LEGALE <- as.numeric(u1_2020$FL_ORA_LEGALE)
u1_2020$TIPO_DATO <- NULL
sapply(u1_2020,class)

#u6_2018
sapply(u6_2018,class)
u6_2018$POTENZA_MASSIMA <- as.numeric(u6_2018$POTENZA_MASSIMA)
u6_2018$DATA <- as.Date(as.character(u6_2018$DATA),format =  "%Y%m%d")
u6_2018$TIPO_DATO <- NULL
sapply(u6_2018,class)

#u6_2019
sapply(u6_2019,class)
u6_2019$POTENZA_MASSIMA <- as.numeric(u6_2019$POTENZA_MASSIMA)
u6_2019$CONSUMO_ATTIVA_PRELEVATA <- as.numeric(commadot(u6_2019$CONSUMO_ATTIVA_PRELEVATA))
u6_2019$CONSUMO_REATTIVA_INDUTTIVA_PRELEVATA <- as.numeric(commadot(u6_2019$CONSUMO_REATTIVA_INDUTTIVA_PRELEVATA))
u6_2019$POTENZA_MASSIMA <- as.numeric(u6_2019$POTENZA_MASSIMA)
u6_2019$DATA <- as.Date(as.character(u6_2019$DATA),format =  "%Y%m%d")
u6_2019$TIPO_DATO <- NULL

sapply(u6_2019,class)

#u6_2020
sapply(u6_2020,class)
u6_2020$POTENZA_MASSIMA <- as.numeric(u6_2020$POTENZA_MASSIMA)
u6_2020$CONSUMO_ATTIVA_PRELEVATA <- as.numeric(commadot(u6_2020$CONSUMO_ATTIVA_PRELEVATA))
u6_2020$CONSUMO_REATTIVA_INDUTTIVA_PRELEVATA <- as.numeric(commadot(u6_2020$CONSUMO_REATTIVA_INDUTTIVA_PRELEVATA))
u6_2020$POTENZA_MASSIMA <- as.numeric(u6_2020$POTENZA_MASSIMA)
u6_2020$DATA <- as.Date(as.character(u6_2020$DATA),format =  "%Y%m%d")
u6_2020$ORA <- as.numeric(u6_2020$ORA)
u6_2020$FL_ORA_LEGALE <- as.numeric(u6_2020$FL_ORA_LEGALE)
u6_2020$TIPO_DATO <- NULL
sapply(u6_2020,class)

# data export (stesso discorso di cambiare gli slash con doppio backslash per windows se dà problemi) --------
#SCOMMENTARE SE SI VUOLE SALVARE

# write.csv(u1_2018,paste0(save_dir,'/u1_2018.csv'),row.names = FALSE)
# write.csv(u1_2019,paste0(save_dir,'/u1_2019.csv'),row.names = FALSE)
# write.csv(u1_2020,paste0(save_dir,'/u1_2020.csv'),row.names = FALSE)
# write.csv(u6_2018,paste0(save_dir,'/u6_2018.csv'),row.names = FALSE)
# write.csv(u6_2019,paste0(save_dir,'/u6_2019.csv'),row.names = FALSE)
# write.csv(u6_2020,paste0(save_dir,'/u6_2020.csv'),row.names = FALSE)
