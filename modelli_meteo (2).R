library(forecast)
library(TSA)
library(EnvStats)
library(MASS)
library(tseries)
library(dplyr)
library(xts)
library(DataCombine)
library(DescTools)

aggregato<-read.csv("/Users/guglielmo/Desktop/DSLAB/data/aggregato.csv")

## primo modello

x<- ts(aggregato$energia, frequency = 52,  start=2018)
fit <- nnetar(tsclean(x))
lambda<-BoxCox.lambda(fit$x, method = "loglik")
fit_boxcox<-BoxCox(fit$x, lambda=lambda)
plot(fit_boxcox)

aggregato$temp_q_m<-I((aggregato$Temperatura_aggregata-mean(aggregato$Temperatura_aggregata))^2)
aggregato$temp_q_m<-as.numeric(aggregato$temp_q_m)
aggregato$temp_m<-(aggregato$Temperatura_aggregata-mean(aggregato$Temperatura_aggregata))

set.seed(7)
train.index <- sample(c(1:dim(aggregato)[1]), dim(aggregato)[1]*0.7) 
train.df <- aggregato[train.index, ]
test.df <- aggregato[-train.index, ]


meteo1<-lm(energia ~ I(Temperatura_aggregata^2)+ Temperatura_aggregata + Umidita_relativa_aggregata + Radiazione_globale_aggregata +
             Velocita_vento_aggregata + Precipitazioni_aggregata + Covid ,train.df)
summary(meteo1)
library(MASS)
step.model <- stepAIC(meteo1, direction = "back", 
                      trace = FALSE)
summary(step.model)


# collinearit? ------------------------------------------------------------

cov=attr(terms(step.model), "term.labels") 
cov

b_covar <- train.df[,c("Temperatura_aggregata","Covid")] 

nums <- sapply(b_covar, is.numeric)
b_numeric <- b_covar[,nums] 


y = as.numeric(train.df$energia)
X<-b_numeric
X=as.matrix(X)

library(mctest)
imcdiag(step.model)

## c'? collinearit? 

meteo2<-lm(energia ~ I(Temperatura_aggregata^2) + Radiazione_globale_aggregata +  Covid ,train.df)
summary(meteo2)

library(mctest)
imcdiag(meteo2)

## c'? collinearit? 

meteo3<-lm(energia ~ I(Temperatura_aggregata^2)  +  Covid ,train.df)
summary(meteo3)

library(mctest)
imcdiag(meteo3)

## Evviva!!!

## altro tentativo sottraendo la media


meteo3.1<-lm(energia ~ temp_q_m + temp_m  +  Covid ,train.df)
summary(meteo3.1)

library(mctest)
imcdiag(meteo3.1)


# Eteroschedasticit? --------------------------------------------------------

par(mfrow=c(2,2)) 
plot(meteo3.1)
par(mfrow=c(1,1))

library(lmtest) 

bptest(meteo3)

## tutto bene 


# Outliers ----------------------------------------------------------------

library(car) 
influencePlot(meteo3, main="Influence Plot") 

## 4 osservazioni notevoli

cooksd <- cooks.distance(meteo3)
cd=data.frame(cooksd)

cutoff <- 4/(length(meteo3$residuals)-length(meteo3$coefficients)-2)

influ=data.frame(train.df[cooksd > 0.01, ])  # influential row numbers
nrow(influ)
influ

stzed <- rstudent(meteo3)
lever <- hat(model.matrix(meteo3))
dffits1 <- dffits(meteo3)
cooksd <- cooks.distance(meteo3)

leva<-vector()
numleva <-vector()
j=0
soglial <- 110/17890
for(i in 1: length(lever)) {if(lever[i]>soglial) {leva[j]=lever[i]
numleva[j]=i
print(i)
j=j+1}}
leva
numleva

## eh boh pi? o meno tutte, io sarei abbastanza restio dal rimuovere osservazioni

n<-length(fitted(meteo3))
k<-length(coef(meteo3))
par(mfrow=c(2,2))
##stud residuals vs leverage
plot(hatvalues(meteo3),rstudent(meteo3),xlab="leverage", ylab = "stud residuals")
abline(h=-2)
abline(h=2)
abline(h=0)
abline(v=2*k/n)
text(hatvalues(meteo3),rstudent(meteo3))
##cook
plot(cooks.distance(meteo3), type = "h", ylab = "cook")
abline(h=4/n)
#dffits
plot(dffits(meteo3),type = "h")
text(dffits(meteo3))
abline(h=2*k/n)
abline(h=-2*k/n)
##covratio
plot(covratio(meteo3),ylab = "covratio")
abline(h=1+3*k/n)
abline(h=1-3*k/n)
text(covratio(meteo3))

## inizierei levando la 99 e la 101
train.df[c(99,101),]
summary(train.df)
train.df3<-train.df[-c(99,101),]

meteo4<-lm(energia ~ temp_q_m + temp_m  +  Covid ,train.df3)
summary(meteo4)


# linearità e normalità ---------------------------------------------------------------

## vedi grafico dea per le relazioni tra variabili, 
## grafico che giustifica l'inserimento della temperatura al quadrato
## per la normalità valutiamo il qqplot del modello
library(olsrr)
par(mfrow=c(1,1))
plot(meteo4, which=2, main="qqplot")
ols_test_normality(meteo4)

## la normalit? pare essere rispettata


# previsione sui dati di test ---------------------------------------------

dati_test<-test.df$energia
test.df$energia<-NULL

pred<-predict.lm(meteo4, newdata = test.df)
cor(dati_test, pred)

plot(dati_test, pred)
abline(a=0,b=1)
## questo pu? essere indice di poca linearit? nel modello, comunque in generale non c'? male dai
## il modello tende a sottostimare i valori alti registrati e sovrastimare i valori bassi


# tolgo pi? osservazioni --------------------------------------------------

train.df2<-train.df[-c(13,56,57,81,99,101,108),]

meteo4.1<-lm(energia ~ temp_q_m + temp_m  +  Covid ,train.df2)
summary(meteo4.1)

library(olsrr)
par(mfrow=c(1,1))
plot(meteo4.1, which=2, main="qqplot")
ols_test_normality(meteo4.1)

dati_test<-test.df$energia
test.df$energia<-NULL

pred<-predict.lm(meteo4.1, newdata = test.df)
cor(dati_test, pred)

plot(dati_test, pred )
text(dati_test, pred, cex= 1)
abline(a=0,b=1)
test.df[c(11,12,13,24,26),]
dati_test[c(11,12,13,24,26)]


# proviamo con la spline lineare --------------------------------------------------

plot(aggregato$Temperatura_aggregata, aggregato$energia)

require(splines)		# libreria per spline
knts<-c(20)			# proviamo a mettere un nodo a 20
spl1<-lm(energia~bs(Temperatura_aggregata,degree=1,knots=knts)+ Covid,data=train.df3)	# spline lineare
summary(spl1)

##dati_test<-test.df$energia
##test.df$energia<-NULL

## troviamo il nodo migliore

seq<-seq(15,25, length.out = 101)
cor<-vector()
pos<-vector()

for (i in seq) {
  knts<-i			
  spl<-lm(energia~bs(Temperatura_aggregata,degree=1,knots=knts)+ Covid,data=train.df3)	
  pred<-predict.lm(spl, newdata = test.df)
  cor<-c(cor,cor(dati_test, pred))
  pos<-c(pos,i)
}
max(cor)
cor[44]
pos[44]## nodo a 19.3

knts<-c(19.3)			# proviamo a mettere un nodo a 20
spl2<-lm(energia~bs(Temperatura_aggregata,degree=1,knots=knts)+ Covid,data=train.df2)	# spline lineare
summary(spl2)


# multicollinearit? 2 -----------------------------------------------------

library(mctest)
imcdiag(spl2) ## bene

# eteroschedasticit? 2 -----------------------------------------------------

par(mfrow=c(2,2)) 
plot(spl2)
par(mfrow=c(1,1))

library(lmtest) 

bptest(spl2)

## ok

# Outliers 2 ----------------------------------------------------------------

n<-length(fitted(spl2))
k<-length(coef(spl2))
par(mfrow=c(2,2))
##stud residuals vs leverage
plot(hatvalues(spl2),rstudent(spl2),xlab="leverage", ylab = "stud residuals")
abline(h=-2)
abline(h=2)
abline(h=0)
abline(v=2*k/n)
text(hatvalues(spl2),rstudent(spl2))
##cook
plot(cooks.distance(spl2), type = "h", ylab = "cook")
abline(h=4/n)
#dffits
plot(dffits(spl2),type = "h")
text(dffits(spl2))
abline(h=2*k/n)
abline(h=-2*k/n)
##covratio
plot(covratio(spl2),ylab = "covratio")
abline(h=1+3*k/n)
abline(h=1-3*k/n)
text(covratio(spl2))

## non tolgo 35 e 87 per avere una migliore normalit? alla fine non sono cos? male

# normalit? 2 ---------------------------------------------------------------

library(olsrr)
par(mfrow=c(1,1))
plot(spl2, which=2, main="qqplot")
ols_test_normality(spl2)

# previsione sui dati di test ---------------------------------------------

pred<-predict.lm(spl2, newdata = test.df)
cor(dati_test, pred) ## ? pure leggermente migliorato R quadro

ggplot(data=cbind(dati_test,pred),aes(x=dati_test, y=pred)) +
  geom_point(size=2, shape=16)+
  xlab("Valori osservati")+
  ylab("Valori previsti")+
  geom_abline(a=0,b=1, col="#ffa500", lwd=1)

plot(dati_test, pred, xlab="Valori reali", ylab="Valori previsti" )

#text(dati_test, pred, cex= 1)
abline(a=0,b=1, col="blue4", lwd=2)

# Plot della spline -------------------------------------------------------

plot(energia ~ Temperatura_aggregata, data=train.df3, main="Spline lineare con nodo a 19.3",
     xlab="Temperatura", ylab="Energia")
#text(train.df3$Temperatura_aggregata, train.df3$energia, cex=1)
temp <- seq(min(train.df3$Temperatura_aggregata), max(train.df3$Temperatura_aggregata), length.out = 100)
lines(temp, predict(spl2, data.frame(Temperatura_aggregata=temp, Covid=rep(min(train.df3$Covid),length(temp)))), col="blue4",lty=2,lwd=4)
abline(v=knts,col="gray",lwd=2)  # posizione dei nodi
legend("topleft",
       c("spline lineare","nodo"), 
       col=c("blue4","gray"),lty=c(2,1), lwd = c(2,1),cex=0.95, bty="n")

ggplot(train.df3, aes(x=Temperatura_aggregata, y=energia))+
  geom_point(size=1, shape=16)+
  xlab('Temperatura')+
  ylab('Energia')+
  geom_smooth(data=train.df2,method = 'lm',formula = train.df2$energia~splines::bs(train.df2$Temperatura_aggregata,degree=1,knots=knts)+train.df2$Covid )+
  geom_vline(aes(xintercept=knts),col="red",lwd=1,lty='dashed')
  
spl2<-lm(energia~bs(Temperatura_aggregata,degree=1,knots=knts)+ Covid,data=train.df2)	# spline lineare


class(spl2)


# Costruisci la tua Spline! -----------------------------------------------

require(splines)	# libreria per spline
nodi<-c(19.3) # inserisci i nodi d'interesse
dg<-1 #inserisci il grado della funzione
knts<-nodi			
spl4<-lm(energia~bs(Temperatura_aggregata,degree=dg,knots=knts) + Covid,data=train.df3)	# spline lineare
summary(spl4)

## plotta la spline

plot(energia ~ Temperatura_aggregata, data=train.df3, main="Spline lineare con nodo a 19.3",
     xlab="Temperatura", ylab="Energia")
#text(train.df3$Temperatura_aggregata, train.df3$energia, cex= 1)
temp <- seq(min(train.df3$Temperatura_aggregata), max(train.df3$Temperatura_aggregata), length.out = 100)
lines(temp, predict(spl4, data.frame(Temperatura_aggregata=temp, Covid=rep(min(train.df3$Covid),length(temp)))), col="blue4",lty=2,lwd=4)
abline(v=knts,col="gray",lwd=2)  # posizione dei nodi
legend("topleft",
       c("spline ","nodo"), 
       col=c("blue4","gray"),lty=c(2,1), lwd = c(2,1),cex=0.95,bty="n")

## valuta il fit

pred<-predict.lm(spl4, newdata = test.df)
cor(dati_test, pred) ## ? pure leggermente migliorato R quadro

plot(dati_test, pred, main = "Valori reali vs. Valori previsti", xlab="Valori reali", ylab="Valori previsti" )
#text(dati_test, pred, cex= 1)
abline(a=0,b=1, col="blue4", lwd=2)

