library(readr)
library(dplyr)
library(corrplot)
library(mgcv)
library(lattice)
library(tidyverse)
library(car)
library(lubridate)

library(datiInquinanti)
library(datiMeteo)

setwd("/home/rmorelli/R/pulvirus/analisi/GAM")

dm <- datiMeteo::dati_meteo %>% filter(station_eu_code == "IT0953A")

df <- filter(pm10, station_eu_code == "IT0953A") %>% 
  inner_join(dm, by = c("station_eu_code", "date") ) 
  
#Per vedere formato e nomi variabili
# str(df)

# creo un subset con covariate e concentrazione
dfSub <- df[c(5, 7:27)]

# statistica descrittiva ####
summary(dfSub)
quantile(dfSub$value, na.rm = TRUE)
boxplot(dfSub$value, na.rm=TRUE)
var(dfSub$value, na.rm=TRUE)
var(dfSub, na.rm=TRUE)
sd(dfSub$value, na.rm=TRUE)
dotchart(dfSub$value, xlab="PM10", ylab = "ORDINE DEI DATI")





# trasformazione logaritmica variabile dipendente e andamento ####
conc <- log(dfSub$value)
hist(conc)
qqnorm(log(dfSub$value))
qqline(log(dfSub$value))


# eseguo la standardizzazione di dfSub e aggiungo di nuovo la conc di PM10 non standardizzata ####
dfSubstand <- scale(dfSub) # standardizzazione

var <- dfSub["value"]
# colnames(pm10)[1] <- "pm10"

dfSubstandall <- cbind(as.data.frame( dfSubstand ), var$value)
names(dfSubstandall)[23] <- "v"

# analisi di correlazione ####
dfSub.cor1 <- cor(dfSubstandall, use = "pairwise.complete.obs")
corrplot(dfSub.cor1)
corrplot(dfSub.cor1, method = "number")
# library(psych)
# pairs.panels(dfSub.cor1)

# GAM non standardizzato #####
#solo per confrontare i risultati (Out1) con quelli (Out) del successivo modello Gam
#nota che per le componenti u10m e v10m del vento ho utilizzato l'interazione tra le 2 variabili
Out1 <- gam(log(value)~s(pwspeed) + s(t2m) + s(wspeed) + s(ptp) + s(tp) + s(u10m,v10m) + s(pblmax) + s(sp) + s(rh), data = dfSub)

# eseguo funzioni per valutare performance del modello e assunti di base
summary(Out1)
gam.check(Out1)
plot(Out1)
AIC(Out1)
BIC(Out1)
summary(residuals(Out1))
deviance(Out1)
sum(influence(Out1)) #degrees of fredoom modello totale


# GAM standardizzato #####
Out2 <- gam(log(v) ~ s(ptp) + s(pwspeed) + s(tmin2m) + s(tp, k = 13) + s(u10m, v10m) + s(pblmax, wspeed) + s(sp) + s(rh) + s(pblmin), 
           data = dfSubstandall)

# eseguo funzioni per valutare performance del modello e assunti di base ####
summary(Out2)
gam.check(Out2)
plot(Out2)
AIC(Out2)
BIC(Out2)
summary(residuals(Out2))
deviance(Out2)
sum(influence(Out2))


# calcolo pseudo julian day con anno d'inizio del contatore al primo gennaio 2012 ####
dfSubstandall$psjday <- as.numeric( df$date - ymd(20130101) )


# GAM su dataframe standardizzato e con julian day ####
Out3 <- gam(log(v) ~ s(pwspeed) + s(ptp) + s(tmin2m) + s(tp) + s(u10m, v10m) + s(pblmax,wspeed) + s(sp) + s(rh) + s(pblmin) + s(psjday, k = 15), data = dfSubstandall)

summary(Out3)
gam.check(Out3)
plot(Out3) # cercare di isolare i mesi
AIC(Out3)
BIC(Out3)

Gam.object <- Out3