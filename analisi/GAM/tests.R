# pacchetti ####
library(lubridate)
library(purrr)
library(mgcv)
library(dplyr)
library(knitr)

library(datiInquinanti)
library(datiMeteo)
setwd("~/R/pulvirus/analisi/GAM")

# fegatelli ####
# station_eu_code == "IT0953A" | station_eu_code == "IT0888A"

# argomenti ####
args <- commandArgs(trailingOnly = TRUE)

cat(args[1], args[2], "\n", sep = " ---- ")

pltnt <- args[1]
region_id <- args[2]

if(is.na(args[1])) {
  pltnt <- "pm10"
  cod_reg <- 2  
}


# preparazione dei dati ####
df <- inner_join(get(pltnt) %>% filter( reporting_year >= 2016), dati_meteo, by = c("station_eu_code", "date") ) %>% 
  inner_join(
    stazioniAria %>% 
      filter(region_id == cod_reg) %>% 
      select(c("station_eu_code")), by = c("station_eu_code")
    ) %>% 
  mutate(value = ifelse(value <= 0, 0.2, value) )

# subset con covariate e concentrazione
dfSub <- df %>% 
  select(-c(date, pollutant_fk, station_code, coordx, coordy, altitude, altitudedem))


# le variabili di interesse
vars <- c("value", "t2m", "tmin2m", "tmax2m", "tp", "ptp", "rh", "u10m", "v10m",
          "sp", "nirradiance", "pbl00", "pbl12", "pblmin", "pblmax", "wdir", "wspeed", "pwspeed")

# standardizzazione
dfSubStand <- as.data.frame(scale(dfSub[,vars]))

# aggiungo codice stazione e valore originale 
dfSubStand$v <- dfSub$value
dfSubStand$station_eu_code <- dfSub$station_eu_code
dfSubStand$jd = as.numeric( df$date - ymd(20130101) )

# sperimentazioni modello ####

# start_time <- Sys.time() 
# calcolo del modello per tutte le stazioni
# models <- dfSubStand %>%
#   split(.$station_eu_code) %>%
#   map(~gam(log(v) ~ s(jd) + s(pwspeed) + s(wspeed) + s(tp) + s(sp) + s(ptp) + s(nirradiance) + s(rh) , 
#            data = .) )
# end_time <- Sys.time()
# end_time - start_time # tempo di esecuzione

# Estraiamo gli R-sq.(adj) ed AIC per tutti i modelli
# models %>% 
#   map(summary.gam) %>% 
#   map_dbl(~.$r.sq) %>% 
#   round(3) -> rsq

# models %>% map_dbl(AIC) -> aics

# sink("esempio.md")
# inner_join(
#   data.frame(attributes(aics), aics, row.names = NULL),
#   data.frame(attributes(rsq), rsq, row.names = NULL),
#   by = "names"
# ) %>% kable(format = "pipe", padding = 6) %>% print()
# sink()

# sink("esempio.md", append = TRUE)
# my_list %>% kable(format = "pipe", padding = 6) %>%  print()
# sink()


# eval(parse(text = "gam(log(v) ~ s(jd) + s(pwspeed), data = dfSubStand)"))

# ESEMPIO 1 ####
# vars <- c("t2m", "tmin2m", "tmax2m", "tp", "ptp", "rh", "u10m", "v10m",
#           "sp", "nirradiance", "pbl00", "pbl12", "pblmin", "pblmax", "wdir", "wspeed", "pwspeed", "jd")
# 
# # le combinazioni di classe 4
# c1 <- combn(vars, 17) %>% 
#   data.frame()
# 
# # sistemiamo le "spline"
# y <- lapply(c1, function(x) paste0("s(", x, ")"))
# x <- do.call(rbind, y)
# z <- cbind(x, mod = apply(x, 1, paste0, collapse = " + "))
# 
# # w conterrà le stringhe dei modelli
# w <- lapply(z[,  ncol(z)], function(x) paste0("gam(log(v) ~ ", x, ", data = dfSubStand)"))
# 
# 
# gam_list <- list()
# 
# start_time <- Sys.time() 
# for(i in w) {
#   # gam_list[[i]] <-  eval(parse(text = i))
# }
# end_time <- Sys.time() 
# end_time - start_time # tempo di esecuzione
# 
# # estraggo gli AIC
# gam_list %>% 
#   map_dbl(AIC) %>%
#   round(3) -> aics
# 
# # estraggo gli R²
# gam_list %>% 
#   map(summary.gam) %>%
#   map_dbl(~.$r.sq) %>%
#   round(3) -> rsqs

# ESEMPIO 2 tutte le combinaizioni per più stazioni ####
vars <- c("t2m", "tp", "ptp", "rh", "u10m", "v10m",
          "sp", "nirradiance", "pblmin", "pblmax", "wdir", "wspeed", "pwspeed", "jd")
# le combinazioni di classe N
c1 <- combn(vars, 13) %>% data.frame()

# sistemiamo le "spline"
y <- lapply(c1, function(x) paste0("s(", x, ")"))
x <- do.call(rbind, y)
z <- cbind(x, mod = apply(x, 1, paste0, collapse = " + "))

# w conterrà le stringhe dei modelli
w <- lapply(z[,  ncol(z)], function(x) paste0("gam(log(v) ~ ", x, ", data = .)"))

# tutte le combinazioni per più stazioni
models <- list()

for(i in w) {
  print(i)
  models[[i]] <- dfSubStand %>%
    split(.$station_eu_code) %>%
    map(~eval(parse(text = i))) 
  # map(map_dbl, AIC)
}

save(models, file = glue::glue("~/R/pulvirus/analisi/GAM/{pltnt}_{cod_reg}.RData"), models)
