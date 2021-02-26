library(lubridate)
library(purrr)
library(mgcv)

library(datiInquinanti)
library(datiMeteo)


# preparazione dei dati, potremmo usare il nome regione anziché il region_id, questione di gusti :-)
df <- inner_join(pm10, dati_meteo, by = c("station_eu_code", "date") ) %>% 
  inner_join(
    stazioniAria %>% 
      filter(region_id == 3) %>% select(c("station_eu_code")), by = c("station_eu_code")
    ) %>% 
  mutate(jd = as.numeric( date - ymd(20130101) ), value = ifelse(value <= 0, 0.2, value) ) 

# subset con covariate e concentrazione ####
dfSub <- df %>% 
  select(-c(date, pollutant_fk, station_code, coordx, coordy, altitude, altitudedem))

# le variabili di interesse ####
vars <- c("value", "t2m", "tmin2m", "tmax2m", "tp", "ptp", "rh", "u10m", "v10m",
          "sp", "nirradiance", "pbl00", "pbl12", "pblmin", "pblmax", "wdir", "wspeed", "pwspeed", "jd")

# eseguo la standardizzazione di dfSub e aggiungo di nuovo la conc di PM10 non standardizzata ####
dfSubStand <- as.data.frame(scale(dfSub[,vars])) # standardizzazione
dfSubStand$v <- dfSub$value
dfSubStand$station_eu_code <- dfSub$station_eu_code
# dfSubStand$region_id <- dfSub$region_id

start_time <- Sys.time() 

# calcolo dei modelli per tutte le stazioni
models <- dfSubStand %>%
  split(.$station_eu_code) %>%
  map(~gam(log(v) ~ s(jd) + s(wdir) + s(pwspeed) + s(wspeed) + s(tp) + s(sp) + s(ptp)  + s(nirradiance) + s(rh) , 
           data = .) )

end_time <- Sys.time()

end_time - start_time # tempo di esecuzione

# Estraiamo gli R-sq.(adj) per tutti i modelli 
models %>% 
  map(summary.gam) %>% map_dbl(~.$r.sq) %>% 
  round(3) %>% print()

for (i in vars) {
  # print(i)
  j <- match(i, colnames(dfSubStand))
  out <- gam((v) ~ s(dfSubStand[, j]), data = dfSubStand )
  # summary(out) %>% grep("R-sq.(adj)") %>%  print()
  summary(out) %>% capture.output() -> pippo
  grep("adj", pippo, value =  TRUE) -> radj
  print( paste(i, ":", round(AIC(out),2 ), "R ", radj ))
}

# for (i in colnames(dfSubStand)) { 
# mod1 <- dfSubStand %>%
#   split(.$station_eu_code) %>%
#   map(~gam(log(v) ~ s(get(i)) , data = .) )
# }
# 
# mod1 %>% 
#   map(summary.gam) %>% 
#   map_dbl(~.$r.sq) %>% 
#   round(3) %>% print()


