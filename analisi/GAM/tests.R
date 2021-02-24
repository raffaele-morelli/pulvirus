library(datiInquinanti)
library(datiMeteo)

df <- inner_join(pm10, dati_meteo, by = c("station_eu_code", "date") ) %>% 
  inner_join(
    stazioniAria %>% filter(region_id == 6) %>% select(c("station_eu_code")), by = c("station_eu_code")
    ) 
  

# subset con covariate e concentrazione ####
dfSub <- df %>% #filter(!is.na(value)) %>% 
  select(-c(date, pollutant_fk, station_code, coordx, coordy, altitude, altitudedem))

# eseguo la standardizzazione di dfSub e aggiungo di nuovo la conc di PM10 non standardizzata ####
vars <- c("value", "t2m", "tmin2m", "tmax2m", "tp", "ptp", "rh", "u10m", "v10m", "sp", "nirradiance", "pbl00", "pbl12", "pblmin", "pblmax", "wdir", "wspeed", "pwspeed")

dfSubStand <- as.data.frame(scale(dfSub[,vars])) # standardizzazione
dfSubStand$v <- dfSub$value
dfSubStand$station_eu_code <- dfSub$station_eu_code

start_time <- Sys.time()

models <- dfSubStand %>% 
  split(.$station_eu_code) %>%  
  map(~gam((v) ~ s(ptp) + s(pwspeed) + s(tmin2m) + s(tp) + s(u10m, v10m) + s(pblmax, wspeed) + s(sp) + s(rh) + s(pblmin), 
           data = .) )

end_time <- Sys.time()

end_time - start_time

# models %>% map(summary.gam) %>% map_dbl(~.$s.pv[2])
