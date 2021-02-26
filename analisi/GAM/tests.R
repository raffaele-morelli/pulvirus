library(lubridate)
library(purrr)
library(mgcv)
library(dplyr)
library(knitr)

library(datiInquinanti)
library(datiMeteo)


# preparazione dei dati, potremmo usare il nome regione anziché il region_id, questione di gusti :-)
df <- inner_join(pm10, dati_meteo, by = c("station_eu_code", "date") ) %>% 
  inner_join(
    stazioniAria %>% 
      filter(region_id == 20) %>% select(c("station_eu_code")), by = c("station_eu_code")
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

# aggiungo codice stazione e valore originale ####
dfSubStand$v <- dfSub$value
dfSubStand$station_eu_code <- dfSub$station_eu_code

start_time <- Sys.time() 

# calcolo dei modelli per tutte le stazioni
models <- dfSubStand %>%
  split(.$station_eu_code) %>%
  map(~gam(log(v) ~ s(jd) + s(pwspeed) + s(wspeed) + s(tp) + s(sp) + s(ptp) + s(nirradiance) + s(rh) , 
           data = .) )

end_time <- Sys.time()

end_time - start_time # tempo di esecuzione

# Estraiamo gli R-sq.(adj) ed AIC per tutti i modelli ####
sink("esempio.md")
models %>% 
  map(summary.gam) %>% map_dbl(~.$r.sq) %>% 
  round(3) %>% kable() %>% print()

models %>% map(AIC) 

sink()

sink("esempio.md", append = TRUE)

my_list <- list()
for (i in vars) {
  # print(i)
  j <- match(i, colnames(dfSubStand))
  out <- gam(log(v) ~ s(dfSubStand[, j]), data = dfSubStand )
  # summary(out) %>% grep("R-sq.(adj)") %>%  print()
  summary(out) %>% capture.output() -> sommario
  grep("adj", sommario, value =  TRUE) -> radj

  my_list[[i]] <- c(i, round(AIC(out),2 ), radj)
}
my_mat <- do.call(rbind, my_list)

my_df %>% kable() %>%  print()
sink()

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


