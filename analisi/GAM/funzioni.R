prepareDF <- function(code) {
  # importazione dati ####
  dm <- datiMeteo::dati_meteo %>% 
    filter(station_eu_code == code)
  
  df <- filter(no2, station_eu_code == code) %>% 
    inner_join(dm, by = c("station_eu_code", "date") ) 
  
  # subset con covariate e concentrazione ####
  dfSub <- df %>% 
    select(-c(reporting_year, station_eu_code, date, pollutant_fk, station_code, coordx, coordy, altitude, altitudedem))
  
  # eseguo la standardizzazione di dfSub e aggiungo di nuovo la conc di PM10 non standardizzata ####
  dfSubStand <- as.data.frame(scale(dfSub)) # standardizzazione
  dfSubStand$v <- dfSub$value
  
  return(dfSubStand)
}

