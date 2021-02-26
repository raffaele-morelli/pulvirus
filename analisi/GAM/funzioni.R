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

# estrazione parametri GAM check ####
estrai <- function(Out) {
  gam_obj <- capture.output( mgcv:::anova.gam(Out) )
  gam_tbl <- gam_obj[11:length(gam_obj)]
  
  str_spl = function(x){
    x <- gsub("<", "", x)    # il simbolo '<' ci rompe le scatole per lo split successivo
    strsplit(x, "\\s+")[[1]] # splittiamo per spazi multipli
  }
  
  tmp.df <- do.call(rbind, lapply(gam_tbl, str_spl)) %>% as.data.frame()
  
  names(tmp.df) <- c("Vs", "edf", "Ref.df", "F", "p-value") 
  return( tmp.df )
}

