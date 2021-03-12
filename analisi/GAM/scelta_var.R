# pacchetti ####
library(lubridate)
library(purrr)
library(mgcv)
library(dplyr)
library(knitr)

library(datiInquinanti)
library(datiMeteo)
# setwd("~/R/pulvirus/analisi/GAM")

# fegatelli ####
# station_eu_code == "IT0953A" | station_eu_code == "IT0888A"

# ARGOMENTI ####
args <- commandArgs(trailingOnly = TRUE)

cat(args[1], args[2], "\n", sep = " ---- ")

if(is.na(args[1])) {
  pltnt <- "pm10"
  cod_reg <- 12  
}else{
  pltnt <- args[1]
  region_id <- args[2]
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


# FUNZIONI ####
# Costruisce le stringhe dei modelli a partire dal vettore iniziale ma 
# eliminando le variabili già presenti in AICS e la n-1 per cui costruire
# le stringhe dei modelli
# 
# @backward (bool) : se deve calcolare i modelli backward 
buildMods <- function(backward = FALSE) {
  AICS <- get("AICS", envir = .GlobalEnv)
  l <- length(AICS)
  
  vars <- get("vars", envir = .GlobalEnv)
  v_alive <- get("v_alive", envir = .GlobalEnv)
  v_dead <- get("v_dead", envir = .GlobalEnv)
  
  if( l > 1 & backward == TRUE) {
    # le combinazioni di classe N senza la penultima variabile
    c1 <- combn(vars[!vars %in% c("value", names(AICS[-c(length(AICS)-1)]), names(v_alive), v_dead ) ], 1) %>% 
      data.frame()
    
    # sistemiamo le "spline" testuali
    y0 <- lapply(c1, function(x) paste0("s(", x, ")"))
    
    # costruisco le "spline" con le variabili in AICS tranne l'ultima
    c2 <- lapply( c( names(AICS[-c(length(AICS))]), v_dead ), function(x) rep(x, length(c1)))
    y1 <- lapply(c2, function(x) paste0("s(", x, ")"))
    y1 <- do.call(cbind, y1)
    
  }else{
    # le combinazioni di classe N senza le variabili già in v_alive
    c1 <- combn(vars[!vars %in% c("value", names(v_alive), v_dead)], 1) %>% 
      data.frame()
    
    y0 <- lapply(c1, function(x) paste0("s(", x, ")"))
    
    c2 <- lapply(names(AICS)[!names(AICS) %in% v_dead], function(x) rep(x, length(c1)))
    y1 <- lapply(c2, function(x) paste0("s(", x, ")"))
    y1 <- do.call(cbind, y1)
  }
  
  x <- cbind(y0, y1)
  z <- data.frame(mod = apply(x, 1, paste0, collapse = " + "))
  
  # w conterrà le stringhe dei modelli
  w <- lapply(z[,  ncol(z)], function(x) paste0("gam(log(value) ~ ", x, ", data = .)"))
  return(w)
}


# Estrae gli AIC e restituisce il minimo e la relativa variabile
# @models : lista con i modelli per tutte le stazioni
bestMod <- function(models) {
  # estrazione AIC
  models %>% map(~ map_dbl(.x, AIC)) %>% do.call(rbind, .) %>% as.data.frame() -> aics
  
  # modello più performante
  rownames(aics[-c(1),])[ apply(aics[-c(1),], 2, FUN = which.min)] %>%
    data.frame("mod" = .) %>% 
    group_by(mod) %>% 
    tally(sort = TRUE) -> tab
  
  # il minimo AIC
  apply(aics[-c(1),], 2, FUN = min) %>% 
    data.frame("aic" = .) %>% summarise(min(aic)) %>% as.numeric() -> minaic
  
  # estrazioni variabili  
  nvar <- gsub("[\\(\\)]", "", regmatches(tab$mod, gregexpr("\\(.*?\\)", tab$mod) )[[1]])
  log_print(sprintf("%s", nvar %>% unlist()) , hide_notes = TRUE)
  
  return(c(as.numeric(minaic), nvar[!nvar %in% c("logvalue")]))
}


sceltaVar <- function(varsel = c(), check = FALSE) {
  AICS <- get("AICS", envir = .GlobalEnv)
  vars <- get("vars", envir = .GlobalEnv)
  v_alive <- get("v_alive", envir = .GlobalEnv)
  v_dead <- get("v_dead", envir = .GlobalEnv)
  
  # costruisce le stringhe dei modelli
  log_print("START", hide_notes = TRUE)
  models <- list()
  
  w <- buildMods()
  for(i in w) {
    models[[i]] <- dfSub %>% split(.$station_eu_code) %>%
      map(~eval(parse(text = i)))
  }
  
  bestMod(models) -> aicVar # AICs del modello migliore
  
  tmp <- list()
  tmp[[aicVar[2]]] <- c(tmp, aicVar)
  
  log_print(sprintf("AICS: %s", AICS ) , hide_notes = TRUE)
  log_print(sprintf("Ultima variabile: %s", tmp ) , hide_notes = TRUE)
  assign("AICS", c(AICS, tmp), envir = .GlobalEnv)
  
  log_print(sprintf("Variabile scelta %s con AIC %s ", aicVar[2], aicVar[1]), hide_notes = TRUE )
  
  AICS <- get("AICS", envir = .GlobalEnv)
  # log_print(AICS, hide_notes = TRUE)
  
  n <- length(AICS)
  log_print(sprintf("length(AICS): %s ", n), hide_notes = TRUE )
  
  if( n > 1) {
    if( as.numeric( AICS[[n]][1]) < as.numeric( AICS[[n-1]][1] ) ) {
      # seleziono il modello BACKWARD con AIC minimo
      w <- buildMods(backward = TRUE)
      log_print("Calcolo BACKWARD", hide_notes = TRUE)
      models <- list()
      for(i in w) {
        # print(i)
        models[[i]] <- dfSub %>% split(.$station_eu_code) %>% 
          map(~eval(parse(text = i)))
      }
      
      bestMod(models) -> aicBack # AIC del backward
      log_print(sprintf("Dati backward:  %s", aicBack), hide_notes = TRUE)
      
      q <- cor(dfSub %>% select(c(names(AICS), aicBack[-c(1)])), use = "pairwise.complete.obs") %>% 
        data.frame()
      
      if( as.numeric( aicBack[1]) < as.numeric( AICS[[n]][1]) ) {
        
        if( all(abs(q[, aicBack[-c(1)][1] ]) < 0.7) ) {

          assign("vars", vars[!vars %in% names(AICS[-c(length(AICS)-1)])], envir = .GlobalEnv)
          assign("v_alive", c(names(AICS[-c(length(AICS)-1)]), envir = .GlobalEnv) )

          log_print("Scelgo il backward ", hide_notes = TRUE)
          return()
        }
      }
      
      q <- cor(dfSub %>% select(names(AICS)), use = "pairwise.complete.obs") %>% data.frame()
      
      if( all(q[ -c(length(AICS)), names(AICS[length(AICS)])] < 0.7) ) {
        
        assign("vars", vars[!vars %in% names(AICS)], envir = .GlobalEnv)
        
        assign("v_alive", c(names(AICS)[!names(AICS) %in% v_dead]),  envir = .GlobalEnv )
        log_print("Scelgo il modello N", hide_notes = TRUE)
        sceltaVar()
        # return()
      }else{
        # devo eliminare la variabile perché troppo correlata
        assign("vars", vars[!vars %in% names(AICS[length(AICS)]) ], envir = .GlobalEnv)
        assign("v_dead", c(v_dead, names(AICS[length(AICS)])), envir = .GlobalEnv )
        log_print("Scelgo il modello N-1", hide_notes = TRUE)
        sceltaVar()
        # return()
      }

    }else{
      return("Fine per scelta MODELLO iniziale")
    }
    
  }else{
    sceltaVar()
  }
  
  log_print("Fine per scelta MODELLO", hide_notes = TRUE)
  log_print(get("v_alive"), hide_notes = TRUE)
  log_print("----------------------")
  return(" ")
}

# RUN ####
AICS <- list()
v_dead <- list()
v_alive <- list()

f_log <- file.path(tempdir(), glue::glue("pulvirus_{pltnt}_{cod_reg}.log"))
lf <- log_open(f_log)

vars <- c("t2m", "tmin2m", "tmax2m", "tp", "ptp", "rh", "u10m", "v10m",
          "sp", "nirradiance", "pbl00", "pbl12", "pblmin", "pblmax", "wdir", 
          "wspeed", "pwspeed")
start_time <- Sys.time()
sceltaVar()
end_time <- Sys.time()
log_print(sprintf("Esecuzione %s", end_time - start_time), hide_notes = TRUE)

# writeLines(readLines(lf))
log_close()
