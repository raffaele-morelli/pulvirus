# Pacchetti ####
library(lubridate)
library(purrr)
library(mgcv)
library(dplyr)
library(knitr)
library(logr)


library(datiInquinanti)
library(datiMeteo)
# Se i pacchetti non sono presenti occorre installarli con questi due comandi
# remotes::install_github("raffaele-morelli/datiInquinanti", force = TRUE)
# remotes::install_github("raffaele-morelli/datiMeteo", force = TRUE)

setwd("~/R/pulvirus/analisi/GAM/scelta/")

source("funzioni.R")

# Scrittura del file RData ####
saveRData <- function(cod_eu = NA) {
  # carico le liste che ho popolato nella routine "sceltaVar"
  v_fixed <- get("v_fixed", envir = .GlobalEnv)
  v_dead <- get("v_dead", envir = .GlobalEnv)
  AICS <- get("AICS", envir = .GlobalEnv)
  
  nanni <- dfSub$reporting_year %>% unique() %>% length()
  cappa <- 2*nanni +1 
  
  if(length(v_fixed) == 0)
    return("v_fixed non può essere vuoto")
  
  # costruisco la stringa del modello a partire dalle variabili scelte
  # y0 <- lapply(v_fixed, function(x) paste0("s(", x, ")"))
  y0 <- lapply(v_fixed, 
               function(x) { 
                 ifelse( x == "jd", 
                         paste0("s(", x, ", k=", cappa, ")" ),
                         ifelse(x == "wkd", "wkd", paste0("s(", x, ")"))
                         ) 
               }
  )
  y1 <- do.call(cbind, y0)
  z <- data.frame(mod = apply(y1, 1, paste0, collapse = " + "))
  
  # w conterrà le stringhe dei modelli
  w <- lapply(z[, ncol(z)], function(x) paste0("gam((value) ~ ", x, ", gamma=1.4, family=gaussian(link=log), data = .)"))
  
  # calcolo il modello finale per tutte le stazioni
  models <- list()
  for(i in w) {
    models[[i]] <- dfSub %>%
      split(.$station_eu_code) %>%
      map(~eval(parse(text = i)))
  }
  
  # salvataggio del file RData con le liste e variabili di interesse
  save(models, v_fixed, v_dead, AICS, cod_reg, pltnt,
       file = glue::glue("{out_dir}/{pltnt}_{cod_reg}_{cod_eu}.RData"))
  
  return(glue::glue("{out_dir}/{pltnt}_{cod_reg}_{cod_eu}.RData"))
}

# ARGOMENTI per l'esecuzione da riga di comando o da RStudio ####
args <- commandArgs(trailingOnly = TRUE)

# se non stiamo eseguendo da riga di comando allora devo impostare
# i due parametri a mano 
if(is.na(args[1])) {
  pltnt <- "pm10"
  cod_reg <- 10
}else{
  pltnt <- args[1]
  cod_reg <- args[2]
}

# Variabili ambiente ####
out_dir <- glue::glue("output/{cod_reg}/{pltnt}")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

dfStazioni <- preparaDataframe(pltnt, cod_reg)

for (cod_eu in dfStazioni$station_eu_code %>% unique()) {

  dfSub <- filter(dfStazioni, station_eu_code == cod_eu)
  
  # inizializzo le liste che popolerà la funzione "sceltaVar"
  AICS <- list()
  v_dead <- list()
  v_fixed <- list()
  
  # il set di variabili iniziali che voglio includere
  # variabili da inizializzare ad ogni tornata
  vars <- c("t2m", "tmin2m", "tmax2m", "tp", "ptp", "rh", "u10m", "v10m",
            "sp", "nirradiance", "pbl00", "pbl12", "pblmin", "pblmax", "wdir", 
            "wspeed", "pwspeed", "jd")
  
  # apro il file di log
  f_log <- file.path(out_dir, glue::glue("pulvirus_{pltnt}_{cod_reg}_{cod_eu}.log"))
  lf <- log_open(f_log)
  
  log_print(sprintf("Stazione: %s", cod_eu), hide_notes = TRUE )
  
  sceltaVar()
  log_close()
  
  saveRData(cod_eu)
}


