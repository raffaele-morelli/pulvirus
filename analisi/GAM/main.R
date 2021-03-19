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

setwd("~/R/pulvirus/analisi/GAM")

source("scelta_var.R")


# ARGOMENTI per l'esecuzione da riga di comando o da RStudio ####
args <- commandArgs(trailingOnly = TRUE)
setwd("~/R/pulvirus/analisi/GAM")

out_dir <- "output/"

# se non stiamo eseguendo da riga di comando allora devo impostare
# i due parametri a mano 
if(is.na(args[1])) {
  pltnt <- "no2"
  cod_reg <- 2
}else{
  pltnt <- args[1]
  cod_reg <- args[2]
}

dfSub <- preparaDataframe(pltnt, cod_reg)

# Fase di RUN ####

# inizializzo le liste che popolerà la funzione "sceltaVar"
AICS <- list()
v_dead <- list()
v_fixed <- list()

# apro il file di log
f_log <- file.path(out_dir, glue::glue("pulvirus_{pltnt}_{cod_reg}.log"))
lf <- log_open(f_log)

# il set di variabili iniziali che voglio includere
vars <- c("t2m", "tmin2m", "tmax2m", "tp", "ptp", "rh", "u10m", "v10m",
          "sp", "nirradiance", "pbl00", "pbl12", "pblmin", "pblmax", "wdir", 
          "wspeed", "pwspeed", "jd")

sceltaVar()

# writeLines(readLines(lf))
log_close()

# Fase di preparazione per la scrittura del file RData ####

# carico le liste che ho popolato nella routine "sceltaVar"
v_fixed <- get("v_fixed", envir = .GlobalEnv)
v_dead <- get("v_dead", envir = .GlobalEnv)
AICS <- get("AICS", envir = .GlobalEnv)

# costruisco la stringa del modello a partire dalle variabili scelte
y0 <- lapply(v_fixed, function(x) paste0("s(", x, ")"))
y1 <- do.call(cbind, y0)
z <- data.frame(mod = apply(y1, 1, paste0, collapse = " + "))

# w conterrà le stringhe dei modelli
w <- lapply(z[, ncol(z)], function(x) paste0("gam(log(value) ~ ", x, ", data = .)"))

# calcolo il modello finale per tutte le stazioni
models <- list()
for(i in w) {
  models[[i]] <- dfSub %>% 
    split(.$station_eu_code) %>% 
    map(~eval(parse(text = i)))
}

# salvataggio del file RData con le liste e variabili di interesse
save(models, v_fixed, v_dead, AICS, cod_reg, pltnt, 
     file = glue::glue("{out_dir}/{pltnt}_{cod_reg}.RData"))