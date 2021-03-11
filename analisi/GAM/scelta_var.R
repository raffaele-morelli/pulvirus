library(dplyr)
library(purrr)
library(mgcv)
library(logr)


# Costruisce le stringhe dei modelli a partire dal vettore iniziale ma 
# eliminando le variabili già presenti in AICS e la n-1 per cui costruire
# le stringhe dei modelli
# 
# @backward (bool) : se deve calcolare i modelli backward 
buildMods <- function(backward = FALSE) {
  AICS <- get("AICS", envir = .GlobalEnv)
  l <- length(AICS)
  
  vars <- get("vars", envir = .GlobalEnv)
  v_fixed <- get("v_fixed", envir = .GlobalEnv)
  v_dead <- get("v_dead", envir = .GlobalEnv)
  
  if( l > 1 & backward == TRUE) {
    # le combinazioni di classe N senza la penultima variabile
    c1 <- combn(vars[!vars %in% c("value", names(AICS[-c(length(AICS)-1)]), names(v_fixed), v_dead ) ], 1) %>% 
      data.frame()
    
    # sistemiamo le "spline" testuali
    y0 <- lapply(c1, function(x) paste0("s(", x, ")"))
    
    # costruisco le "spline" con le variabili in AICS tranne l'ultima
    c2 <- lapply( c( names(AICS[-c(length(AICS))]), v_dead ), function(x) rep(x, length(c1)))
    y1 <- lapply(c2, function(x) paste0("s(", x, ")"))
    y1 <- do.call(cbind, y1)
    
  }else{
    # le combinazioni di classe N senza le variabili già in v_fixed
    c1 <- combn(vars[!vars %in% c("value", names(v_fixed), v_dead)], 1) %>% 
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
  log_print(sprintf("Miglior modello: %s", nvar %>% unlist()), hide_notes = TRUE)
  
  return(c(as.numeric(minaic), nvar[!nvar %in% c("logvalue")]))
}


sceltaVar <- function(varsel = c(), check = FALSE) {
  AICS <- get("AICS", envir = .GlobalEnv)
  vars <- get("vars", envir = .GlobalEnv)
  v_fixed <- get("v_fixed", envir = .GlobalEnv)
  v_dead <- get("v_dead", envir = .GlobalEnv)
  
  # costruisce le stringhe dei modelli ####
  log_print("MODELLI")
  models <- list()
  
  w <- buildMods()
  for(i in w) {
    models[[i]] <- dfSub %>% split(.$station_eu_code) %>%
      map(~eval(parse(text = i)))
  }
  
  bestMod(models) -> aicVar # AICs del modello migliore ####
  
  tmp <- list()
  tmp[[aicVar[2]]] <- c(tmp, aicVar)

  log_print(sprintf("Ultima variabile: %s", tmp ) , hide_notes = TRUE)
  log_print(sprintf("AICS: %s", c(AICS, tmp) ) , hide_notes = TRUE)
  assign("AICS", c(AICS, tmp), envir = .GlobalEnv)
  
  log_print(sprintf("Variabile scelta %s con AIC %s ", aicVar[2], aicVar[1]), hide_notes = TRUE )
  
  AICS <- get("AICS", envir = .GlobalEnv)
  log_print(AICS, hide_notes = TRUE)
  n <- length(AICS)
  
  log_print(sprintf("length(AICS): %s ", n), hide_notes = TRUE )
  
  if( n > 1) {

    if( as.numeric( AICS[[n]][1]) < as.numeric( AICS[[n-1]][1] ) ) {
      # log_print("controllo AIC n n-1 ", hide_notes = TRUE)
      
      # seleziono il modello BACKWARD con AIC minimo
      w <- buildMods(backward = TRUE)
      log_print("Calcolo modelli backward", hide_notes = TRUE)
      models <- list()
      for(i in w) {
        # print(i)
        models[[i]] <- dfSub %>% split(.$station_eu_code) %>% 
          map(~eval(parse(text = i)))
      }
      
      bestMod(models) -> aicBack # AICs ####
      log_print(sprintf("backward %s", aicBack) )
      
      q <- cor(dfSub %>% select(c(names(AICS), aicBack[-c(1)])), use = "pairwise.complete.obs") %>% 
        data.frame()
      
      if( as.numeric( aicBack[1]) < as.numeric( AICS[[n]][1]) ) {
        
        if( all(abs(q[, aicBack[-c(1)][1] ]) < 0.7) ) {

          assign("vars", vars[!vars %in% names(AICS[-c(length(AICS)-1)])], envir = .GlobalEnv)
          assign("v_fixed", c(names(AICS[-c(length(AICS)-1)]), envir = .GlobalEnv) )

          log_print("Scelgo il backward ")
          return()
        }
      }
      
      q <- cor(dfSub %>% select(names(AICS)), use = "pairwise.complete.obs") %>% data.frame()
      
      if(all(q[ -c(length(AICS)), names(AICS[length(AICS)])] < 0.7)) {
        
        assign("vars", vars[!vars %in% names(AICS)], envir = .GlobalEnv)
        
        assign("v_fixed", c(names(AICS)[!names(AICS) %in% v_dead]),  envir = .GlobalEnv )
        log_print("Scelgo il modello N")
        return()
      }else{
        # devo eliminare la variabile perché troppo correlata
        assign("vars", vars[!vars %in% names(AICS[length(AICS)]) ], envir = .GlobalEnv)
        assign("v_dead", c(v_dead, names(AICS[length(AICS)])), envir = .GlobalEnv )
        log_print("Scelgo il modello N-1")
        return()
      }

    }
    log_print("Fine per scelta MODELLO")
    
  }
  log_print("Fine")
}

f_log <- file.path("~/R/pulvirus/analisi/GAM/", glue::glue("logfile.log"))
lf <- log_open(f_log)

rm(AICS, v_dead, v_fixed)

AICS <- list()
v_dead <- c()
v_fixed <- c()
aicBack <- c()

vars <- c("t2m", "tmin2m", "tmax2m", "tp", "ptp", "rh", "u10m", "v10m",
          "sp", "nirradiance", "pbl00", "pbl12", "pblmin", "pblmax", "wdir", 
          "wspeed", "pwspeed")
sceltaVar()
sceltaVar()
sceltaVar()
sceltaVar()
sceltaVar()
sceltaVar()
sceltaVar()
sceltaVar()
sceltaVar()
sceltaVar()

# writeLines(readLines(lf))
# log_close()


# caso singolo
# models %>% map(map_dbl, AIC) %>% .[[2]]

# generalizziamo 
# models %>% map(~ .x %>% map_dbl(AIC))
# oppure:

# for(i in names(aics)) {
#   min(aics[-c(1), i]) %>% print()
# }

# apply(aics[-c(1),], 2, FUN = which.min)
# rownames(aics[-c(1),])[ apply(aics[-c(1),], 2, FUN = which.min)] %>% View()



# regmatches(tab$mod, gregexpr("(?<=\\().*?(?=\\))", tab$mod, perl=T) )[[1]]
# rownames(as.data.frame( vars[-c(1),])) [ apply(aics[-c(1),], 2, FUN = which.min)]