# FUNZIONI ####

# Preparazione del dataframe 
preparaDataframe <- function(pltnt, cod_reg) {
  df <- inner_join(get(pltnt) %>% 
                     filter( reporting_year >= 2016), dati_meteo, by = c("station_eu_code", "date") ) %>% 
    inner_join(
      stazioniAria %>% 
        filter(region_id == cod_reg) %>% 
        select(c("station_eu_code")), by = c("station_eu_code")
    ) %>% 
    mutate(value = ifelse(value <= 0.2, 0.2, value) )
  
  # subset solo con covariate e concentrazione, tolgo le variabili che non mi servono
  # con una select
  dfSub <- df %>% 
    select(-c(date, pollutant_fk, station_code, coordx, coordy, altitude, altitudedem))
  
  # aggiunta del Julian day
  dfSub$jd = as.numeric( df$date - ymd(20130101) )
  return(dfSub)
}


# Costruisce le stringhe dei modelli a partire dal vettore iniziale ma 
# eliminando le variabili già presenti in AICS e la n-1 per cui costruire
# le stringhe dei modelli
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
  models %>% map(~ map_dbl(.x, AIC)) %>% do.call(rbind, .) %>% 
    as.data.frame() -> aics
  
  log_print("")
  log_print(aics, hide_notes = TRUE)

  # modello più performante
  rownames(aics)[ apply(aics, 2, FUN = which.min)] %>%
    data.frame("mod" = .) %>% 
    group_by(mod) %>% 
    tally(sort = TRUE) -> tab
  
  log_print("Modelli migliori" , hide_notes = TRUE)
  log_print(tab %>% as.data.frame() %>%  print() , hide_notes = TRUE)
  
  # il minimo AIC
  apply(aics, 2, FUN = min) %>% 
    data.frame("aic" = .) %>% 
    summarise(min(aic)) %>% 
    as.numeric() -> minaic
  
  # estrazioni variabili: prendo tutto ciò che è tra parentesi tonde
  nvar <- gsub("[\\(\\)]", "", regmatches(tab$mod, gregexpr("\\(.*?\\)", tab$mod) )[[1]])

  log_print(sprintf("Modello migliore: %s", paste(nvar[!nvar %in% c("logvalue")], collapse = " - ") ) , hide_notes = TRUE)
  
  return(c(as.numeric(minaic), nvar[!nvar %in% c("logvalue")]))
}

# Funzione chiave che rappresenta il diagramma di flusso 
sceltaVar <- function() {
  AICS <- get("AICS", envir = .GlobalEnv)
  vars <- get("vars", envir = .GlobalEnv)
  v_fixed <- get("v_fixed", envir = .GlobalEnv)
  v_dead <- get("v_dead", envir = .GlobalEnv)
  
  log_print("---------- START ----------", hide_notes = TRUE)
  
  # conterrà gli oggetti GAM calcolati sulle stazioni
  models <- list()
  
  w <- buildMods() # costruisce le stringhe dei modelli
  for(i in w) {
    # calcoliamo il GAM stazione per stazione con le stringhe in "w"
    models[[i]] <- dfSub %>% split(.$station_eu_code) %>%
      map(~eval(parse(text = i)))
  }
  
  bestMod(models) -> aicVar # AIC del modello migliore
  
  # una lista di appoggio da concatenare in AICS
  tmp <- list()
  tmp[[aicVar[2]]] <- c(tmp, aicVar)
  
  # Log delle risultanze
  log_print(sprintf("Variabile scelta %s con AIC %s ", aicVar[2], aicVar[1]), hide_notes = TRUE )
  log_print(sprintf("Ultima variabile: %s", paste(tmp, collapse = " -- ") ) , hide_notes = TRUE)
  
  # Salvataggio lista con gli AIC dei modelli elaborati finora
  assign("AICS", c(AICS, tmp), envir = .GlobalEnv)

  # Carico gli AIC attuali e loggo
  AICS <- get("AICS", envir = .GlobalEnv)
  log_print(sprintf("AICS: %s", ifelse(length(AICS) > 0, AICS, ""  )) , hide_notes = TRUE)
  
  n <- length(AICS) # quanti AIC ho finora
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
      log_print(sprintf("Dati backward:  %s", paste(aicBack, collapse = "::")), hide_notes = TRUE)
      
      q <- cor(dfSub %>% select(c(names(AICS), aicBack[-c(1)])), use = "pairwise.complete.obs") %>% 
        data.frame()
      
      if( as.numeric( aicBack[1]) < as.numeric( AICS[[n]][1]) ) {
        
        if( all(abs(q[, aicBack[-c(1)][1] ]) < 0.7) ) {

          assign("vars", vars[!vars %in% names(AICS[-c(length(AICS)-1)])], envir = .GlobalEnv)
          assign("v_fixed", c(names(AICS[-c(length(AICS)-1)]), envir = .GlobalEnv) )

          log_print("Scelgo il backward ", hide_notes = TRUE)
          sceltaVar()
          # return()
        }
      }
      
      q <- cor(dfSub %>% select(names(AICS)), use = "pairwise.complete.obs") %>% data.frame()
      
      if( all(q[ -c(length(AICS)), names(AICS[length(AICS)])] < 0.7) ) {
        
        assign("vars", vars[!vars %in% names(AICS)], envir = .GlobalEnv)
        
        assign("v_fixed", c(names(AICS)[!names(AICS) %in% v_dead]),  envir = .GlobalEnv )
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
  log_print(paste(get("v_fixed", envir = .GlobalEnv), collapse = " + "), hide_notes = TRUE)
  log_print("----------------------")
}

