library(dplyr)
library(purrr)
library(mgcv)

# Costruisce le stringhe dei modelli
# @vars : le variabili per cui costruire le stringhe dei modelli
buildMods <- function() {
  AICS <- get("AICS")
  l <- length(AICS)
  
  # le combinazioni di classe N
  c1 <- combn(vars[!vars %in% c("value", names(AICS))], 1) %>% data.frame()
  
  # sistemiamo le "spline"
  y0 <- lapply(c1, function(x) paste0("s(", x, ")"))
  

  if( l > 1) {
    # costruisco le "spline" con le variabili in AICS 
    c2 <- lapply(names(AICS[-c(length(AICS)-1)]), function(x) rep(x, length(c1)))
    y1 <- lapply(c2, function(x) paste0("s(", x, ")"))
    y1 <- do.call(cbind, y1)
  }else{
    c2 <- lapply(names(AICS), function(x) rep(x, length(c1)))
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
  log_print(sprintf("Nvar:: %s", nvar), hide_notes = TRUE)
  
  return(c(as.numeric(minaic), nvar[!nvar %in% c("logvalue")]))
}


creaSpline <- function(v) {
  y <- lapply(v, function(x) paste0("s(", x, ")"))
  x <- do.call(rbind, y)
  z <- cbind(x, mod = apply(x, 1, paste0, collapse = " + "))
  return(x)
}

# creaSpline(names(AICS))

library(logr)

sceltaVar <- function(varsel = c(), check = FALSE) {
  log_print("----------------------------------------------", hide_notes = TRUE)
  AICS <- get("AICS")
  
  # log_print(v, hide_notes = TRUE)
  w <- buildMods() # buildMods 1st pass ####

  log_print("Calcolo modelli", hide_notes = TRUE)
  models <- list()
  for(i in w) {
    # print(i)
    models[[i]] <- dfSub %>%
      split(.$station_eu_code) %>%
      map(~eval(parse(text = i)))
  }
  
  bestMod(models) -> aicVar # AICs ####
  
  log_print(t, hide_notes = TRUE)
  tmp <- list()
  
  tmp[[aicVar[2]]] <- c(tmp, aicVar)

  log_print(sprintf("Ultima variabile: %s", tmp) , hide_notes = TRUE)
  log_print(sprintf("Stato AICS: %s", c(AICS, tmp)), hide_notes = TRUE)
  assign("AICS", c(AICS, tmp), envir = .GlobalEnv)
  
  log_print(sprintf("Variabile scelta %s con AIC %s ", aicVar[2], aicVar[1]), hide_notes = TRUE )
  
  n <- length(AICS)
  if( n > 1) {
    if( as.numeric( AICS[[n]][1]) < as.numeric( AICS[[n-1]][1] ) ) {
      # seleziono il modello backward con AIC minimo
      w <- buildMods()
      log_print("Calcolo modelli", hide_notes = TRUE)
      models <- list()
      for(i in w) {
        # print(i)
        models[[i]] <- dfSub %>%
          split(.$station_eu_code) %>%
          map(~eval(parse(text = i)))
      }
      
      bestMod(models) -> aicBack # AICs ####
      if( as.numeric( aicBack[1]) < as.numeric( AICS[[n]][1]) ) {
        return("Controllo la correlazione del backward")
      }else{
        return("Controllo la correlazione del modello n")
      }
    }    
  }

  
  if(check == TRUE) {
    # confronto AIC attuale con quello della variabile che ho tolto inline (la penultima)
    # una delle due la tolgo definitivamente dal vettore `vars`
    # sceltaVar( varsel = names(AICS[-c(length(AICS)-1)]),  check = FALSE)
  }else{
    # log_print(sprintf("Ricorsione con %s ", aicVar[2]), hide_notes = TRUE)
    # sceltaVar(varsel = aicVar[2])
    # # log_print(AICS)
    # return("XXXX")
  }
  # log_print(sprintf("Ricorsione con %s ", aicVar[2]), hide_notes = TRUE)
  # sceltaVar(varsel = aicVar[2])
  {  
    # if(as.numeric( aicVar[1] ) < minimoaic) {
    #   log_print(sprintf("Variabile scelta %s con AIC %s ", aicVar[2], aicVar[1]) )
    #   
    #   log_print( varsel[!varsel %in% c( aicVar[2])] )
    #   
    #   w2 <- buildMods(varsel[!varsel %in% c( aicVar[2])]) # buildMods 2nd pass
    #   log_print(w2)
    #   
    #   log_print("Calcolo modelli 2nd pass")
    # 
    #   models2 <- list()
    #   for(i in w2) {
    #     models2[[i]] <- df %>%
    #       split(.$station_eu_code) %>%
    #       map(~eval(parse(text = i)))
    #   }
    # 
    #   bestMod(models2) -> aicVar2 # AICs 
    #   log_print(sprintf("Variabile 2nd pass %s con AIC %s ", aicVar2[2], aicVar2[1]) )
    #   
    # }else{
    #   log_print(sprintf("Variabile scartata %s con AIC %s ", aicVar[2], aicVar[1]) )
    # }
  }

  return("Fine")

}

tmp <- file.path(".", glue::glue("test.log"))
lf <- log_open(tmp)

rm(AICS)
AICS <- list()
vars <- c("t2m", "tmin2m", "tmax2m", "tp", "ptp", "rh", "u10m", "v10m",
          "sp", "nirradiance", "pbl00", "pbl12", "pblmin", "pblmax", "wdir", 
          "wspeed", "pwspeed")
sceltaVar()

writeLines(readLines(lf))
log_close()


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