# Costruisce le stringhe dei modelli
# @vars : le variabili per cui costruire le stringhe dei modelli
buildMods <- function(vars) {
  # le combinazioni di classe N
  c1 <- combn(vars, 1) %>% data.frame()
  
  # sistemiamo le "spline"
  y <- lapply(c1, function(x) paste0("s(", x, ")"))
  x <- do.call(rbind, y)
  z <- cbind(x, mod = apply(x, 1, paste0, collapse = " + "))
  
  # w conterrà le stringhe dei modelli
  w <- lapply(z[,  ncol(z)], function(x) paste0("gam(log(value) ~ ", x, ", data = .)"))
  # log_print("passo ------------------")
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
  log_print(nvar[!nvar %in% c("logvalue")])
  
  return(c(as.numeric(minaic), nvar[!nvar %in% c("logvalue")]))
}


# vars <- c("t2m", "tp", "ptp", "rh", "u10m", "v10m")
aics <- list()
# df <- dfSubStand
# vars <- vars

sceltaVar <- function(varsel = c()) {
  library(logr)
  tmp <- file.path(tempdir(), "test.log")
  # Open log
  lf <- log_open(tmp)
  
  # Send message to log
  log_print("--------------------------------")
  
  if(length(varsel > 0)) {
    v <- vars[!vars %in% varsel]  
  }else{
    v <- vars
  }
  log_print(v)
  w <- buildMods(v) # buildMods 1st pass ####
  # log_print(w)
  
  # log_print("Calcolo modelli")
  # 
  models <- list()
  for(i in w) {
    # print(i)
    models[[i]] <- df %>%
      split(.$station_eu_code) %>%
      map(~eval(parse(text = i)))
  }

  bestMod(models) -> aicVar # AICs ####
  aics[[aicVar[2]]] <<- as.numeric( aicVar[1] )
  log_print(sprintf("Variabile scelta %s con AIC %s ", aicVar[2], aicVar[1]) )
  
  # if(length(aics) > 5) {
  #   writeLines(readLines(lf))
  #   log_close()
  #   return("XXXX")
  # }
  if(length(aics) <= 1) {
    sceltaVar(varsel = aicVar[2])
  }else{
    log_print(aics)
  }
  


{  
  # if(as.numeric( aicVar[1] ) < minimoaic) {
  #   log_print(sprintf("Variabile scelta %s con AIC %s ", aicVar[2], aicVar[1]) )
  #   
  #   log_print( varsel[!varsel %in% c( aicVar[2])] )
  #   
  #   w2 <- buildMods(varsel[!varsel %in% c( aicVar[2])]) # buildMods 2nd pass ####
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
  #   bestMod(models2) -> aicVar2 # AICs #####
  #   log_print(sprintf("Variabile 2nd pass %s con AIC %s ", aicVar2[2], aicVar2[1]) )
  #   
  # }else{
  #   log_print(sprintf("Variabile scartata %s con AIC %s ", aicVar[2], aicVar[1]) )
  # }
  }
  
  # Close log
  writeLines(readLines(lf))
  log_close()
  
  return("Fine")

  {
  # 
  # if (minimoaic1 < minimoaic) {
  #   variabilescelta1 <- colnames(df[, i]) #varscelta  corrisponde all'indice del minimo di df[,i]
  #   varsel1 <- c(varsel, variabilescelta1) #vettore delle variabile scelte (verifca inseirmento variabile in coda la vettor)      
    # if (length(varsel)>1) { #evita di fare test backward in caso ci sia solo una variabile selezionata
    
  #   #var2 <- varsel[length(varsel)-1]
  #   #varsel2 <- varsel[-(length(varsel)-1)] #tolgo la variabile selezionata prima di variabile scelta
  #   mod1 < -creaMod_vettore(varsel1) #funzione che a partire dalle variabili selezionate crea 
      # la stringa iniziale per il modello...s(x1)+s(x2)....)
  #   df1 <- df[-variabilescelta1]
  #   for (j in seq_along(df2[, j])) {
  #     if (colnames(df2[, j])	!= 'value') {
  #       out2[j] <- gam(log(df[, 1]) ~ mod1 + s(df1[, j]), data = df1)
  #       AIC2 <- AIC(out[j])
  #       minimoaic2 <- min (AIC1)
  #     }
  #   }
  #   
  #   if (minimoaic2 < minimoaic1) {
  #     variabilescelta2 <- colnames(df[, i])#varscelta2  corrisponde all'indice del minimo di df[,j]
  #     varsel2 <- c(varsel, variabilescelta2) #vettore delle variabile scelte
  #     mod2 < -creaMod_vettore(varsel2)
  #     minimoaic <- minimoaic2
  #     df2 <- df1[-variabilescelta2]
  #     
  #     if (mat_corr(varsel) < 0.7) {
  #       # verificare se la variabile selezionata per prima è sempre la più significativa
  #       # fisso la seconda e verifico che sia ancora la più signficativa
  #       sceltaVar(df2, varsel2, mod2, minimoaic2, mat_corr)
  #     }else{
  #       df <- df1[-variabilescelta2]
  #       sceltaVar(df1, varsel1, mod1, minimoaic1, mat_corr)
  #     }
  #   }
  # }
  # if (minimoaic1 <= minimoaic2) {
  #   mod <-  mod1
  #   out <- out1[i]
  # } else{
  #   if (minimoaic1 > minimoaic) {
  #     # mod <- mod
  #     # out <-  ?  ?  ?  ?
  #   }
  # }
    }
}

sceltaVar()


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
