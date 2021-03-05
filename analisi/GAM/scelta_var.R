
# vars <- c("t2m", "tp", "ptp", "rh", "u10m", "v10m")

sceltaVar <- function(df, varsel, mod, minimoaic, mat_corr) {
  for (i in vars ) {
    
    if (colnames(df[, i])	!= 'value') {
      out1[i] <- gam(log(df[, 1]) ~ mod + s(df[, i]), data = df)
      AIC1 <- AIC(out[i])
      minimoaic1 <- min(AIC1)
    }
    
  }
  return(models)
  
  if (minimoaic1 < minimoaic) {
    variabilescelta1 <- colnames(df[, i]) #varscelta  corrisponde all'indice del minimo di df[,i]
    varsel1 <- c(varsel, variabilescelta1) #vettore delle variabile scelte (verifca inseirmento variabile in coda la vettor)      if (length(varsel)>1) { #evita di fare test backward in caso ci sia solo una variabile selezionata
    #var2<-varsel[length(varsel)-1]
    #varsel2 <- varsel[-(length(varsel)-1)] #tolgo la variabile selezionata prima di variabile scelta
    mod1 < -creaMod_vettore(varsel1) #funzione che a partire dalle variabili selezionate crea la stringa iniziale per il modello...s(x1)+s(x2)....)
    df1 <- df[-variabilescelta1]
    for (j in seq_along(df2[, j])) {
      if (colnames(df2[, j])	!= 'value') {
        out2[j] <- gam(log(df[, 1]) ~ mod1 + s(df1[, j]), data = df1)
        AIC2 <- AIC(out[j])
        minimoaic2 <- min (AIC1)
      }
    }
    
    if (minimoaic2 < minimoaic1) {
      variabilescelta2 <-
        colnames(df[, i])#varscelta2  corrisponde all'indice del minimo di df[,j]
      varsel2 <-
        C(varsel, variabilescelta2) #vettore delle variabile scelte
      mod2 < -creaMod_vettore(varsel2)
      minimoaic <- minimoaic2
      df2 <- df1[-variabilescelta2]
      if (mat_corr(varsel) < 0.7) {
        sceltaVar(df2, varsel2, mod2, minimoaic2, mat_corr)
      }
      else {
        df <- df1[-variabilescelta2]
        sceltaVar(df1, varsel1, mod1, minimoaic1, mat_corr)
      }
    }
  }
  if (minimoaic1 <= minimoaic2) {
    mod <-  mod1
    out <- out1[i]
  } else{
    if (minimoaic1 > minimoaic) {
      # mod <- mod
      # out <-  ?  ?  ?  ?
    }
  }
}