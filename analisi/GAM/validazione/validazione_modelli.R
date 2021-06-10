# https://m-clark.github.io/generalized-additive-models/application.html
library(datiInquinanti)
library(datiMeteo)
library(dplyr)
library(lubridate)
library(corrplot)
library(GGally)
library(mgcv)
library(mgcViz)
library(Metrics)
library(glue)
library(FSMUMI)

setwd("/home/rmorelli/R/pulvirus/analisi/GAM/")
source("scelta/funzioni.R")

basedir <- "~/R/pulvirus/analisi/GAM/"
outdir <- "~/R/pulvirus/analisi/GAM/validazione/"
pltnt <- "pm25"

rdatas <- list.files(path = glue("{basedir}/scelta/output/"), 
                     pattern = paste0("^", pltnt, "_(.*).RData"), 
                     recursive = TRUE, 
                     full.names = TRUE)
my_list <- list()
my_mat <- matrix()

for (i in rdatas) {
  load(i)
  
  codreg <- stringr::str_split(basename(i), "_")[[1]][2]
  f <- stringr::str_split(basename(i), "_")[[1]][3]
  eu_code <- stringr::str_split(f, "\\.")[[1]][1]
  print(eu_code)
  
  preparaDataframe(pltnt, codreg) %>% 
    filter(station_eu_code == eu_code) -> df
  
  # if(nrow(df) == 0) {
  #   cat("stazione senza dati nel file", eu_code)
  #   next
  # }
  
  df %>% 
    mutate(wkd = weekdays(date, abbreviate = TRUE)) -> dfSub
  
  dfSub %>% mutate(
    dclass = case_when(
      wkd %in% c("dom") ~ "weekend",
      TRUE ~ "feriali")) -> df

  df %>% filter(!is.na(value)) -> dfc
  
  d <- nrow(dfc) * 0.8
  s <- sample(dfc$jd, size = d) 
  
  
  # dataset ####
  pdf <- dfc[which( !(dfc$jd %in% s)),] # predict
  tdf <- dfc %>% filter(jd %in% s) # training

  modello <- gsub("data = .", "data = tdf", names(models))
  mod_tdf <- eval(parse(text = modello))
  
  modello1 <- gsub(" ~ ", " ~ dclass + ", modello)
  mod_tdf1 <- eval(parse(text = modello1))
  
  if(mod_tdf1$aic < mod_tdf$aic) {
    mod_def <- mod_tdf1
    modello_def <- modello1
  }else{
    mod_def <- mod_tdf
    modello_def <- modello
  }
  
  pred_y <- predict.gam(mod_def, newdata = pdf)
  
  
  # Factor of 2 ####
  tmpdf <- cbind(mod_def$fitted.values, mod_def$y) %>% 
    as.data.frame() %>%  
    setNames(c("pred", "obs"))
  
  n <- length(mod_def$y) 
  tmpdf %>% 
    mutate(flag = ifelse(between(pred/obs, 0.5, 2), pred/obs, 0) ) %>% 
    filter(flag > 0) %>% 
    nrow() -> nvalidate

  # Fractional BIAS  ####
  compute.fb(tmpdf$pred, tmpdf$obs) -> fb
  
  # Normalized Mean Square Error  ####
  tmpdf %>% mutate(num = (obs - pred)^2, den = obs*pred) %>% 
    summarise(sum(num)/sum(den)) -> nmse
  
  
  rsq <- function (x, y) cor(x, y) ^ 2
  
  rsq(pdf$value, as.numeric(pred_y))
  rsq(mod_def$y, mod_def$fitted.values)

  
  my_list[[eu_code]] <- c(modello_def,
                         rmse(pdf$value, exp(as.numeric(pred_y))),  # 20%
                         rmse(mod_def$y, mod_def$fitted.values), # 80%
                         mse(pdf$value, exp(as.numeric(pred_y))), # 20%
                         mse(mod_def$y, mod_def$fitted.values), # 80%
                         rsq(pdf$value, exp(as.numeric(pred_y))),
                         rsq(mod_def$y, mod_def$fitted.values),
                         nvalidate/n, # FAC2
                         fb, # Fractional BIAS
                         nmse # Normalized Mean Square Error
                         )
  # return("-")
}

my_mat <- do.call(rbind, my_list)
my_df <- data.frame(id = names(my_list), my_mat)

# names(my_df)
colnames(my_df) <- c("station_eu_code", "modello", "rmse_20", "rmse_80", "mse_20", "mse_80", "rsq_20", "rsq_80", "FAC2", "FB", "NMSE")

my_df$modello <- as.character(my_df$modello)

sapply(my_df, function(x) { as.vector(x)}) %>%
  write.table(file = glue::glue("{outdir}/validazione_{pltnt}.csv"), sep = ";", row.names = FALSE)

# library("hydroGOF")
# hydroGOF::nrmse(exp( as.numeric( pred_y ) ), pdf$value)
# hydroGOF::nrmse(mod_tdf$fitted.values, mod_tdf$y)
