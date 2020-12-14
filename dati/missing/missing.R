library(naniar)
library(visdat)
library(Hmisc)
library(dplyr)
library(readr)
library(lubridate)
library(RPostgreSQL)
# library(logr)
library(knitr)
library(markdown)
library(janitor)

setwd("~/R/pulvirus/dati/missing")

pltnt <- "pm10"
df <- read_csv(paste0("../db-feedback/", pltnt, ".csv"), col_types = cols(X1 = col_skip()))

# df <- filter(df, grepl("IT", station_eu_code))

stazioni <- read_csv("~/R/pulvirus/dati/stazioni-metadati.csv")
codici_stazioni <-  df %>% inner_join(stazioni, by = c("station_eu_code")) %>% select(station_eu_code) %>% unique()

nv.anni <- c()
v.anni <- c()

for (i in codici_stazioni$station_eu_code ) {
  # print(i)
  if(is.na(i)) next()
  
  anni <- df %>% filter(df$station_eu_code == i) %>% filter(reporting_year > 2015) %>% select(reporting_year) %>% unique() %>% nrow()

  if( anni < 5) {
    nv.anni <- c(nv.anni, i)
  }
  if(anni == 5) {
    v.anni <- c(v.anni, i)
  }
  
}

if(file.exists(glue::glue("{pltnt}/{pltnt}_summary.md"))) file.remove(glue::glue("{pltnt}/{pltnt}_summary.md"))
sink(glue::glue("{pltnt}/{pltnt}_summary.md"), append = FALSE)
cat("\n## Numero stazioni 2016-2020: \n")
cat("\n## Non valide per anni:\n", length( nv.anni), "\n" )
cat("\n")
cat("\n## Valide:\n", length( v.anni), "\n" )
sink()


my_list <- list()
for (i in v.anni) {
  tmp.df <- df %>% filter(station_eu_code == i) %>% filter( reporting_year > 2015 )
  tmp.df$month <-  month(tmp.df$date)

  tmp.df <- tmp.df %>% 
    # filter(reporting_year < 2020) %>%
    mutate(stagione =
             case_when(
               month %in% c(1,2,12) ~ "inverno",
               month %in% c(3,4,5) ~ "primavera",
               month %in% c(6,7,8) ~ "estate",
               TRUE ~ "autunno"
             ) 
    )

  # un mese e' valido se contiene almeno il 75% di dati validi (senza ulteriori sulla continuità dei blocchi 
  # dei dati mancanti all'interno del mese)
  
  # calcolo i mesi validi ####
  
  # METODO 1
  # mc.mesi <- tmp.df %>% group_by(reporting_year, month, stagione) %>%
  #   miss_var_summary() %>% filter(variable == "value" ) %>%
  #   mutate(flag_mese = ifelse(pct_miss > 25, 0, 1) )

  # METODO 2 ####
  mc.mesi  <- tmp.df %>%
    filter(!is.na(value)) %>%
    group_by(reporting_year, month, stagione) %>%
    summarise(n = n()) %>%
    ungroup() %>% mutate(flag_mese = ifelse(n < 23, 0, 1)) 

  # un anno e' valido se contiene tutte le stagioni valide (almeno due mesi validi per stagione)
  # calcolo stagioni valide ####
  mc.stagioni <- mc.mesi %>% 
    group_by(reporting_year, stagione) %>% 
    summarise(n = sum(flag_mese), .groups = 'drop') %>% 
    mutate(flag_stagione = ifelse(n > 1, 1, 0)) 
  
  mc.anni <- mc.stagioni %>% 
    select(reporting_year, flag_stagione) %>% 
    group_by(reporting_year) %>% 
    summarise(n = sum(flag_stagione),  .groups = 'drop') %>% 
    mutate(flag_anno = ifelse(n != 4, 0, 1) )

  v1 <- mc.anni %>% filter(reporting_year > 2015 & reporting_year < 2020 & flag_anno == 1) %>% nrow() == 4
  v2 <- mc.mesi %>% filter(reporting_year == 2020 & month < 7 & flag_mese == 1) %>% nrow() == 6

  my_list[[i]] <- c(v1, v2, v1 & v2)
}

my_mat <- do.call(rbind, my_list)
my_df <- data.frame(id = names(my_list), my_mat)

colnames(my_df) <- c("station_eu_code", "p16_19", "p20", "Valida")


sink(glue::glue("{pltnt}/{pltnt}_summary.md"), append = TRUE)
cat("\n## Numero stazioni valide - dettaglio: \n")
cat( sprintf("Valide: %s \n", my_df %>% filter(Valida == TRUE) %>% count() ) )
cat( sprintf("Non valide: %s \n", my_df %>% filter(Valida == FALSE) %>% count() ))
sink()

v <- my_df %>% filter(Valida == TRUE)
nv <- my_df %>% filter(Valida == FALSE)

v.regione <-  merge(v, stazioni[, c("station_eu_code", "region_id", "regione", "provincia")], by.x = "station_eu_code", by.y = "station_eu_code", all.x = TRUE)
nv.regione <-  merge(nv, stazioni[, c("station_eu_code", "region_id", "regione", "provincia")], by.x = "station_eu_code", by.y = "station_eu_code", all.x = TRUE)

sink(glue::glue("{pltnt}/{pltnt}_valide_summary.md"), append = FALSE)
cat("\n# VALIDE\n")

v.regione[order(v.regione$regione),] %>% 
  group_by(regione) %>% summarise( n = n()) %>% 
  adorn_totals() %>%
  kable(format = "markdown") %>% print()

for(i in unique( v.regione$region_id) ) {
  reg <- v.regione %>% filter(region_id == i) %>% select(regione) %>% unique()
  
  cat( sprintf("\n## %s : %s\n", reg, v.regione %>% filter(region_id == i) %>% nrow()  ) )
  v.regione %>% filter(region_id == i) %>% kable(format = "markdown") %>% print()
  cat("\n")
}
sink()


sink(glue::glue("{pltnt}/{pltnt}_non_valide_summary.md"), append = FALSE)

cat("\n# NON VALIDE\n")
nv.regione[order(nv.regione$regione),] %>% 
  group_by(regione) %>% summarise( n = n()) %>% 
  adorn_totals() %>%
  kable(format = "markdown") %>% print()

cat("\n")
for(i in unique( nv.regione$region_id) ) {
  reg <- nv.regione %>% filter(region_id == i) %>% select(regione) %>% unique()
  
  cat( sprintf("\n## %s : %s\n", reg, nv.regione %>% filter(region_id == i) %>% nrow()  ) )
  nv.regione %>% filter(region_id == i) %>% kable(format = "markdown") %>% print()
  cat("\n")
}
sink()

v$pollutant_fk <- rep(unique(tmp.df$pollutant_fk)[1], length(v[,1]) ) 
nv$pollutant_fk <- rep(unique(tmp.df$pollutant_fk)[1], length(nv[,1]) ) 

write_csv(v, glue::glue("{pltnt}/{pltnt}_valide.csv") )
write_csv(nv, glue::glue("{pltnt}/{pltnt}_non_valide.csv") )