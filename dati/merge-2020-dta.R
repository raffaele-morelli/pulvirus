# library(ggplot2)
library(readr)
library(dplyr)

setwd("~/R/pulvirus")

# pltnt <- "C6H6"
# pltnt <- "CO"
# pltnt <- "NO2"
# pltnt <- "NOX|NOx"
# pltnt <- "O3"
pltnt <- "PM10_"
# pltnt <- "PM25|PM2.5"
# pltnt <- "SO2"

prefix <- ""

# importazione ####
data.files <-  list.files(path = "dati/dati-agenzie/LOM_FVG_VEN_VDA_PIE/csv", pattern = paste0(prefix, pltnt), full.names = TRUE)

data <- lapply( data.files, function(x) read_csv(x) )

tmp.df <- do.call(rbind, data)

tmp.df <- tmp.df %>% mutate(
  pollutant_fk = case_when(
    pltnt == "C6H6" ~ 20,
    pltnt == "CO" ~ 10,
    pltnt == "NO2" ~ 8,
    grepl("NOX", pltnt) ~ 9,
    pltnt == "O3" ~ 7,
    pltnt == "PM10" ~ 5,
    pltnt == "PM2.5" ~ 6001,
    pltnt == "PM25" ~ 6001,
    pltnt == "SO2" ~ 1,
  )
)

ind <- duplicated(tmp.df[, c("station_eu_code", "date")])
View(tmp.df[ ind, c("station_eu_code", "date")])

write.csv(tmp.df[which(!is.na(tmp.df$date)), c("reporting_year", "pollutant_fk", "station_eu_code","date","value")], 
          paste0("dati/csv-2020-dta/", pltnt, "_dta_2020.csv"), 
          row.names = FALSE)
