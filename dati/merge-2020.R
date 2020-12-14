# library(ggplot2)
library(readr)
library(dplyr)

setwd("~/R/pulvirus")

# pltnt <- "C6H6"
# pltnt <- "CO"
# pltnt <- "NO2"
pltnt <- "NOX"
# pltnt <- "O3"
# pltnt <- "PM10"
# pltnt <- "PM25|PM2.5"
# pltnt <- "SO2"

prefix <- ""

# importazione ####
data.files <-  list.files(path = "dati/dati-agenzie/csv-regioni", pattern = paste0(prefix, pltnt), full.names = TRUE)

data <- lapply( data.files, function(x) read_delim(x, 
                                                   ",", escape_double = FALSE, 
                                                   trim_ws = TRUE) )

for(i in 1:length(data.files)) {
  names(data[[i]])[4] <- pltnt
  test.dupl <- data[[i]]
  ind <- duplicated( test.dupl[,c("station_eu_code","date")] )
  print( unique(ind) )
  # View(test.dupl[which(ind),])
}

tmp.df <- do.call(rbind, data)

tmp.df <- tmp.df %>% mutate(
  pollutant_fk = case_when(
    pltnt == "C6H6" ~ 20,
    pltnt == "CO" ~ 10,
    pltnt == "NO2" ~ 8,
    pltnt == "NOX" ~ 9,
    pltnt == "NOx" ~ 9,
    pltnt == "O3" ~ 7,
    pltnt == "PM10" ~ 5,
    pltnt == "PM2.5" ~ 6001,
    pltnt == "PM25" ~ 6001,
    pltnt == "SO2" ~ 1,
  )
)


colnames(tmp.df)[4] <- "value"

ind <- duplicated(tmp.df[, c("station_eu_code", "date")])
 # View(tmp.df[ ind, c("station_eu_code", "date")])

write.csv(tmp.df[which(!is.na(tmp.df$date)), c("reporting_year","pollutant_fk","station_eu_code","date","value")], 
          paste0("dati/csv-2020/", pltnt, "_2020.csv"), 
          row.names = FALSE)
