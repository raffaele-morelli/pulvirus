library(readr)
library(dplyr)
setwd("~/R/pulvirus/dati/missing/")
data.files <- list.files(path = ".", pattern = "*valide.csv", recursive = TRUE)
data <- lapply(data.files, function(x) read_csv(x) )
data1 <- do.call(rbind, data)

write_csv(data1, "stazioni_pulvirus.csv")
stazioni <- read_csv("~/R/pulvirus/dati/stazioni-metadati.csv")

sink(glue::glue("~/R/pulvirus/dati/missing/stazioni_pulvirus.md"), append = FALSE)

merge(data1, stazioni[, c("station_eu_code", "region_id", "regione", "provincia")], 
      by.x = "station_eu_code", by.y = "station_eu_code", 
      all.x = TRUE) %>% kableExtra::kable(format = "markdown")

sink()


