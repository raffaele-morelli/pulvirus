library(readr)
setwd("~/R/pulvirus/dati/missing/")
data.files <- list.files(path = ".", pattern = "*valide.csv", recursive = TRUE)
data <- lapply(data.files, function(x) read_csv(x) )
write_csv(do.call(rbind, data), "stazioni_pulvirus.csv")

