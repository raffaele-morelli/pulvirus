library(RPostgreSQL)
library(dplyr)

con <- dbConnect(PostgreSQL(), 
                 host = "10.158.102.164", 
                 user= "srv-pulvirus", 
                 password="pulvirus#20", 
                 dbname="pulvirus")

# directory di destinazione dei file csv
out.dir <- "/home/rmorelli/R/pulvirus/dati/db-feedback"

# la lista delle tabelle
# dbListTables(con)

# sql <- paste0("SELECT * FROM public.lookup_inquinanti;")
# rs <- dbSendQuery(con, sql)
# fetch(rs, n = -1)

pltnt <- "no2"

sql <- paste0("SELECT * FROM public.", pltnt, ";")
rs <- dbSendQuery(con, sql)

df <- fetch(rs, n = -1)

write.csv(df, paste0(out.dir, "/", pltnt,".csv"), row.names = FALSE )

# for (i in unique( df$region_id) ) {
#   rs.tmp <- dbSendQuery(con, 
#               paste0("SELECT * 
#                      FROM public.benzene 
#                      WHERE station_eu_code IN 
#                      (SELECT station_eu_code FROM public.stazioni_metadati WHERE region_id=", i,")") )
#   df.tmp <- fetch(rs.tmp, n = -1)
#   dbClearResult(rs.tmp)
#   
#   regione <-  df_stazioni %>% filter(region_id == i) %>% select(regione) %>% unique
#   
#   write.table(df.tmp, paste0(out.dir, "/", i, "_", regione, ".csv"), sep = ",", row.names = FALSE )
# }

# per ripulire in caso di stop/errori del tipo "RS-DBI driver: (connection with pending rows, close resultSet before continuing)"
# dbClearResult(dbListResults(con)[[1]]) 

dbDisconnect(con)
