library(RPostgreSQL)
library(dplyr)

con <- dbConnect(PostgreSQL(), 
                 host = "10.158.102.164", 
                 user = "srv-pulvirus", 
                 password = "pulvirus#20", 
                 dbname = "pulvirus")

# directory di destinazione dei file csv
# out.dir <- "/home/rmorelli/R/pulvirus/dati/db-feedback"

# la lista delle tabelle
dbListTables(con)

sql <- paste0("SELECT * FROM public.riepilogo_valide;")
rs <- dbSendQuery(con, sql)
df <- fetch(rs, n = -1)

dbDisconnect(con)


for(pltnt in names(df[-c(1,2, ncol(df))])) {
  for( region_id in which(!is.na(df["c6h6"]))  ) {
    cat(glue::glue("Rscript scelta_var.R {pltnt} {region_id} & "), sep = "\n")
  }
}

basedir <- "~/R/pulvirus"
# snip per generare i report ####
rmarkdown::render("~/R/pulvirus/analisi/GAM/pulvirus_estrazioni_template.Rmd",
                  params = list(rdatafile = glue("{basedir}/analisi/GAM/output/no2_2.RData"), titolo = "VDA"),
                  output_dir = glue("{basedir}/analisi/GAM/report"),
                  output_file = "no2_2"
                  )
