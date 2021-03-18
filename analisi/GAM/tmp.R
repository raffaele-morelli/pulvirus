library(RPostgreSQL)
library(dplyr)
library(stringr)

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

basedir <- "~/R/pulvirus"
webdir <- "~/public_html/pulvirus"

for(pltnt in names(df[-c(1,2, ncol(df))])) {
  for( region_id in which(!is.na(df[pltnt]))  ) {
    cat(glue::glue("Rscript scelta_var.R {pltnt} {region_id} & "), sep = "\n")
  }
}

rdatas <- list.files(path = glue("{basedir}/analisi/GAM/output"),
                     pattern = "*.RData", 
                     full.names = TRUE)

for(i in rdatas) {
  # f <- tools::file_path_sans_ext(i)
  # print(f)
  f <- tools::file_path_sans_ext(basename(i))
  print(f)
  regione <- stazioniAria %>% filter(region_id == str_split(f, "_")[[1]][2]) %>% select(regione) %>% unique()
  
  cat(
    sprintf("rmarkdown::render(\"~/R/pulvirus/analisi/GAM/pulvirus_estrazioni_template.Rmd\",
      params = list(rdatafile = \"%s\", titolo = \"%s\"),
      output_dir = \"%s\",
      output_file = \"%s\"
              )", i, regione, webdir, f),
    "\n"
  )
}

# snip per generare i report ####
rmarkdown::render("~/R/pulvirus/analisi/GAM/pulvirus_estrazioni_template.Rmd",
                  params = list(rdatafile = glue("{basedir}/analisi/GAM/output/no2_12.RData"), titolo = "Lazio"),
                  output_dir = glue("{webdir}"),
                  output_file = "no2_12"
                  )
