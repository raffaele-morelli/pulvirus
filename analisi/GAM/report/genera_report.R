# il percorso dove risiede lo script
base.dir <- "~/R/pulvirus/analisi/GAM/report"

# directory di output
web.dir <- "~/public_html/pulvirus"

# directory con i file RData
rdata.dir <- "~/R/pulvirus/analisi/GAM/scelta/output"

rdatas <- list.files(path = glue("{rdata.dir}"),
                     pattern = "*.RData", recursive = TRUE,
                     full.names = TRUE)

# snip per generare i report ####
for (i in rdatas) {
  parti <- str_split(basename(i), "_")
  fn <- tools::file_path_sans_ext(parti[[1]][3])
  
  rmarkdown::render(glue::glue("{base.dir}/pulvirus_estrazioni_template_single.Rmd"),
                    params = list(rdatafile = glue("{i}")),
                    output_dir = glue("{web.dir}"),
                    output_file = glue("{parti[[1]][1]}_{parti[[1]][2]}_{fn}")
  )
  return(" ")
}