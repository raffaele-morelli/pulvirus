library(dplyr)
library(purrr)
library(mgcv)
library(logr)
library(knitr)
library(kableExtra)
library(stringr)
library(glue)
library(DT)
library(datiInquinanti)

basedir <- "~/R/pulvirus"

# estrazione parametri GAM check ####
estrai <- function(Out) {
  gam_obj <-  mgcv:::anova.gam(Out) 
  return(gam_obj)
  # gam_tbl <- gam_obj[11:length(gam_obj)]
  # 
  # tmp.df <- do.call(rbind, lapply(gam_tbl, str_spl)) %>% 
  #   as.data.frame()
  # 
  # names(tmp.df) <- c("Vs", "edf", "Ref.df", "F", "p-value") 
  # return( tmp.df[-c(1),] )
  # return(gam_obj$s.table)
}

# split per spazi multipli ####
str_spl = function(x){
  x <- gsub("<", "", x)    # il simbolo '<' ci rompe le scatole per lo split successivo
  strsplit(x, "\\s+")[[1]]
}


# RUN ####
rdatas <- list.files(path = glue("{basedir}/analisi/GAM/output"),
                     pattern = "*.RData", 
                     full.names = TRUE)


reportTheFn <- function(f) {
  load(f)
  out_dir <- glue("{basedir}/analisi/GAM/report")
  out.file <- glue::glue("{out_dir}/pulvirus_estrazioni_{pltnt}_{cod_reg}.Rmd")
  regione <- stazioniAria %>% filter(region_id == cod_reg) %>% select(regione) %>% unique()

  sink(out.file)
  
  header <- glue::glue("---\n", "title: 'Report modelli: {regione} - {pltnt}'\n", "author: Pluto\n", "---\n")
  
  cat(header, sep = "\n\n")
  
  cat(sprintf("Modello: %s", names(models)), "\n")
  cat("\n\n")
  
  cat(sprintf("```{r, echo=FALSE}
library(DT)
library(purrr)
library(mgcv)
```\n\n"))
  

  cat(sprintf("```{r}\nload('%s')\n```\n\n", f))
  
  cat("# AICS \n")
  
  cat("\n
```{r, echo=FALSE}
datatable(models %>% 
  map(~ map_dbl(.x, AIC)) %>%  do.call(cbind, .) %>% 
  as.data.frame())
```\n\n")
  
  cat("\n# R squared \n")
  
  cat("```{r, echo=FALSE}
datatable(
    models[[1]] %>%
    map(summary.gam) %>%
    map_dbl(~.$r.sq) %>% as.data.frame()
  )
```\n\n")
  
  cat("# P-values\n")
  
  j <- 1
  for (i in 1:length( models[[1]] ) ) {
    cat("##", names(models[[1]][j]), sep = "\n")
    t <- estrai(models[[1]][[j]])
    t$s.table %>% kable( format = "markdown") %>% print()
    cat("------------------\n", sep = "\n")
    j <- j + 1
  }
  
  sink()
  
  # markdown::markdownToHTML(out.file, output = str_replace(out.file, ".Rmd", ".html"))
  rmarkdown::render(out.file)
}

for(i in rdatas) {
  rm(list = setdiff(ls(), c("rdatas", "i", "reportTheFn", "estrai", "str_spl", "basedir") ) )
  # reportTheFn(i)
}

reportTheFn(glue("{basedir}/analisi/GAM/output/no2_12.RData"))

