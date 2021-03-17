library(dplyr)
library(purrr)
library(magrittr)
library(mgcv)
library(logr)
library(knitr)
library(kableExtra)
library(stringr)
library(glue)
library(DT)
library(datiInquinanti)

basedir <- "~/R/pulvirus"
# basedir <- "~/R/pulvirus.github.io"


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

# chiudi le connessioni a sink ####
sink.reset <- function(){
  for(i in seq_len(sink.number())){
    sink(NULL)
  }
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
  
  cat(sprintf("\n\n## Modello:\n\n %s", names(models)), "\n")
  cat("\n\n")
  
  cat(sprintf("```{r, echo=FALSE}
library(DT)
library(purrr)
library(mgcv)
load('%s')
```\n\n", f) )

  
  cat("# AICS \n")
  
  cat("\n
```{r, echo=FALSE}
aics <- models %>% 
  map(~ map_dbl(.x, AIC)) %>%  do.call(cbind, .) %>% 
  as.data.frame() %>% set_colnames(c('AIC'))

ggplot(aics) + 
geom_col(aes(x = rownames(aics), y = AIC), fill = 'dodgerblue')  +  
theme(axis.text.x = element_text(angle = 90))

datatable(aics)
```\n\n")
  
  cat("\n# R squared \n")
  
  cat("```{r, echo=FALSE}
rsquared <- models[[1]] %>%
    map(summary.gam) %>%
    map_dbl(~.$r.sq) %>% as.data.frame() %>% set_colnames(c('R'))

ggplot(rsquared) + 
geom_col(aes(x = rownames(rsquared), y = R), fill = 'red')  +  
theme(axis.text.x = element_text(angle = 90))

datatable(rsquared)
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
  rm(list = setdiff(ls(), c("rdatas", "i", "reportTheFn", "estrai", "str_spl", "basedir", "sink.reset") ) )
  # reportTheFn(i)
}

reportTheFn(glue("{basedir}/analisi/GAM/output/no2_12.RData"))



sink.reset()
