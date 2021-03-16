library(dplyr)
library(purrr)
library(mgcv)
library(logr)
library(knitr)
library(kableExtra)
library(stringr)
library(DT)

# estrazione parametri GAM check ####
estrai <- function(Out) {
  gam_obj <- capture.output( mgcv:::anova.gam(Out) )
  gam_tbl <- gam_obj[11:length(gam_obj)]
  
  tmp.df <- do.call(rbind, lapply(gam_tbl, str_spl)) %>% 
    as.data.frame()
  
  names(tmp.df) <- c("Vs", "edf", "Ref.df", "F", "p-value") 
  return( tmp.df[-c(1),] )
}

# split per spazi multipli ####
str_spl = function(x){
  x <- gsub("<", "", x)    # il simbolo '<' ci rompe le scatole per lo split successivo
  strsplit(x, "\\s+")[[1]]
}


# RUN ####


rdatas <- list.files(path = "/home/rmorelli/R/pulvirus/analisi/GAM/output", 
                     pattern = "*.RData", 
                     full.names = TRUE)


reportTheFn <- function(f) {
  # 
  load(f)
  out_dir <- "/home/rmorelli/R/pulvirus/analisi/GAM/report"
  out.file <- glue::glue("{out_dir}/pulvirus_estrazioni{pltnt}_{cod_reg}.Rmd")
  

  sink(out.file)
  
  header <- r"(---
title: Report modelli
author: GAM
---
)"
  
  cat(header, sep = "\n\n")
  
  cat("Modello: ", names(models), "\n")
  cat("\n\n")
  
  
  cat("# AICS \n")
  
  # models %>% 
  #   map(~ map_dbl(.x, AIC)) %>% 
  #   do.call(cbind, .) %>% 
  #   as.data.frame() %>%  
  #   kable(format = "markdown", col.names = c("")) %>% 
  #   kable_styling() %>%  
  #   print()

  tabella1 <- r"(```{r, echo=FALSE}
  library(DT)
  library(purrr)
  datatable(models %>% 
              map(~ map_dbl(.x, AIC)) %>% 
              do.call(cbind, .) %>% 
              as.data.frame())
```
)"
  
  cat(tabella1, "\n")
 
  
  cat("")

  
  cat("\n# R squared \n")
  
tabella2 <- r"(```{r, echo=FALSE}
  library(DT)
  library(purrr)
  datatable(
    models[[1]] %>%
    map(summary.gam) %>%
    map_dbl(~.$r.sq) %>% as.data.frame()

  )
```
)"
cat(tabella2, "\n")

  cat("\n\n")
  cat("# P-values\n")
  
  j <- 1
  for (i in models[[1]]) {
    cat("##", names(models[[1]][j]), sep = "\n")
    estrai(i) %>% kable( format = "markdown", row.names = FALSE) %>%
      kable_styling() %>%
      print(row.names = FALSE)
    # capture.output( gam.check( i ) ) %>% print()
    cat("------------------\n", sep = "\n")
    j <- j + 1
  }
  
  sink()
  
  # markdown::markdownToHTML(out.file, output = str_replace(out.file, ".Rmd", ".html"))
  rmarkdown::render(out.file)
}

for(i in rdatas) {
  rm(list = setdiff(ls(), c("rdatas", "i", "reportTheFn", "estrai", "str_spl") ) )
  # load(i)
  # print(i)
  reportTheFn(i)
}

# reportTheFn("/home/rmorelli/R/pulvirus/analisi/GAM/output/c6h6_3.RData")

