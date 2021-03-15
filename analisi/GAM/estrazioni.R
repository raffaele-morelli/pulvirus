library(dplyr)
library(purrr)
library(mgcv)
library(logr)
library(knitr)
library(kableExtra)
library(stringr)

out_dir <- "/home/rmorelli/R/pulvirus/analisi/GAM/report"
out.file <- glue::glue("{out_dir}/pulvirus_test_estrazioni.md")
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

models %>% 
  map(~ map_dbl(.x, AIC)) %>% 
  do.call(cbind, .) %>% 
  as.data.frame() %>%  
  kable(format = "markdown", col.names = c("")) %>% 
  kable_styling() %>%  
  print()

cat("\n# R squared \n")

models[[1]] %>%
  map(summary.gam) %>%
  map_dbl(~.$r.sq)%>% 
  kable(format = "markdown") %>% 
  kable_styling() %>%
  print(row.names = FALSE)

cat("\n\n")
cat("# P-values\n")

for (i in models[[1]]) {
  # cat("##", names(models[[1]][1]), sep = "\n")
  estrai(i) %>% kable(caption = names(models[[1]][1]), format = "markdown") %>%
    kable_styling() %>%
    print(col.names = FALSE)
  # capture.output( gam.check( i ) ) %>% print()
  cat("------------------\n", sep = "\n")
}

sink()

markdown::markdownToHTML(out.file, output = str_replace(out.file, ".md", ".html"))


