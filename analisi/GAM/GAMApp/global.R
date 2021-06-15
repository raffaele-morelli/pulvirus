library(shiny)
library(leaflet)
library(dplyr)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(scales)
library(openair)
library(skimr)
library(purrr)
library(kableExtra)

require(rmweather) 
require(DALEX)

library(mgcv)
library(mgcViz)
library(magrittr)

library(shiny)
library(shinythemes)

# remotes::install_github("raffaele-morelli/datiMeteo", force = TRUE)
# remotes::install_github("raffaele-morelli/datiInquinanti", force = TRUE)
library(datiInquinanti)
library(datiMeteo)


base_path <- "/home/rmorelli/R/pulvirus/analisi/GAM/scelta/output/"


# riclassificazione stazioni nel campo "tipo_s"
stazioniAria %>%
  mutate(tipo_s = case_when(
    zona_tipo == "FU" ~ "Fondo urbano/suburbano",
    zona_tipo == "FS" ~ "Fondo urbano/suburbano",
    # zona_tipo == "U" ~ "Fondo urbano/suburbano",
    # zona_tipo == "S" ~ "Fondo urbano/suburbano",
    # zona_tipo == "METEOS" ~ "Fondo urbano/suburbano",
    # zona_tipo == "METEOU" ~ "Fondo urbano/suburbano",
    zona_tipo == "TU" ~ "Traffico",
    zona_tipo == "TS" ~ "Traffico",
    tipo_zona == "R" ~ "Rurale",

    tipo_zona == "R-nearcity" ~ "Rurale",
    tipo_zona == "R-regional" ~ "Rurale",
    tipo_zona == "R-remote" ~ "Rurale",
    tipo_stazione == "I" ~ "Industriale",
    tipo_stazione == "F/I" ~ "Industriale",
  )) -> stazioniAria


# selezione delle sole stazioni presenti nei dataset degl inquinanti
# per i layer leaflet
no2Staz <- no2 %>% select(station_eu_code) %>% unique() %>% inner_join(stazioniAria, by = c("station_eu_code"))
noxStaz <- nox %>% select(station_eu_code) %>% unique() %>% inner_join(stazioniAria, by = c("station_eu_code"))
pm10Staz <- pm10 %>% select(station_eu_code) %>% unique() %>% inner_join(stazioniAria, by = c("station_eu_code")) 
pm25Staz <- pm25 %>% select(station_eu_code) %>% unique() %>% inner_join(stazioniAria, by = c("station_eu_code"))

stazUniche <- c(
  unique(no2Staz$station_eu_code), 
  unique(pm10Staz$station_eu_code ), 
  unique(pm25Staz$station_eu_code),
  unique(noxStaz$station_eu_code)
  ) %>% unique()

# usiamo una paletta divergente 
paletta = leaflet::colorFactor(RColorBrewer::brewer.pal(5, "Spectral"), domain = unique(stazioniAria$tipo_s))


theme_pulvirus <- function() {
  theme_minimal() %+replace% 
    
    theme(plot.title = element_text(size = 12),
          text = element_text(size = 11), legend.position = "top",
          axis.title = element_text(face = "bold"),
          axis.text.x = element_text(size = 11),
          panel.background = element_rect(fill = "white",colour = "white", size = 0.3, linetype = "solid"),
          axis.line = element_line(colour = "darkblue", 
                                   size = 0.4, linetype = "solid"))
}
