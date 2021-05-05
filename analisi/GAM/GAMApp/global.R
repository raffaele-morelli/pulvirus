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
library(mgcv)
library(mgcViz)
library(magrittr)

library(shiny)
library(shinythemes)

library(datiInquinanti)
library(datiMeteo)

no2Staz <- no2 %>% select(station_eu_code) %>% unique() %>% left_join(stazioniAria, by = c("station_eu_code"))
pm10Staz <- pm10 %>% select(station_eu_code) %>% unique() %>% left_join(stazioniAria, by = c("station_eu_code")) 
pm25Staz <- pm25 %>% select(station_eu_code) %>% unique() %>% left_join(stazioniAria, by = c("station_eu_code"))

stazUniche <- c(unique(no2Staz$station_eu_code), unique( pm10Staz$station_eu_code ), unique( pm25$station_eu_code) ) %>% unique()

theme_pulvirus <- function() {
  theme_minimal() %+replace% 
    
    theme(plot.title = element_text(size = 10, family = "Tahoma", face = "bold"),
          text = element_text(size = 10, family = "Tahoma"), legend.position = "top",
          axis.title = element_text(face = "bold"),
          axis.text.x = element_text(size = 10),
          panel.background = element_rect(fill = "white",colour = "white", size = 0.3, linetype = "solid"),
          axis.line = element_line(colour = "darkblue", 
                                   size = 0.4, linetype = "solid"))
}
