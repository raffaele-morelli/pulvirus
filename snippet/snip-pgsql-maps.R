library(RPostgreSQL)
library(sf)
library(dplyr)
library(rgdal)
library(ggplot2)

aria <- readOGR("PG:dbname=pulvirus host=10.158.102.164 port=5432 user=srv-pulvirus password=pulvirus#20", "stazioni_aria")

world_map_crs <- "+init=epsg:4326"

st_as_sf(aria)  %>% 
  st_transform(world_map_crs) %>% 
  ggplot() +
  geom_sf() 


meteo <- readOGR("PG:dbname=pulvirus host=10.158.102.164 port=5432 user=srv-pulvirus password=pulvirus#20", "stazioni_meteo")

st_as_sf(meteo)  %>% 
  st_transform(world_map_crs) %>% 
  ggplot() +
  geom_sf() 
