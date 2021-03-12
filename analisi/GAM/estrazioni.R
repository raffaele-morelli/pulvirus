models %>% 
  map(~ map_dbl(.x, AIC)) %>% 
  do.call(rbind, .) %>% 
  as.data.frame() 

models[[1]] %>%
  map(summary.gam) %>%
  map_dbl(~.$r.sq) 

# %>% split(.$station_eu_code) 

gam.check( models[[1]]$IT0983A )
