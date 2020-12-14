SELECT * FROM public.benzene WHERE station_eu_code IN (SELECT station_eu_code FROM public.stazioni_metadati WHERE region_id=1);
