SELECT *
FROM no2 
WHERE station_eu_code IN 
( SELECT DISTINCT station_eu_code FROM no2 WHERE reporting_year=2020 ) ORDER BY date ASC