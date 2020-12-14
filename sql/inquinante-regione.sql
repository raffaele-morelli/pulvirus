SELECT * 
FROM no2 WHERE station_eu_code IN (
SELECT  station_eu_code
  FROM stazioni_aria
  WHERE region_id=12) AND reporting_year=2020
  
