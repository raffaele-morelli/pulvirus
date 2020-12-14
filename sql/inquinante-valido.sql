SELECT * 
FROM no2 
WHERE station_eu_code IN (
   SELECT  station_eu_code  FROM stazioni_pulvirus  WHERE valida=1 AND pollutant_fk=5
  )
