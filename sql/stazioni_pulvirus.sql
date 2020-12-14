SELECT descrizione, count(station_eu_code) 
  FROM stazioni_pulvirus a JOIN lookup_inquinanti b ON a.pollutant_fk=b.pollutant_fk
  WHERE valida=true 
  GROUP BY a.pollutant_fk, descrizione;
