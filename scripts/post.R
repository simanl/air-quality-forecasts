##guardado de tabla maestra y tablas adicionales
post <- function(){
  save(tabla.maestra, file = "tablas/series/tabla_maestra.RData")  
  save(O3Obispado, file = "tablas/series/O3Obispado.RData")
  save(O3Pastora, file = "tablas/series/O3Pastora.RData")
  save(O3Bernabe, file = "tablas/series/O3Bernabe.RData")
  save(O3Catarina, file = "tablas/series/O3Catarina.RData")
  save(O3Nicolas, file = "tablas/series/O3Nicolas.RData")
  save(PM10Obispado, file = "tablas/series/PM10Obispado.RData")
  save(PM10Pastora, file = "tablas/series/PM10Pastora.RData")
  save(PM10Bernabe, file = "tablas/series/PM10Bernabe.RData")
  save(PM10Catarina, file = "tablas/series/PM10Catarina.RData")
  save(PM10Nicolas, file = "tablas/series/PM10Nicolas.RData")
}
