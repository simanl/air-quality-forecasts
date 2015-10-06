SPCA <- function(x, ...)
  UseMethod("SPCA")

SPCA.default <- function(SPCA_HOME = "~/SPCA", entrada, salida){
##SCPA_HOME
  setwd(SPCA_HOME)

##lectura de tabla de entrada
  if(missing(entrada))
    tabla.entrada <- as.data.frame(get(tabla.entrada, envir = .GlobalEnv))
    else
      tabla.entrada <- lecturaCSV(substitute(entrada))

  
##datos historicos horarios
  load("tablas/series/tabla_maestra.RData", .GlobalEnv) #tabla maestra final (horaria) con solo los sitios de interes
  system(paste("cp ", SPCA_HOME, "/tablas/series/tabla_maestra.RData ", SPCA_HOME, "/tablas/series/tabla_maestra.RData.bkp", sep = ""), intern = T)
  datos.historicos.wdf <- tabla.maestra[(nrow(tabla.maestra) - 200):nrow(tabla.maestra), ] #se seleccionan los 200 ultimos para hacer la imputacion

##datos historicos imputados
  load("tablas/series/tabla_maestra_imputada.RData", .GlobalEnv) #tabla maestra final (horaria) imputada con solo los sitios de interes
  system(paste("cp ", SPCA_HOME, "/tablas/series/tabla_maestra_imputada.RData ", SPCA_HOME, "/tablas/series/tabla_maestra_imputada.RData.bkp", sep = ""), intern = T)
  load("tablas/series/O3Obispado.RData", .GlobalEnv)
  load("tablas/series/O3Pastora.RData", .GlobalEnv)
  load("tablas/series/O3Nicolas.RData", .GlobalEnv)
  load("tablas/series/O3Bernabe.RData", .GlobalEnv)
  load("tablas/series/O3Catarina.RData", .GlobalEnv)
  load("tablas/series/PM10Obispado.RData", .GlobalEnv)
  load("tablas/series/PM10Pastora.RData", .GlobalEnv)
  load("tablas/series/PM10Nicolas.RData", .GlobalEnv)
  load("tablas/series/PM10Bernabe.RData", .GlobalEnv)
  load("tablas/series/PM10Catarina.RData", .GlobalEnv)
  

##validacion
  tabla.entrada <- validacion(tabla.entrada)

##columnas adicionales (variables de calendario y estaciones)
  tabla.entrada.cols.adicionales <- colsTemp(tabla.entrada)
  
##imputacion
  imputaciones <- imputacion(tabla.entrada.cols.adicionales, datos.historicos.wdf, modelos.dir = "modelos/modelos_de_imputacion")

##actualizacion de las series: periodos y modelos
  O3Obispado <- update(O3Obispado, tail(imputaciones, 30))
  O3Pastora <- update(O3Pastora, tail(imputaciones, 30))
  O3Nicolas <- update(O3Nicolas, tail(imputaciones, 30))
  O3Bernabe <- update(O3Bernabe, tail(imputaciones, 30))
  O3Catarina <- update(O3Catarina, tail(imputaciones, 30))

  PM10Obispado <- update(PM10Obispado, tail(imputaciones, 30))
  PM10Pastora <- update(PM10Pastora, tail(imputaciones, 30))
  PM10Nicolas <- update(PM10Nicolas, tail(imputaciones, 30))
  PM10Bernabe <- update(PM10Bernabe, tail(imputaciones, 30))
  PM10Catarina <- update(PM10Catarina, tail(imputaciones, 30))

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

# debug(pronostico.stSIMA)
##pronostico de las series
  O3Obispado.pronostico <- as.data.frame(pronostico(O3Obispado, modelos.var.met = "modelos/modelos_de_pronostico/modelos_pronostico_met_O3_obispado.RData"))
# print(O3Obispado)
# print(O3Obispado.pronostico)
# stop()
  O3Pastora.pronostico <- as.data.frame(pronostico(O3Pastora, modelos.var.met = "modelos/modelos_de_pronostico/modelos_pronostico_met_O3_pastora.RData"))
cat("paso Pastora\n")  
  O3Nicolas.pronostico <- as.data.frame(pronostico(O3Nicolas, modelos.var.met = "modelos/modelos_de_pronostico/modelos_pronostico_met_O3_nicolas.RData"))
cat("paso Nicolas\n")  
  O3Bernabe.pronostico <- as.data.frame(pronostico(O3Bernabe, modelos.var.met = "modelos/modelos_de_pronostico/modelos_pronostico_met_O3_bernabe.RData"))
cat("paso Bernabe\n")  
  O3Catarina.pronostico <- as.data.frame(pronostico(O3Catarina, modelos.var.met = "modelos/modelos_de_pronostico/modelos_pronostico_met_O3_catarina.RData"))
cat("paso Catarina\n")
# print(O3Obispado)
# print(O3Obispado.pronostico)
# stop()
  PM10Obispado.pronostico <- as.data.frame(pronostico(PM10Obispado, modelos.var.met = "modelos/modelos_de_pronostico/modelos_pronostico_met_PM10_obispado.RData"))
cat("paso Obispado\n")

  PM10Pastora.pronostico <- as.data.frame(pronostico(PM10Pastora, modelos.var.met = "modelos/modelos_de_pronostico/modelos_pronostico_met_PM10_pastora.RData"))
cat("paso Pastora\n")  
  PM10Nicolas.pronostico <- as.data.frame(pronostico(PM10Nicolas, modelos.var.met = "modelos/modelos_de_pronostico/modelos_pronostico_met_PM10_nicolas.RData"))
cat("paso Nicolas\n")  
  PM10Bernabe.pronostico <- as.data.frame(pronostico(PM10Bernabe, modelos.var.met = "modelos/modelos_de_pronostico/modelos_pronostico_met_PM10_bernabe.RData"))
cat("paso Bernabe\n")  
  PM10Catarina.pronostico <- as.data.frame(pronostico(PM10Catarina, modelos.var.met = "modelos/modelos_de_pronostico/modelos_pronostico_met_PM10_catarina.RData"))
cat("paso Catarina\n")


##salida
  O3Obispado.pronostico.salida <- tablaSalida(O3Obispado, O3Obispado.pronostico[,1], norma = c(0, 70, 95, 154, 204, Inf))
  O3Pastora.pronostico.salida <- tablaSalida(O3Pastora, O3Pastora.pronostico[,1], norma = c(0, 70, 95, 154, 204, Inf))
  O3Nicolas.pronostico.salida <- tablaSalida(O3Nicolas, O3Nicolas.pronostico[,1], norma = c(0, 70, 95, 154, 204, Inf))
  O3Bernabe.pronostico.salida <- tablaSalida(O3Bernabe, O3Bernabe.pronostico[,1], norma = c(0, 70, 95, 154, 204, Inf))
  O3Catarina.pronostico.salida <- tablaSalida(O3Catarina, O3Catarina.pronostico[,1], norma = c(0, 70, 95, 154, 204, Inf))
  tabla.salida.O3 <- cbind(data.frame(contaminante = "O3"), rbind(O3Obispado.pronostico.salida, O3Pastora.pronostico.salida, O3Nicolas.pronostico.salida, O3Bernabe.pronostico.salida, O3Catarina.pronostico.salida))
  PM10Obispado.pronostico.salida <- tablaSalida(PM10Obispado, PM10Obispado.pronostico[,1], norma = c(0, 40, 75, 214, 354, Inf))
  PM10Pastora.pronostico.salida <- tablaSalida(PM10Pastora, PM10Pastora.pronostico[,1], norma = c(0, 40, 75, 214, 354, Inf))
  PM10Nicolas.pronostico.salida <- tablaSalida(PM10Nicolas, PM10Nicolas.pronostico[,1], norma = c(0, 40, 75, 214, 354, Inf))
  PM10Bernabe.pronostico.salida <- tablaSalida(PM10Bernabe, PM10Bernabe.pronostico[,1], norma = c(0, 40, 75, 214, 354, Inf))
  PM10Catarina.pronostico.salida <- tablaSalida(PM10Catarina, PM10Catarina.pronostico[,1], norma = c(0, 40, 75, 214, 354, Inf))
  tabla.salida.PM10 <- cbind(data.frame(contaminante = "PM10"), rbind(PM10Obispado.pronostico.salida, PM10Pastora.pronostico.salida, PM10Nicolas.pronostico.salida, PM10Bernabe.pronostico.salida, PM10Catarina.pronostico.salida))

  tabla.salida.PM2.5 <- tabla.salida.O3
  tabla.salida.PM2.5$contaminante <- "PM2.5"
  tabla.salida.PM2.5$pronostico <- NA
  
  
  tabla.salida <- rbind(tabla.salida.O3, tabla.salida.PM10, tabla.salida.PM2.5)
  if(missing(salida)) return(tabla.salida)
   else write.csv(tabla.salida, salida, row.names = F)
}

