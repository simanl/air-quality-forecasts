SPCA <- function(x, ...)
  UseMethod("SPCA")

SPCA.default <- function(SPCA_HOME = "~/SPCA", entrada, salida){
##SCPA_HOME
  setwd(SPCA_HOME)
  
##datos historicos horarios
#   tabla.maestra$fecha <- as.Date(tabla.maestra$fecha)
#   datos.historicos <- tabla.maestra[tabla.maestra$fecha < as.Date("2014-06-01"), ] ##desechar los datos posteriores a mayo 30 2014 en 'tabla_maestra_final_reducida.csv'
#   datos.historicos <- colsTemp(datos.historicos) ##hacerlo de antemano en 'tabla_maestra_final_reducida.csv'
  load("tablas/series/tabla_maestra.RData", .GlobalEnv) #tabla maestra final (horaria) con solo los sitios de interes
#print(max(tabla.maestra$fecha))  
  system(paste("cp ", SPCA_HOME, "/tablas/series/tabla_maestra.RData ", SPCA_HOME, "/tablas/series/tabla_maestra.RData.bkp", sep = ""), intern = T)
  datos.historicos.wdf <- tabla.maestra[(nrow(tabla.maestra) - 200):nrow(tabla.maestra), ] #se seleccionan los 200 ultimos para hacer la imputacion

##datos historicos imputados
#   tabla.maestra.imputada <- read.csv("tablas/tabla_maestra_final_imputaciones_v3.csv") ##cambiar nombre a tabla_maestra_final_reducida_imputada.csv
#   tabla.maestra.imputada$fecha <- as.Date(tabla.maestra.imputada$fecha)
#   tabla.maestra.imputada <- colsTemp(tabla.maestra.imputada) ##hacerlo de antemano en 'tabla_maestra_final_reducida.csv'
  load("tablas/series/tabla_maestra_imputada.RData", .GlobalEnv) #tabla maestra final (horaria) imputada con solo los sitios de interes
#print(max(tabla.maestra.imputada$fecha))  
  system(paste("cp ", SPCA_HOME, "/tablas/series/tabla_maestra_imputada.RData ", SPCA_HOME, "/tablas/series/tabla_maestra_imputada.RData.bkp", sep = ""), intern = T)
# print(ncol(tabla.maestra))
# print(ncol(tabla.maestra.imputada))
# cat("corrio\n")
# 
  load("tablas/series/O3Obispado.RData", .GlobalEnv)
#print(getwd())
##lectura de tabla de entrada
  if(missing(entrada))
    tabla.entrada <- lecturaCSV("tablas/series/entrada.csv")
    else
      tabla.entrada <- lecturaCSV(substitute(entrada))

##validacion
  tabla.entrada <- validacion(tabla.entrada)

##columnas adicionales (variables de calendario y estaciones)
  tabla.entrada.cols.adicionales <- colsTemp(tabla.entrada)
  
##imputacion
  imputaciones <- imputacion(tabla.entrada.cols.adicionales, datos.historicos.wdf, modelos.dir = "~/SPCA/modelos/modelos_de_imputacion")

##actualizacion de las series: periodos y modelos
  O3Obispado <- update(O3Obispado, tail(imputaciones, 30))

##pronostico de las series
  O3Obispado.pronostico <- as.data.frame(pronostico(O3Obispado, modelos.var.met = "~/SPCA/modelos/modelos_de_pronostico/modelos_pronostico_met_O3_obispado.RData"))
 
##salida
  O3Obispado.pronostico.salida <- tablaSalida(O3Obispado, O3Obispado.pronostico[,1], norma = c(0, 71, 96, 155, 205, 1000))
  write.csv(O3Obispado.pronostico.salida, salida, row.names = F)
  write.csv(O3Obispado.pronostico.salida, paste(attr(O3Obispado.pronostico.salida, "nombre"), "csv", sep = "."), row.names = F)  
}


# SPCA(entrada = "~/SPCA/tablas/series/entrada.csv")

