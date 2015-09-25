tablaMaestra("~/pronostico de calidad del aire/datos/tabla_maestra_final_reducida.csv", fecha.final = "2014-05-31",
  image = "~/pronostico de calidad del aire/SPCA/tablas/series/tabla_maestra.RData")
tablaImputada("~/pronostico de calidad del aire/datos/tabla_maestra_final_imputaciones_v3.csv",
  image = "~/SPCA/tablas/series/tabla_maestra_imputada.RData")
load("~/SPCA/tablas/series/tabla_maestra_imputada.RData")
O3Obispado <- stSIMA(tabla.maestra.imputada, contaminante = "O3", sitio = "Obispado", pesos = "~/pronostico de calidad del aire/datos/pesos_contaminantes_v2.csv",
  modelos = list())
O3Pastora <- stSIMA(tabla.maestra.imputada, contaminante = "O3", sitio = "La Pastora", pesos = "~/pronostico de calidad del aire/datos/pesos_contaminantes_v2.csv",
  modelos = list())
O3Nicolas <- stSIMA(tabla.maestra.imputada, contaminante = "O3", sitio = "San Nicolas", pesos = "~/pronostico de calidad del aire/datos/pesos_contaminantes_v2.csv",
  modelos = list())
O3Catarina <- stSIMA(tabla.maestra.imputada, contaminante = "O3", sitio = "Santa Catarina", pesos = "~/pronostico de calidad del aire/datos/pesos_contaminantes_v2.csv",
  modelos = list())
O3Bernabe <- stSIMA(tabla.maestra.imputada, contaminante = "O3", sitio = "San Bernabe", pesos = "~/pronostico de calidad del aire/datos/pesos_contaminantes_v2.csv",
  modelos = list())

  
library(forecast)

#generamos la matriz.X
matriz.X.EstPer <- model.matrix( ~ O3Obispado$tabla.periodos.sitio$estacion + O3Obispado$tabla.periodos.sitio$periodo)
matriz.X.met <- data.frame(TOUT = O3Obispado$tabla.periodos.sitio$TOUT, HR = O3Obispado$tabla.periodos.sitio$HR,
                           SR = O3Obispado$tabla.periodos.sitio$SR, WS = O3Obispado$tabla.periodos.sitio$WS,
                           WDR = O3Obispado$tabla.periodos.sitio$WDR)

matriz.X <- cbind(matriz.X.EstPer, matriz.X.met)

obispado.arima.O3.EstPer.TRHWsWd <- Arima(O3Obispado$tabla.periodos.sitio$O3, xreg = matriz.X, order = c(2,0,2), seasonal = list(order = c(1,0,0), period = 4), include.mean = F)
class(obispado.arima.O3.EstPer.TRHWsWd) <- c(class(obispado.arima.O3.EstPer.TRHWsWd),"ArimaSIMA.EstPer.TRHWsWd")
obispado.arima.O3.EstPer.TRHWsWd$sitio <- "Obispado"
obispado.arima.O3.EstPer.TRHWsWd$contaminante <- "O3"
obispado.arima.O3.EstPer.TRHWsWd$descripcion <- "Modelo para O3, Obispado con regresores: estacion, periodo del dia, TOUT, RS, HR, WS, WDR"

save(obispado.arima.O3.EstPer.TRHWsWd, file = "~/SPCA/modelos/modelos_de_pronostico/O3Obispado_mod_pron.RData")
  
O3Obispado <- stSIMA(tabla.maestra.imputada, contaminante = "O3", sitio = "Obispado", pesos = "~/pronostico de calidad del aire/datos/pesos_contaminantes_v2.csv",
  modelos = "~/SPCA/modelos/modelos_de_pronostico/O3Obispado_mod_pron.RData")
save(O3Obispado, file = "~/SPCA/tablas/series/O3Obispado.RData")


#generamos la matriz.X
matriz.X.EstPer <- model.matrix( ~ O3Obispado$tabla.periodos.sitio$estacion + O3Obispado$tabla.periodos.sitio$periodo)
matriz.X.met <- data.frame(TOUT = O3Obispado$tabla.periodos.sitio$TOUT, HR = O3Obispado$tabla.periodos.sitio$HR,
                           SR = O3Obispado$tabla.periodos.sitio$SR, WS = O3Obispado$tabla.periodos.sitio$WS,
                           WDR = O3Obispado$tabla.periodos.sitio$WDR)

matriz.X <- cbind(matriz.X.EstPer, matriz.X.met)

obispado.arima.O3.EstPer.TRHWsWd <- Arima(O3Obispado$tabla.periodos.sitio$O3, xreg = matriz.X, order = c(2,0,2), seasonal = list(order = c(1,0,0), period = 4), include.mean = F)
class(obispado.arima.O3.EstPer.TRHWsWd) <- c(class(obispado.arima.O3.EstPer.TRHWsWd),"ArimaSIMA.EstPer.TRHWsWd")
obispado.arima.O3.EstPer.TRHWsWd$sitio <- "Obispado"
obispado.arima.O3.EstPer.TRHWsWd$contaminante <- "O3"
obispado.arima.O3.EstPer.TRHWsWd$descripcion <- "Modelo para O3, Obispado con regresores: estacion, periodo del dia, TOUT, RS, HR, WS, WDR"

save(obispado.arima.O3.EstPer.TRHWsWd, file = "~/SPCA/modelos/modelos_de_pronostico/O3Obispado_mod_pron.RData")
  
O3Obispado <- stSIMA(tabla.maestra.imputada, contaminante = "O3", sitio = "Obispado", pesos = "~/pronostico de calidad del aire/datos/pesos_contaminantes_v2.csv",
  modelos = "~/SPCA/modelos/modelos_de_pronostico/O3Obispado_mod_pron.RData")
save(O3Obispado, file = "~/SPCA/tablas/series/O3Obispado.RData")



