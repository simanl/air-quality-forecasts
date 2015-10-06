rm(list=ls())
# tablaMaestra("~/pronostico de calidad del aire/datos/tabla_maestra_final_reducida.csv", fecha.final = "2014-05-31",
#   image = "~/pronostico de calidad del aire/SPCA/tablas/series/tabla_maestra.RData")
# tablaImputada("~/pronostico de calidad del aire/datos/tabla_maestra_final_imputaciones_v3.csv",
#   image = "~/SPCA/tablas/series/tabla_maestra_imputada.RData")
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
matriz.X <- as.matrix(cbind(matriz.X.EstPer, matriz.X.met))
residuales <- lm(log(O3Obispado$tabla.periodos.sitio$O3+1) ~ as.matrix(matriz.X) -1)$residuals

# obispado.arima.O3.EstPer.TRHWsWd <- Arima(log(O3Obispado$tabla.periodos.sitio$O3+1), xreg = matriz.X, order = c(2,0,1), seasonal = list(order = c(2,0,1), period = 4), include.mean = F)
obispado.arima.O3.EstPer.TRHWsWd <- Arima(log(O3Obispado$tabla.periodos.sitio$O3+1), xreg = matriz.X, order = c(2,0,1), seasonal = list(order = c(2,0,2), period = 4), include.mean = F) ##este
# obispado.arima.O3.EstPer.TRHWsWd <- Arima(log(O3Obispado$tabla.periodos.sitio$O3+1), xreg = matriz.X, order = c(1,0,1), seasonal = list(order = c(2,0,2), period = 4), include.mean = F)

arima.sim <- simulate.Arima(obispado.arima.O3.EstPer.TRHWsWd, nsim = 50000)
tsdiag(obispado.arima.O3.EstPer.TRHWsWd)
par(mfcol = c(2, 2))
acf(arima.sim, main = obispado.arima.O3.EstPer.TRHWsWd$call, lag.max = 36)
pacf(arima.sim, lag.max = 36)
acf(residuales, lag.max = 36)
pacf(residuales, lag.max = 36)

class(obispado.arima.O3.EstPer.TRHWsWd) <- c(class(obispado.arima.O3.EstPer.TRHWsWd),"ArimaSIMA.EstPer.TRHWsWd")
obispado.arima.O3.EstPer.TRHWsWd$sitio <- "Obispado"
obispado.arima.O3.EstPer.TRHWsWd$contaminante <- "O3"
obispado.arima.O3.EstPer.TRHWsWd$descripcion <- "Modelo para O3, Obispado con regresores: estacion, periodo del dia, TOUT, RS, HR, WS, WDR"
obispado.arima.O3.EstPer.TRHWsWd$trans <- function(x) log(x + 1)
obispado.arima.O3.EstPer.TRHWsWd$trans.inv <- function(x) exp(x) - 1

save(obispado.arima.O3.EstPer.TRHWsWd, file = "~/SPCA/modelos/modelos_de_pronostico/O3Obispado_mod_pron.RData")
  
O3Obispado <- stSIMA(tabla.maestra.imputada, contaminante = "O3", sitio = "Obispado", pesos = "~/pronostico de calidad del aire/datos/pesos_contaminantes_v2.csv",
  modelos = "~/SPCA/modelos/modelos_de_pronostico/O3Obispado_mod_pron.RData")
save(O3Obispado, file = "~/SPCA/tablas/series/O3Obispado.RData")


#generamos la matriz.X
matriz.X.EstPer <- model.matrix( ~ O3Pastora$tabla.periodos.sitio$estacion + O3Pastora$tabla.periodos.sitio$periodo)
matriz.X.met <- data.frame(TOUT = O3Pastora$tabla.periodos.sitio$TOUT, HR = O3Pastora$tabla.periodos.sitio$HR,
                           SR = O3Pastora$tabla.periodos.sitio$SR, WS = O3Pastora$tabla.periodos.sitio$WS,
                           WDR = O3Pastora$tabla.periodos.sitio$WDR)

matriz.X <- as.matrix(cbind(matriz.X.EstPer, matriz.X.met))
head(matriz.X)
residuales <- lm(log(O3Pastora$tabla.periodos.sitio$O3+1) ~ as.matrix(matriz.X) -1)$residuals
pastora.arima.O3.EstPer.TRHWsWd <- Arima(log(O3Pastora$tabla.periodos.sitio$O3+1), xreg = matriz.X, order = c(2,0,1), seasonal = list(order = c(1,0,2), period = 4), include.mean = F)##este
# pastora.arima.O3.EstPer.TRHWsWd <- Arima(log(O3Pastora$tabla.periodos.sitio$O3+1), xreg = matriz.X, order = c(1,0,1), seasonal = list(order = c(3,0,2), period = 2), include.mean = F)

tsdiag(pastora.arima.O3.EstPer.TRHWsWd)
arima.sim <- simulate.Arima(pastora.arima.O3.EstPer.TRHWsWd, nsim = 50000)
par(mfcol = c(2, 2))
acf(arima.sim, main = pastora.arima.O3.EstPer.TRHWsWd$call)
pacf(arima.sim)
acf(residuales)
pacf(residuales)

class(pastora.arima.O3.EstPer.TRHWsWd) <- c(class(pastora.arima.O3.EstPer.TRHWsWd),"ArimaSIMA.EstPer.TRHWsWd")
pastora.arima.O3.EstPer.TRHWsWd$sitio <- "La Pastora"
pastora.arima.O3.EstPer.TRHWsWd$contaminante <- "O3"
pastora.arima.O3.EstPer.TRHWsWd$descripcion <- "Modelo para O3, Pastora con regresores: estacion, periodo del dia, TOUT, RS, HR, WS, WDR"
pastora.arima.O3.EstPer.TRHWsWd$trans <- function(x) log(x + 1)
pastora.arima.O3.EstPer.TRHWsWd$trans.inv <- function(x) exp(x) - 1

save(pastora.arima.O3.EstPer.TRHWsWd, file = "~/SPCA/modelos/modelos_de_pronostico/O3Pastora_mod_pron.RData")
  
O3Pastora <- stSIMA(tabla.maestra.imputada, contaminante = "O3", sitio = "La Pastora", pesos = "~/pronostico de calidad del aire/datos/pesos_contaminantes_v2.csv",
  modelos = "~/SPCA/modelos/modelos_de_pronostico/O3Pastora_mod_pron.RData")
save(O3Pastora, file = "~/SPCA/tablas/series/O3Pastora.RData")



#generamos la matriz.X
matriz.X.EstPer <- model.matrix( ~ O3Nicolas$tabla.periodos.sitio$estacion + O3Nicolas$tabla.periodos.sitio$periodo)
matriz.X.met <- data.frame(TOUT = O3Nicolas$tabla.periodos.sitio$TOUT, HR = O3Nicolas$tabla.periodos.sitio$HR,
                           SR = O3Nicolas$tabla.periodos.sitio$SR, WS = O3Nicolas$tabla.periodos.sitio$WS,
                           WDR = O3Nicolas$tabla.periodos.sitio$WDR)

matriz.X <- as.matrix(cbind(matriz.X.EstPer, matriz.X.met))
residuales <- lm(log(O3Nicolas$tabla.periodos.sitio$O3 +1)~ as.matrix(matriz.X) -1)$residuals

nicolas.arima.O3.EstPer.TRHWsWd <- Arima(log(O3Nicolas$tabla.periodos.sitio$O3+1), xreg = matriz.X, order = c(2,0,1), seasonal = list(order = c(2,0,1), period = 4), include.mean = F)
tsdiag(nicolas.arima.O3.EstPer.TRHWsWd)
arima.sim <- simulate.Arima(nicolas.arima.O3.EstPer.TRHWsWd, nsim = 50000)
par(mfcol = c(2, 2))
acf(arima.sim, main = nicolas.arima.O3.EstPer.TRHWsWd$call)
pacf(arima.sim)
acf(residuales)
pacf(residuales)

class(nicolas.arima.O3.EstPer.TRHWsWd) <- c(class(nicolas.arima.O3.EstPer.TRHWsWd),"ArimaSIMA.EstPer.TRHWsWd")
nicolas.arima.O3.EstPer.TRHWsWd$sitio <- "San Nicolas"
nicolas.arima.O3.EstPer.TRHWsWd$contaminante <- "O3"
nicolas.arima.O3.EstPer.TRHWsWd$descripcion <- "Modelo para O3, Nicolas con regresores: estacion, periodo del dia, TOUT, RS, HR, WS, WDR"
nicolas.arima.O3.EstPer.TRHWsWd$trans <- function(x) log(x + 1)
nicolas.arima.O3.EstPer.TRHWsWd$trans.inv <- function(x) exp(x) - 1

save(nicolas.arima.O3.EstPer.TRHWsWd, file = "~/SPCA/modelos/modelos_de_pronostico/O3Nicolas_mod_pron.RData")
  
O3Nicolas <- stSIMA(tabla.maestra.imputada, contaminante = "O3", sitio = "San Nicolas", pesos = "~/pronostico de calidad del aire/datos/pesos_contaminantes_v2.csv",
  modelos = "~/SPCA/modelos/modelos_de_pronostico/O3Nicolas_mod_pron.RData")
save(O3Nicolas, file = "~/SPCA/tablas/series/O3Nicolas.RData")

#generamos la matriz.X
matriz.X.EstPer <- model.matrix( ~ O3Bernabe$tabla.periodos.sitio$estacion + O3Bernabe$tabla.periodos.sitio$periodo)
matriz.X.met <- data.frame(TOUT = O3Bernabe$tabla.periodos.sitio$TOUT, HR = O3Bernabe$tabla.periodos.sitio$HR,
                           SR = O3Bernabe$tabla.periodos.sitio$SR, WS = O3Bernabe$tabla.periodos.sitio$WS,
                           WDR = O3Bernabe$tabla.periodos.sitio$WDR)

matriz.X <- cbind(matriz.X.EstPer, matriz.X.met)
matriz.X <- as.matrix(cbind(matriz.X.EstPer, matriz.X.met))
residuales <- lm(log(O3Bernabe$tabla.periodos.sitio$O3+1) ~ as.matrix(matriz.X) -1)$residuals

bernabe.arima.O3.EstPer.TRHWsWd <- Arima(log(O3Bernabe$tabla.periodos.sitio$O3+1), xreg = matriz.X, order = c(2,0,1), seasonal = list(order = c(2,0,1), period = 4), include.mean = F)
tsdiag(bernabe.arima.O3.EstPer.TRHWsWd)
arima.sim <- simulate.Arima(bernabe.arima.O3.EstPer.TRHWsWd, nsim = 50000)
par(mfcol = c(2, 2))
acf(arima.sim, main = bernabe.arima.O3.EstPer.TRHWsWd$call)
pacf(arima.sim)
acf(residuales)
pacf(residuales)

class(bernabe.arima.O3.EstPer.TRHWsWd) <- c(class(bernabe.arima.O3.EstPer.TRHWsWd),"ArimaSIMA.EstPer.TRHWsWd")
bernabe.arima.O3.EstPer.TRHWsWd$sitio <- "San Bernabe"
bernabe.arima.O3.EstPer.TRHWsWd$contaminante <- "O3"
bernabe.arima.O3.EstPer.TRHWsWd$descripcion <- "Modelo para O3, Bernabe con regresores: estacion, periodo del dia, TOUT, RS, HR, WS, WDR"
bernabe.arima.O3.EstPer.TRHWsWd$trans <- function(x) log(x + 1)
bernabe.arima.O3.EstPer.TRHWsWd$trans.inv <- function(x) exp(x) - 1

save(bernabe.arima.O3.EstPer.TRHWsWd, file = "~/SPCA/modelos/modelos_de_pronostico/O3Bernabe_mod_pron.RData")
  
O3Bernabe <- stSIMA(tabla.maestra.imputada, contaminante = "O3", sitio = "San Bernabe", pesos = "~/pronostico de calidad del aire/datos/pesos_contaminantes_v2.csv",
  modelos = "~/SPCA/modelos/modelos_de_pronostico/O3Bernabe_mod_pron.RData")
save(O3Bernabe, file = "~/SPCA/tablas/series/O3Bernabe.RData")

#generamos la matriz.X
matriz.X.EstPer <- model.matrix( ~ O3Catarina$tabla.periodos.sitio$estacion + O3Catarina$tabla.periodos.sitio$periodo)
matriz.X.met <- data.frame(TOUT = O3Catarina$tabla.periodos.sitio$TOUT, HR = O3Catarina$tabla.periodos.sitio$HR,
                           SR = O3Catarina$tabla.periodos.sitio$SR, WS = O3Catarina$tabla.periodos.sitio$WS,
                           WDR = O3Catarina$tabla.periodos.sitio$WDR)

matriz.X <- cbind(matriz.X.EstPer, matriz.X.met)
matriz.X <- as.matrix(cbind(matriz.X.EstPer, matriz.X.met))
residuales <- lm(log(O3Catarina$tabla.periodos.sitio$O3+1) ~ as.matrix(matriz.X) -1)$residuals

catarina.arima.O3.EstPer.TRHWsWd <- Arima(log(O3Catarina$tabla.periodos.sitio$O3+1), xreg = matriz.X, order = c(2,0,1), seasonal = list(order = c(1,0,1), period = 4), include.mean = F)
tsdiag(catarina.arima.O3.EstPer.TRHWsWd)
arima.sim <- simulate.Arima(catarina.arima.O3.EstPer.TRHWsWd, nsim = 50000)
par(mfcol = c(2, 2))
acf(arima.sim, main = catarina.arima.O3.EstPer.TRHWsWd$call)
pacf(arima.sim)
acf(residuales)
pacf(residuales)

class(catarina.arima.O3.EstPer.TRHWsWd) <- c(class(catarina.arima.O3.EstPer.TRHWsWd),"ArimaSIMA.EstPer.TRHWsWd")
catarina.arima.O3.EstPer.TRHWsWd$sitio <- "Santa Catarina"
catarina.arima.O3.EstPer.TRHWsWd$contaminante <- "O3"
catarina.arima.O3.EstPer.TRHWsWd$descripcion <- "Modelo para O3, Catarina con regresores: estacion, periodo del dia, TOUT, RS, HR, WS, WDR"
catarina.arima.O3.EstPer.TRHWsWd$trans <- function(x) log(x + 1)
catarina.arima.O3.EstPer.TRHWsWd$trans.inv <- function(x) exp(x) - 1

save(catarina.arima.O3.EstPer.TRHWsWd, file = "~/SPCA/modelos/modelos_de_pronostico/O3Catarina_mod_pron.RData")
  
O3Catarina <- stSIMA(tabla.maestra.imputada, contaminante = "O3", sitio = "Santa Catarina", pesos = "~/pronostico de calidad del aire/datos/pesos_contaminantes_v2.csv",
  modelos = "~/SPCA/modelos/modelos_de_pronostico/O3Catarina_mod_pron.RData")
save(O3Catarina, file = "~/SPCA/tablas/series/O3Catarina.RData")


######PM10
PM10Obispado <- stSIMA(tabla.maestra.imputada, contaminante = "PM10", sitio = "Obispado", pesos = "~/pronostico de calidad del aire/datos/pesos_contaminantes_v2.csv",
  modelos = list())
PM10Pastora <- stSIMA(tabla.maestra.imputada, contaminante = "PM10", sitio = "La Pastora", pesos = "~/pronostico de calidad del aire/datos/pesos_contaminantes_v2.csv",
  modelos = list())
PM10Nicolas <- stSIMA(tabla.maestra.imputada, contaminante = "PM10", sitio = "San Nicolas", pesos = "~/pronostico de calidad del aire/datos/pesos_contaminantes_v2.csv",
  modelos = list())
PM10Catarina <- stSIMA(tabla.maestra.imputada, contaminante = "PM10", sitio = "Santa Catarina", pesos = "~/pronostico de calidad del aire/datos/pesos_contaminantes_v2.csv",
  modelos = list())
PM10Bernabe <- stSIMA(tabla.maestra.imputada, contaminante = "PM10", sitio = "San Bernabe", pesos = "~/pronostico de calidad del aire/datos/pesos_contaminantes_v2.csv",
  modelos = list())

#generamos la matriz.X
PM10Obispado$tabla.periodos.sitio$fecha <- as.Date(PM10Obispado$tabla.periodos.sitio$fecha)
PM10Obispado.tabla <- PM10Obispado$tabla.periodos.sitio[PM10Obispado$tabla.periodos.sitio$fecha >= "2013-01-01",]
matriz.X.EstPer <- model.matrix( ~ PM10Obispado.tabla$estacion + PM10Obispado.tabla$periodo)
matriz.X.met <- data.frame(TOUT = PM10Obispado.tabla$TOUT, HR = PM10Obispado.tabla$HR,
                           SR = PM10Obispado.tabla$SR, WS = PM10Obispado.tabla$WS,
                           WDR = PM10Obispado.tabla$WDR)

matriz.X <- cbind(matriz.X.EstPer, matriz.X.met)
matriz.X <- as.matrix(cbind(matriz.X.EstPer, matriz.X.met))
residuales <- lm(log(PM10Obispado.tabla$PM10+1) ~ as.matrix(matriz.X) -1)$residuals
par(mfcol = c(2, 1))
acf(residuales)
pacf(residuales)
ts.plot(PM10Obispado.tabla$PM10)
ts.plot(PM10Obispado.tabla$WDR)
ts.plot(PM10Obispado.tabla$TOUT)
ts.plot(PM10Obispado.tabla$HR)
ts.plot(PM10Obispado.tabla$SR)
ts.plot(PM10Obispado.tabla$WS)

obispado.arima.PM10.EstPer.TRHWsWd <- Arima(log(PM10Obispado.tabla$PM10 + 1), xreg = matriz.X, order = c(1,0,0), seasonal = list(order = c(2,0,1), period = 4), include.mean = F)
tsdiag(obispado.arima.PM10.EstPer.TRHWsWd)
par(mfcol = c(2, 2))
arima.sim <- simulate.Arima(obispado.arima.PM10.EstPer.TRHWsWd, nsim = 50000)
acf(arima.sim, main = obispado.arima.PM10.EstPer.TRHWsWd$call, lag.max = 40)
pacf(arima.sim, lag.max = 40)
acf(residuales, lag.max = 40)
pacf(residuales, lag.max = 40)

class(obispado.arima.PM10.EstPer.TRHWsWd) <- c(class(obispado.arima.PM10.EstPer.TRHWsWd),"ArimaSIMA.EstPer.TRHWsWd")
obispado.arima.PM10.EstPer.TRHWsWd$sitio <- "Obispado"
obispado.arima.PM10.EstPer.TRHWsWd$contaminante <- "PM10"
obispado.arima.PM10.EstPer.TRHWsWd$descripcion <- "Modelo para PM10, Obispado con regresores: estacion, periodo del dia, TOUT, RS, HR, WS, WDR"
obispado.arima.PM10.EstPer.TRHWsWd$trans <- function(x) log(x + 1)
obispado.arima.PM10.EstPer.TRHWsWd$trans.inv <- function(x) exp(x) - 1

save(obispado.arima.PM10.EstPer.TRHWsWd, file = "~/SPCA/modelos/modelos_de_pronostico/PM10Obispado_mod_pron.RData")
  
PM10Obispado <- stSIMA(tabla.maestra.imputada, contaminante = "PM10", sitio = "Obispado", pesos = "~/pronostico de calidad del aire/datos/pesos_contaminantes_v2.csv",
  modelos = "~/SPCA/modelos/modelos_de_pronostico/PM10Obispado_mod_pron.RData")
save(PM10Obispado, file = "~/SPCA/tablas/series/PM10Obispado.RData")

##pastora
PM10Pastora$tabla.periodos.sitio$fecha <- as.Date(PM10Pastora$tabla.periodos.sitio$fecha)
ts.plot(PM10Pastora$tabla.periodos.sitio$PM10)

PM10Pastora.tabla <- PM10Pastora$tabla.periodos.sitio
matriz.X.EstPer <- model.matrix( ~ PM10Pastora.tabla$estacion + PM10Pastora.tabla$periodo)
matriz.X.met <- data.frame(TOUT = PM10Pastora.tabla$TOUT, HR = PM10Pastora.tabla$HR,
                           SR = PM10Pastora.tabla$SR, WS = PM10Pastora.tabla$WS,
                           WDR = PM10Pastora.tabla$WDR)

matriz.X <- cbind(matriz.X.EstPer, matriz.X.met)
matriz.X <- as.matrix(cbind(matriz.X.EstPer, matriz.X.met))
residuales <- lm(log(PM10Pastora.tabla$PM10+1) ~ as.matrix(matriz.X) -1)$residuals
par(mfcol = c(2, 1))
acf(residuales)
pacf(residuales)
ts.plot(PM10Pastora.tabla$PM10)
ts.plot(PM10Pastora.tabla$WDR)
ts.plot(PM10Pastora.tabla$TOUT)
ts.plot(PM10Pastora.tabla$HR)
ts.plot(PM10Pastora.tabla$SR)
ts.plot(PM10Pastora.tabla$WS)

pastora.arima.PM10.EstPer.TRHWsWd <- Arima(log(PM10Pastora.tabla$PM10 + 1), xreg = matriz.X, order = c(1,0,0), seasonal = list(order = c(3,0,1), period = 4), include.mean = F)
tsdiag(pastora.arima.PM10.EstPer.TRHWsWd)
par(mfcol = c(2, 2))
arima.sim <- simulate.Arima(pastora.arima.PM10.EstPer.TRHWsWd, nsim = 50000)
acf(arima.sim, main = pastora.arima.PM10.EstPer.TRHWsWd$call, lag.max = 40)
pacf(arima.sim, lag.max = 40)
acf(residuales, lag.max = 40)
pacf(residuales, lag.max = 40)

class(pastora.arima.PM10.EstPer.TRHWsWd) <- c(class(pastora.arima.PM10.EstPer.TRHWsWd),"ArimaSIMA.EstPer.TRHWsWd")
pastora.arima.PM10.EstPer.TRHWsWd$sitio <- "La Pastora"
pastora.arima.PM10.EstPer.TRHWsWd$contaminante <- "PM10"
pastora.arima.PM10.EstPer.TRHWsWd$descripcion <- "Modelo para PM10, Pastora con regresores: estacion, periodo del dia, TOUT, RS, HR, WS, WDR"
pastora.arima.PM10.EstPer.TRHWsWd$trans <- function(x) log(x + 1)
pastora.arima.PM10.EstPer.TRHWsWd$trans.inv <- function(x) exp(x) - 1

save(pastora.arima.PM10.EstPer.TRHWsWd, file = "~/SPCA/modelos/modelos_de_pronostico/PM10Pastora_mod_pron.RData")
  
PM10Pastora <- stSIMA(tabla.maestra.imputada, contaminante = "PM10", sitio = "La Pastora", pesos = "~/pronostico de calidad del aire/datos/pesos_contaminantes_v2.csv",
  modelos = "~/SPCA/modelos/modelos_de_pronostico/PM10Pastora_mod_pron.RData")
save(PM10Pastora, file = "~/SPCA/tablas/series/PM10Pastora.RData")

##catarina
PM10Catarina$tabla.periodos.sitio$fecha <- as.Date(PM10Catarina$tabla.periodos.sitio$fecha)
ts.plot(PM10Catarina$tabla.periodos.sitio$PM10[PM10Obispado$tabla.periodos.sitio$fecha >= "2013-01-01"])

PM10Catarina.tabla <- PM10Catarina$tabla.periodos.sitio[PM10Obispado$tabla.periodos.sitio$fecha >= "2013-01-01",]
matriz.X.EstPer <- model.matrix( ~ PM10Catarina.tabla$estacion + PM10Catarina.tabla$periodo)
matriz.X.met <- data.frame(TOUT = PM10Catarina.tabla$TOUT, HR = PM10Catarina.tabla$HR,
                           SR = PM10Catarina.tabla$SR, WS = PM10Catarina.tabla$WS,
                           WDR = PM10Catarina.tabla$WDR)

matriz.X <- cbind(matriz.X.EstPer, matriz.X.met)
matriz.X <- as.matrix(cbind(matriz.X.EstPer, matriz.X.met))
residuales <- lm(log(PM10Catarina.tabla$PM10+1) ~ as.matrix(matriz.X) -1)$residuals
par(mfcol = c(2, 1))
acf(residuales)
pacf(residuales)
ts.plot(PM10Catarina.tabla$PM10)
ts.plot(PM10Catarina.tabla$WDR)
ts.plot(PM10Catarina.tabla$TOUT)
ts.plot(PM10Catarina.tabla$HR)
ts.plot(PM10Catarina.tabla$SR)
ts.plot(PM10Catarina.tabla$WS)

catarina.arima.PM10.EstPer.TRHWsWd <- Arima(log(PM10Catarina.tabla$PM10 + 1), xreg = matriz.X, order = c(1,0,0), seasonal = list(order = c(3,0,1), period = 4), include.mean = F)
tsdiag(catarina.arima.PM10.EstPer.TRHWsWd)
par(mfcol = c(2, 2))
arima.sim <- simulate.Arima(catarina.arima.PM10.EstPer.TRHWsWd, nsim = 50000)
acf(arima.sim, main = catarina.arima.PM10.EstPer.TRHWsWd$call, lag.max = 40)
pacf(arima.sim, lag.max = 40)
acf(residuales, lag.max = 40)
pacf(residuales, lag.max = 40)

class(catarina.arima.PM10.EstPer.TRHWsWd) <- c(class(catarina.arima.PM10.EstPer.TRHWsWd),"ArimaSIMA.EstPer.TRHWsWd")
catarina.arima.PM10.EstPer.TRHWsWd$sitio <- "Santa Catarina"
catarina.arima.PM10.EstPer.TRHWsWd$contaminante <- "PM10"
catarina.arima.PM10.EstPer.TRHWsWd$descripcion <- "Modelo para PM10, Catarina con regresores: estacion, periodo del dia, TOUT, RS, HR, WS, WDR"
catarina.arima.PM10.EstPer.TRHWsWd$trans <- function(x) log(x + 1)
catarina.arima.PM10.EstPer.TRHWsWd$trans.inv <- function(x) exp(x) - 1

save(catarina.arima.PM10.EstPer.TRHWsWd, file = "~/SPCA/modelos/modelos_de_pronostico/PM10Catarina_mod_pron.RData")
  
PM10Catarina <- stSIMA(tabla.maestra.imputada, contaminante = "PM10", sitio = "Santa Catarina", pesos = "~/pronostico de calidad del aire/datos/pesos_contaminantes_v2.csv",
  modelos = "~/SPCA/modelos/modelos_de_pronostico/PM10Catarina_mod_pron.RData")
save(PM10Catarina, file = "~/SPCA/tablas/series/PM10Catarina.RData")


##bernabe
PM10Bernabe$tabla.periodos.sitio$fecha <- as.Date(PM10Bernabe$tabla.periodos.sitio$fecha)
ts.plot(PM10Bernabe$tabla.periodos.sitio$PM10[PM10Obispado$tabla.periodos.sitio$fecha >= "2013-01-01"])

PM10Bernabe.tabla <- PM10Bernabe$tabla.periodos.sitio[PM10Obispado$tabla.periodos.sitio$fecha >= "2013-01-01",]

matriz.X.EstPer <- model.matrix( ~ PM10Bernabe.tabla$estacion + PM10Bernabe.tabla$periodo)
matriz.X.met <- data.frame(TOUT = PM10Bernabe.tabla$TOUT, HR = PM10Bernabe.tabla$HR,
                           SR = PM10Bernabe.tabla$SR, WS = PM10Bernabe.tabla$WS,
                           WDR = PM10Bernabe.tabla$WDR)

matriz.X <- cbind(matriz.X.EstPer, matriz.X.met)
matriz.X <- as.matrix(cbind(matriz.X.EstPer, matriz.X.met))
residuales <- lm(log(PM10Bernabe.tabla$PM10+1) ~ as.matrix(matriz.X) -1)$residuals
par(mfcol = c(2, 1))
acf(residuales)
pacf(residuales)
ts.plot(PM10Bernabe.tabla$PM10)
ts.plot(PM10Bernabe.tabla$WDR)
ts.plot(PM10Bernabe.tabla$TOUT)
ts.plot(PM10Bernabe.tabla$HR)
ts.plot(PM10Bernabe.tabla$SR)
ts.plot(PM10Bernabe.tabla$WS)

bernabe.arima.PM10.EstPer.TRHWsWd <- Arima(log(PM10Bernabe.tabla$PM10 + 1), xreg = matriz.X, order = c(1,0,0), seasonal = list(order = c(1,0,1), period = 4), include.mean = F)
tsdiag(bernabe.arima.PM10.EstPer.TRHWsWd)
par(mfcol = c(2, 2))
arima.sim <- simulate.Arima(bernabe.arima.PM10.EstPer.TRHWsWd, nsim = 50000)
acf(arima.sim, main = bernabe.arima.PM10.EstPer.TRHWsWd$call, lag.max = 40)
pacf(arima.sim, lag.max = 40)
acf(residuales, lag.max = 40)
pacf(residuales, lag.max = 40)

class(bernabe.arima.PM10.EstPer.TRHWsWd) <- c(class(bernabe.arima.PM10.EstPer.TRHWsWd),"ArimaSIMA.EstPer.TRHWsWd")
bernabe.arima.PM10.EstPer.TRHWsWd$sitio <- "San Bernabe"
bernabe.arima.PM10.EstPer.TRHWsWd$contaminante <- "PM10"
bernabe.arima.PM10.EstPer.TRHWsWd$descripcion <- "Modelo para PM10, Bernabe con regresores: estacion, periodo del dia, TOUT, RS, HR, WS, WDR"
bernabe.arima.PM10.EstPer.TRHWsWd$trans <- function(x) log(x + 1)
bernabe.arima.PM10.EstPer.TRHWsWd$trans.inv <- function(x) exp(x) - 1

save(bernabe.arima.PM10.EstPer.TRHWsWd, file = "~/SPCA/modelos/modelos_de_pronostico/PM10Bernabe_mod_pron.RData")
  
PM10Bernabe <- stSIMA(tabla.maestra.imputada, contaminante = "PM10", sitio = "San Bernabe", pesos = "~/pronostico de calidad del aire/datos/pesos_contaminantes_v2.csv",
  modelos = "~/SPCA/modelos/modelos_de_pronostico/PM10Bernabe_mod_pron.RData")
save(PM10Bernabe, file = "~/SPCA/tablas/series/PM10Bernabe.RData")

##nicolas
PM10Nicolas$tabla.periodos.sitio$fecha <- as.Date(PM10Nicolas$tabla.periodos.sitio$fecha)
ts.plot(PM10Nicolas$tabla.periodos.sitio$PM10[PM10Obispado$tabla.periodos.sitio$fecha >= "2013-01-01"])

PM10Nicolas.tabla <- PM10Nicolas$tabla.periodos.sitio[PM10Obispado$tabla.periodos.sitio$fecha >= "2013-01-01",]
# PM10Nicolas.tabla <- PM10Nicolas$tabla.periodos.sitio

matriz.X.EstPer <- model.matrix( ~ PM10Nicolas.tabla$estacion + PM10Nicolas.tabla$periodo)
matriz.X.met <- data.frame(TOUT = PM10Nicolas.tabla$TOUT, HR = PM10Nicolas.tabla$HR,
                           SR = PM10Nicolas.tabla$SR, WS = PM10Nicolas.tabla$WS,
                           WDR = PM10Nicolas.tabla$WDR)

matriz.X <- cbind(matriz.X.EstPer, matriz.X.met)
matriz.X <- as.matrix(cbind(matriz.X.EstPer, matriz.X.met))
residuales <- lm(log(PM10Nicolas.tabla$PM10+1) ~ as.matrix(matriz.X) -1)$residuals
par(mfcol = c(2, 1))
acf(residuales)
pacf(residuales)
ts.plot(PM10Nicolas.tabla$PM10)
ts.plot(PM10Nicolas.tabla$WDR)
ts.plot(PM10Nicolas.tabla$TOUT)
ts.plot(PM10Nicolas.tabla$HR)
ts.plot(PM10Nicolas.tabla$SR)
ts.plot(PM10Nicolas.tabla$WS)

nicolas.arima.PM10.EstPer.TRHWsWd <- Arima(log(PM10Nicolas.tabla$PM10 + 1), xreg = matriz.X, order = c(2,0,0), seasonal = list(order = c(3,0,1), period = 4), include.mean = F)
tsdiag(nicolas.arima.PM10.EstPer.TRHWsWd)
par(mfcol = c(2, 2))
arima.sim <- simulate.Arima(nicolas.arima.PM10.EstPer.TRHWsWd, nsim = 50000)
acf(arima.sim, main = nicolas.arima.PM10.EstPer.TRHWsWd$call, lag.max = 40)
pacf(arima.sim, lag.max = 40)
acf(residuales, lag.max = 40)
pacf(residuales, lag.max = 40)

class(nicolas.arima.PM10.EstPer.TRHWsWd) <- c(class(nicolas.arima.PM10.EstPer.TRHWsWd),"ArimaSIMA.EstPer.TRHWsWd")
nicolas.arima.PM10.EstPer.TRHWsWd$sitio <- "San Nicolas"
nicolas.arima.PM10.EstPer.TRHWsWd$contaminante <- "PM10"
nicolas.arima.PM10.EstPer.TRHWsWd$descripcion <- "Modelo para PM10, Nicolas con regresores: estacion, periodo del dia, TOUT, RS, HR, WS, WDR"
nicolas.arima.PM10.EstPer.TRHWsWd$trans <- function(x) log(x + 1)
nicolas.arima.PM10.EstPer.TRHWsWd$trans.inv <- function(x) exp(x) - 1

save(nicolas.arima.PM10.EstPer.TRHWsWd, file = "~/SPCA/modelos/modelos_de_pronostico/PM10Nicolas_mod_pron.RData")
  
PM10Nicolas <- stSIMA(tabla.maestra.imputada, contaminante = "PM10", sitio = "San Nicolas", pesos = "~/pronostico de calidad del aire/datos/pesos_contaminantes_v2.csv",
  modelos = "~/SPCA/modelos/modelos_de_pronostico/PM10Nicolas_mod_pron.RData")
save(PM10Nicolas, file = "~/SPCA/tablas/series/PM10Nicolas.RData")
