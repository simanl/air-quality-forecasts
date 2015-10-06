### función que genera el pronóstico de la matriz X ###

pronostico <- function(x, ...)
  UseMethod("pronostico")

Xreg <- function(x, ...)
  UseMethod("Xreg")

#x es el objeto que sale de stSIMA
##x es el objeto ArimaSIMA.dummyTemp
##periodos.nuevo 
Xreg.ArimaSIMA.dummyTemp <- function(modelo, ultima.fecha, ultimo.periodo){    
  ## función que genera el pronóstico de la matriz X ###
  pronostico.xreg <- function(ultima.fecha, ultimo.periodo){
    #x es el periodo '00 a 05'... n es cuantos hacia adelante
    pronostico.periodo <- function(x, n = 4){
      vector.cuarto.dia <- rep(c("00 a 05", "06 a 11", "12 a 17", "18 a 23"), 2*n)
      aux <- match(x, vector.cuarto.dia)
      pronostico.cuarto.dia <- vector.cuarto.dia[(aux+1):(aux+n)]
      return(pronostico.cuarto.dia)
    }
    
    #regresa las fechas posteriores
    pronostico.fecha <- function(fecha.actual, periodo.pron){
      fecha.pron <- vector()
      if(periodo.pron[1] == "00 a 05"){
        fecha.pron <- rep(fecha.actual + 1, 4)
      } else if(periodo.pron[2] == "00 a 05") {
        fecha.pron[1] <- fecha.actual
        fecha.pron[2:4] <- fecha.actual + 1
      } else if(periodo.pron[3] == "00 a 05") {
        fecha.pron[1:2] <- fecha.actual
        fecha.pron[3:4] <- fecha.actual + 1
      } else if(periodo.pron[4] == "00 a 05") {
        fecha.pron[1:3] <- fecha.actual
        fecha.pron[4] <- fecha.actual + 1
      }
      return(as.Date(fecha.pron))
    }
    
    #regresa la estacion a partir de una fecha
    getSeason <- function(DATES) {
      WS <- as.Date("2012-12-21", format = "%Y-%m-%d") # Invierno
      SE <- as.Date("2012-3-21",  format = "%Y-%m-%d") # Primavera
      SS <- as.Date("2012-6-21",  format = "%Y-%m-%d") # Verano
      FE <- as.Date("2012-9-21",  format = "%Y-%m-%d") # Otono
      
      # Convierte las fechas a la fecha correspondiente del 2012 (porque es bisiesto)
      d <- as.Date(strftime(DATES, format="2012-%m-%d"))
      
      ifelse (d >= WS | d < SE, "Invierno",
              ifelse (d >= SE & d < SS, "Primavera",
                      ifelse (d >= SS & d < FE, "Verano", "Otono")))
    }
    
    pron.periodo <- pronostico.periodo(ultimo.periodo)
    pron.fecha <- as.Date(pronostico.fecha(ultima.fecha, pron.periodo))
    pron.estacion <- getSeason(pron.fecha)
    pron.estacion <- factor(pron.estacion, levels = c("Invierno", "Otono", "Primavera", "Verano"))
    
    matriz.X.pronostico <- model.matrix(~ pron.estacion + pron.periodo)
    return(matriz.X.pronostico)
  } 
  
  xreg.pron <- pronostico.xreg(ultima.fecha = ultima.fecha, ultimo.periodo = ultimo.periodo)
  return(xreg.pron)
}


pronostico.stSIMA <- function(x){
  modelos <- x$modelos.pronostico
  tabla.periodos <- x$tabla.periodos.sitio
  lista.pronosticos <- lapply(modelos, pronostico, tabla.periodos)
  #   lista.pronosticos <- pronostico(modelo = modelos, tabla.periodos = tabla.periodos)
  return(lista.pronosticos)
}


pronostico.ArimaSIMA.dummyTemp <- function(modelo, tabla.periodos){
  require(forecast)
  ultimo.renglon <- nrow(tabla.periodos)
  aux <- tabla.periodos[ultimo.renglon, c("fecha", "periodo")]
  ult.fecha <- as.Date(aux[1, "fecha"])
  ult.periodo <- as.factor(aux[1, "periodo"])
  matrix.X <- Xreg(modelo, ultima.fecha = ult.fecha, ultimo.periodo = ult.periodo)
  pronostico <- forecast(modelo, h = 4, xreg = matrix.X)
  class(pronostico) <- "pronosticoSIMA"
  return(pronostico)
}

Xreg <- function(x, ...)
  UseMethod("Xreg")

Xreg.ArimaSIMA.EstPer.TRHWsWd <- function(modelo, tabla.periodos, modelos.var.met){
  ult.renglon <- tabla.periodos[nrow(tabla.periodos),]
  
  pronostico.xreg.temp <- function(ultima.fecha, ultimo.periodo){
        #x es el periodo '00 a 05'... n es cuantos hacia adelante
        pronostico.periodo <- function(x, n = 4){
          vector.cuarto.dia <- rep(c("00 a 05", "06 a 11", "12 a 17", "18 a 23"), 2*n)
          aux <- match(x, vector.cuarto.dia)
          pronostico.cuarto.dia <- vector.cuarto.dia[(aux+1):(aux+n)]
          return(pronostico.cuarto.dia)
        }
        
        #regresa las fechas posteriores
        pronostico.fecha <- function(fecha.actual, periodo.pron){
          fecha.pron <- vector()
          if(periodo.pron[1] == "00 a 05"){
            fecha.pron <- rep(fecha.actual + 1, 4)
          } else if(periodo.pron[2] == "00 a 05") {
            fecha.pron[1] <- fecha.actual
            fecha.pron[2:4] <- fecha.actual + 1
          } else if(periodo.pron[3] == "00 a 05") {
            fecha.pron[1:2] <- fecha.actual
            fecha.pron[3:4] <- fecha.actual + 1
          } else if(periodo.pron[4] == "00 a 05") {
            fecha.pron[1:3] <- fecha.actual
            fecha.pron[4] <- fecha.actual + 1
          }
          return(as.Date(fecha.pron))
        }
        
        #regresa la estacion a partir de una fecha
        getSeason <- function(DATES) {
          WS <- as.Date("2012-12-21", format = "%Y-%m-%d") # Invierno
          SE <- as.Date("2012-3-21",  format = "%Y-%m-%d") # Primavera
          SS <- as.Date("2012-6-21",  format = "%Y-%m-%d") # Verano
          FE <- as.Date("2012-9-21",  format = "%Y-%m-%d") # Otono
          
          # Convierte las fechas a la fecha correspondiente del 2012 (porque es bisiesto)
          d <- as.Date(strftime(DATES, format="2012-%m-%d"))
          
          ifelse (d >= WS | d < SE, "Invierno",
                  ifelse (d >= SE & d < SS, "Primavera",
                          ifelse (d >= SS & d < FE, "Verano", "Otono")))
        }
        
        pron.periodo <- pronostico.periodo(ultimo.periodo)
        pron.fecha <- as.Date(pronostico.fecha(ultima.fecha, pron.periodo))
        pron.estacion <- getSeason(pron.fecha)
        pron.estacion <- factor(pron.estacion, levels = c("Invierno", "Otono", "Primavera", "Verano"))
        matriz.X.pronostico <- model.matrix(~ pron.estacion + pron.periodo)
        return(matriz.X.pronostico)
    } 

  xreg.pron.temp <- pronostico.xreg.temp(ultima.fecha = ult.renglon[,"fecha"], ultimo.periodo = ult.renglon[,"periodo"])

  pronostico.xreg.met <- function(tabla.periodos){
#    load("R/modelos de pronostico/modelos_pronostico_met_O3_obispado.RData")
   load(modelos.var.met)
   pronostico.univ.met <- function(serie, modelo.arima){
    require(forecast)
    serie.trans <- modelo.arima$trans(serie)
    nuevo.modelo <- Arima(serie.trans, model = modelo.arima)
    pronostico.trans <- forecast(nuevo.modelo, h = 4)$mean
    pronostico <- modelo.arima$trans.inv(pronostico.trans)
    return(pronostico)
   }
    #TOUT
    arima.tout <- get(ls()[grep("arima.tout", ls())])
    pron.TOUT <- pronostico.univ.met(tabla.periodos$TOUT, arima.tout)
    
    #HR
    arima.hr <- get(ls()[grep("arima.hr", ls())])

    pron.HR <- pronostico.univ.met(tabla.periodos$HR, arima.hr)
    aux <- pron.HR < 0
    pron.HR[aux] <- 0
    aux <- pron.HR > 100
    pron.HR[aux] <- 100
    
    #SR
    arima.sr <- get(ls()[grep("arima.sr", ls())])

    pron.SR <- pronostico.univ.met(tabla.periodos$SR, arima.sr)
    aux <- pron.SR < 0
    pron.SR[aux] <- 0
    
    #WS
    arima.ws <- get(ls()[grep("arima.ws", ls())])    
    pron.WS <- pronostico.univ.met(tabla.periodos$WS, arima.ws)
    aux <- pron.WS < 0
    pron.WS[aux] <- 0
    
    #WDR
    arima.sin.wdr <- get(ls()[grep("arima.sin.wdr", ls())])    
    pron.sin.wdr <- pronostico.univ.met(sin(tabla.periodos$WDR*pi/180), arima.sin.wdr)
    arima.cos.wdr <- get(ls()[grep("arima.cos.wdr", ls())])        
    pron.cos.wdr <- pronostico.univ.met(cos(tabla.periodos$WDR*pi/180), arima.cos.wdr)
    pron.WDR <- atan2(x = pron.cos.wdr, y = pron.sin.wdr) * 180/pi
    aux <- pron.WDR < 0
    pron.WDR[aux] <- pron.WDR[aux] + 360
    
    matriz.X.met <- cbind(TOUT = pron.TOUT, HR = pron.HR, SR = pron.SR , WS = pron.WS, WDR = pron.WDR)
    return(matriz.X.met)
  }

  xreg.pron.met <- pronostico.xreg.met(tabla.periodos)
  matriz.X <- cbind(xreg.pron.temp, xreg.pron.met)
  colnames(matriz.X) <- c("(Intercept)", "Otono", "Primavera", "Verano",
                          "06 a 11", "12 a 17", "18 a 23", "TOUT", "HR", "SR", "WS", "WDR")
  return(matriz.X)
}

pronostico <- function(x, ...)
  UseMethod("pronostico")

pronostico.stSIMA <- function(x, ...){
  modelos <- x$modelos.pronostico
  tabla.periodos <- x$tabla.periodos.sitio
#   contaminante <- tabla.periodos[ , x$contaminante]
# print(head(contaminante))  
#   modelos <- lapply(modelos, function(x, y) {x$x <- y; return(x)}, contaminante)
# print(head(modelos[[1]]$x))  
  lista.pronosticos <- lapply(modelos, pronostico, tabla.periodos, ...)
# print(lista.pronosticos)
#   lista.pronosticos <- lapply(lista.pronosticos, function(x, y) y(x), x$trans.inv)
# print(lista.pronosticos)    
  return(lista.pronosticos)
}

pronostico.ArimaSIMA.EstPer.TRHWsWd <- function(modelo, tabla.periodos, modelos.var.met, ...){
  require(forecast)
  matrix.X <- Xreg(modelo, tabla.periodos, modelos.var.met)
# print(modelo$x)
  contaminante <- tabla.periodos[ , modelo$contaminante]
print(tail(contaminante))
  modelo$x <- modelo$trans(contaminante)
  pronostico <- forecast(modelo, h = 4, xreg = matrix.X)
  pronostico.vector <- modelo$trans.inv(pronostico$mean)
# print(pronostico)
  pronostico$descripcion <- modelo$descripcion
  attr(pronostico.vector, "forecast.obj") <- pronostico
  class(pronostico.vector) <- c("pronosticoSIMA")
  return(pronostico.vector)
}

as.data.frame.pronosticoSIMA <- function(x, ...){
  as.data.frame(c(x))
}

# print.pronosticoSIMA <- function(x, ...){
#   if(!is.null(attr(x, "forecast.obj"))){
#     forecast.obj <- attr(x, "forecast.obj")
#     if(!is.null(forecast.obj$descripcion)) cat("\n", x$descripcion, "\n")    
#   }
#   print(as.data.frame(attr(x, "forecast.obj")))
# }



