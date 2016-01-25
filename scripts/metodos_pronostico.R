### función que genera el pronóstico de la matriz X ###

pronostico <- function(x, ...)
  UseMethod("pronostico")

Xreg <- function(x, ...)
  UseMethod("Xreg")
  

pronostico.stSIMA <- function(x, ...){
  modelos <- x$modelos.pronostico
  tabla.periodos <- x$tabla.periodos.sitio
  lista.pronosticos <- lapply(modelos, pronostico, tabla.periodos, ...)
  return(lista.pronosticos)
}

as.data.frame.pronosticoSIMA <- function(x, ...){
  as.data.frame(c(x))
}



Xreg.ArimaSIMA.EstPer <- function(modelo, ultima.fecha, ultimo.periodo){    
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
    nuevo.modelo.obj <- Arima(serie.trans, model = modelo.arima)
    pronostico.trans <- forecast(nuevo.modelo.obj, h = 4)$mean
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


pronostico.ArimaSIMA.EstPer <- function(modelo, tabla.periodos, ...){
  require(forecast)
  ultimo.renglon <- nrow(tabla.periodos)
  aux <- tabla.periodos[ultimo.renglon, c("fecha", "periodo")]
  ult.fecha <- as.Date(aux[1, "fecha"])
  ult.periodo <- as.factor(aux[1, "periodo"])
  matrix.X <- Xreg(modelo, ultima.fecha = ult.fecha, ultimo.periodo = ult.periodo)
  modelo$x <- modelo$trans(contaminante)
  pronostico <- forecast(modelo, h = 4, xreg = matrix.X)
  pronostico.vector <- modelo$trans.inv(pronostico$mean)
# print(pronostico)
  pronostico$descripcion <- modelo$descripcion
  attr(pronostico.vector, "pred.limits") <- modelo$trans.inv(data.frame(lower=pronostico$lower[,2], upper = pronostico$upper[,2]))
  return(pronostico.vector)
}

pronostico.ArimaSIMA.EstPer.TRHWsWd <- function(modelo, tabla.periodos, modelos.var.met, ...){
  require(forecast)
  matrix.X <- Xreg(modelo, tabla.periodos, modelos.var.met)
# print(modelo$x)
  contaminante <- tabla.periodos[ , modelo$contaminante]
# print(tail(contaminante))
  modelo$x <- modelo$trans(contaminante)
  pronostico <- forecast(modelo, h = 4, xreg = matrix.X)
  pronostico.vector <- modelo$trans.inv(pronostico$mean)
# print(pronostico)
  pronostico$descripcion <- modelo$descripcion
  attr(pronostico.vector, "pred.limits") <- modelo$trans.inv(data.frame(lower=pronostico$lower[,2], upper = pronostico$upper[,2]))
  return(pronostico.vector)
}


# print.pronosticoSIMA <- function(x, ...){
#   if(!is.null(attr(x, "forecast.obj"))){
#     forecast.obj <- attr(x, "forecast.obj")
#     if(!is.null(forecast.obj$descripcion)) cat("\n", x$descripcion, "\n")    
#   }
#   print(as.data.frame(attr(x, "forecast.obj")))
# }

###Nuevos metodos: Obispado
Xreg.ArimaSIMA.O3Obispado <- function(modelo, tabla.periodos, modelos.var.met){

  require(tree)
  
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

    pron.periodo <- factor(pronostico.periodo(ultimo.periodo), levels = c("00 a 05", "06 a 11", "12 a 17", "18 a 23"))
    pron.fecha <- as.Date(pronostico.fecha(ultima.fecha, pron.periodo))
    pron.estacion <- getSeason(pron.fecha)
    pron.estacion <- factor(pron.estacion, levels = c("Invierno", "Otono", "Primavera", "Verano"))
    mes <- factor(months(pron.fecha), levels = c("January", "February", "March", "April", "May", "June", "July",
      "August", "September", "October", "November", "December"))
#     matriz.X.temp <- model.matrix(~ pron.estacion + pron.periodo + mes)
#     attr(matriz.X.temp, "data.frame.temp") <- data.frame(estacion = pron.estacion, periodo = pron.periodo, mes = mes)
#     return(matriz.X.temp)
    data.frame.temp <- data.frame(estacion = pron.estacion, periodo = pron.periodo, mes = mes)
    return(data.frame.temp)
  } 

  pronostico.xreg.met <- function(tabla.periodos){
#    load("R/modelos de pronostico/modelos_pronostico_met_O3_obispado.RData")
   load(modelos.var.met)
   pronostico.univ.met <- function(serie, modelo.arima){
    require(forecast)
    serie.trans <- modelo.arima$trans(serie)
    nuevo.modelo.obj <- Arima(serie.trans, model = modelo.arima)
    pronostico.trans <- forecast(nuevo.modelo.obj, h = 4)$mean
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
    seno <- sin(pron.WDR*pi/180)
    coseno <- cos(pron.WDR*pi/180)
    angulo <- -60
    R <- matrix(c(cos(angulo*pi/180), sin(angulo*pi/180), -sin(angulo*pi/180), cos(angulo*pi/180)), 2, 2)
    rotados <- R%*%rbind(coseno, seno)
    pron.WDR.rot  <- abs(atan2(rotados[2, ], rotados[1, ])*180/pi)
    data.frame.met <- data.frame(TOUT = pron.TOUT, HR = pron.HR, WS = pron.WS, SR = pron.SR, WDR = pron.WDR, WDR.rot = pron.WDR.rot)
    return(data.frame.met)
  }

  data.frame.pron.temp <- pronostico.xreg.temp(ultima.fecha = ult.renglon[,"fecha"], ultimo.periodo = ult.renglon[,"periodo"])
  data.frame.pron.met <- pronostico.xreg.met(tabla.periodos)

  contaminante <- tabla.periodos[ , modelo$contaminante]
  O3.96 <- contaminante >= 96
  WDR <- tabla.periodos$WDR
  seno <- sin(WDR*pi/180)
  coseno <- cos(WDR*pi/180)
  angulo <- -60
  R <- matrix(c(cos(angulo*pi/180), sin(angulo*pi/180), -sin(angulo*pi/180), cos(angulo*pi/180)), 2, 2)
  rotados <- R%*%rbind(coseno, seno)
  WDR.rot  <- abs(atan2(rotados[2, ], rotados[1, ])*180/pi)
  mes <- factor(tabla.periodos$mes, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
  
  df <- data.frame(estacion = tabla.periodos$estacion, periodo = tabla.periodos$periodo, mes = mes, TOUT = tabla.periodos$TOUT,
    HR = tabla.periodos$HR, SR = tabla.periodos$SR, WS = tabla.periodos$WS, WDR = tabla.periodos$WDR, WDR.rot = WDR.rot)

  ##tree clasification
#   tree.fit <- tree(as.factor(O3.96) ~ estacion + mes + periodo + I(log(TOUT+273.15+1)) + HR + I(HR^2) + I(HR^3) + I(1/(WS+1)) + SR + I(SR^2) + WDR,
#     data = df, split = "gini")
#   tree.fit.pred <- predict(tree.fit, newdata = df, type = "class")
#   tree.fit.pred <- factor(tree.fit.pred, levels = c("FALSE", "TRUE"))
#   
#   df.new <- data.frame(data.frame.pron.temp, data.frame.pron.met)
#   tree.pred <- factor(predict(tree.fit, newdata = df.new, type = "class"), levels = c("FALSE", "TRUE"))  
  try.res <- try({tree(as.factor(O3.96) ~ estacion + mes + periodo + I(log(TOUT+273.15+1)) + HR + I(HR^2) + I(HR^3) + I(1/(WS+1)) + SR + I(SR^2) + WDR,
    data = df, split = "gini")}, silent = TRUE)
  if(!inherits(try.res, "try-error")){
    tree.fit <- try.res
    tree.fit.pred <- predict(tree.fit, newdata = df, type = "class")
    tree.fit.pred <- factor(tree.fit.pred, levels = c("FALSE", "TRUE"))
    df.new <- data.frame(data.frame.pron.temp, data.frame.pron.met)
    tree.pred <- factor(predict(tree.fit, newdata = df.new, type = "class"), levels = c("FALSE", "TRUE"))    
  }
    else{
      glm.fitt <- glm(as.factor(O3.96) ~ estacion + mes + periodo + I(log(TOUT+273.15+1)) + HR + I(HR^2) + I(HR^3) + I(1/(WS+1)) + SR + I(SR^2) + WDR,
	family = binomial(link = logit), data = df)
	tree.fit.pred <- factor(.5 < predict(glm.fitt, newdata = df, type = "response"), levels = c("FALSE", "TRUE"))
	df.new <- data.frame(data.frame.pron.temp, data.frame.pron.met)
	tree.pred <- factor(.5 < predict(glm.fitt, newdata = df.new, type = "response"), levels = c("FALSE", "TRUE"))	
    }
  
  X <- model.matrix( ~ mes + periodo + I(log(TOUT+273.15+1)) + HR + I(HR^2) + I(HR^3) + I(1/(WS+1)) + SR + I(SR^2) + WDR.rot + tree.pred,
    data = cbind(df, tree.pred = tree.fit.pred))
  X.new <- model.matrix( ~ mes + periodo + I(log(TOUT+273.15+1)) + HR + I(HR^2) + I(HR^3) + I(1/(WS+1)) + SR + I(SR^2) + WDR.rot + tree.pred,
    data = cbind(df.new, tree.pred = tree.pred))
  return(list(X = X, X.new = X.new, tree.df = df))
}

pronostico.ArimaSIMA.O3Obispado <- function(modelo, tabla.periodos, modelos.var.met, estimacion = FALSE, ...){
  require(forecast)
  
  matriz.X <- Xreg(modelo, tabla.periodos, modelos.var.met)$X
  matriz.X.new <- Xreg(modelo, tabla.periodos, modelos.var.met)$X.new
  contaminante <- tabla.periodos[ , modelo$contaminante]
  serie.trans <- modelo$trans(contaminante)
  if(estimacion){
    if(tabla.periodos$periodo[nrow(tabla.periodos)] == "00 a 05"){
      nuevo.modelo.obj <- Arima(serie.trans, xreg = matriz.X, include.mean = F, order = c(3,0,0), seasonal = list(order = c(3, 0, 0), period = 4),
	optim.control = list(maxit= 10000))
    }
  }
  else
    nuevo.modelo.obj <- Arima(serie.trans, xreg = matriz.X, include.mean = F, model = modelo)
    
#   pronostico.trans <- forecast(nuevo.modelo.obj.obj, xreg = matriz.X.new, h = 4)$mean
#   pronostico <- forecast(modelo, h = 4, xreg = matriz.X.new)
#   pronostico.vector <- modelo$trans.inv(pronostico$mean)
#   pronostico$descripcion <- modelo$descripcion

  pronostico <- forecast(nuevo.modelo.obj, h = 4, xreg = matriz.X.new)
  pronostico.vector <- modelo$trans.inv(pronostico$mean)
  pronostico$descripcion <- modelo$descripcion
  
  attr(pronostico.vector, "pred.limits") <- modelo$trans.inv(data.frame(lower=pronostico$lower[,2], upper = pronostico$upper[,2]))
  attr(pronostico.vector, "descripcion") <- modelo$descripcion
  
  return(pronostico.vector)
}

Xreg.ArimaSIMA.PM10Obispado <- function(modelo, tabla.periodos, modelos.var.met){

  require(tree)
  
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

    pron.periodo <- factor(pronostico.periodo(ultimo.periodo), levels = c("00 a 05", "06 a 11", "12 a 17", "18 a 23"))
    pron.fecha <- as.Date(pronostico.fecha(ultima.fecha, pron.periodo))
    pron.estacion <- getSeason(pron.fecha)
    pron.estacion <- factor(pron.estacion, levels = c("Invierno", "Otono", "Primavera", "Verano"))
    mes <- factor(months(pron.fecha), levels = c("January", "February", "March", "April", "May", "June", "July",
      "August", "September", "October", "November", "December"))
    diaSem <- factor(weekdays(pron.fecha), levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
    data.frame.temp <- data.frame(estacion = pron.estacion, periodo = pron.periodo, mes = mes, diaSem = diaSem)
    return(data.frame.temp)
  } 

  pronostico.xreg.met <- function(tabla.periodos){
#    load("R/modelos de pronostico/modelos_pronostico_met_O3_obispado.RData")
   load(modelos.var.met)
   pronostico.univ.met <- function(serie, modelo.arima){
    require(forecast)
    serie.trans <- modelo.arima$trans(serie)
    nuevo.modelo.obj <- Arima(serie.trans, model = modelo.arima)
    pronostico.trans <- forecast(nuevo.modelo.obj, h = 4)$mean
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
#     seno <- sin(pron.WDR*pi/180)
#     coseno <- cos(pron.WDR*pi/180)
#     angulo <- -60
#     R <- matrix(c(cos(angulo*pi/180), sin(angulo*pi/180), -sin(angulo*pi/180), cos(angulo*pi/180)), 2, 2)
#     rotados <- R%*%rbind(coseno, seno)
    pron.WDR.rot  <- pron.WDR
    pron.WDR.rot[pron.WDR >= 180]  <- abs(pron.WDR[pron.WDR >= 180]-360) 
    
    data.frame.met <- data.frame(TOUT = pron.TOUT, HR = pron.HR, WS = pron.WS, SR = pron.SR, WDR = pron.WDR, WDR.rot = pron.WDR.rot)
    return(data.frame.met)
  }

  data.frame.pron.temp <- pronostico.xreg.temp(ultima.fecha = ult.renglon[,"fecha"], ultimo.periodo = ult.renglon[,"periodo"])
  data.frame.pron.met <- pronostico.xreg.met(tabla.periodos)

  contaminante <- tabla.periodos[ , modelo$contaminante]
  PM10.76 <- contaminante >= 76
  WDR <- tabla.periodos$WDR
#   seno <- sin(WDR*pi/180)
#   coseno <- cos(WDR*pi/180)
#   angulo <- -60
#   R <- matrix(c(cos(angulo*pi/180), sin(angulo*pi/180), -sin(angulo*pi/180), cos(angulo*pi/180)), 2, 2)
#   rotados <- R%*%rbind(coseno, seno)
  WDR.rot  <- WDR
  WDR.rot[WDR >= 180]  <- abs(WDR[WDR >= 180]-360)
  
  mes <- factor(tabla.periodos$mes, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
  diaSem <- factor(tabla.periodos$diaSem, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  df <- data.frame(diaSem = tabla.periodos$diaSem, estacion = tabla.periodos$estacion, periodo = tabla.periodos$periodo, mes = mes, TOUT = tabla.periodos$TOUT,
    HR = tabla.periodos$HR, SR = tabla.periodos$SR, WS = tabla.periodos$WS, WDR = tabla.periodos$WDR, WDR.rot = WDR.rot)

  ##tree clasification
  try.res <- try({tree(as.factor(PM10.76) ~ estacion + mes + diaSem + periodo + TOUT + HR + I(HR^2) + I(HR^3) + WS + I(WS^2) + SR + I(SR^2) + WDR,
    data = df, split = "gini")}, silent = TRUE)
  if(!inherits(try.res, "try-error")){
    tree.fit <- try.res
    tree.fit.pred <- predict(tree.fit, newdata = df, type = "class")
    tree.fit.pred <- factor(tree.fit.pred, levels = c("FALSE", "TRUE"))
    df.new <- data.frame(data.frame.pron.temp, data.frame.pron.met)
    tree.pred <- factor(predict(tree.fit, newdata = df.new, type = "class"), levels = c("FALSE", "TRUE"))    
  }
    else{
      glm.fitt <- glm(as.factor(PM10.76) ~ estacion + mes + diaSem + periodo + TOUT + HR + I(HR^2) + I(HR^3) + WS + I(WS^2) + SR + I(SR^2) + WDR,
	family = binomial(link = logit), data = df)
	tree.fit.pred <- factor(.5 < predict(glm.fitt, newdata = df, type = "response"), levels = c("FALSE", "TRUE"))
	df.new <- data.frame(data.frame.pron.temp, data.frame.pron.met)
	tree.pred <- factor(.5 < predict(glm.fitt, newdata = df.new, type = "response"), levels = c("FALSE", "TRUE"))	
    }
#   tree.fit <- tree(as.factor(PM10.76) ~ estacion + mes + diaSem + periodo + TOUT + HR + I(HR^2) + I(HR^3) + WS + I(WS^2) + SR + I(SR^2) + WDR,
#     data = df, split = "gini")
#   tree.fit.pred <- predict(tree.fit, newdata = df, type = "class")
#   tree.fit.pred <- factor(tree.fit.pred, levels = c("FALSE", "TRUE"))
  
#   df.new <- data.frame(data.frame.pron.temp, data.frame.pron.met)
#   tree.pred <- factor(predict(tree.fit, newdata = df.new, type = "class"), levels = c("FALSE", "TRUE"))  

  X <- model.matrix( ~ mes + diaSem + periodo + TOUT + HR + I(HR^2) + I(HR^3) + WS + I(WS^2) + SR + I(SR^2) + WDR.rot + I(WDR.rot^2) + tree.pred,
    data = cbind(df, tree.pred = tree.fit.pred))
  X.new <- model.matrix( ~ mes + diaSem + periodo + TOUT + HR + I(HR^2) + I(HR^3) + WS + I(WS^2) + SR + I(SR^2) + WDR.rot + I(WDR.rot^2) + tree.pred,
    data = cbind(df.new, tree.pred = tree.pred))
  return(list(X = X, X.new = X.new, tree.df = df))
}

pronostico.ArimaSIMA.PM10Obispado <- function(modelo, tabla.periodos, modelos.var.met, estimacion = FALSE, ...){
  require(forecast)
  
  matriz.X <- Xreg(modelo, tabla.periodos, modelos.var.met)$X
  matriz.X.new <- Xreg(modelo, tabla.periodos, modelos.var.met)$X.new
  contaminante <- tabla.periodos[ , modelo$contaminante]
  serie.trans <- modelo$trans(contaminante)
  if(estimacion){
    if(tabla.periodos$periodo[nrow(tabla.periodos)] == "00 a 05"){
      nuevo.modelo.obj <- Arima(serie.trans, xreg = matriz.X, include.mean = F, order = c(4,0,0), seasonal = list(order = c(1, 0, 0), period = 8),
	optim.control = list(maxit= 10000))
    }
  }
  else
    nuevo.modelo.obj <- Arima(serie.trans, xreg = matriz.X, include.mean = F, model = modelo)
    
  
  pronostico <- forecast(modelo, h = 4, xreg = matriz.X.new)
  pronostico.vector <- modelo$trans.inv(pronostico$mean)
  pronostico$descripcion <- modelo$descripcion
  
  attr(pronostico.vector, "pred.limits") <- modelo$trans.inv(data.frame(lower=pronostico$lower[,2], upper = pronostico$upper[,2]))
  attr(pronostico.vector, "descripcion") <- modelo$descripcion
  
  return(pronostico.vector)
}


###Nuevos metodos: Pastora
Xreg.ArimaSIMA.O3Pastora <- function(modelo, tabla.periodos, modelos.var.met){

  require(tree)
  
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

    pron.periodo <- factor(pronostico.periodo(ultimo.periodo), levels = c("00 a 05", "06 a 11", "12 a 17", "18 a 23"))
    pron.fecha <- as.Date(pronostico.fecha(ultima.fecha, pron.periodo))
    pron.estacion <- getSeason(pron.fecha)
    pron.estacion <- factor(pron.estacion, levels = c("Invierno", "Otono", "Primavera", "Verano"))
    mes <- factor(months(pron.fecha), levels = c("January", "February", "March", "April", "May", "June", "July",
      "August", "September", "October", "November", "December"))
#     matriz.X.temp <- model.matrix(~ pron.estacion + pron.periodo + mes)
#     attr(matriz.X.temp, "data.frame.temp") <- data.frame(estacion = pron.estacion, periodo = pron.periodo, mes = mes)
#     return(matriz.X.temp)
    data.frame.temp <- data.frame(estacion = pron.estacion, periodo = pron.periodo, mes = mes)
    return(data.frame.temp)
  } 

  pronostico.xreg.met <- function(tabla.periodos){
#    load("R/modelos de pronostico/modelos_pronostico_met_O3_obispado.RData")
   load(modelos.var.met)
   pronostico.univ.met <- function(serie, modelo.arima){
    require(forecast)
    serie.trans <- modelo.arima$trans(serie)
    nuevo.modelo.obj <- Arima(serie.trans, model = modelo.arima)
    pronostico.trans <- forecast(nuevo.modelo.obj, h = 4)$mean
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
    seno <- sin(pron.WDR*pi/180)
    coseno <- cos(pron.WDR*pi/180)
    angulo <- -85
    R <- matrix(c(cos(angulo*pi/180), sin(angulo*pi/180), -sin(angulo*pi/180), cos(angulo*pi/180)), 2, 2)
    rotados <- R%*%rbind(coseno, seno)
    pron.WDR.rot  <- abs(atan2(rotados[2, ], rotados[1, ])*180/pi)
    data.frame.met <- data.frame(TOUT = pron.TOUT, HR = pron.HR, WS = pron.WS, SR = pron.SR, WDR = pron.WDR, WDR.rot = pron.WDR.rot)
    return(data.frame.met)
  }

  data.frame.pron.temp <- pronostico.xreg.temp(ultima.fecha = ult.renglon[,"fecha"], ultimo.periodo = ult.renglon[,"periodo"])
  data.frame.pron.met <- pronostico.xreg.met(tabla.periodos)

  contaminante <- tabla.periodos[ , modelo$contaminante]
  O3.96 <- contaminante >= 96
  WDR <- tabla.periodos$WDR
  seno <- sin(WDR*pi/180)
  coseno <- cos(WDR*pi/180)
  angulo <- -85
  R <- matrix(c(cos(angulo*pi/180), sin(angulo*pi/180), -sin(angulo*pi/180), cos(angulo*pi/180)), 2, 2)
  rotados <- R%*%rbind(coseno, seno)
  WDR.rot  <- abs(atan2(rotados[2, ], rotados[1, ])*180/pi)
  mes <- factor(tabla.periodos$mes, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
  
  df <- data.frame(estacion = tabla.periodos$estacion, periodo = tabla.periodos$periodo, mes = mes, TOUT = tabla.periodos$TOUT,
    HR = tabla.periodos$HR, SR = tabla.periodos$SR, WS = tabla.periodos$WS, WDR = tabla.periodos$WDR, WDR.rot = WDR.rot)

  ##tree clasification
  try.res <- try({tree(as.factor(O3.96) ~ estacion + mes + periodo + I(log(TOUT+273.15+1)) + HR + I(HR^2) + I(HR^3) + I(1/(WS+1)) + SR + I(SR^2) + WDR,
    data = df, split = "gini")}, silent = TRUE)
  if(!inherits(try.res, "try-error")){
    tree.fit <- try.res
    tree.fit.pred <- predict(tree.fit, newdata = df, type = "class")
    tree.fit.pred <- factor(tree.fit.pred, levels = c("FALSE", "TRUE"))
    df.new <- data.frame(data.frame.pron.temp, data.frame.pron.met)
    tree.pred <- factor(predict(tree.fit, newdata = df.new, type = "class"), levels = c("FALSE", "TRUE"))    
  }
    else{
      glm.fitt <- glm(as.factor(O3.96) ~ estacion + mes + periodo + I(log(TOUT+273.15+1)) + HR + I(HR^2) + I(HR^3) + I(1/(WS+1)) + SR + I(SR^2) + WDR,
	family = binomial(link = logit), data = df)
	tree.fit.pred <- factor(.5 < predict(glm.fitt, newdata = df, type = "response"), levels = c("FALSE", "TRUE"))
	df.new <- data.frame(data.frame.pron.temp, data.frame.pron.met)
	tree.pred <- factor(.5 < predict(glm.fitt, newdata = df.new, type = "response"), levels = c("FALSE", "TRUE"))	
    }

  X <- model.matrix( ~ mes + periodo + I(log(TOUT+273.15+1)) + HR + I(HR^2) + I(HR^3) + I(1/(WS+1)) + SR + I(SR^2) + WDR.rot + tree.pred,
    data = cbind(df, tree.pred = tree.fit.pred))
  X.new <- model.matrix( ~ mes + periodo + I(log(TOUT+273.15+1)) + HR + I(HR^2) + I(HR^3) + I(1/(WS+1)) + SR + I(SR^2) + WDR.rot + tree.pred,
    data = cbind(df.new, tree.pred = tree.pred))
  return(list(X = X, X.new = X.new, tree.df = df))
}

pronostico.ArimaSIMA.O3Pastora <- function(modelo, tabla.periodos, modelos.var.met, estimacion = FALSE, ...){
  require(forecast)
  
  matriz.X <- Xreg(modelo, tabla.periodos, modelos.var.met)$X
  matriz.X.new <- Xreg(modelo, tabla.periodos, modelos.var.met)$X.new
  contaminante <- tabla.periodos[ , modelo$contaminante]
  serie.trans <- modelo$trans(contaminante)
  if(estimacion){
    if(tabla.periodos$periodo[nrow(tabla.periodos)] == "00 a 05"){
      nuevo.modelo.obj <- Arima(serie.trans, xreg = matriz.X, include.mean = F, order = c(3,0,0), seasonal = list(order = c(3, 0, 0), period = 4),
	optim.control = list(maxit= 10000))
    }
  }
  else
    nuevo.modelo.obj <- Arima(serie.trans, xreg = matriz.X, include.mean = F, model = modelo)
    
  
  pronostico <- forecast(modelo, h = 4, xreg = matriz.X.new)
  pronostico.vector <- modelo$trans.inv(pronostico$mean)
  pronostico$descripcion <- modelo$descripcion
  
  attr(pronostico.vector, "pred.limits") <- modelo$trans.inv(data.frame(lower=pronostico$lower[,2], upper = pronostico$upper[,2]))
  attr(pronostico.vector, "descripcion") <- modelo$descripcion
  
  return(pronostico.vector)
}

Xreg.ArimaSIMA.PM10Pastora <- function(modelo, tabla.periodos, modelos.var.met){

  require(tree)
  
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

    pron.periodo <- factor(pronostico.periodo(ultimo.periodo), levels = c("00 a 05", "06 a 11", "12 a 17", "18 a 23"))
    pron.fecha <- as.Date(pronostico.fecha(ultima.fecha, pron.periodo))
    pron.estacion <- getSeason(pron.fecha)
    pron.estacion <- factor(pron.estacion, levels = c("Invierno", "Otono", "Primavera", "Verano"))
    mes <- factor(months(pron.fecha), levels = c("January", "February", "March", "April", "May", "June", "July",
      "August", "September", "October", "November", "December"))
    diaSem <- factor(weekdays(pron.fecha), levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
    data.frame.temp <- data.frame(estacion = pron.estacion, periodo = pron.periodo, mes = mes, diaSem = diaSem)
    return(data.frame.temp)
  } 

  pronostico.xreg.met <- function(tabla.periodos){
#    load("R/modelos de pronostico/modelos_pronostico_met_O3_obispado.RData")
   load(modelos.var.met)
   pronostico.univ.met <- function(serie, modelo.arima){
    require(forecast)
    serie.trans <- modelo.arima$trans(serie)
    nuevo.modelo.obj <- Arima(serie.trans, model = modelo.arima)
    pronostico.trans <- forecast(nuevo.modelo.obj, h = 4)$mean
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
#     seno <- sin(pron.WDR*pi/180)
#     coseno <- cos(pron.WDR*pi/180)
#     angulo <- -60
#     R <- matrix(c(cos(angulo*pi/180), sin(angulo*pi/180), -sin(angulo*pi/180), cos(angulo*pi/180)), 2, 2)
#     rotados <- R%*%rbind(coseno, seno)
    pron.WDR.rot  <- pron.WDR
    pron.WDR.rot[pron.WDR >= 180]  <- abs(pron.WDR[pron.WDR >= 180]-360) 
    
    data.frame.met <- data.frame(TOUT = pron.TOUT, HR = pron.HR, WS = pron.WS, SR = pron.SR, WDR = pron.WDR, WDR.rot = pron.WDR.rot)
    return(data.frame.met)
  }

  data.frame.pron.temp <- pronostico.xreg.temp(ultima.fecha = ult.renglon[,"fecha"], ultimo.periodo = ult.renglon[,"periodo"])
  data.frame.pron.met <- pronostico.xreg.met(tabla.periodos)

  contaminante <- tabla.periodos[ , modelo$contaminante]
  PM10.76 <- contaminante >= 76
  WDR <- tabla.periodos$WDR
#   seno <- sin(WDR*pi/180)
#   coseno <- cos(WDR*pi/180)
#   angulo <- -60
#   R <- matrix(c(cos(angulo*pi/180), sin(angulo*pi/180), -sin(angulo*pi/180), cos(angulo*pi/180)), 2, 2)
#   rotados <- R%*%rbind(coseno, seno)
  WDR.rot  <- WDR
  WDR.rot[WDR >= 180]  <- abs(WDR[WDR >= 180]-360)
  
  mes <- factor(tabla.periodos$mes, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
  diaSem <- factor(tabla.periodos$diaSem, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  df <- data.frame(diaSem = tabla.periodos$diaSem, estacion = tabla.periodos$estacion, periodo = tabla.periodos$periodo, mes = mes, TOUT = tabla.periodos$TOUT,
    HR = tabla.periodos$HR, SR = tabla.periodos$SR, WS = tabla.periodos$WS, WDR = tabla.periodos$WDR, WDR.rot = WDR.rot)

  ##tree clasification
  try.res <- try({tree(as.factor(PM10.76) ~ estacion + mes + diaSem + periodo + TOUT + HR + I(HR^2) + I(HR^3) + WS + I(WS^2) + SR + I(SR^2) + WDR,
    data = df, split = "gini")}, silent = TRUE)
  if(!inherits(try.res, "try-error")){
    tree.fit <- try.res
    tree.fit.pred <- predict(tree.fit, newdata = df, type = "class")
    tree.fit.pred <- factor(tree.fit.pred, levels = c("FALSE", "TRUE"))
    df.new <- data.frame(data.frame.pron.temp, data.frame.pron.met)
    tree.pred <- factor(predict(tree.fit, newdata = df.new, type = "class"), levels = c("FALSE", "TRUE"))    
  }
    else{
      glm.fitt <- glm(as.factor(PM10.76) ~ estacion + mes + diaSem + periodo + TOUT + HR + I(HR^2) + I(HR^3) + WS + I(WS^2) + SR + I(SR^2) + WDR,
	family = binomial(link = logit), data = df)
	tree.fit.pred <- factor(.5 < predict(glm.fitt, newdata = df, type = "response"), levels = c("FALSE", "TRUE"))
	df.new <- data.frame(data.frame.pron.temp, data.frame.pron.met)
	tree.pred <- factor(.5 < predict(glm.fitt, newdata = df.new, type = "response"), levels = c("FALSE", "TRUE"))	
    }

  X <- model.matrix( ~ mes + diaSem + periodo + TOUT + HR + I(HR^2) + I(HR^3) + WS + I(WS^2) + SR + I(SR^2) + WDR.rot + I(WDR.rot^2) + tree.pred,
    data = cbind(df, tree.pred = tree.fit.pred))
  X.new <- model.matrix( ~ mes + diaSem + periodo + TOUT + HR + I(HR^2) + I(HR^3) + WS + I(WS^2) + SR + I(SR^2) + WDR.rot + I(WDR.rot^2) + tree.pred,
    data = cbind(df.new, tree.pred = tree.pred))
  return(list(X = X, X.new = X.new, tree.df = df))
}

pronostico.ArimaSIMA.PM10Pastora <- function(modelo, tabla.periodos, modelos.var.met, estimacion = FALSE, ...){
  require(forecast)
  
  matriz.X <- Xreg(modelo, tabla.periodos, modelos.var.met)$X
  matriz.X.new <- Xreg(modelo, tabla.periodos, modelos.var.met)$X.new
  contaminante <- tabla.periodos[ , modelo$contaminante]
  serie.trans <- modelo$trans(contaminante)
  if(estimacion){
    if(tabla.periodos$periodo[nrow(tabla.periodos)] == "00 a 05"){
      nuevo.modelo.obj <- Arima(serie.trans, xreg = matriz.X, include.mean = F, order = c(4,0,0), seasonal = list(order = c(1, 0, 0), period = 8),
	optim.control = list(maxit= 10000))
    }
  }
  else
    nuevo.modelo.obj <- Arima(serie.trans, xreg = matriz.X, include.mean = F, model = modelo)
    
  
  pronostico <- forecast(modelo, h = 4, xreg = matriz.X.new)
  pronostico.vector <- modelo$trans.inv(pronostico$mean)
  pronostico$descripcion <- modelo$descripcion
  
  attr(pronostico.vector, "pred.limits") <- modelo$trans.inv(data.frame(lower=pronostico$lower[,2], upper = pronostico$upper[,2]))
  attr(pronostico.vector, "descripcion") <- modelo$descripcion
  
  return(pronostico.vector)
}



###Nuevos metodos: Catarina
Xreg.ArimaSIMA.O3Catarina <- function(modelo, tabla.periodos, modelos.var.met){

  require(tree)
  
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

    pron.periodo <- factor(pronostico.periodo(ultimo.periodo), levels = c("00 a 05", "06 a 11", "12 a 17", "18 a 23"))
    pron.fecha <- as.Date(pronostico.fecha(ultima.fecha, pron.periodo))
    pron.estacion <- getSeason(pron.fecha)
    pron.estacion <- factor(pron.estacion, levels = c("Invierno", "Otono", "Primavera", "Verano"))
    mes <- factor(months(pron.fecha), levels = c("January", "February", "March", "April", "May", "June", "July",
      "August", "September", "October", "November", "December"))
#     matriz.X.temp <- model.matrix(~ pron.estacion + pron.periodo + mes)
#     attr(matriz.X.temp, "data.frame.temp") <- data.frame(estacion = pron.estacion, periodo = pron.periodo, mes = mes)
#     return(matriz.X.temp)
    data.frame.temp <- data.frame(estacion = pron.estacion, periodo = pron.periodo, mes = mes)
    return(data.frame.temp)
  } 

  pronostico.xreg.met <- function(tabla.periodos){
#    load("R/modelos de pronostico/modelos_pronostico_met_O3_obispado.RData")
   load(modelos.var.met)
   pronostico.univ.met <- function(serie, modelo.arima){
    require(forecast)
    serie.trans <- modelo.arima$trans(serie)
    nuevo.modelo.obj <- Arima(serie.trans, model = modelo.arima)
    pronostico.trans <- forecast(nuevo.modelo.obj, h = 4)$mean
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
    seno <- sin(pron.WDR*pi/180)
    coseno <- cos(pron.WDR*pi/180)
    angulo <- -90
    R <- matrix(c(cos(angulo*pi/180), sin(angulo*pi/180), -sin(angulo*pi/180), cos(angulo*pi/180)), 2, 2)
    rotados <- R%*%rbind(coseno, seno)
    pron.WDR.rot  <- abs(atan2(rotados[2, ], rotados[1, ])*180/pi)
    data.frame.met <- data.frame(TOUT = pron.TOUT, HR = pron.HR, WS = pron.WS, SR = pron.SR, WDR = pron.WDR, WDR.rot = pron.WDR.rot)
    return(data.frame.met)
  }

  data.frame.pron.temp <- pronostico.xreg.temp(ultima.fecha = ult.renglon[,"fecha"], ultimo.periodo = ult.renglon[,"periodo"])
  data.frame.pron.met <- pronostico.xreg.met(tabla.periodos)

  contaminante <- tabla.periodos[ , modelo$contaminante]
  O3.96 <- contaminante >= 96
  WDR <- tabla.periodos$WDR
  seno <- sin(WDR*pi/180)
  coseno <- cos(WDR*pi/180)
  angulo <- -90
  R <- matrix(c(cos(angulo*pi/180), sin(angulo*pi/180), -sin(angulo*pi/180), cos(angulo*pi/180)), 2, 2)
  rotados <- R%*%rbind(coseno, seno)
  WDR.rot  <- abs(atan2(rotados[2, ], rotados[1, ])*180/pi)
  mes <- factor(tabla.periodos$mes, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
  
  df <- data.frame(estacion = tabla.periodos$estacion, periodo = tabla.periodos$periodo, mes = mes, TOUT = tabla.periodos$TOUT,
    HR = tabla.periodos$HR, SR = tabla.periodos$SR, WS = tabla.periodos$WS, WDR = tabla.periodos$WDR, WDR.rot = WDR.rot)

  ##tree clasification
  try.res <- try({tree(as.factor(O3.96) ~ estacion + mes + periodo + I(log(TOUT+273.15+1)) + HR + I(HR^2) + I(HR^3) + I(1/(WS+1)) + SR + I(SR^2) + WDR,
    data = df, split = "gini")}, silent = TRUE)
  if(!inherits(try.res, "try-error")){
    tree.fit <- try.res
    tree.fit.pred <- predict(tree.fit, newdata = df, type = "class")
    tree.fit.pred <- factor(tree.fit.pred, levels = c("FALSE", "TRUE"))
    df.new <- data.frame(data.frame.pron.temp, data.frame.pron.met)
    tree.pred <- factor(predict(tree.fit, newdata = df.new, type = "class"), levels = c("FALSE", "TRUE"))    
  }
    else{
      glm.fitt <- glm(as.factor(O3.96) ~ estacion + mes + periodo + I(log(TOUT+273.15+1)) + HR + I(HR^2) + I(HR^3) + I(1/(WS+1)) + SR + I(SR^2) + WDR,
	family = binomial(link = logit), data = df)
	tree.fit.pred <- factor(.5 < predict(glm.fitt, newdata = df, type = "response"), levels = c("FALSE", "TRUE"))
	df.new <- data.frame(data.frame.pron.temp, data.frame.pron.met)
	tree.pred <- factor(.5 < predict(glm.fitt, newdata = df.new, type = "response"), levels = c("FALSE", "TRUE"))	
    }

  X <- model.matrix( ~ mes + periodo + I(log(TOUT+273.15+1)) + HR + I(HR^2) + I(HR^3) + I(1/(WS+1)) + SR + I(SR^2) + WDR.rot + tree.pred,
    data = cbind(df, tree.pred = tree.fit.pred))
  X.new <- model.matrix( ~ mes + periodo + I(log(TOUT+273.15+1)) + HR + I(HR^2) + I(HR^3) + I(1/(WS+1)) + SR + I(SR^2) + WDR.rot + tree.pred,
    data = cbind(df.new, tree.pred = tree.pred))
  return(list(X = X, X.new = X.new, tree.df = df))
}

pronostico.ArimaSIMA.O3Catarina <- function(modelo, tabla.periodos, modelos.var.met, estimacion = FALSE, ...){
  require(forecast)
  
  matriz.X <- Xreg(modelo, tabla.periodos, modelos.var.met)$X
  matriz.X.new <- Xreg(modelo, tabla.periodos, modelos.var.met)$X.new
  contaminante <- tabla.periodos[ , modelo$contaminante]
  serie.trans <- modelo$trans(contaminante)
  if(estimacion){
    if(tabla.periodos$periodo[nrow(tabla.periodos)] == "00 a 05"){
      nuevo.modelo.obj <- Arima(serie.trans, xreg = matriz.X, include.mean = F, order = c(3,0,0), seasonal = list(order = c(3, 0, 0), period = 4),
	optim.control = list(maxit= 10000))
    }
  }
  else
    nuevo.modelo.obj <- Arima(serie.trans, xreg = matriz.X, include.mean = F, model = modelo)
    
  
  pronostico <- forecast(modelo, h = 4, xreg = matriz.X.new)
  pronostico.vector <- modelo$trans.inv(pronostico$mean)
  pronostico$descripcion <- modelo$descripcion
  
  attr(pronostico.vector, "pred.limits") <- modelo$trans.inv(data.frame(lower=pronostico$lower[,2], upper = pronostico$upper[,2]))
  attr(pronostico.vector, "descripcion") <- modelo$descripcion
  
  return(pronostico.vector)
}

Xreg.ArimaSIMA.PM10Catarina <- function(modelo, tabla.periodos, modelos.var.met){

  require(tree)
  
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

    pron.periodo <- factor(pronostico.periodo(ultimo.periodo), levels = c("00 a 05", "06 a 11", "12 a 17", "18 a 23"))
    pron.fecha <- as.Date(pronostico.fecha(ultima.fecha, pron.periodo))
    pron.estacion <- getSeason(pron.fecha)
    pron.estacion <- factor(pron.estacion, levels = c("Invierno", "Otono", "Primavera", "Verano"))
    mes <- factor(months(pron.fecha), levels = c("January", "February", "March", "April", "May", "June", "July",
      "August", "September", "October", "November", "December"))
    diaSem <- factor(weekdays(pron.fecha), levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
    data.frame.temp <- data.frame(estacion = pron.estacion, periodo = pron.periodo, mes = mes, diaSem = diaSem)
    return(data.frame.temp)
  } 

  pronostico.xreg.met <- function(tabla.periodos){
#    load("R/modelos de pronostico/modelos_pronostico_met_O3_obispado.RData")
   load(modelos.var.met)
   pronostico.univ.met <- function(serie, modelo.arima){
    require(forecast)
    serie.trans <- modelo.arima$trans(serie)
    nuevo.modelo.obj <- Arima(serie.trans, model = modelo.arima)
    pronostico.trans <- forecast(nuevo.modelo.obj, h = 4)$mean
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
#     seno <- sin(pron.WDR*pi/180)
#     coseno <- cos(pron.WDR*pi/180)
#     angulo <- -60
#     R <- matrix(c(cos(angulo*pi/180), sin(angulo*pi/180), -sin(angulo*pi/180), cos(angulo*pi/180)), 2, 2)
#     rotados <- R%*%rbind(coseno, seno)
    pron.WDR.rot  <- pron.WDR
    pron.WDR.rot[pron.WDR >= 180]  <- abs(pron.WDR[pron.WDR >= 180]-360) 
    
    data.frame.met <- data.frame(TOUT = pron.TOUT, HR = pron.HR, WS = pron.WS, SR = pron.SR, WDR = pron.WDR, WDR.rot = pron.WDR.rot)
    return(data.frame.met)
  }

  data.frame.pron.temp <- pronostico.xreg.temp(ultima.fecha = ult.renglon[,"fecha"], ultimo.periodo = ult.renglon[,"periodo"])
  data.frame.pron.met <- pronostico.xreg.met(tabla.periodos)

  contaminante <- tabla.periodos[ , modelo$contaminante]
  PM10.76 <- contaminante >= 76
  WDR <- tabla.periodos$WDR
#   seno <- sin(WDR*pi/180)
#   coseno <- cos(WDR*pi/180)
#   angulo <- -60
#   R <- matrix(c(cos(angulo*pi/180), sin(angulo*pi/180), -sin(angulo*pi/180), cos(angulo*pi/180)), 2, 2)
#   rotados <- R%*%rbind(coseno, seno)
  WDR.rot  <- WDR
  WDR.rot[WDR >= 180]  <- abs(WDR[WDR >= 180]-360)
  
  mes <- factor(tabla.periodos$mes, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
  diaSem <- factor(tabla.periodos$diaSem, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  df <- data.frame(diaSem = tabla.periodos$diaSem, estacion = tabla.periodos$estacion, periodo = tabla.periodos$periodo, mes = mes, TOUT = tabla.periodos$TOUT,
    HR = tabla.periodos$HR, SR = tabla.periodos$SR, WS = tabla.periodos$WS, WDR = tabla.periodos$WDR, WDR.rot = WDR.rot)

  ##tree clasification
  try.res <- try({tree(as.factor(PM10.76) ~ estacion + mes + diaSem + periodo + TOUT + HR + I(HR^2) + I(HR^3) + WS + I(WS^2) + SR + I(SR^2) + WDR,
    data = df, split = "gini")}, silent = TRUE)
  if(!inherits(try.res, "try-error")){
    tree.fit <- try.res
    tree.fit.pred <- predict(tree.fit, newdata = df, type = "class")
    tree.fit.pred <- factor(tree.fit.pred, levels = c("FALSE", "TRUE"))
    df.new <- data.frame(data.frame.pron.temp, data.frame.pron.met)
    tree.pred <- factor(predict(tree.fit, newdata = df.new, type = "class"), levels = c("FALSE", "TRUE"))    
  }
    else{
      glm.fitt <- glm(as.factor(PM10.76) ~ estacion + mes + diaSem + periodo + TOUT + HR + I(HR^2) + I(HR^3) + WS + I(WS^2) + SR + I(SR^2) + WDR,
	family = binomial(link = logit), data = df)
	tree.fit.pred <- factor(.5 < predict(glm.fitt, newdata = df, type = "response"), levels = c("FALSE", "TRUE"))
	df.new <- data.frame(data.frame.pron.temp, data.frame.pron.met)
	tree.pred <- factor(.5 < predict(glm.fitt, newdata = df.new, type = "response"), levels = c("FALSE", "TRUE"))	
    }

  X <- model.matrix( ~ mes + diaSem + periodo + TOUT + HR + I(HR^2) + I(HR^3) + WS + I(WS^2) + SR + I(SR^2) + WDR.rot + I(WDR.rot^2) + tree.pred,
    data = cbind(df, tree.pred = tree.fit.pred))
  X.new <- model.matrix( ~ mes + diaSem + periodo + TOUT + HR + I(HR^2) + I(HR^3) + WS + I(WS^2) + SR + I(SR^2) + WDR.rot + I(WDR.rot^2) + tree.pred,
    data = cbind(df.new, tree.pred = tree.pred))
  return(list(X = X, X.new = X.new, tree.df = df))
}

pronostico.ArimaSIMA.PM10Catarina <- function(modelo, tabla.periodos, modelos.var.met, estimacion = FALSE, ...){
  require(forecast)
  
  matriz.X <- Xreg(modelo, tabla.periodos, modelos.var.met)$X
  matriz.X.new <- Xreg(modelo, tabla.periodos, modelos.var.met)$X.new
  contaminante <- tabla.periodos[ , modelo$contaminante]
  serie.trans <- modelo$trans(contaminante)
  if(estimacion){
    if(tabla.periodos$periodo[nrow(tabla.periodos)] == "00 a 05"){
      nuevo.modelo.obj <- Arima(serie.trans, xreg = matriz.X, include.mean = F, order = c(4,0,0), seasonal = list(order = c(1, 0, 0), period = 8),
	optim.control = list(maxit= 10000))
    }
  }
  else
    nuevo.modelo.obj <- Arima(serie.trans, xreg = matriz.X, include.mean = F, model = modelo)
    
  
  pronostico <- forecast(modelo, h = 4, xreg = matriz.X.new)
  pronostico.vector <- modelo$trans.inv(pronostico$mean)
  pronostico$descripcion <- modelo$descripcion
  
  attr(pronostico.vector, "pred.limits") <- modelo$trans.inv(data.frame(lower=pronostico$lower[,2], upper = pronostico$upper[,2]))
  attr(pronostico.vector, "descripcion") <- modelo$descripcion
  
  return(pronostico.vector)
}


###Nuevos metodos: Bernabe
Xreg.ArimaSIMA.O3Bernabe <- function(modelo, tabla.periodos, modelos.var.met){

  require(tree)
  
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

    pron.periodo <- factor(pronostico.periodo(ultimo.periodo), levels = c("00 a 05", "06 a 11", "12 a 17", "18 a 23"))
    pron.fecha <- as.Date(pronostico.fecha(ultima.fecha, pron.periodo))
    pron.estacion <- getSeason(pron.fecha)
    pron.estacion <- factor(pron.estacion, levels = c("Invierno", "Otono", "Primavera", "Verano"))
    mes <- factor(months(pron.fecha), levels = c("January", "February", "March", "April", "May", "June", "July",
      "August", "September", "October", "November", "December"))
#     matriz.X.temp <- model.matrix(~ pron.estacion + pron.periodo + mes)
#     attr(matriz.X.temp, "data.frame.temp") <- data.frame(estacion = pron.estacion, periodo = pron.periodo, mes = mes)
#     return(matriz.X.temp)
    data.frame.temp <- data.frame(estacion = pron.estacion, periodo = pron.periodo, mes = mes)
    return(data.frame.temp)
  } 

  pronostico.xreg.met <- function(tabla.periodos){
#    load("R/modelos de pronostico/modelos_pronostico_met_O3_obispado.RData")
   load(modelos.var.met)
   pronostico.univ.met <- function(serie, modelo.arima){
    require(forecast)
    serie.trans <- modelo.arima$trans(serie)
    nuevo.modelo.obj <- Arima(serie.trans, model = modelo.arima)
    pronostico.trans <- forecast(nuevo.modelo.obj, h = 4)$mean
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
    seno <- sin(pron.WDR*pi/180)
    coseno <- cos(pron.WDR*pi/180)
    angulo <- -100
    R <- matrix(c(cos(angulo*pi/180), sin(angulo*pi/180), -sin(angulo*pi/180), cos(angulo*pi/180)), 2, 2)
    rotados <- R%*%rbind(coseno, seno)
    pron.WDR.rot  <- abs(atan2(rotados[2, ], rotados[1, ])*180/pi)
    data.frame.met <- data.frame(TOUT = pron.TOUT, HR = pron.HR, WS = pron.WS, SR = pron.SR, WDR = pron.WDR, WDR.rot = pron.WDR.rot)
    return(data.frame.met)
  }

  data.frame.pron.temp <- pronostico.xreg.temp(ultima.fecha = ult.renglon[,"fecha"], ultimo.periodo = ult.renglon[,"periodo"])
  data.frame.pron.met <- pronostico.xreg.met(tabla.periodos)

  contaminante <- tabla.periodos[ , modelo$contaminante]
  O3.96 <- contaminante >= 96
  WDR <- tabla.periodos$WDR
  seno <- sin(WDR*pi/180)
  coseno <- cos(WDR*pi/180)
  angulo <- -100
  R <- matrix(c(cos(angulo*pi/180), sin(angulo*pi/180), -sin(angulo*pi/180), cos(angulo*pi/180)), 2, 2)
  rotados <- R%*%rbind(coseno, seno)
  WDR.rot  <- abs(atan2(rotados[2, ], rotados[1, ])*180/pi)
  mes <- factor(tabla.periodos$mes, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
  
  df <- data.frame(estacion = tabla.periodos$estacion, periodo = tabla.periodos$periodo, mes = mes, TOUT = tabla.periodos$TOUT,
    HR = tabla.periodos$HR, SR = tabla.periodos$SR, WS = tabla.periodos$WS, WDR = tabla.periodos$WDR, WDR.rot = WDR.rot)

  ##tree clasification
  try.res <- try({tree(as.factor(O3.96) ~ estacion + mes + periodo + I(log(TOUT+273.15+1)) + HR + I(HR^2) + I(HR^3) + I(1/(WS+1)) + SR + I(SR^2) + WDR,
    data = df, split = "gini")}, silent = TRUE)
  if(!inherits(try.res, "try-error")){
    tree.fit <- try.res
    tree.fit.pred <- predict(tree.fit, newdata = df, type = "class")
    tree.fit.pred <- factor(tree.fit.pred, levels = c("FALSE", "TRUE"))
    df.new <- data.frame(data.frame.pron.temp, data.frame.pron.met)
    tree.pred <- factor(predict(tree.fit, newdata = df.new, type = "class"), levels = c("FALSE", "TRUE"))    
  }
    else{
      glm.fitt <- glm(as.factor(O3.96) ~ estacion + mes + periodo + I(log(TOUT+273.15+1)) + HR + I(HR^2) + I(HR^3) + I(1/(WS+1)) + SR + I(SR^2) + WDR,
	family = binomial(link = logit), data = df)
	tree.fit.pred <- factor(.5 < predict(glm.fitt, newdata = df, type = "response"), levels = c("FALSE", "TRUE"))
	df.new <- data.frame(data.frame.pron.temp, data.frame.pron.met)
	tree.pred <- factor(.5 < predict(glm.fitt, newdata = df.new, type = "response"), levels = c("FALSE", "TRUE"))	
    }

  X <- model.matrix( ~ mes + periodo + I(log(TOUT+273.15+1)) + HR + I(HR^2) + I(HR^3) + I(1/(WS+1)) + SR + I(SR^2) + WDR.rot + tree.pred,
    data = cbind(df, tree.pred = tree.fit.pred))
  X.new <- model.matrix( ~ mes + periodo + I(log(TOUT+273.15+1)) + HR + I(HR^2) + I(HR^3) + I(1/(WS+1)) + SR + I(SR^2) + WDR.rot + tree.pred,
    data = cbind(df.new, tree.pred = tree.pred))
  return(list(X = X, X.new = X.new, tree.df = df))
}

pronostico.ArimaSIMA.O3Bernabe <- function(modelo, tabla.periodos, modelos.var.met, estimacion = FALSE, ...){
  require(forecast)
  
  matriz.X <- Xreg(modelo, tabla.periodos, modelos.var.met)$X
  matriz.X.new <- Xreg(modelo, tabla.periodos, modelos.var.met)$X.new
  contaminante <- tabla.periodos[ , modelo$contaminante]
  serie.trans <- modelo$trans(contaminante)
  if(estimacion){
    if(tabla.periodos$periodo[nrow(tabla.periodos)] == "00 a 05"){
      nuevo.modelo.obj <- Arima(serie.trans, xreg = matriz.X, include.mean = F, order = c(3,0,0), seasonal = list(order = c(3, 0, 0), period = 4),
	optim.control = list(maxit= 10000))
    }
  }
  else
    nuevo.modelo.obj <- Arima(serie.trans, xreg = matriz.X, include.mean = F, model = modelo)
    
  
  pronostico <- forecast(modelo, h = 4, xreg = matriz.X.new)
  pronostico.vector <- modelo$trans.inv(pronostico$mean)
  pronostico$descripcion <- modelo$descripcion
  
  attr(pronostico.vector, "pred.limits") <- modelo$trans.inv(data.frame(lower=pronostico$lower[,2], upper = pronostico$upper[,2]))
  attr(pronostico.vector, "descripcion") <- modelo$descripcion
  
  return(pronostico.vector)
}

Xreg.ArimaSIMA.PM10Bernabe <- function(modelo, tabla.periodos, modelos.var.met){

  require(tree)
  
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

    pron.periodo <- factor(pronostico.periodo(ultimo.periodo), levels = c("00 a 05", "06 a 11", "12 a 17", "18 a 23"))
    pron.fecha <- as.Date(pronostico.fecha(ultima.fecha, pron.periodo))
    pron.estacion <- getSeason(pron.fecha)
    pron.estacion <- factor(pron.estacion, levels = c("Invierno", "Otono", "Primavera", "Verano"))
    mes <- factor(months(pron.fecha), levels = c("January", "February", "March", "April", "May", "June", "July",
      "August", "September", "October", "November", "December"))
    diaSem <- factor(weekdays(pron.fecha), levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
    data.frame.temp <- data.frame(estacion = pron.estacion, periodo = pron.periodo, mes = mes, diaSem = diaSem)
    return(data.frame.temp)
  } 

  pronostico.xreg.met <- function(tabla.periodos){
#    load("R/modelos de pronostico/modelos_pronostico_met_O3_obispado.RData")
   load(modelos.var.met)
   pronostico.univ.met <- function(serie, modelo.arima){
    require(forecast)
    serie.trans <- modelo.arima$trans(serie)
    nuevo.modelo.obj <- Arima(serie.trans, model = modelo.arima)
    pronostico.trans <- forecast(nuevo.modelo.obj, h = 4)$mean
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
#     seno <- sin(pron.WDR*pi/180)
#     coseno <- cos(pron.WDR*pi/180)
#     angulo <- -60
#     R <- matrix(c(cos(angulo*pi/180), sin(angulo*pi/180), -sin(angulo*pi/180), cos(angulo*pi/180)), 2, 2)
#     rotados <- R%*%rbind(coseno, seno)
    pron.WDR.rot  <- pron.WDR
    pron.WDR.rot[pron.WDR >= 180]  <- abs(pron.WDR[pron.WDR >= 180]-360) 
    
    data.frame.met <- data.frame(TOUT = pron.TOUT, HR = pron.HR, WS = pron.WS, SR = pron.SR, WDR = pron.WDR, WDR.rot = pron.WDR.rot)
    return(data.frame.met)
  }

  data.frame.pron.temp <- pronostico.xreg.temp(ultima.fecha = ult.renglon[,"fecha"], ultimo.periodo = ult.renglon[,"periodo"])
  data.frame.pron.met <- pronostico.xreg.met(tabla.periodos)

  contaminante <- tabla.periodos[ , modelo$contaminante]
  PM10.76 <- contaminante >= 76
  WDR <- tabla.periodos$WDR
#   seno <- sin(WDR*pi/180)
#   coseno <- cos(WDR*pi/180)
#   angulo <- -60
#   R <- matrix(c(cos(angulo*pi/180), sin(angulo*pi/180), -sin(angulo*pi/180), cos(angulo*pi/180)), 2, 2)
#   rotados <- R%*%rbind(coseno, seno)
  WDR.rot  <- WDR
  WDR.rot[WDR >= 180]  <- abs(WDR[WDR >= 180]-360)
  
  mes <- factor(tabla.periodos$mes, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
  diaSem <- factor(tabla.periodos$diaSem, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  df <- data.frame(diaSem = tabla.periodos$diaSem, estacion = tabla.periodos$estacion, periodo = tabla.periodos$periodo, mes = mes, TOUT = tabla.periodos$TOUT,
    HR = tabla.periodos$HR, SR = tabla.periodos$SR, WS = tabla.periodos$WS, WDR = tabla.periodos$WDR, WDR.rot = WDR.rot)

  ##tree clasification
  try.res <- try({tree(as.factor(PM10.76) ~ estacion + mes + diaSem + periodo + TOUT + HR + I(HR^2) + I(HR^3) + WS + I(WS^2) + SR + I(SR^2) + WDR,
    data = df, split = "gini")}, silent = TRUE)
  if(!inherits(try.res, "try-error")){
    tree.fit <- try.res
    tree.fit.pred <- predict(tree.fit, newdata = df, type = "class")
    tree.fit.pred <- factor(tree.fit.pred, levels = c("FALSE", "TRUE"))
    df.new <- data.frame(data.frame.pron.temp, data.frame.pron.met)
    tree.pred <- factor(predict(tree.fit, newdata = df.new, type = "class"), levels = c("FALSE", "TRUE"))    
  }
    else{
      glm.fitt <- glm(as.factor(PM10.76) ~ estacion + mes + diaSem + periodo + TOUT + HR + I(HR^2) + I(HR^3) + WS + I(WS^2) + SR + I(SR^2) + WDR,
	family = binomial(link = logit), data = df)
	tree.fit.pred <- factor(.5 < predict(glm.fitt, newdata = df, type = "response"), levels = c("FALSE", "TRUE"))
	df.new <- data.frame(data.frame.pron.temp, data.frame.pron.met)
	tree.pred <- factor(.5 < predict(glm.fitt, newdata = df.new, type = "response"), levels = c("FALSE", "TRUE"))	
    }

  X <- model.matrix( ~ mes + diaSem + periodo + TOUT + HR + I(HR^2) + I(HR^3) + WS + I(WS^2) + SR + I(SR^2) + WDR.rot + I(WDR.rot^2) + tree.pred,
    data = cbind(df, tree.pred = tree.fit.pred))
  X.new <- model.matrix( ~ mes + diaSem + periodo + TOUT + HR + I(HR^2) + I(HR^3) + WS + I(WS^2) + SR + I(SR^2) + WDR.rot + I(WDR.rot^2) + tree.pred,
    data = cbind(df.new, tree.pred = tree.pred))
  return(list(X = X, X.new = X.new, tree.df = df))
}

pronostico.ArimaSIMA.PM10Bernabe <- function(modelo, tabla.periodos, modelos.var.met, estimacion = FALSE, ...){
  require(forecast)
  
  matriz.X <- Xreg(modelo, tabla.periodos, modelos.var.met)$X
  matriz.X.new <- Xreg(modelo, tabla.periodos, modelos.var.met)$X.new
  contaminante <- tabla.periodos[ , modelo$contaminante]
  serie.trans <- modelo$trans(contaminante)
  if(estimacion){
    if(tabla.periodos$periodo[nrow(tabla.periodos)] == "00 a 05"){
      nuevo.modelo.obj <- Arima(serie.trans, xreg = matriz.X, include.mean = F, order = c(4,0,0), seasonal = list(order = c(1, 0, 0), period = 8),
	optim.control = list(maxit= 10000))
    }
  }
  else
    nuevo.modelo.obj <- Arima(serie.trans, xreg = matriz.X, include.mean = F, model = modelo)
    
  
  pronostico <- forecast(modelo, h = 4, xreg = matriz.X.new)
  pronostico.vector <- modelo$trans.inv(pronostico$mean)
  pronostico$descripcion <- modelo$descripcion
  
  attr(pronostico.vector, "pred.limits") <- modelo$trans.inv(data.frame(lower=pronostico$lower[,2], upper = pronostico$upper[,2]))
  attr(pronostico.vector, "descripcion") <- modelo$descripcion

  return(pronostico.vector)
}

###Nuevos metodos: Nicolas
Xreg.ArimaSIMA.O3Nicolas <- function(modelo, tabla.periodos, modelos.var.met){

  require(tree)
  
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

    pron.periodo <- factor(pronostico.periodo(ultimo.periodo), levels = c("00 a 05", "06 a 11", "12 a 17", "18 a 23"))
    pron.fecha <- as.Date(pronostico.fecha(ultima.fecha, pron.periodo))
    pron.estacion <- getSeason(pron.fecha)
    pron.estacion <- factor(pron.estacion, levels = c("Invierno", "Otono", "Primavera", "Verano"))
    mes <- factor(months(pron.fecha), levels = c("January", "February", "March", "April", "May", "June", "July",
      "August", "September", "October", "November", "December"))
#     matriz.X.temp <- model.matrix(~ pron.estacion + pron.periodo + mes)
#     attr(matriz.X.temp, "data.frame.temp") <- data.frame(estacion = pron.estacion, periodo = pron.periodo, mes = mes)
#     return(matriz.X.temp)
    data.frame.temp <- data.frame(estacion = pron.estacion, periodo = pron.periodo, mes = mes)
    return(data.frame.temp)
  } 

  pronostico.xreg.met <- function(tabla.periodos){
#    load("R/modelos de pronostico/modelos_pronostico_met_O3_obispado.RData")
   load(modelos.var.met)
   pronostico.univ.met <- function(serie, modelo.arima){
    require(forecast)
    serie.trans <- modelo.arima$trans(serie)
    nuevo.modelo.obj <- Arima(serie.trans, model = modelo.arima)
    pronostico.trans <- forecast(nuevo.modelo.obj, h = 4)$mean
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
    seno <- sin(pron.WDR*pi/180)
    coseno <- cos(pron.WDR*pi/180)
    angulo <- -100
    R <- matrix(c(cos(angulo*pi/180), sin(angulo*pi/180), -sin(angulo*pi/180), cos(angulo*pi/180)), 2, 2)
    rotados <- R%*%rbind(coseno, seno)
    pron.WDR.rot  <- abs(atan2(rotados[2, ], rotados[1, ])*180/pi)
    data.frame.met <- data.frame(TOUT = pron.TOUT, HR = pron.HR, WS = pron.WS, SR = pron.SR, WDR = pron.WDR, WDR.rot = pron.WDR.rot)
    return(data.frame.met)
  }

  data.frame.pron.temp <- pronostico.xreg.temp(ultima.fecha = ult.renglon[,"fecha"], ultimo.periodo = ult.renglon[,"periodo"])
  data.frame.pron.met <- pronostico.xreg.met(tabla.periodos)

  contaminante <- tabla.periodos[ , modelo$contaminante]
  O3.96 <- contaminante >= 96
  WDR <- tabla.periodos$WDR
  seno <- sin(WDR*pi/180)
  coseno <- cos(WDR*pi/180)
  angulo <- -100
  R <- matrix(c(cos(angulo*pi/180), sin(angulo*pi/180), -sin(angulo*pi/180), cos(angulo*pi/180)), 2, 2)
  rotados <- R%*%rbind(coseno, seno)
  WDR.rot  <- abs(atan2(rotados[2, ], rotados[1, ])*180/pi)
  mes <- factor(tabla.periodos$mes, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
  
  df <- data.frame(estacion = tabla.periodos$estacion, periodo = tabla.periodos$periodo, mes = mes, TOUT = tabla.periodos$TOUT,
    HR = tabla.periodos$HR, SR = tabla.periodos$SR, WS = tabla.periodos$WS, WDR = tabla.periodos$WDR, WDR.rot = WDR.rot)

  ##tree clasification
  try.res <- try({tree(as.factor(O3.96) ~ estacion + mes + periodo + I(log(TOUT+273.15+1)) + HR + I(HR^2) + I(HR^3) + I(1/(WS+1)) + SR + I(SR^2) + WDR,
    data = df, split = "gini")}, silent = TRUE)
  if(!inherits(try.res, "try-error")){
    tree.fit <- try.res
    tree.fit.pred <- predict(tree.fit, newdata = df, type = "class")
    tree.fit.pred <- factor(tree.fit.pred, levels = c("FALSE", "TRUE"))
    df.new <- data.frame(data.frame.pron.temp, data.frame.pron.met)
    tree.pred <- factor(predict(tree.fit, newdata = df.new, type = "class"), levels = c("FALSE", "TRUE"))    
  }
    else{
      glm.fitt <- glm(as.factor(O3.96) ~ estacion + mes + periodo + I(log(TOUT+273.15+1)) + HR + I(HR^2) + I(HR^3) + I(1/(WS+1)) + SR + I(SR^2) + WDR,
	family = binomial(link = logit), data = df)
	tree.fit.pred <- factor(.5 < predict(glm.fitt, newdata = df, type = "response"), levels = c("FALSE", "TRUE"))
	df.new <- data.frame(data.frame.pron.temp, data.frame.pron.met)
	tree.pred <- factor(.5 < predict(glm.fitt, newdata = df.new, type = "response"), levels = c("FALSE", "TRUE"))	
    }

  X <- model.matrix( ~ mes + periodo + I(log(TOUT+273.15+1)) + HR + I(HR^2) + I(HR^3) + I(1/(WS+1)) + SR + I(SR^2) + WDR.rot + tree.pred,
    data = cbind(df, tree.pred = tree.fit.pred))
  X.new <- model.matrix( ~ mes + periodo + I(log(TOUT+273.15+1)) + HR + I(HR^2) + I(HR^3) + I(1/(WS+1)) + SR + I(SR^2) + WDR.rot + tree.pred,
    data = cbind(df.new, tree.pred = tree.pred))
  return(list(X = X, X.new = X.new, tree.df = df))
}

pronostico.ArimaSIMA.O3Nicolas <- function(modelo, tabla.periodos, modelos.var.met, estimacion = FALSE, ...){
  require(forecast)
  
  matriz.X <- Xreg(modelo, tabla.periodos, modelos.var.met)$X
  matriz.X.new <- Xreg(modelo, tabla.periodos, modelos.var.met)$X.new
  contaminante <- tabla.periodos[ , modelo$contaminante]
  serie.trans <- modelo$trans(contaminante)
  if(estimacion){
    if(tabla.periodos$periodo[nrow(tabla.periodos)] == "00 a 05"){
      nuevo.modelo.obj <- Arima(serie.trans, xreg = matriz.X, include.mean = F, order = c(3,0,0), seasonal = list(order = c(3, 0, 0), period = 4),
	optim.control = list(maxit= 10000))
    }
  }
  else
    nuevo.modelo.obj <- Arima(serie.trans, xreg = matriz.X, include.mean = F, model = modelo)
    
  
  pronostico <- forecast(modelo, h = 4, xreg = matriz.X.new)
  pronostico.vector <- modelo$trans.inv(pronostico$mean)
  pronostico$descripcion <- modelo$descripcion
  
  attr(pronostico.vector, "pred.limits") <- modelo$trans.inv(data.frame(lower=pronostico$lower[,2], upper = pronostico$upper[,2]))
  attr(pronostico.vector, "descripcion") <- modelo$descripcion

  return(pronostico.vector)
}

Xreg.ArimaSIMA.PM10Nicolas <- function(modelo, tabla.periodos, modelos.var.met){

  require(tree)
  
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

    pron.periodo <- factor(pronostico.periodo(ultimo.periodo), levels = c("00 a 05", "06 a 11", "12 a 17", "18 a 23"))
    pron.fecha <- as.Date(pronostico.fecha(ultima.fecha, pron.periodo))
    pron.estacion <- getSeason(pron.fecha)
    pron.estacion <- factor(pron.estacion, levels = c("Invierno", "Otono", "Primavera", "Verano"))
    mes <- factor(months(pron.fecha), levels = c("January", "February", "March", "April", "May", "June", "July",
      "August", "September", "October", "November", "December"))
    diaSem <- factor(weekdays(pron.fecha), levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
    data.frame.temp <- data.frame(estacion = pron.estacion, periodo = pron.periodo, mes = mes, diaSem = diaSem)
    return(data.frame.temp)
  } 

  pronostico.xreg.met <- function(tabla.periodos){
#    load("R/modelos de pronostico/modelos_pronostico_met_O3_obispado.RData")
   load(modelos.var.met)
   pronostico.univ.met <- function(serie, modelo.arima){
    require(forecast)
    serie.trans <- modelo.arima$trans(serie)
    nuevo.modelo.obj <- Arima(serie.trans, model = modelo.arima)
    pronostico.trans <- forecast(nuevo.modelo.obj, h = 4)$mean
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
#     seno <- sin(pron.WDR*pi/180)
#     coseno <- cos(pron.WDR*pi/180)
#     angulo <- -60
#     R <- matrix(c(cos(angulo*pi/180), sin(angulo*pi/180), -sin(angulo*pi/180), cos(angulo*pi/180)), 2, 2)
#     rotados <- R%*%rbind(coseno, seno)
    pron.WDR.rot  <- pron.WDR
    pron.WDR.rot[pron.WDR >= 180]  <- abs(pron.WDR[pron.WDR >= 180]-360) 
    
    data.frame.met <- data.frame(TOUT = pron.TOUT, HR = pron.HR, WS = pron.WS, SR = pron.SR, WDR = pron.WDR, WDR.rot = pron.WDR.rot)
    return(data.frame.met)
  }

  data.frame.pron.temp <- pronostico.xreg.temp(ultima.fecha = ult.renglon[,"fecha"], ultimo.periodo = ult.renglon[,"periodo"])
  data.frame.pron.met <- pronostico.xreg.met(tabla.periodos)

  contaminante <- tabla.periodos[ , modelo$contaminante]
  PM10.76 <- contaminante >= 76
  WDR <- tabla.periodos$WDR
#   seno <- sin(WDR*pi/180)
#   coseno <- cos(WDR*pi/180)
#   angulo <- -60
#   R <- matrix(c(cos(angulo*pi/180), sin(angulo*pi/180), -sin(angulo*pi/180), cos(angulo*pi/180)), 2, 2)
#   rotados <- R%*%rbind(coseno, seno)
  WDR.rot  <- WDR
  WDR.rot[WDR >= 180]  <- abs(WDR[WDR >= 180]-360)
  
  mes <- factor(tabla.periodos$mes, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
  diaSem <- factor(tabla.periodos$diaSem, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  df <- data.frame(diaSem = tabla.periodos$diaSem, estacion = tabla.periodos$estacion, periodo = tabla.periodos$periodo, mes = mes, TOUT = tabla.periodos$TOUT,
    HR = tabla.periodos$HR, SR = tabla.periodos$SR, WS = tabla.periodos$WS, WDR = tabla.periodos$WDR, WDR.rot = WDR.rot)

  ##tree clasification
  try.res <- try({tree(as.factor(PM10.76) ~ estacion + mes + diaSem + periodo + TOUT + HR + I(HR^2) + I(HR^3) + WS + I(WS^2) + SR + I(SR^2) + WDR,
    data = df, split = "gini")}, silent = TRUE)
  if(!inherits(try.res, "try-error")){
    tree.fit <- try.res
    tree.fit.pred <- predict(tree.fit, newdata = df, type = "class")
    tree.fit.pred <- factor(tree.fit.pred, levels = c("FALSE", "TRUE"))
    df.new <- data.frame(data.frame.pron.temp, data.frame.pron.met)
    tree.pred <- factor(predict(tree.fit, newdata = df.new, type = "class"), levels = c("FALSE", "TRUE"))    
  }
    else{
      glm.fitt <- glm(as.factor(PM10.76) ~ estacion + mes + diaSem + periodo + TOUT + HR + I(HR^2) + I(HR^3) + WS + I(WS^2) + SR + I(SR^2) + WDR,
	family = binomial(link = logit), data = df)
	tree.fit.pred <- factor(.5 < predict(glm.fitt, newdata = df, type = "response"), levels = c("FALSE", "TRUE"))
	df.new <- data.frame(data.frame.pron.temp, data.frame.pron.met)
	tree.pred <- factor(.5 < predict(glm.fitt, newdata = df.new, type = "response"), levels = c("FALSE", "TRUE"))	
    }

  X <- model.matrix( ~ mes + diaSem + periodo + TOUT + HR + I(HR^2) + I(HR^3) + WS + I(WS^2) + SR + I(SR^2) + WDR.rot + I(WDR.rot^2) + tree.pred,
    data = cbind(df, tree.pred = tree.fit.pred))
  X.new <- model.matrix( ~ mes + diaSem + periodo + TOUT + HR + I(HR^2) + I(HR^3) + WS + I(WS^2) + SR + I(SR^2) + WDR.rot + I(WDR.rot^2) + tree.pred,
    data = cbind(df.new, tree.pred = tree.pred))
  return(list(X = X, X.new = X.new, tree.df = df))
}

pronostico.ArimaSIMA.PM10Nicolas <- function(modelo, tabla.periodos, modelos.var.met, estimacion = FALSE, ...){
  require(forecast)
  
  matriz.X <- Xreg(modelo, tabla.periodos, modelos.var.met)$X
  matriz.X.new <- Xreg(modelo, tabla.periodos, modelos.var.met)$X.new
  contaminante <- tabla.periodos[ , modelo$contaminante]
  serie.trans <- modelo$trans(contaminante)
  if(estimacion){
    if(tabla.periodos$periodo[nrow(tabla.periodos)] == "00 a 05"){
      nuevo.modelo.obj <- Arima(serie.trans, xreg = matriz.X, include.mean = F, order = c(4,0,0), seasonal = list(order = c(1, 0, 0), period = 8),
	optim.control = list(maxit= 10000))
    }
  }
  else
    nuevo.modelo.obj <- Arima(serie.trans, xreg = matriz.X, include.mean = F, model = modelo)
    
  
  pronostico <- forecast(modelo, h = 4, xreg = matriz.X.new)
  pronostico.vector <- modelo$trans.inv(pronostico$mean)
  pronostico$descripcion <- modelo$descripcion
  
  attr(pronostico.vector, "pred.limits") <- modelo$trans.inv(data.frame(lower=pronostico$lower[,2], upper = pronostico$upper[,2]))
  attr(pronostico.vector, "descripcion") <- modelo$descripcion
 
  return(pronostico.vector)
}
