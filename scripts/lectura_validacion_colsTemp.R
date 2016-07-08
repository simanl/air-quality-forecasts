READ <- function(x, ...)
  UseMethod("READ")

READ.default <- function(file, tm.ur, check = TRUE,  header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "", ...){
  mc <- as.list(match.call(expand.dots = T))
  mc$file <- file
  tabla <- do.call("read.csv", mc[-c(1L, 3L)])
  if(check) init.check(tabla, tm.ur)
  attr(tabla, "validacion") <- FALSE
  attr(tabla, "completa") <- FALSE
  class(tabla) <- c("datos.SIMA", "data.frame")
  return(tabla)
}

READ.data.frame <- function(x, tm.ur, check = TRUE, ...){
  if(check) init.check(x, tm.ur)
  attr(x, "validacion") <- FALSE
  attr(x, "completa") <- FALSE
  class(x) <- c("datos.SIMA", "data.frame")
  return(x)
}

# print.datos.SIMA <- function(x, ...){
#   print(x[])
# }

validacion <- function(x, ...)
  UseMethod("validacion")
  
validacion.datos.SIMA <- function(x, ...){
  ##QUeda pediente toda la programacion que modifica cosas de robert e ivan da origen a la tabla maestra final
  
  ## valores por abajo de 0, se pone 0 salvo que en el script mencione
  ## otra cosa 
  if(any(x$CO < 0, na.rm = T)) x$CO[x$CO < 0] <- NA
  if(any(x$NO < 0, na.rm = T)) x$NO[x$NO < 0] <- NA
  if(any(x$NO2 < 0, na.rm = T)) x$NO2[x$NO2 < 0] <- NA
  if(any(x$NOX < 0, na.rm = T)) x$NOX[x$NOX < 0] <- NA
  if(any(x$NOX < 1, na.rm = T)) x$NOX[x$NOX < 1] <- NA  
  if(any(x$O3 < 0, na.rm = T)) x$O3[x$O3 < 0] <- NA
  if(any(x$PM10 < 0, na.rm = T)) x$PM10[x$PM10 < 0] <- NA
  if(any(x$PM2.5 < 0, na.rm = T)) x$PM2.5[x$PM2.5 < 0] <- NA
  #if(any(x$PRS < 0, na.rm = T)) x$PRS[x$PRS < 0] <- 0
  if(any(x$RAINF < 0, na.rm = T)) x$RAINF[x$RAINF < 0] <- NA
  if(any(x$HR < 0, na.rm = T)) x$HR[x$HR < 0] <- NA
  if(any(x$SO2 < 0, na.rm = T)) x$SO2[x$SO2 < 0] <- NA
  if(any(x$SR < 0, na.rm = T)) x$SR[x$SR < 0] <- NA
  if(any(x$WS < 0, na.rm = T)) x$WS[x$WS < 0] <- NA
  if(any(x$WDR < 0, na.rm = T)) x$WDR[x$WDR < 0] <- NA
  
  ##valores por encima de cierto valor (donde aplique)
  if(any(x$HR > 100, na.rm = T)) x$HR[x$HR > 100] <- 100
  if(any(x$WDR > 360, na.rm = T)) x$WDR[x$WDR > 360] <- 360
  if(any(x$WS > 45,na.rm = T)) x$WS[x$WS > 45] <- 45
  if(any(x$RAINF > 10, na.rm = T)) x$RAINF[x$RAINF > 10] <- NA
  
  ## PRS abajo de 900 poner NA (revisar porque en ocasiones manejaban otras unidades)
  if(any(x$PRS < 900, na.rm = T)) x$PRS[x$PRS < 900] <- NA
  ## información en decimales (dejar?)
  if(any(x$O3 != floor(x$O3), na.rm = T)){
    cuales <- which(x$O3 != floor(x$O3))
    x$O3[cuales] <- NA
  }  
  
  if(any(x$PM10 != floor(x$PM10), na.rm = T)){
    cuales <- which(x$PM10 != floor(x$PM10))
    x$PM10[cuales] <- NA
  }
  
  if(any(x$PM2.5 != floor(x$PM2.5), na.rm = T)){
    cuales <- which(x$PM2.5 != floor(x$PM2.5))
    x$PM2.5[cuales] <- NA
  }
  
  attr(x, "validacion") <- TRUE
  return(x)
}

colsTemp <- function(x, ...)
  UseMethod("colsTemp")
  
colsTemp.datos.SIMA <- function(x, ...){
  x$fecha <- as.Date(x$fecha)
  dia <- format(x$fecha, format = "%d")
  mes <- factor(format(x$fecha, format = "%B"), levels = c("January", "February", "March", "April", "May", "June", "July",
    "August", "September", "October", "November", "December"))
  ano <- format(x$fecha, format = "%Y")
#   diaSem <- weekdays(x$fecha)
  diaSem <- factor(weekdays(x$fecha), levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  
  getSeason <- function(DATES) {
    WS <- as.Date("2012-12-21", format = "%Y-%m-%d") # Invierno
    SE <- as.Date("2012-3-21",  format = "%Y-%m-%d") # Primavera
    SS <- as.Date("2012-6-21",  format = "%Y-%m-%d") # Verano
    FE <- as.Date("2012-9-21",  format = "%Y-%m-%d") # Otoño
    
    # Convierte las fechas a la fecha correspondiente del 2012 (porque es bisiesto)
    d <- as.Date(strftime(DATES, format="2012-%m-%d"))
    
    ifelse (d >= WS | d < SE, "Invierno",
            ifelse (d >= SE & d < SS, "Primavera",
                    ifelse (d >= SS & d < FE, "Verano", "Otono")))
  }
  estacion <- getSeason(x$fecha)
 
  getSeasonMes <- function(DATES) {
    WS <- as.Date("2012-12-01", format = "%Y-%m-%d") # Invierno
    SE <- as.Date("2012-3-01",  format = "%Y-%m-%d") # Primavera
    SS <- as.Date("2012-6-01",  format = "%Y-%m-%d") # Verano
    FE <- as.Date("2012-9-01",  format = "%Y-%m-%d") # Otoño
    
    # Convierte las fechas a la fecha correspondiente del 2012 (porque es bisiesto)
    d <- as.Date(strftime(DATES, format="2012-%m-%d"))
    
    ifelse (d >= WS | d < SE, "Invierno",
            ifelse (d >= SE & d < SS, "Primavera",
                    ifelse (d >= SS & d < FE, "Verano", "Otono")))
  }
  estacion.mes <- getSeasonMes(x$fecha)

  dia <- as.numeric(dia)
  ano <- as.numeric(ano)
  
  aux <- mes == "December" & dia >= 21
  ano.temp <- ano
  ano.temp[aux] <- ano[aux] + 1
  estacion.ano <- paste(estacion, ano.temp, sep = "-")
  
  aux <- mes == "December"
  ano.temp <- ano
  ano.temp[aux] <- ano[aux] + 1 
  estacion.mes.ano <- paste(estacion.mes, ano.temp, sep = "-")
  
  dia.juliano <- julian(x$fecha, origin = as.Date("2009-01-01"))
  
  cuarto.dia <- function(x){
    if(x <= 5 & x >= 0){
      q <- "00 a 05"
    } else if (x >= 6 & x <= 11) {
      q <- "06 a 11"
    } else if (x >= 12 & x <= 17) {
      q <- "12 a 17"
    } else if (x >= 18 & x <= 23){
      q <- "18 a 23"
    }
    return(q) 
  }
  
  periodo <- sapply(x$hora, cuarto.dia)
  juliano.periodo <- paste(formatC(dia.juliano, width = 4, flag = "0"), periodo, sep = "_")
  colsTemp.df <- data.frame(dia, mes, ano, diaSem, estacion, estacion.ano, estacion.mes, estacion.mes.ano, dia.juliano, periodo, juliano.periodo)
  
# print(colnames(colsTemp.df))
  tabla <- data.frame(colsTemp.df, x)
# print(colnames(tabla))
  attr(tabla, "validacion") <- attr(x, "validacion")
  attr(tabla, "completa") <- TRUE
  class(tabla) <- class(x)
  return(tabla)
}

colsTemp.data.frame <- function(x, ...){
  x$fecha <- as.Date(x$fecha)
  dia <- format(x$fecha, format = "%d")
  mes <- factor(format(x$fecha, format = "%B"), levels = c("January", "February", "March", "April", "May", "June", "July",
    "August", "September", "October", "November", "December"))
  ano <- format(x$fecha, format = "%Y")
#   diaSem <- weekdays(x$fecha)
  diaSem <- factor(weekdays(x$fecha), levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  
  getSeason <- function(DATES) {
    WS <- as.Date("2012-12-21", format = "%Y-%m-%d") # Invierno
    SE <- as.Date("2012-3-21",  format = "%Y-%m-%d") # Primavera
    SS <- as.Date("2012-6-21",  format = "%Y-%m-%d") # Verano
    FE <- as.Date("2012-9-21",  format = "%Y-%m-%d") # Otoño
    
    # Convierte las fechas a la fecha correspondiente del 2012 (porque es bisiesto)
    d <- as.Date(strftime(DATES, format="2012-%m-%d"))
    
    ifelse (d >= WS | d < SE, "Invierno",
            ifelse (d >= SE & d < SS, "Primavera",
                    ifelse (d >= SS & d < FE, "Verano", "Otono")))
  }
  estacion <- getSeason(x$fecha)
 
  getSeasonMes <- function(DATES) {
    WS <- as.Date("2012-12-01", format = "%Y-%m-%d") # Invierno
    SE <- as.Date("2012-3-01",  format = "%Y-%m-%d") # Primavera
    SS <- as.Date("2012-6-01",  format = "%Y-%m-%d") # Verano
    FE <- as.Date("2012-9-01",  format = "%Y-%m-%d") # Otoño
    
    # Convierte las fechas a la fecha correspondiente del 2012 (porque es bisiesto)
    d <- as.Date(strftime(DATES, format="2012-%m-%d"))
    
    ifelse (d >= WS | d < SE, "Invierno",
            ifelse (d >= SE & d < SS, "Primavera",
                    ifelse (d >= SS & d < FE, "Verano", "Otono")))
  }
  estacion.mes <- getSeasonMes(x$fecha)

  dia <- as.numeric(dia)
  ano <- as.numeric(ano)
  
  aux <- mes == "December" & dia >= 21
  ano.temp <- ano
  ano.temp[aux] <- ano[aux] + 1
  estacion.ano <- paste(estacion, ano.temp, sep = "-")
  
  aux <- mes == "December"
  ano.temp <- ano
  ano.temp[aux] <- ano[aux] + 1 
  estacion.mes.ano <- paste(estacion.mes, ano.temp, sep = "-")
  
  dia.juliano <- julian(x$fecha, origin = as.Date("2009-01-01"))
  
  cuarto.dia <- function(x){
    if(x <= 5 & x >= 0){
      q <- "00 a 05"
    } else if (x >= 6 & x <= 11) {
      q <- "06 a 11"
    } else if (x >= 12 & x <= 17) {
      q <- "12 a 17"
    } else if (x >= 18 & x <= 23){
      q <- "18 a 23"
    }
    return(q) 
  }
  
  periodo <- sapply(x$hora, cuarto.dia)
  juliano.periodo <- paste(formatC(dia.juliano, width = 4, flag = "0"), periodo, sep = "_")
  # print(periodo)
  colsTemp.df <- data.frame(dia, mes, ano, diaSem, estacion, estacion.ano, estacion.mes, estacion.mes.ano, dia.juliano, periodo, juliano.periodo)
# print(colnames(colsTemp.df))
  noenx <- !(colnames(colsTemp.df) %in% colnames(x))
  tabla <- data.frame(colsTemp.df[, noenx], x)
# print(colnames(tabla))
  attr(tabla, "validacion") <- attr(x, "validacion")
  attr(tabla, "completa") <- TRUE
  class(tabla) <- class(x)
  return(tabla)
}

init.check <- function(tabla, tm.ur, ...)
  UseMethod("init.check")

init.check.default <- function(tabla, tm.ur, ...){
  tabla.nombres <- c("fecha","hora","sitio","CO","NO","NO2","NOX","O3","PM10","PM2.5","PRS","RAINF","HR","SO2","SR","TOUT","WS","WDR")
  if(nrow(tabla)!= 30) stop("Tabla de datos no es de seis horas por cinco sitios.") 
  if(ncol(tabla)!= 18) stop("Tabla de datos no tiene 18 columnas.")
  if(!all(colnames(tabla) %in% tabla.nombres)) stop("Nombres de columna no son los apropiados.")
  if(length(unique(tabla$sitio))!=5) stop("Tabla de datos no tiene exactamente 5 sitios.")
  
  fecha.tabla.maestra <- as.Date(tm.ur$fecha)
  hora.tabla.maestra <- as.integer(tm.ur$hora)

  ##chequeo de continuidad
  for(i in unique(tabla$sitio)){
    tabla.sitio <-tabla[tabla$sitio == i, ]
    if(nrow(tabla.sitio)!=6) stop("Al menos el Sitio", i, "no contiene 6 horas exactamente.")
    if(any(diff(tabla.sitio$hora) != 1)) stop(paste("Al menos el Sitio", i, "no contiene horas contiguas o en orden."))
    juliano.actual <- julian(as.Date(tabla.sitio$fecha))
# cat("\njuliano actual", juliano.actual)
    juliano.anterior <- julian(as.Date(fecha.tabla.maestra))
# cat("\njuliano anterior",juliano.anterior)
    if(length(unique(juliano.actual)) > 1) stop(paste("Al menos el Sitio", i, "contiene fechas con mas de una dia."))
    if(juliano.actual[1] - juliano.anterior > 1 || juliano.actual[1] - juliano.anterior < 0 ) stop(paste("Al menos el Sitio", i, "no contiene fechas (dias) consecutivas con el historico."))
     if(juliano.actual[1] - juliano.anterior == 1){
      if(hora.tabla.maestra != 23 || tabla.sitio$hora[1] != 0) stop(paste("Al menos el Sitio", i, "no contiene horas consecutivas con el historico (o el historico no se actualizo la ultima vez o la nueva tabla no es consecutiva)."))
    }
    if(juliano.actual[1] - juliano.anterior == 0){
      if(hora.tabla.maestra + 1 != tabla.sitio$hora[1]) stop(paste("Al menos el Sitio", i, "no contiene horas consecutivas con el historico."))
    }
  }
}
