## generar la clase que contenga la información necesaria para generar los periodos

stSIMA <- function(x, ...)
  UseMethod("stSIMA")

stSIMA.default <- function(tabla.horaria, contaminante = "O3", sitio = "Obispado", pesos, modelos.pronostico,
    trans = function(x) log(x + 1), trans.inv = function(x) exp(x) - 1){
  objeto <- list()
  
  if(missing(modelos.pronostico)) stop("Falta 'modelos', el cual no tiene valor predeterminado.")

  if(is.character(modelos.pronostico)){
    modelos.env <- new.env()
    load(modelos.pronostico, modelos.env)
    objeto$modelos.pronostico <- as.list(modelos.env)
  }
   else{
    if(is.list(modelos.pronostico)) objeto$modelos.pronostico <- modelos.pronostico
      else stop("'modelos' es una ruta al directorio de modelos o una lista no vacia con modelos de pronostico")
   }

   
  ##chequeo de modelos
  lapply(objeto$modelos.pronostico, function(x){
    if(sitio != x$sitio || contaminante != x$contaminante){
      cat("\nModelo de pronostico no es de ", contaminante, " y sitio ", sitio, ".\n", sep = "")
      stop()    
    }
  })

  tabla.horaria.sitio <- tabla.horaria[tabla.horaria$sitio == sitio, ]

  if(is.character(pesos)){
    pesos.csv <- read.csv(pesos)
    pesos <- pesos.csv[pesos.csv$sitio == sitio & pesos.csv$cont == contaminante, ]
  }
  else
    if(is.data.frame(pesos)) pesos <- pesos[pesos$sitio == sitio & pesos$cont == contaminante, ]

  objeto$tabla.periodos.sitio <- tabla.periodos(tabla.horaria.sitio, contaminante = contaminante, pesos = pesos) 
  objeto$sitio <- sitio
  objeto$contaminante <- contaminante
  objeto$trans <- trans
  objeto$trans.inv <- trans.inv
  objeto$tabla.horaria.sitio <- tabla.horaria.sitio
  objeto$pesos <- pesos  
  
  
  class(objeto) <- "stSIMA"
  serie <- paste(contaminante, sitio, sep = "")
  assign(serie[1], objeto)

#   if(!missing(image))
#     save(list = serie, file = image)
#     else
  return(objeto)
}

update.stSIMA <- function(x, tabla.horaria.nueva, modelos.pronostico, check = TRUE){
  require(data.table)
  if(!missing(tabla.horaria.nueva)){
    tabla.horaria.nueva.sitio <- tabla.horaria.nueva[tabla.horaria.nueva$sitio == x$sitio, ]
    
    ##chequeo de consistencia en fechas y horas
    if(check){
      ultimo.renglon.anterior <- x$tabla.horaria.sitio[nrow(x$tabla.horaria.sitio), ]
      primer.renglon.nuevo <- tabla.horaria.nueva.sitio[1, ]
      juliano.anterior <- julian(as.Date(ultimo.renglon.anterior$fecha))
      juliano.nuevo <- julian(as.Date(primer.renglon.nuevo$fecha))
      hora.anterior <- as.numeric(ultimo.renglon.anterior$hora)
      hora.nueva <- as.numeric(primer.renglon.nuevo$hora)
      mensaje <- paste(": Error en las tablas stSIMA; posible reinicializacion fallida del sistema. Consultar la seccion 5.12 Reinicializacion del Sistema de la Documentacion.")
      if(hora.anterior == 23){
	if(hora.nueva != 0 | juliano.anterior + 1 != juliano.nuevo) stop(paste("En sitio ", x$sitio, mensaje, sep = ""))
      }
      if(hora.anterior == 5){
	if(hora.nueva != 6 | juliano.anterior != juliano.nuevo) stop(paste("En sitio ", x$sitio, mensaje, sep = ""))
      }
      if(hora.anterior == 11){
	if(hora.nueva != 12 | juliano.anterior != juliano.nuevo) stop(paste("En sitio ", x$sitio, mensaje, sep = ""))
      }
      if(hora.anterior == 17){
	if(hora.nueva != 18 | juliano.anterior != juliano.nuevo) stop(paste("En sitio ", x$sitio, mensaje, sep = ""))
      }
    }
    x$tabla.horaria.sitio <- as.data.frame(rbindlist(list(x$tabla.horaria.sitio, tabla.horaria.nueva.sitio)))
    renglon.nuevo <- renglon.periodo(tabla.horaria.nueva.sitio, contaminante = x$contaminante, pesos = x$pesos)
    x$tabla.periodos.sitio <- as.data.frame(rbindlist(list(x$tabla.periodos.sitio, renglon.nuevo)))
  }
  else{
    if(!missing(modelos.pronostico)){
		if(is.character(modelos.pronostico)){
			modelos.env <- new.env()
			load(modelos.pronostico, modelos.env)
			x$modelos.pronostico <- as.list(modelos.env)
		}
		else{
			if(is.list(modelos.pronostico)) x$modelos.pronostico <- modelos.pronostico
			else stop("'modelos' es una ruta al directorio de modelos o una lista no vacia con modelos de pronostico")
		}
	}
    else
      print("Nada que actualizar!")
    }
  return(x)
}

print.stSIMA <- function(x){
  cat("\nSerie de tiempo horaria:", x$contaminante, x$sitio, "(6 horas del ultimo periodo)\n")
  print(tail(x$tabla.horaria.sitio, 6))
  cat("\nSerie de tiempo por periodo:", x$contaminante, x$sitio, "(ultimos 6 periodos)\n")
  print(tail(x$tabla.periodos.sitio, 6))
}
  
head.stSIMA <- function(x, n = 6L, tabla.horaria = TRUE, ...){
  if(tabla.horaria) return(head(x$tabla.horaria.sitio, n))
    else return(head(x$tabla.periodos.sitio, n))
}

tail.stSIMA <- function(x, n = 6L, tabla.horaria = TRUE, ...){
  if(tabla.horaria) return(tail(x$tabla.horaria.sitio, n))
    else return(tail(x$tabla.periodos.sitio, n))
}

summary.stSIMA <- function(x){
  ultimos <- tail(x, n = 1)
  cat("\nSitio:\n")
  cat(" ", x$sitio)
  cat("\nUltima fecha y hora:\n")
  cat(" ", paste(as.character(ultimos$fecha), ", ",  as.character(ultimos$hora), " horas.\n", sep = ""))
}

SAVE <- function(x, ...)
  UseMethod("SAVE")

SAVE.stSIMA <- function(x, file, tabla = "horaria", tail = TRUE, n = 6){
  if(tabla == "horaria"){
    if(tail) write.csv(tail(x, n = n), file, row.names = F)
      else write.csv(x$tabla.horaria.sitio, file, row.names = F)
    }
    else{
      if(tail) write.csv(tail(x, n = n, F), file, row.names = F)
	else write.csv(x$tabla.periodos.sitio, file, row.names = F)
    }
}

angulo.prom <- function(theta = 0, w = rep(1, length(theta))){
  theta[which(theta > 359)] <- 0
  coseno <- weighted.mean(cos(theta*pi/180), w)
  seno <- weighted.mean(sin(theta*pi/180), w)
  resultado <- atan2(seno, coseno)*180/pi
  if(resultado > 0) return(resultado)
  else return(360 + resultado)
}

##renglon periodo calcula el periodo, lleva al final un control de que variables meteorologicas son promediadas
##presumiblemente esta funcion tendria que cambiar
##si otras variables son incluidas
renglon.periodo <- function(tabla.horaria.sitio, contaminante = "O3", pesos){

  periodo <- as.character(tabla.horaria.sitio$periodo[1])
# cat('\nrenglon periodo2\n')
# print(periodo)  
  
  pesos <- pesos[pesos$periodo == periodo, 2:7]
# cat('\nrenglon periodo3\n')
# print(pesos)  

  maximo <- max(tabla.horaria.sitio[ ,contaminante])
  if(is.na(maximo))
    aux <- which(is.na(tabla.horaria.sitio[ ,contaminante]))[1]
    else
      aux <- which(tabla.horaria.sitio[ ,contaminante] == maximo)[1]
  df.salida <- tabla.horaria.sitio[aux,]
# cat('\nrenglon periodo4\n')
# print(tabla.horaria$HR)  
  df.salida$TOUT <- weighted.mean(tabla.horaria.sitio$TOUT, pesos)
  df.salida$HR <- weighted.mean(tabla.horaria.sitio$HR, pesos)
  df.salida$WS <- weighted.mean(tabla.horaria.sitio$WS, pesos)
  df.salida$SR <- weighted.mean(tabla.horaria.sitio$SR, pesos)
  df.salida$WDR <- angulo.prom(theta = tabla.horaria.sitio$WDR, w = pesos)
  df.salida$NOX <- weighted.mean(tabla.horaria.sitio$NOX, pesos)
  
#   if(sum(is.na(tabla.horaria.sitio$TOUT)) == 0){
#     df.salida$TOUT <- weighted.mean(tabla.horaria.sitio$TOUT, pesos)
#   } else {
#     df.salida$TOUT <- tabla.horaria.sitio[aux, "TOUT"] 
#   }
  
  if(contaminante == "O3"){
    df.salida$O3 <- maximo
    df.salida$PM10 <- weighted.mean(tabla.horaria.sitio$PM10, pesos)
    df.salida$PM2.5 <- weighted.mean(tabla.horaria.sitio$PM2.5, pesos)
  }
    else{
      if(contaminante == "PM10"){
	df.salida$PM10 <- maximo
	df.salida$O3 <- weighted.mean(tabla.horaria.sitio$O3, pesos)
	df.salida$PM2.5 <- weighted.mean(tabla.horaria.sitio$PM2.5, pesos)    
      }
      else{
	df.salida$PM10 <- weighted.mean(tabla.horaria.sitio$PM10, pesos)    
	df.salida$O3 <- weighted.mean(tabla.horaria.sitio$O3, pesos)
	df.salida$PM2.5 <- maximo    
      }
  }
  attr(df.salida, "met.prom.pond") <- c("HR", "WS", "SR", "WDR", "TOUT", "NOX")
  return(df.salida)
}

tabla.periodos <- function(tabla.horaria.sitio, contaminante = "O3", pesos){
  require(data.table)
# print(sitio)
  lista <- by(tabla.horaria.sitio, list(as.character(tabla.horaria.sitio$juliano.periodo)), renglon.periodo, contaminante = contaminante, pesos = pesos)
#   tabla <- do.call(rbind, lista)
  tabla <- as.data.frame(rbindlist(lista))
  tabla <- tabla[order(tabla$dia.juliano, tabla$juliano.periodo), ]
  return(tabla)
}

##tabla.periodos y renglon.periodo funcionan con ***un solo sitio, no toman sitios adicionales***
