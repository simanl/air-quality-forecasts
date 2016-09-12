tablaMaestra <- function(x, ...)
  UseMethod("tablaMaestra")

tablaMaestra.default <- function(x, fecha.final, image){
##x: ruta
##fecha.final: "Anio-mes-dia"
##image: ruta para guardar la imagen con el objeto
  tabla.maestra <- read.csv(x) #lee la tabla maestra final reducida (solo los sitios de interes)
  tabla.maestra$fecha <- as.Date(tabla.maestra$fecha)
  if(!missing(fecha.final))
    tabla.maestra <- tabla.maestra[tabla.maestra$fecha <= as.Date(fecha.final), ]
  tabla.maestra <- colsTemp(tabla.maestra)
  attr(tabla.maestra, "fecha") <- max(tabla.maestra$fecha)
  attr(tabla.maestra, "hora") <- tabla.maestra[nrow(tabla.maestra), "hora"]
  
  if(!missing(image))
    save(tabla.maestra, file = image)
    else
      return(tabla.maestra)
}

tablaImputada <- function(x, ...)
  UseMethod("tablaImputada")

tablaImputada.default <- function(x, fecha.final, image){
##x: ruta
##fecha.final: "Anio-mes-dia"
##image: ruta para guardar la imagen con el objeto  
  tabla.maestra.imputada <- read.csv(x) #lee la tabla maestra final reducida imputada
  tabla.maestra.imputada$fecha <- as.Date(tabla.maestra.imputada$fecha)
  if(!missing(fecha.final))
    tabla.maestra.imputada <- tabla.maestra.imputada[tabla.maestra.imputada$fecha <= as.Date(fecha.final), ]
  tabla.maestra.imputada <- colsTemp(tabla.maestra.imputada)
  attr(tabla.maestra.imputada, "fecha") <- max(tabla.maestra.imputada$fecha)
  attr(tabla.maestra.imputada, "hora") <- tabla.maestra.imputada[nrow(tabla.maestra.imputada), "hora"]
  
  if(!missing(image))
    save(tabla.maestra.imputada, file = image)
    else
      return(tabla.maestra.imputada)
}

tablaSalida <- function(x, ...)
  UseMethod("tablaSalida")
  
tablaSalida.stSIMA <- function(x, pronostico, norma, ...){
  if(missing(pronostico) || missing(norma)) stop("Falta(n) 'pronostico' o 'norma' sin valores predeterminados")
  sitio <- x$sitio
  contaminante <- x$contaminante
  n <- nrow(x$tabla.periodos.sitio)
  ultima.fecha <- as.Date(x$tabla.periodos.sitio$fecha[n])
  ultimo.periodo <- x$tabla.periodos.sitio$periodo[n]
  if(ultimo.periodo == "00 a 05"){
    periodos <- c("06 a 11", "12 a 17", "18 a 23", "00 a 05")
    fechas <- c(rep(ultima.fecha, 3), ultima.fecha + 1)
  }
  if(ultimo.periodo == "06 a 11"){
    periodos <- c("12 a 17", "18 a 23", "00 a 05", "06 a 11")
    fechas <- c(rep(ultima.fecha, 2), rep(ultima.fecha + 1, 2))
  }
  if(ultimo.periodo == "12 a 17"){
    periodos <- c("18 a 23", "00 a 05", "06 a 11", "12 a 17")
    fechas <- c(rep(ultima.fecha, 1), rep(ultima.fecha + 1, 3))    
  }
  if(ultimo.periodo == "18 a 23"){ 
    periodos <- c("00 a 05", "06 a 11", "12 a 17", "18 a 23")
    fechas <- rep(ultima.fecha + 1, 4)
  }
  
  categorias <- cut(pronostico, breaks = norma, labels = c("buena", "regular", "mala", "muy mala", "extremadamente mala"),
                      include.lowest = TRUE)
  resultado <- data.frame(sitio = sitio, fecha = fechas, periodo = periodos, pronostico = as.numeric(round(pronostico)), categoria = categorias)  
  return(resultado)
}

tablaPronostico <- function(x, ...)
  UseMethod("tablaPronostico")

tablaPronostico.stSIMA <- function(x, pronostico, norma, ...){
  if(missing(pronostico)) stop("Falta 'pronostico'")
  sitio <- x$sitio
  contaminante <- x$contaminante
  n <- nrow(x$tabla.periodos.sitio)
  ultima.fecha <- as.Date(x$tabla.periodos.sitio$fecha[n])
  ultimo.periodo <- x$tabla.periodos.sitio$periodo[n]
  if(ultimo.periodo == "00 a 05"){
    periodos <- c("06 a 11", "12 a 17", "18 a 23", "00 a 05")
    fechas <- c(rep(ultima.fecha, 3), ultima.fecha + 1)
  }
  if(ultimo.periodo == "06 a 11"){
    periodos <- c("12 a 17", "18 a 23", "00 a 05", "06 a 11")
    fechas <- c(rep(ultima.fecha, 2), rep(ultima.fecha + 1, 2))
  }
  if(ultimo.periodo == "12 a 17"){
    periodos <- c("18 a 23", "00 a 05", "06 a 11", "12 a 17")
    fechas <- c(rep(ultima.fecha, 1), rep(ultima.fecha + 1, 3))    
  }
  if(ultimo.periodo == "18 a 23"){ 
    periodos <- c("00 a 05", "06 a 11", "12 a 17", "18 a 23")
    fechas <- rep(ultima.fecha + 1, 4)
  }
  
  categorias <- cut(pronostico, breaks = norma, labels = c("buena", "regular", "mala", "muy mala", "extremadamente mala"),
                      include.lowest = TRUE)
  resultado <- data.frame(sitio = sitio, fecha = fechas, periodo = periodos, pronostico = pronostico, categorias = categorias, h = 1:4)
  attr(resultado, "pronostico.obj") <- pronostico
#   attr(resultado, "nombre") <- paste(as.character(contaminante), fechas[1], periodos[1], sep = "_")
#   attr(resultado, "valores") <- data.frame(sitio = sitio, fecha = fechas, periodo = periodos, pronostico = pronostico)
  return(resultado)
}

nas.ok <- function(x, ...)
  UseMethod("nas.ok")

nas.ok.default <- function(x, ...){
  if(length(x)!=72) stop("Numero de horas debe ser 72.")
  x <- is.na(x)
  rachas <- rle(x)
  nas <- as.logical(rachas$values)
  long <- rachas$lengths
  if(all(!nas)) return(TRUE)
  if(any(long[nas]>=24)){
      aux1 <- x[25:48]
      aux2 <- x[49:72]
      if(sum(aux1) == 0 && sum(aux2) == 0) return(TRUE)
	else return(FALSE)
  }
    else return(TRUE)
}
