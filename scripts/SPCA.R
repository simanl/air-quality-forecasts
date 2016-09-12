SPCA <- function(x, ...)
  UseMethod("SPCA")

SPCA.default <- function(entrada, salida){

  # Set SCPA_HOME from the environment variable:
  SPCA_HOME <- Sys.getenv("SPCA_HOME")

  # Or from the current working directory if it isn't set:
  if(SPCA_HOME == "") SPCA_HOME <- getwd()

  # Set the working directory to whatever value SPCA_HOME finally has:
  setwd(SPCA_HOME)
   
##datos historicos horarios
  load("tablas/series/tabla_maestra.RData", .GlobalEnv) #tabla maestra final (horaria) con solo los sitios de interes

##datos historicos imputados
  load("tablas/series/tabla_maestra_imputada.RData", .GlobalEnv) #tabla maestra final (horaria) imputada con solo los sitios de interes

##necesario para validar la tabla de entrada
 tabla.maestra.ult.renglon <- tabla.maestra[nrow(tabla.maestra), ]
 
##lectura de tabla de entrada
 
  if(missing(entrada))  
    tabla.entrada <- READ(as.data.frame(get("tabla.entrada", envir = .GlobalEnv)), tabla.maestra.ult.renglon, check = T) #procedimiento usado en el ambiente de produccion 
    else
      tabla.entrada <- READ(substitute(entrada), tabla.maestra.ult.renglon, check = T) #bueno para corridas locales
##carga de objetos stSIMA
#load("tablas/series/O3Obispado.RData", .GlobalEnv)
# load("tablas/series/O3Pastora.RData", .GlobalEnv)
# load("tablas/series/O3Nicolas.RData", .GlobalEnv)
# load("tablas/series/O3Bernabe.RData", .GlobalEnv)
# load("tablas/series/O3Catarina.RData", .GlobalEnv)
# load("tablas/series/PM10Obispado.RData", .GlobalEnv)
# load("tablas/series/PM10Pastora.RData", .GlobalEnv)
# load("tablas/series/PM10Nicolas.RData", .GlobalEnv)
# load("tablas/series/PM10Bernabe.RData", .GlobalEnv)
# load("tablas/series/PM10Catarina.RData", .GlobalEnv)
  

##validacion: datos en rango, sobre todo
  tabla.entrada <- validacion(tabla.entrada)

##columnas adicionales (variables de calendario y estaciones)
  tabla.entrada.cols.adicionales <- colsTemp(tabla.entrada)
  
##adjunta a la tabla maestra la nueva info
  tabla.maestra <- rbind(tabla.maestra, tabla.entrada.cols.adicionales)
  assign("tabla.maestra", tabla.maestra, envir = .GlobalEnv) #asignacion para recuperarla en post()
  
##imputacion
  datos.historicos.wdf <- tabla.maestra[(nrow(tabla.maestra) - 720):nrow(tabla.maestra), ] #se seleccionan los 200 ultimos para hacer la imputacion
  imputaciones <- imputacion(tabla.entrada.cols.adicionales, datos.historicos.wdf, modelos.dir = "modelos/modelos_de_imputacion")

##adjunta la tabla maestra imputada
  tabla.maestra.imputada <- rbind(tabla.maestra.imputada, imputaciones)
  assign("tabla.maestra.imputada", tabla.maestra.imputada, envir = .GlobalEnv) #asignacion para recuperarla en post()
 
 ##carga de objetos stSIMA
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

##actualizacion de las series en los objetos stSIMA: periodos y modelos
  O3Obispado <- update(O3Obispado, tail(imputaciones, 30), "/modelos/modelos_de_pronostico/O3Obispado_mod_pron.RData")
  O3Pastora <- update(O3Pastora, tail(imputaciones, 30), "/modelos/modelos_de_pronostico/O3Pastora_mod_pron.RData")
  O3Nicolas <- update(O3Nicolas, tail(imputaciones, 30), "/modelos/modelos_de_pronostico/O3Nicolas_mod_pron.RData")
  O3Bernabe <- update(O3Bernabe, tail(imputaciones, 30), "/modelos/modelos_de_pronostico/O3Bernabe_mod_pron.RData")
  O3Catarina <- update(O3Catarina, tail(imputaciones, 30), "/modelos/modelos_de_pronostico/O3Catarina_mod_pron.RData")

  PM10Obispado <- update(PM10Obispado, tail(imputaciones, 30), "/modelos/modelos_de_pronostico/PM10Obispado_mod_pron.RData")
  PM10Pastora <- update(PM10Pastora, tail(imputaciones, 30), "/modelos/modelos_de_pronostico/PM10Pastora_mod_pron.RData")
  PM10Nicolas <- update(PM10Nicolas, tail(imputaciones, 30), "/modelos/modelos_de_pronostico/PM10Nicolas_mod_pron.RData")
  PM10Bernabe <- update(PM10Bernabe, tail(imputaciones, 30), "/modelos/modelos_de_pronostico/PM10Bernabe_mod_pron.RData")
  PM10Catarina <- update(PM10Catarina, tail(imputaciones, 30), "/modelos/modelos_de_pronostico/PM10Catarina_mod_pron.RData")

##asignaciones para recuperarlas en post()
  assign("O3Obispado", O3Obispado, envir = .GlobalEnv)
  assign("O3Pastora", O3Pastora, envir = .GlobalEnv)
  assign("O3Nicolas", O3Nicolas, envir = .GlobalEnv)
  assign("O3Bernabe", O3Bernabe, envir = .GlobalEnv)
  assign("O3Catarina", O3Catarina, envir = .GlobalEnv)

  assign("PM10Obispado", PM10Obispado, envir = .GlobalEnv)
  assign("PM10Pastora", PM10Pastora, envir = .GlobalEnv)
  assign("PM10Nicolas", PM10Nicolas, envir = .GlobalEnv)
  assign("PM10Bernabe", PM10Bernabe, envir = .GlobalEnv)
  assign("PM10Catarina", PM10Catarina, envir = .GlobalEnv)
  
##pronostico de las series
  O3Obispado.pronostico <- pronostico(O3Obispado, modelos.var.met = "modelos/modelos_de_pronostico/modelos_pronostico_met_O3_obispado.RData", estimacion = FALSE)[[1]]
cat("O3 Obispado\n")
  O3Pastora.pronostico <- pronostico(O3Pastora, modelos.var.met = "modelos/modelos_de_pronostico/modelos_pronostico_met_O3_pastora.RData", estimacion = FALSE)[[1]]
cat("O3 Pastora\n")  
  O3Nicolas.pronostico <- pronostico(O3Nicolas, modelos.var.met = "modelos/modelos_de_pronostico/modelos_pronostico_met_O3_nicolas.RData", estimacion = FALSE)[[1]]
cat("O3 Nicolas\n")  
  O3Bernabe.pronostico <- pronostico(O3Bernabe, modelos.var.met = "modelos/modelos_de_pronostico/modelos_pronostico_met_O3_bernabe.RData", estimacion = FALSE)[[1]]
cat("O3 Bernabe\n")  
  O3Catarina.pronostico <- pronostico(O3Catarina, modelos.var.met = "modelos/modelos_de_pronostico/modelos_pronostico_met_O3_catarina.RData", estimacion = FALSE)[[1]]
cat("O3 Catarina\n")
  PM10Obispado.pronostico <- pronostico(PM10Obispado, modelos.var.met = "modelos/modelos_de_pronostico/modelos_pronostico_met_PM10_obispado.RData", estimacion = FALSE)[[1]]
cat("PM10 Obispado\n")
  PM10Pastora.pronostico <- pronostico(PM10Pastora, modelos.var.met = "modelos/modelos_de_pronostico/modelos_pronostico_met_PM10_pastora.RData", estimacion = FALSE)[[1]]
cat("PM10 Pastora\n")  
  PM10Nicolas.pronostico <- pronostico(PM10Nicolas, modelos.var.met = "modelos/modelos_de_pronostico/modelos_pronostico_met_PM10_nicolas.RData", estimacion = FALSE)[[1]]
cat("PM10 Nicolas\n")  
  PM10Bernabe.pronostico <- pronostico(PM10Bernabe, modelos.var.met = "modelos/modelos_de_pronostico/modelos_pronostico_met_PM10_bernabe.RData", estimacion = FALSE)[[1]]
cat("PM10 Bernabe\n")  
  PM10Catarina.pronostico <- pronostico(PM10Catarina, modelos.var.met = "modelos/modelos_de_pronostico/modelos_pronostico_met_PM10_catarina.RData", estimacion = FALSE)[[1]]
cat("PM10 Catarina\n")

##tablas de pronosticos  
  O3Obispado.tabla.pronostico <- tablaPronostico(O3Obispado, O3Obispado.pronostico, norma = c(0, 70, 95, 154, 204, Inf))
  O3Pastora.tabla.pronostico <- tablaPronostico(O3Pastora, O3Pastora.pronostico, norma = c(0, 70, 95, 154, 204, Inf))
  O3Nicolas.tabla.pronostico <- tablaPronostico(O3Nicolas, O3Nicolas.pronostico, norma = c(0, 70, 95, 154, 204, Inf))
  O3Bernabe.tabla.pronostico <- tablaPronostico(O3Bernabe, O3Bernabe.pronostico, norma = c(0, 70, 95, 154, 204, Inf))
  O3Catarina.tabla.pronostico <- tablaPronostico(O3Catarina, O3Catarina.pronostico, norma = c(0, 70, 95, 154, 204, Inf))
  
  PM10Obispado.tabla.pronostico <- tablaPronostico(PM10Obispado, PM10Obispado.pronostico, norma = c(0, 40, 75, 214, 354, Inf))
  PM10Pastora.tabla.pronostico <- tablaPronostico(PM10Pastora, PM10Pastora.pronostico, norma = c(0, 40, 75, 214, 354, Inf))
  PM10Nicolas.tabla.pronostico <- tablaPronostico(PM10Nicolas, PM10Nicolas.pronostico, norma = c(0, 40, 75, 214, 354, Inf))
  PM10Bernabe.tabla.pronostico <- tablaPronostico(PM10Bernabe, PM10Bernabe.pronostico, norma = c(0, 40, 75, 214, 354, Inf))
  PM10Catarina.tabla.pronostico <- tablaPronostico(PM10Catarina, PM10Catarina.pronostico, norma = c(0, 40, 75, 214, 354, Inf))

##asignaciones para recuperarlas en post()
  assign("O3Obispado.tabla.pronostico", O3Obispado.tabla.pronostico, envir=.GlobalEnv)
  assign("O3Pastora.tabla.pronostico", O3Pastora.tabla.pronostico, envir=.GlobalEnv)
  assign("O3Nicolas.tabla.pronostico", O3Nicolas.tabla.pronostico, envir=.GlobalEnv)
  assign("O3Bernabe.tabla.pronostico", O3Bernabe.tabla.pronostico, envir=.GlobalEnv)
  assign("O3Catarina.tabla.pronostico", O3Catarina.tabla.pronostico, envir=.GlobalEnv)
  assign("PM10Obispado.tabla.pronostico", PM10Obispado.tabla.pronostico, envir=.GlobalEnv)
  assign("PM10Pastora.tabla.pronostico", PM10Pastora.tabla.pronostico, envir=.GlobalEnv)
  assign("PM10Nicolas.tabla.pronostico", PM10Nicolas.tabla.pronostico, envir=.GlobalEnv)
  assign("PM10Bernabe.tabla.pronostico", PM10Bernabe.tabla.pronostico, envir=.GlobalEnv)
  assign("PM10Catarina.tabla.pronostico", PM10Catarina.tabla.pronostico, envir=.GlobalEnv)

##tabla de salida
  ventana <- 72*5 #72 horas por cinco sitios
  tabla.maestra.reciente <- tabla.maestra[(nrow(tabla.maestra) - ventana + 1):nrow(tabla.maestra), ] #para validacion de nas
  O3Obispado.pronostico.salida <- tablaSalida(O3Obispado, O3Obispado.pronostico, norma = c(0, 70, 95, 154, 204, Inf)) #tabla de salida
  if(!nas.ok(tabla.maestra.reciente[tabla.maestra.reciente$sitio == "Obispado", "O3"])){ 
    O3Obispado.pronostico.salida$categoria <- O3Obispado.pronostico.salida$pronostico <- NA #no se publica si se excede de nas
  }
  O3Pastora.pronostico.salida <- tablaSalida(O3Pastora, O3Pastora.pronostico, norma = c(0, 70, 95, 154, 204, Inf)) #...
  if(!nas.ok(tabla.maestra.reciente[tabla.maestra.reciente$sitio == "La Pastora", "O3"])){
    O3Pastora.pronostico.salida$categoria <- O3Pastora.pronostico.salida$pronostico <- NA
  }
  O3Nicolas.pronostico.salida <- tablaSalida(O3Nicolas, O3Nicolas.pronostico, norma = c(0, 70, 95, 154, 204, Inf))
  if(!nas.ok(tabla.maestra.reciente[tabla.maestra.reciente$sitio == "San Nicolas", "O3"])){
    O3Nicolas.pronostico.salida$categoria <- O3Nicolas.pronostico.salida$pronostico <- NA
  }
  O3Bernabe.pronostico.salida <- tablaSalida(O3Bernabe, O3Bernabe.pronostico, norma = c(0, 70, 95, 154, 204, Inf))
  if(!nas.ok(tabla.maestra.reciente[tabla.maestra.reciente$sitio == "San Bernabe", "O3"])){
    O3Bernabe.pronostico.salida$categoria <- O3Bernabe.pronostico.salida$pronostico <- NA
  }  
  O3Catarina.pronostico.salida <- tablaSalida(O3Catarina, O3Catarina.pronostico, norma = c(0, 70, 95, 154, 204, Inf))
  if(!nas.ok(tabla.maestra.reciente[tabla.maestra.reciente$sitio == "Santa Catarina", "O3"])){
    O3Catarina.pronostico.salida$categoria <- O3Catarina.pronostico.salida$pronostico <- NA
  }
  tabla.salida.O3 <- cbind(data.frame(contaminante = "O3"), rbind(O3Obispado.pronostico.salida, O3Pastora.pronostico.salida, O3Nicolas.pronostico.salida, O3Bernabe.pronostico.salida, O3Catarina.pronostico.salida))

  PM10Obispado.pronostico.salida <- tablaSalida(PM10Obispado, PM10Obispado.pronostico, norma = c(0, 40, 75, 214, 354, Inf))
  if(!nas.ok(tabla.maestra.reciente[tabla.maestra.reciente$sitio == "Obispado", "PM10"])){ 
    PM10Obispado.pronostico.salida$categoria <- PM10Obispado.pronostico.salida$pronostico <- NA  #no se publica si se excede de nas
  } 
  PM10Pastora.pronostico.salida <- tablaSalida(PM10Pastora, PM10Pastora.pronostico, norma = c(0, 40, 75, 214, 354, Inf))
  if(!nas.ok(tabla.maestra.reciente[tabla.maestra.reciente$sitio == "La Pastora", "PM10"])){
    PM10Pastora.pronostico.salida$categoria <- PM10Pastora.pronostico.salida$pronostico <- NA
  }
  PM10Nicolas.pronostico.salida <- tablaSalida(PM10Nicolas, PM10Nicolas.pronostico, norma = c(0, 40, 75, 214, 354, Inf))
  if(!nas.ok(tabla.maestra.reciente[tabla.maestra.reciente$sitio == "San Nicolas", "PM10"])){
    PM10Nicolas.pronostico.salida$categoria <- PM10Nicolas.pronostico.salida$pronostico <- NA
  }  
  PM10Bernabe.pronostico.salida <- tablaSalida(PM10Bernabe, PM10Bernabe.pronostico, norma = c(0, 40, 75, 214, 354, Inf))
  if(!nas.ok(tabla.maestra.reciente[tabla.maestra.reciente$sitio == "San Bernabe", "PM10"])){
    PM10Bernabe.pronostico.salida$categoria <- PM10Bernabe.pronostico.salida$pronostico <- NA
  }
  PM10Catarina.pronostico.salida <- tablaSalida(PM10Catarina, PM10Catarina.pronostico, norma = c(0, 40, 75, 214, 354, Inf))
  if(!nas.ok(tabla.maestra.reciente[tabla.maestra.reciente$sitio == "Santa Catarina", "PM10"])){
    PM10Catarina.pronostico.salida$categoria <- PM10Catarina.pronostico.salida$pronostico <- NA
  } 
  
  tabla.salida.PM10 <- cbind(data.frame(contaminante = "PM10"), rbind(PM10Obispado.pronostico.salida, PM10Pastora.pronostico.salida, PM10Nicolas.pronostico.salida, PM10Bernabe.pronostico.salida, PM10Catarina.pronostico.salida))
  
  ##PM2.5 ni se pronostica, ni se publica
  tabla.salida.PM2.5 <- tabla.salida.O3
  tabla.salida.PM2.5$contaminante <- "PM2.5"
  tabla.salida.PM2.5$pronostico <- NA
  tabla.salida.PM2.5$categoria <- NA
  
  ##tabla de salida completa
  tabla.salida <<- rbind(tabla.salida.O3, tabla.salida.PM10, tabla.salida.PM2.5)
  
  if(missing(salida)) return(tabla.salida)
   else write.csv(tabla.salida, salida, row.names = F)
}
