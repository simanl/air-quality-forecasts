imputacion <- function(x, ...)
  UseMethod("imputacion")

#imputacion.datos.SIMA <- function(x, tabla.historica, actualizar = TRUE, modelos.dir = "modelos/modelos de imputacion"){
imputacion.datos.SIMA <- function(x, tabla.historica, modelos.dir = "modelos/modelos_de_imputacion"){
  ##1. ozono
  #inlcusion de modelos
  ruta <- paste(modelos.dir, "/modelos_imputacion_O3.RData", sep = "")
  modelos.imputacion <- new.env()
  load(ruta, modelos.imputacion)
#  print(ls(modelos.imputacion))
  obispado <- x[x$sitio == "Obispado", ]
  catarina <- x[x$sitio == "Santa Catarina", ]
  bernabe <- x[x$sitio == "San Bernabe", ]
  nicolas <- x[x$sitio == "San Nicolas", ]
  pastora <- x[x$sitio == "La Pastora", ]
  
  ##1.1 obispado
  O3.obispado.nuevo <- obispado$O3
  if(any(is.na(obispado$O3))){
    O3.obispado.historico <- tabla.historica$O3[tabla.historica$sitio == "Obispado"]
    imputacion.O3.obispado <- imputacion(modelos.imputacion$O3.obispado.imp, O3.obispado.nuevo, O3.obispado.historico)
    imputacion.O3.obispado[imputacion.O3.obispado  < 0] <- 0
    x$O3[x$sitio == "Obispado"] <- imputacion.O3.obispado
  }
  
  ##1.2 catarina
  O3.catarina.nuevo <- catarina$O3
  if(any(is.na(catarina$O3))){ 
    O3.catarina.historico <- tabla.historica$O3[tabla.historica$sitio == "Santa Catarina"]
    imputacion.O3.catarina <- imputacion(modelos.imputacion$O3.catarina.imp, O3.catarina.nuevo, O3.catarina.historico)
    imputacion.O3.catarina[imputacion.O3.catarina  < 0] <- 0
    x$O3[x$sitio == "Santa Catarina"] <- imputacion.O3.catarina
  }
  
  ##1.3 bernabe
  O3.bernabe.nuevo <- bernabe$O3
  if(any(is.na(bernabe$O3))){
    O3.bernabe.historico <- tabla.historica$O3[tabla.historica$sitio == "San Bernabe"]
    imputacion.O3.bernabe <- imputacion(modelos.imputacion$O3.bernabe.imp, O3.bernabe.nuevo, O3.bernabe.historico)
    imputacion.O3.bernabe[imputacion.O3.bernabe  < 0] <- 0
    x$O3[x$sitio == "San Bernabe"] <- imputacion.O3.bernabe
  }
  
  ##1.4 nicolas
  O3.nicolas.nuevo <- nicolas$O3
  if(any(is.na(nicolas$O3))){
    O3.nicolas.historico <- tabla.historica$O3[tabla.historica$sitio == "San Nicolas"]
    imputacion.O3.nicolas <- imputacion(modelos.imputacion$O3.nicolas.imp, O3.nicolas.nuevo, O3.nicolas.historico)
    imputacion.O3.nicolas[imputacion.O3.nicolas  < 0] <- 0
    x$O3[x$sitio == "San Nicolas"] <- imputacion.O3.nicolas
  }
  
  ##1.5 pastora
  O3.pastora.nuevo <- pastora$O3
  if(any(is.na(pastora$O3))){
    O3.pastora.historico <- tabla.historica$O3[tabla.historica$sitio == "La Pastora"]
    imputacion.O3.pastora <- imputacion(modelos.imputacion$O3.pastora.imp, O3.pastora.nuevo, O3.pastora.historico)
    imputacion.O3.pastora[imputacion.O3.pastora  < 0] <- 0
    x$O3[x$sitio == "La Pastora"] <- imputacion.O3.pastora
  }
  
  ##2. pm10
  #inlcusion de modelos
  ruta <- paste(modelos.dir, "/modelos_imputacion_PM10.RData", sep = "")
  modelos.imputacion <- new.env()
  load(ruta, modelos.imputacion)
#   print(ls(modelos.imputacion))
  
  ##2.1 obispado
#   obispado <- x[x$sitio == "Obispado", ]
  PM10.obispado.nuevo <- obispado$PM10
  if(any(is.na(obispado$PM10))){
    PM10.obispado.historico <- tabla.historica$PM10[tabla.historica$sitio == "Obispado"]
    imputacion.PM10.obispado <- imputacion(modelos.imputacion$PM10.obispado.imp, PM10.obispado.nuevo, PM10.obispado.historico)
    imputacion.PM10.obispado[imputacion.PM10.obispado  < 0] <- 0
    x$PM10[x$sitio == "Obispado"] <- imputacion.PM10.obispado
  }
  
  ##2.2 catarina
#   catarina <- x[x$sitio == "Santa Catarina", ]
  PM10.catarina.nuevo <- catarina$PM10
  if(any(is.na(catarina$PM10))){
    PM10.catarina.historico <- tabla.historica$PM10[tabla.historica$sitio == "Santa Catarina"]
    imputacion.PM10.catarina <- imputacion(modelos.imputacion$PM10.catarina.imp, PM10.catarina.nuevo, PM10.catarina.historico)
    imputacion.PM10.catarina[imputacion.PM10.catarina  < 0] <- 0
    x$PM10[x$sitio == "Santa Catarina"] <- imputacion.PM10.catarina
  }
  
  ##2.3 bernabe
#   bernabe <- x[x$sitio == "San Bernabe", ]
  PM10.bernabe.nuevo <- bernabe$PM10
  if(any(is.na(bernabe$PM10))){
    PM10.bernabe.historico <- tabla.historica$PM10[tabla.historica$sitio == "San Bernabe"]
    imputacion.PM10.bernabe <- imputacion(modelos.imputacion$PM10.bernabe.imp, PM10.bernabe.nuevo, PM10.bernabe.historico)
    imputacion.PM10.bernabe[imputacion.PM10.bernabe  < 0] <- 0
    x$PM10[x$sitio == "San Bernabe"] <- imputacion.PM10.bernabe
  }
  
  ##2.4 nicolas
#   nicolas <- x[x$sitio == "San Nicolas", ]
  PM10.nicolas.nuevo <- nicolas$PM10
  if(any(is.na(nicolas$PM10))){
    PM10.nicolas.historico <- tabla.historica$PM10[tabla.historica$sitio == "San Nicolas"]
    imputacion.PM10.nicolas <- imputacion(modelos.imputacion$PM10.nicolas.imp, PM10.nicolas.nuevo, PM10.nicolas.historico)
    imputacion.PM10.nicolas[imputacion.PM10.nicolas  < 0] <- 0
    x$PM10[x$sitio == "San Nicolas"] <- imputacion.PM10.nicolas
  }
  
  ##2.5 pastora
#   pastora <- x[x$sitio == "La Pastora", ]
  PM10.pastora.nuevo <- pastora$PM10
  if(any(is.na(pastora$PM10))){
    PM10.pastora.historico <- tabla.historica$PM10[tabla.historica$sitio == "La Pastora"]
    imputacion.PM10.pastora <- imputacion(modelos.imputacion$PM10.pastora.imp, PM10.pastora.nuevo, PM10.pastora.historico)
    imputacion.PM10.pastora[imputacion.PM10.pastora  < 0] <- 0
    x$PM10[x$sitio == "La Pastora"] <- imputacion.PM10.pastora
  }

  
  ##
  ## NOX por sitio
  ##
  ruta <- paste(modelos.dir, "/modelos_imputacion_NOX.RData", sep = "")
  modelos.imputacion <- new.env()
  load(ruta, modelos.imputacion)
  
  ##Obispado
  NOX.obispado.nuevo <- obispado$NOX
  if(any(is.na(obispado$NOX))){
    NOX.obispado.historico <- tabla.historica$NOX[tabla.historica$sitio == "Obispado"]
    imputacion.NOX.obispado <- imputacion(modelos.imputacion$NOX.obispado.imp, NOX.obispado.nuevo, NOX.obispado.historico)
    imputacion.NOX.obispado[imputacion.NOX.obispado  < 0] <- 0
    x$NOX[x$sitio == "Obispado"] <- imputacion.NOX.obispado
  }

  ##La Pastora
  NOX.pastora.nuevo <- pastora$NOX
  if(any(is.na(pastora$NOX))){
    NOX.pastora.historico <- tabla.historica$NOX[tabla.historica$sitio == "La Pastora"]
    imputacion.NOX.pastora <- imputacion(modelos.imputacion$NOX.pastora.imp, NOX.pastora.nuevo, NOX.pastora.historico)
    imputacion.NOX.pastora[imputacion.NOX.pastora  < 0] <- 0
    x$NOX[x$sitio == "La Pastora"] <- imputacion.NOX.pastora
  }

  
  ##Bernabe
  NOX.bernabe.nuevo <- bernabe$NOX
  if(any(is.na(bernabe$NOX))){
    NOX.bernabe.historico <- tabla.historica$NOX[tabla.historica$sitio == "San Bernabe"]
    imputacion.NOX.bernabe <- imputacion(modelos.imputacion$NOX.bernabe.imp, NOX.bernabe.nuevo, NOX.bernabe.historico)
    imputacion.NOX.bernabe[imputacion.NOX.bernabe  < 0] <- 0
    x$NOX[x$sitio == "San Bernabe"] <- imputacion.NOX.bernabe
  }


  ##Nicolas
  NOX.nicolas.nuevo <- nicolas$NOX
  if(any(is.na(nicolas$NOX))){
    NOX.nicolas.historico <- tabla.historica$NOX[tabla.historica$sitio == "San Nicolas"]
    imputacion.NOX.nicolas <- imputacion(modelos.imputacion$NOX.nicolas.imp, NOX.nicolas.nuevo, NOX.nicolas.historico)
    imputacion.NOX.nicolas[imputacion.NOX.nicolas  < 0] <- 0
    x$NOX[x$sitio == "San Nicolas"] <- imputacion.NOX.nicolas
  }

  ##Catarina
  NOX.catarina.nuevo <- catarina$NOX
  if(any(is.na(catarina$NOX))){
    NOX.catarina.historico <- tabla.historica$NOX[tabla.historica$sitio == "Santa Catarina"]
    imputacion.NOX.catarina <- imputacion(modelos.imputacion$NOX.catarina.imp, NOX.catarina.nuevo, NOX.catarina.historico)
    imputacion.NOX.catarina[imputacion.NOX.catarina  < 0] <- 0
    x$NOX[x$sitio == "Santa Catarina"] <- imputacion.NOX.catarina
  }
  
  
  ##
  ## ... VARIABLES METEOROLOGICAS ... se hace sitio un sitio a la vez
  ##
  
  ##
  ##1. Obispado
  #inclusion de modelos
  ruta <- paste(modelos.dir, "/modelos_imputacion_met_obispado.RData", sep = "")
  modelos.imputacion <- new.env()
  load(ruta, modelos.imputacion)
#   print(ls(modelos.imputacion))
  
  ##1.1 obispado - hr
#   obispado <- x[x$sitio == "Obispado", ]
  HR.obispado.nuevo <- obispado$HR
  if(any(is.na(obispado$HR))){
    HR.obispado.historico <- tabla.historica$HR[tabla.historica$sitio == "Obispado"]
    imputacion.HR.obispado <- imputacion(modelos.imputacion$obispado.hr.imp, HR.obispado.nuevo, HR.obispado.historico)
    imputacion.HR.obispado[imputacion.HR.obispado  < 0] <- 0
    imputacion.HR.obispado[imputacion.HR.obispado  > 100] <- 100
    x$HR[x$sitio == "Obispado"] <- imputacion.HR.obispado
  }
  
  ##1.2 obispado - sr
#   obispado <- x[x$sitio == "Obispado", ]
  SR.obispado.nuevo <- obispado$SR
  if(any(is.na(obispado$SR))){
    SR.obispado.historico <- tabla.historica$SR[tabla.historica$sitio == "Obispado"]
    imputacion.SR.obispado <- imputacion(modelos.imputacion$obispado.sr.imp, SR.obispado.nuevo, SR.obispado.historico)
    imputacion.SR.obispado[imputacion.SR.obispado  < 0] <- 0
    x$SR[x$sitio == "Obispado"] <- imputacion.SR.obispado
  }
  
  ##1.3 obispado - ws
#   obispado <- x[x$sitio == "Obispado", ]
  WS.obispado.nuevo <- obispado$WS
  if(any(is.na(obispado$WS))){
    WS.obispado.historico <- tabla.historica$WS[tabla.historica$sitio == "Obispado"]
    imputacion.WS.obispado <- imputacion(modelos.imputacion$obispado.ws.imp, WS.obispado.nuevo, WS.obispado.historico)
    imputacion.WS.obispado[imputacion.WS.obispado  < 0] <- 0
    x$WS[x$sitio == "Obispado"] <- imputacion.WS.obispado
  }

  ##1.4 obispado - tout
#   obispado <- x[x$sitio == "Obispado", ]
  TOUT.obispado.nuevo <- obispado$TOUT
  if(any(is.na(obispado$TOUT))){
    TOUT.obispado.historico <- tabla.historica$TOUT[tabla.historica$sitio == "Obispado"]
    imputacion.TOUT.obispado <- imputacion(modelos.imputacion$obispado.tout.imp, TOUT.obispado.nuevo, TOUT.obispado.historico)
    x$TOUT[x$sitio == "Obispado"] <- imputacion.TOUT.obispado
  }
  
  ##1.5 obispado - WDR
#   obispado <- x[x$sitio == "Obispado", ]
  WDR.obispado.nuevo <- obispado$WDR
  if(any(is.na(obispado$WDR))){
    WDR.obispado.historico <- tabla.historica$WDR[tabla.historica$sitio == "Obispado"]
    imputacion.SIN.WDR.obispado <- imputacion(modelos.imputacion$obispado.sin.wdr.imp, WDR.obispado.nuevo, WDR.obispado.historico)
    imputacion.COS.WDR.obispado <- imputacion(modelos.imputacion$obispado.cos.wdr.imp, WDR.obispado.nuevo, WDR.obispado.historico)
    
    ##revisar limites imputacion
    imputacion.WDR.obispado <- atan2(x = imputacion.COS.WDR.obispado, y = imputacion.SIN.WDR.obispado) * 180/pi
    aux <- imputacion.WDR.obispado < 0
    imputacion.WDR.obispado[aux] <- imputacion.WDR.obispado[aux] + 360
    
    x$WDR[x$sitio == "Obispado"] <- imputacion.WDR.obispado
  }
  
  ##
  ##2. catarina
  #inclusion de modelos
  ruta <- paste(modelos.dir, "/modelos_imputacion_met_catarina.RData", sep = "")
  modelos.imputacion <- new.env()
  load(ruta, modelos.imputacion)
#   print(ls(modelos.imputacion))
  
  ##2.1 catarina - hr
#   catarina <- x[x$sitio == "Santa Catarina", ]
  HR.catarina.nuevo <- catarina$HR
  if(any(is.na(catarina$HR))){
    HR.catarina.historico <- tabla.historica$HR[tabla.historica$sitio == "Santa Catarina"]
    imputacion.HR.catarina <- imputacion(modelos.imputacion$catarina.hr.imp, HR.catarina.nuevo, HR.catarina.historico)
    imputacion.HR.catarina[imputacion.HR.catarina  < 0] <- 0
    imputacion.HR.catarina[imputacion.HR.catarina  > 100] <- 100
    x$HR[x$sitio == "Santa Catarina"] <- imputacion.HR.catarina
  }
  
  ##2.2 catarina - sr
#   catarina <- x[x$sitio == "Santa Catarina", ]
  SR.catarina.nuevo <- catarina$SR
  if(any(is.na(catarina$SR))){
    SR.catarina.historico <- tabla.historica$SR[tabla.historica$sitio == "Santa Catarina"]
    imputacion.SR.catarina <- imputacion(modelos.imputacion$catarina.sr.imp, SR.catarina.nuevo, SR.catarina.historico)
    imputacion.SR.catarina[imputacion.SR.catarina  < 0] <- 0
    x$SR[x$sitio == "Santa Catarina"] <- imputacion.SR.catarina
  }
  
  ##2.3 catarina - ws
#   catarina <- x[x$sitio == "Santa Catarina", ]
  WS.catarina.nuevo <- catarina$WS
  if(any(is.na(catarina$WS))){
    WS.catarina.historico <- tabla.historica$WS[tabla.historica$sitio == "Santa Catarina"]
    imputacion.WS.catarina <- imputacion(modelos.imputacion$catarina.ws.imp, WS.catarina.nuevo, WS.catarina.historico)
    imputacion.WS.catarina[imputacion.WS.catarina  < 0] <- 0
    x$WS[x$sitio == "Santa Catarina"] <- imputacion.WS.catarina
  }
  
  ##2.4 catarina - tout
#   catarina <- x[x$sitio == "Santa Catarina", ]
  TOUT.catarina.nuevo <- catarina$TOUT
  if(any(is.na(catarina$TOUT))){
    TOUT.catarina.historico <- tabla.historica$TOUT[tabla.historica$sitio == "Santa Catarina"]
    imputacion.TOUT.catarina <- imputacion(modelos.imputacion$catarina.tout.imp, TOUT.catarina.nuevo, TOUT.catarina.historico)
    x$TOUT[x$sitio == "Santa Catarina"] <- imputacion.TOUT.catarina
  }
  
  ##2.5 catarina - WDR
#   catarina <- x[x$sitio == "Santa Catarina", ]
  WDR.catarina.nuevo <- catarina$WDR
  if(any(is.na(catarina$WDR))){
    WDR.catarina.historico <- tabla.historica$WDR[tabla.historica$sitio == "Santa Catarina"]
    imputacion.SIN.WDR.catarina <- imputacion(modelos.imputacion$catarina.sin.wdr.imp, WDR.catarina.nuevo, WDR.catarina.historico)
    imputacion.COS.WDR.catarina <- imputacion(modelos.imputacion$catarina.cos.wdr.imp, WDR.catarina.nuevo, WDR.catarina.historico)
    
    ##revisar limites imputacion
    imputacion.WDR.catarina <- atan2(x = imputacion.COS.WDR.catarina, y = imputacion.SIN.WDR.catarina) * 180/pi
    aux <- imputacion.WDR.catarina < 0
    imputacion.WDR.catarina[aux] <- imputacion.WDR.catarina[aux] + 360
    
    x$WDR[x$sitio == "Santa Catarina"] <- imputacion.WDR.catarina
  }
  
  ##
  ##3. bernabe
  #inclusion de modelos
  ruta <- paste(modelos.dir, "/modelos_imputacion_met_bernabe.RData", sep = "")
  modelos.imputacion <- new.env()
  load(ruta, modelos.imputacion)
#   print(ls(modelos.imputacion))
  
  ##3.1 bernabe - hr
#   bernabe <- x[x$sitio == "San Bernabe", ]
  HR.bernabe.nuevo <- bernabe$HR
  if(any(is.na(bernabe$HR))){
    HR.bernabe.historico <- tabla.historica$HR[tabla.historica$sitio == "San Bernabe"]
    imputacion.HR.bernabe <- imputacion(modelos.imputacion$bernabe.hr.imp, HR.bernabe.nuevo, HR.bernabe.historico)
    imputacion.HR.bernabe[imputacion.HR.bernabe  < 0] <- 0
    imputacion.HR.bernabe[imputacion.HR.bernabe  > 100] <- 100
    x$HR[x$sitio == "San Bernabe"] <- imputacion.HR.bernabe
  }
  
  ##3.2 bernabe - sr
#   bernabe <- x[x$sitio == "San Bernabe", ]
  SR.bernabe.nuevo <- bernabe$SR
  if(any(is.na(bernabe$SR))){
    SR.bernabe.historico <- tabla.historica$SR[tabla.historica$sitio == "San Bernabe"]
    imputacion.SR.bernabe <- imputacion(modelos.imputacion$bernabe.sr.imp, SR.bernabe.nuevo, SR.bernabe.historico)
    imputacion.SR.bernabe[imputacion.SR.bernabe  < 0] <- 0
    x$SR[x$sitio == "San Bernabe"] <- imputacion.SR.bernabe
  }
  
  ##3.3 bernabe - ws
#   bernabe <- x[x$sitio == "San Bernabe", ]
  WS.bernabe.nuevo <- bernabe$WS
  if(any(is.na(bernabe$WS))){
    WS.bernabe.historico <- tabla.historica$WS[tabla.historica$sitio == "San Bernabe"]
    imputacion.WS.bernabe <- imputacion(modelos.imputacion$bernabe.ws.imp, WS.bernabe.nuevo, WS.bernabe.historico)
    imputacion.WS.bernabe[imputacion.WS.bernabe  < 0] <- 0
    x$WS[x$sitio == "San Bernabe"] <- imputacion.WS.bernabe
  }
  
  ##3.4 bernabe - tout
#   bernabe <- x[x$sitio == "San Bernabe", ]
  TOUT.bernabe.nuevo <- bernabe$TOUT
  if(any(is.na(bernabe$TOUT))){
    TOUT.bernabe.historico <- tabla.historica$TOUT[tabla.historica$sitio == "San Bernabe"]
    imputacion.TOUT.bernabe <- imputacion(modelos.imputacion$bernabe.tout.imp, TOUT.bernabe.nuevo, TOUT.bernabe.historico)
    x$TOUT[x$sitio == "San Bernabe"] <- imputacion.TOUT.bernabe
  }
  
  ##3.5 bernabe - WDR
#   bernabe <- x[x$sitio == "San Bernabe", ]
  WDR.bernabe.nuevo <- bernabe$WDR
  if(any(is.na(bernabe$WDR))){
    WDR.bernabe.historico <- tabla.historica$WDR[tabla.historica$sitio == "San Bernabe"]
    imputacion.SIN.WDR.bernabe <- imputacion(modelos.imputacion$bernabe.sin.wdr.imp, WDR.bernabe.nuevo, WDR.bernabe.historico)
    imputacion.COS.WDR.bernabe <- imputacion(modelos.imputacion$bernabe.cos.wdr.imp, WDR.bernabe.nuevo, WDR.bernabe.historico)
    imputacion.WDR.bernabe <- atan2(x = imputacion.COS.WDR.bernabe, y = imputacion.SIN.WDR.bernabe) * 180/pi
    aux <- imputacion.WDR.bernabe < 0
    imputacion.WDR.bernabe[aux] <- imputacion.WDR.bernabe[aux] + 360
    
    x$WDR[x$sitio == "San Bernabe"] <- imputacion.WDR.bernabe
  }

  ##
  ##4. nicolas
  #inclusion de modelos
  ruta <- paste(modelos.dir, "/modelos_imputacion_met_nicolas.RData", sep = "")
  modelos.imputacion <- new.env()
  load(ruta, modelos.imputacion)
#   print(ls(modelos.imputacion))
  
  ##4.1 nicolas - hr
#   nicolas <- x[x$sitio == "San Nicolas", ]
  HR.nicolas.nuevo <- nicolas$HR
  if(any(is.na(nicolas$HR))){
    HR.nicolas.historico <- tabla.historica$HR[tabla.historica$sitio == "San Nicolas"]
    imputacion.HR.nicolas <- imputacion(modelos.imputacion$nicolas.hr.imp, HR.nicolas.nuevo, HR.nicolas.historico)
    imputacion.HR.nicolas[imputacion.HR.nicolas  < 0] <- 0
    imputacion.HR.nicolas[imputacion.HR.nicolas  > 100] <- 100
    x$HR[x$sitio == "San Nicolas"] <- imputacion.HR.nicolas
  }
  
  ##4.2 nicolas - sr
#   nicolas <- x[x$sitio == "San Nicolas", ]
  SR.nicolas.nuevo <- nicolas$SR
  if(any(is.na(nicolas$SR))){
    SR.nicolas.historico <- tabla.historica$SR[tabla.historica$sitio == "San Nicolas"]
    imputacion.SR.nicolas <- imputacion(modelos.imputacion$nicolas.sr.imp, SR.nicolas.nuevo, SR.nicolas.historico)
    imputacion.SR.nicolas[imputacion.SR.nicolas  < 0] <- 0
    x$SR[x$sitio == "San Nicolas"] <- imputacion.SR.nicolas
  }
  
  ##4.3 nicolas - ws
#   nicolas <- x[x$sitio == "San Nicolas", ]
  WS.nicolas.nuevo <- nicolas$WS
  if(any(is.na(nicolas$WS))){
    WS.nicolas.historico <- tabla.historica$WS[tabla.historica$sitio == "San Nicolas"]
    imputacion.WS.nicolas <- imputacion(modelos.imputacion$nicolas.ws.imp, WS.nicolas.nuevo, WS.nicolas.historico)
    imputacion.WS.nicolas[imputacion.WS.nicolas  < 0] <- 0
    x$WS[x$sitio == "San Nicolas"] <- imputacion.WS.nicolas
  }
  
  ##4.4 nicolas - tout
#   nicolas <- x[x$sitio == "San Nicolas", ]
  TOUT.nicolas.nuevo <- nicolas$TOUT
  if(any(is.na(nicolas$TOUT))){
    TOUT.nicolas.historico <- tabla.historica$TOUT[tabla.historica$sitio == "San Nicolas"]
    imputacion.TOUT.nicolas <- imputacion(modelos.imputacion$nicolas.tout.imp, TOUT.nicolas.nuevo, TOUT.nicolas.historico)
    x$TOUT[x$sitio == "San Nicolas"] <- imputacion.TOUT.nicolas
  }
  
  ##4.5 nicolas - WDR
#   nicolas <- x[x$sitio == "San Nicolas", ]
  WDR.nicolas.nuevo <- nicolas$WDR
  if(any(is.na(nicolas$WDR))){
    WDR.nicolas.historico <- tabla.historica$WDR[tabla.historica$sitio == "San Nicolas"]
    imputacion.SIN.WDR.nicolas <- imputacion(modelos.imputacion$nicolas.sin.wdr.imp, WDR.nicolas.nuevo, WDR.nicolas.historico)
    imputacion.COS.WDR.nicolas <- imputacion(modelos.imputacion$nicolas.cos.wdr.imp, WDR.nicolas.nuevo, WDR.nicolas.historico)
    imputacion.WDR.nicolas <- atan2(x = imputacion.COS.WDR.nicolas, y = imputacion.SIN.WDR.nicolas) * 180/pi
    aux <- imputacion.WDR.nicolas < 0
    imputacion.WDR.nicolas[aux] <- imputacion.WDR.nicolas[aux] + 360
    x$WDR[x$sitio == "San Nicolas"] <- imputacion.WDR.nicolas
  }

  ##
  ##5. pastora
  #inclusion de modelos
  ruta <- paste(modelos.dir, "/modelos_imputacion_met_pastora.RData", sep = "")
  modelos.imputacion <- new.env()
  load(ruta, modelos.imputacion)
#   print(ls(modelos.imputacion))
  
  ##5.1 pastora - hr
#   pastora <- x[x$sitio == "La Pastora", ]
  HR.pastora.nuevo <- pastora$HR
  if(any(is.na(pastora$HR))){
    HR.pastora.historico <- tabla.historica$HR[tabla.historica$sitio == "La Pastora"]
    imputacion.HR.pastora <- imputacion(modelos.imputacion$pastora.hr.imp, HR.pastora.nuevo, HR.pastora.historico)
    imputacion.HR.pastora[imputacion.HR.pastora  < 0] <- 0
    imputacion.HR.pastora[imputacion.HR.pastora  > 100] <- 100
    x$HR[x$sitio == "La Pastora"] <- imputacion.HR.pastora
  }
  
  ##5.2 pastora - sr
#   pastora <- x[x$sitio == "La Pastora", ]
  SR.pastora.nuevo <- pastora$SR
  if(any(is.na(pastora$SR))){
    SR.pastora.historico <- tabla.historica$SR[tabla.historica$sitio == "La Pastora"]
    imputacion.SR.pastora <- imputacion(modelos.imputacion$pastora.sr.imp, SR.pastora.nuevo, SR.pastora.historico)
    imputacion.SR.pastora[imputacion.SR.pastora  < 0] <- 0
    x$SR[x$sitio == "La Pastora"] <- imputacion.SR.pastora
  }
  
  ##5.3 pastora - ws
#   pastora <- x[x$sitio == "La Pastora", ]
  WS.pastora.nuevo <- pastora$WS
  if(any(is.na(pastora$WS))){
    WS.pastora.historico <- tabla.historica$WS[tabla.historica$sitio == "La Pastora"]
    imputacion.WS.pastora <- imputacion(modelos.imputacion$pastora.ws.imp, WS.pastora.nuevo, WS.pastora.historico)
    imputacion.WS.pastora[imputacion.WS.pastora  < 0] <- 0
    x$WS[x$sitio == "La Pastora"] <- imputacion.WS.pastora
  }
  
  ##5.4 pastora - tout
#   pastora <- x[x$sitio == "La Pastora", ]
  TOUT.pastora.nuevo <- pastora$TOUT
  if(any(is.na(pastora$TOUT))){
    TOUT.pastora.historico <- tabla.historica$TOUT[tabla.historica$sitio == "La Pastora"]
    imputacion.TOUT.pastora <- imputacion(modelos.imputacion$pastora.tout.imp, TOUT.pastora.nuevo, TOUT.pastora.historico)
    x$TOUT[x$sitio == "La Pastora"] <- imputacion.TOUT.pastora
  }
  
  ##5.5 pastora - WDR
#   pastora <- x[x$sitio == "La Pastora", ]
  WDR.pastora.nuevo <- pastora$WDR
  if(any(is.na(pastora$WDR))){
    WDR.pastora.historico <- tabla.historica$WDR[tabla.historica$sitio == "La Pastora"]
    imputacion.SIN.WDR.pastora <- imputacion(modelos.imputacion$pastora.sin.wdr.imp, WDR.pastora.nuevo, WDR.pastora.historico)
    imputacion.COS.WDR.pastora <- imputacion(modelos.imputacion$pastora.cos.wdr.imp, WDR.pastora.nuevo, WDR.pastora.historico)
    imputacion.WDR.pastora <- atan2(x = imputacion.COS.WDR.pastora, y = imputacion.SIN.WDR.pastora) * 180/pi
    aux <- imputacion.WDR.pastora < 0
    imputacion.WDR.pastora[aux] <- imputacion.WDR.pastora[aux] + 360
    x$WDR[x$sitio == "La Pastora"] <- imputacion.WDR.pastora
  }
  
  
  attr(x, "imputada") <- TRUE
  return(x) ##esta es la salida
}

mimp.kalman <- function(x)
  UseMethod("mimp.kalman")
  
mimp.kalman.Arima.imp <- function(x, ...){
  serie <- serie.imp <- x$serie  
  
  if(!is.na(coef(x)["intercept"]))
    int <- coef(x)["intercept"]
    else
      int <- 0
  serie.centered <- serie - int
  
  Z <- x$model$Z
  aux <- t(KalmanSmooth(serie.centered, x$model)$smooth)*Z
  aux <- aux[which(Z!=0), ]
  if(is.matrix(aux))
    aux <- apply(aux, 2, sum) + int
    else
      aux <- aux + int

  nas <- is.na(serie.centered)   
  serie.imp[nas] <- aux[nas]
  
  resultado <- matrix(c(serie, serie.imp), ncol = 2)
  colnames(resultado) <- c(x$series, paste(x$series, ".imp", sep = ""))
  class(resultado) <- c("mimp.kalman", "matrix")
  resultado
}

mimp.kalman.Arima <- function(x, ...){
  serie <- serie.imp <- x$serie  
  
  if(!is.na(coef(x)["intercept"]))
    int <- coef(x)["intercept"]
    else
      int <- 0
  serie.centered <- serie - int
  
  Z <- x$model$Z
  aux <- t(KalmanSmooth(serie.centered, x$model)$smooth)*Z
  aux <- aux[which(Z!=0), ]
  if(is.matrix(aux))
    aux <- apply(aux, 2, sum) + int
    else
      aux <- aux + int

  nas <- is.na(serie.centered)   
  serie.imp[nas] <- aux[nas]
  
  resultado <- matrix(c(serie, serie.imp), ncol = 2)
  colnames(resultado) <- c(x$series, paste(x$series, ".imp", sep = ""))
  class(resultado) <- c("mimp.kalman", "matrix")
  resultado
}

coef.Arima.imp <- function(x, ...){
  x$coef
}
  
# imputacion.default <- function(x, serie, modelo, funcion){ #x el periodo, serie la serie historica, modelo el modelo Arima.imp. funcion (generica que produce la imputacion)
#   if(is.null(modelo$trans) | is.null(modelo$trans.inv)) stop("'modelo' no contiene la transformacion o transformacion inversa.")
#   modelo$serie <- modelo$trans(c(serie, x))
#   serie.transf.imp <- funcion(modelo)
#   serie.imp <- modelo$trans.inv(serie.transf.imp)
#   n <- length(serie)
#   m <- length(x)
#   if(is.matrix(serie.imp))
#     return(serie.imp[(n + 1):(n + m), ])
#     else
#       return(serie.imp[(n + 1):(n + m)])
# }

imputacion.Arima.imp <- function(modelo, x, serie){ #x el periodo, serie la serie historica, modelo el modelo Arima.imp. funcion (generica que produce la imputacion)
  if(is.null(modelo$trans) | is.null(modelo$trans.inv)) stop("'modelo' no contiene la transformacion o transformacion inversa.")
  modelo$serie <- modelo$trans(c(serie, x))
  serie.transf.imp <- mimp.kalman(modelo)[ , 2]
  serie.imp <- modelo$trans.inv(serie.transf.imp)
  n <- length(serie)
  m <- length(x)
  return(serie.imp[(n + 1):(n + m)])
}


plot.mimp.kalman <- function(x, ...){
  nas <-  is.na(x[, 1])
  x <- as.ts(x)
  plot(x[,2:1], plot.type = "single", col = c("red", "black"), lwd = c(1,2), lty =c(2,1), main = colnames(x)[1])
}
