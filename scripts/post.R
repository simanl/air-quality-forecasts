##guardado de tablas
post <- function(sitema = "nix"){
  SPCA_HOME <- getwd()
  if(sys == "nix"){
	  system(paste("cp '", SPCA_HOME, "/tablas/series/tabla_maestra.RData' '", SPCA_HOME, "/tablas/series/tabla_maestra.RData.bkp'", sep = ""), intern = T)
	  system(paste("cp '", SPCA_HOME, "/tablas/series/tabla_maestra_imputada.RData' '", SPCA_HOME, "/tablas/series/tabla_maestra_imputada.RData.bkp'", sep = ""), intern = T)
	  system(paste("cp '", SPCA_HOME, "/tablas/series/O3Obispado.RData' '", SPCA_HOME, "/tablas/series/O3Obispado.RData.bkp'", sep = ""), intern = T)
	  system(paste("cp '", SPCA_HOME, "/tablas/series/O3Pastora.RData' '", SPCA_HOME, "/tablas/series/O3Pastora.RData.bkp'", sep = ""), intern = T)
	  system(paste("cp '", SPCA_HOME, "/tablas/series/O3Bernabe.RData' '", SPCA_HOME, "/tablas/series/O3Bernabe.RData.bkp'", sep = ""), intern = T)
	  system(paste("cp '", SPCA_HOME, "/tablas/series/O3Catarina.RData' '", SPCA_HOME, "/tablas/series/O3Catarina.RData.bkp'", sep = ""), intern = T)
	  system(paste("cp '", SPCA_HOME, "/tablas/series/O3Nicolas.RData' '", SPCA_HOME, "/tablas/series/O3Nicolas.RData.bkp'", sep = ""), intern = T)
	  system(paste("cp '", SPCA_HOME, "/tablas/series/PM10Obispado.RData' '", SPCA_HOME, "/tablas/series/PM10Obispado.RData.bkp'", sep = ""), intern = T)
	  system(paste("cp '", SPCA_HOME, "/tablas/series/PM10Pastora.RData' '", SPCA_HOME, "/tablas/series/PM10Pastora.RData.bkp'", sep = ""), intern = T)
	  system(paste("cp '", SPCA_HOME, "/tablas/series/PM10Bernabe.RData' '", SPCA_HOME, "/tablas/series/PM10Bernabe.RData.bkp'", sep = ""), intern = T)
	  system(paste("cp '", SPCA_HOME, "/tablas/series/PM10Catarina.RData' '", SPCA_HOME, "/tablas/series/PM10Catarina.RData.bkp'", sep = ""), intern = T)
	  system(paste("cp '", SPCA_HOME, "/tablas/series/PM10Nicolas.RData' '", SPCA_HOME, "/tablas/series/PM10Nicolas.RData.bkp'", sep = ""), intern = T)
 }
  else{
	  system(paste("copy /y '", SPCA_HOME, "/tablas/series/tabla_maestra.RData' '", SPCA_HOME, "/tablas/series/tabla_maestra.RData.bkp'", sep = ""), intern = T)
	  system(paste("copy /y '", SPCA_HOME, "/tablas/series/tabla_maestra_imputada.RData' '", SPCA_HOME, "/tablas/series/tabla_maestra_imputada.RData.bkp'", sep = ""), intern = T)
	  system(paste("copy /y '", SPCA_HOME, "/tablas/series/O3Obispado.RData' '", SPCA_HOME, "/tablas/series/O3Obispado.RData.bkp'", sep = ""), intern = T)
	  system(paste("copy /y '", SPCA_HOME, "/tablas/series/O3Pastora.RData' '", SPCA_HOME, "/tablas/series/O3Pastora.RData.bkp'", sep = ""), intern = T)
	  system(paste("copy /y '", SPCA_HOME, "/tablas/series/O3Bernabe.RData' '", SPCA_HOME, "/tablas/series/O3Bernabe.RData.bkp'", sep = ""), intern = T)
	  system(paste("copy /y '", SPCA_HOME, "/tablas/series/O3Catarina.RData' '", SPCA_HOME, "/tablas/series/O3Catarina.RData.bkp'", sep = ""), intern = T)
	  system(paste("copy /y '", SPCA_HOME, "/tablas/series/O3Nicolas.RData' '", SPCA_HOME, "/tablas/series/O3Nicolas.RData.bkp'", sep = ""), intern = T)
	  system(paste("copy /y '", SPCA_HOME, "/tablas/series/PM10Obispado.RData' '", SPCA_HOME, "/tablas/series/PM10Obispado.RData.bkp'", sep = ""), intern = T)
	  system(paste("copy /y '", SPCA_HOME, "/tablas/series/PM10Pastora.RData' '", SPCA_HOME, "/tablas/series/PM10Pastora.RData.bkp'", sep = ""), intern = T)
	  system(paste("copy /y '", SPCA_HOME, "/tablas/series/PM10Bernabe.RData' '", SPCA_HOME, "/tablas/series/PM10Bernabe.RData.bkp'", sep = ""), intern = T)
	  system(paste("copy /y '", SPCA_HOME, "/tablas/series/PM10Catarina.RData' '", SPCA_HOME, "/tablas/series/PM10Catarina.RData.bkp'", sep = ""), intern = T)
	  system(paste("copy /y '", SPCA_HOME, "/tablas/series/PM10Nicolas.RData' '", SPCA_HOME, "/tablas/series/PM10Nicolas.RData.bkp'", sep = ""), intern = T)	  
	}
  save(tabla.maestra, file = "tablas/series/tabla_maestra.RData")
  save(tabla.maestra.imputada, file = "tablas/series/tabla_maestra_imputada.RData")  
  save(O3Obispado, file = "tablas/series/O3Obispado.RData")
  save(O3Pastora, file = "tablas/series/O3Pastora.RData")
  save(O3Bernabe, file = "tablas/series/O3Bernabe.RData")
  save(O3Catarina, file = "tablas/series/O3Catarina.RData")
  save(O3Nicolas, file = "tablas/series/O3Nicolas.RData")
  save(PM10Obispado, file = "tablas/series/PM10Obispado.RData")
  save(PM10Pastora, file = "tablas/series/PM10Pastora.RData")
  save(PM10Bernabe, file = "tablas/series/PM10Bernabe.RData")
  save(PM10Catarina, file = "tablas/series/PM10Catarina.RData")
  save(PM10Nicolas, file = "tablas/series/PM10Nicolas.RData")
  
  if(file.exists("tablas/pronosticos/O3Obispado.lista.pronostico.RData")){
    load("tablas/pronosticos/O3Obispado.lista.pronostico.RData")
    system(paste("cp '", SPCA_HOME, "/tablas/pronosticos/O3Obispado.lista.pronostico.RData' '", SPCA_HOME, "/tablas/pronosticos/O3Obispado.lista.pronostico.RData.bkp'", sep = ""), intern = T)
    O3Obispado.lista.pronostico[[length(O3Obispado.lista.pronostico) + 1]] <- O3Obispado.tabla.pronostico
    save(O3Obispado.lista.pronostico, file = "tablas/pronosticos/O3Obispado.lista.pronostico.RData")
  }
  else{
   O3Obispado.lista.pronostico <- list()
        O3Obispado.lista.pronostico[[1]] <- O3Obispado.tabla.pronostico
    save(O3Obispado.lista.pronostico, file = "tablas/pronosticos/O3Obispado.lista.pronostico.RData")
  }
  
  if(file.exists("tablas/pronosticos/O3Pastora.lista.pronostico.RData")){
    load("tablas/pronosticos/O3Pastora.lista.pronostico.RData")
    system(paste("cp '", SPCA_HOME, "/tablas/pronosticos/O3Pastora.lista.pronostico.RData' '", SPCA_HOME, "/tablas/pronosticos/O3Pastora.lista.pronostico.RData.bkp'", sep = ""), intern = T)
    O3Pastora.lista.pronostico[[length(O3Pastora.lista.pronostico) + 1]] <- O3Pastora.tabla.pronostico
    save(O3Pastora.lista.pronostico, file = "tablas/pronosticos/O3Pastora.lista.pronostico.RData")
  }
  else{
   O3Pastora.lista.pronostico <- list()
        O3Pastora.lista.pronostico[[1]] <- O3Pastora.tabla.pronostico
    save(O3Pastora.lista.pronostico, file = "tablas/pronosticos/O3Pastora.lista.pronostico.RData")
  }

  if(file.exists("tablas/pronosticos/O3Bernabe.lista.pronostico.RData")){
    load("tablas/pronosticos/O3Bernabe.lista.pronostico.RData")
    system(paste("cp '", SPCA_HOME, "/tablas/pronosticos/O3Bernabe.lista.pronostico.RData' '", SPCA_HOME, "/tablas/pronosticos/O3Bernabe.lista.pronostico.RData.bkp'", sep = ""), intern = T)
    O3Bernabe.lista.pronostico[[length(O3Bernabe.lista.pronostico) + 1]] <- O3Bernabe.tabla.pronostico
    save(O3Bernabe.lista.pronostico, file = "tablas/pronosticos/O3Bernabe.lista.pronostico.RData")
  }
  else{
   O3Bernabe.lista.pronostico <- list()
        O3Bernabe.lista.pronostico[[1]] <- O3Bernabe.tabla.pronostico
    save(O3Bernabe.lista.pronostico, file = "tablas/pronosticos/O3Bernabe.lista.pronostico.RData")
  } 
  
  if(file.exists("tablas/pronosticos/O3Catarina.lista.pronostico.RData")){
    load("tablas/pronosticos/O3Catarina.lista.pronostico.RData")
    system(paste("cp '", SPCA_HOME, "/tablas/pronosticos/O3Catarina.lista.pronostico.RData' '", SPCA_HOME, "/tablas/pronosticos/O3Catarina.lista.pronostico.RData.bkp'", sep = ""), intern = T)
    O3Catarina.lista.pronostico[[length(O3Catarina.lista.pronostico) + 1]] <- O3Catarina.tabla.pronostico
    save(O3Catarina.lista.pronostico, file = "tablas/pronosticos/O3Catarina.lista.pronostico.RData")
  }
  else{
   O3Catarina.lista.pronostico <- list()
        O3Catarina.lista.pronostico[[1]] <- O3Catarina.tabla.pronostico
    save(O3Catarina.lista.pronostico, file = "tablas/pronosticos/O3Catarina.lista.pronostico.RData")
  } 

  if(file.exists("tablas/pronosticos/O3Nicolas.lista.pronostico.RData")){
    load("tablas/pronosticos/O3Nicolas.lista.pronostico.RData")
    system(paste("cp '", SPCA_HOME, "/tablas/pronosticos/O3Nicolas.lista.pronostico.RData' '", SPCA_HOME, "/tablas/pronosticos/O3Nicolas.lista.pronostico.RData.bkp'", sep = ""), intern = T)
    O3Nicolas.lista.pronostico[[length(O3Nicolas.lista.pronostico) + 1]] <- O3Nicolas.tabla.pronostico
    save(O3Nicolas.lista.pronostico, file = "tablas/pronosticos/O3Nicolas.lista.pronostico.RData")
  }
  else{
   O3Nicolas.lista.pronostico <- list()
        O3Nicolas.lista.pronostico[[1]] <- O3Nicolas.tabla.pronostico
    save(O3Nicolas.lista.pronostico, file = "tablas/pronosticos/O3Nicolas.lista.pronostico.RData")
  } 

  if(file.exists("tablas/pronosticos/PM10Obispado.lista.pronostico.RData")){
    load("tablas/pronosticos/PM10Obispado.lista.pronostico.RData")
    system(paste("cp '", SPCA_HOME, "/tablas/pronosticos/PM10Obispado.lista.pronostico.RData' '", SPCA_HOME, "/tablas/pronosticos/PM10Obispado.lista.pronostico.RData.bkp'", sep = ""), intern = T)
    PM10Obispado.lista.pronostico[[length(PM10Obispado.lista.pronostico) + 1]] <- PM10Obispado.tabla.pronostico
    save(PM10Obispado.lista.pronostico, file = "tablas/pronosticos/PM10Obispado.lista.pronostico.RData")
  }
  else{
   PM10Obispado.lista.pronostico <- list()
        PM10Obispado.lista.pronostico[[1]] <- PM10Obispado.tabla.pronostico
    save(PM10Obispado.lista.pronostico, file = "tablas/pronosticos/PM10Obispado.lista.pronostico.RData")
  }
  
  if(file.exists("tablas/pronosticos/PM10Pastora.lista.pronostico.RData")){
    load("tablas/pronosticos/PM10Pastora.lista.pronostico.RData")
    system(paste("cp '", SPCA_HOME, "/tablas/pronosticos/PM10Pastora.lista.pronostico.RData' '", SPCA_HOME, "/tablas/pronosticos/PM10Pastora.lista.pronostico.RData.bkp'", sep = ""), intern = T)
    PM10Pastora.lista.pronostico[[length(PM10Pastora.lista.pronostico) + 1]] <- PM10Pastora.tabla.pronostico
    save(PM10Pastora.lista.pronostico, file = "tablas/pronosticos/PM10Pastora.lista.pronostico.RData")
  }
  else{
   PM10Pastora.lista.pronostico <- list()
        PM10Pastora.lista.pronostico[[1]] <- PM10Pastora.tabla.pronostico
    save(PM10Pastora.lista.pronostico, file = "tablas/pronosticos/PM10Pastora.lista.pronostico.RData")
  }

  if(file.exists("tablas/pronosticos/PM10Bernabe.lista.pronostico.RData")){
    load("tablas/pronosticos/PM10Bernabe.lista.pronostico.RData")
    system(paste("cp '", SPCA_HOME, "/tablas/pronosticos/PM10Bernabe.lista.pronostico.RData' '", SPCA_HOME, "/tablas/pronosticos/PM10Bernabe.lista.pronostico.RData.bkp'", sep = ""), intern = T)
    PM10Bernabe.lista.pronostico[[length(PM10Bernabe.lista.pronostico) + 1]] <- PM10Bernabe.tabla.pronostico
    save(PM10Bernabe.lista.pronostico, file = "tablas/pronosticos/PM10Bernabe.lista.pronostico.RData")
  }
  else{
   PM10Bernabe.lista.pronostico <- list()
        PM10Bernabe.lista.pronostico[[1]] <- PM10Bernabe.tabla.pronostico
    save(PM10Bernabe.lista.pronostico, file = "tablas/pronosticos/PM10Bernabe.lista.pronostico.RData")
  } 
  
  if(file.exists("tablas/pronosticos/PM10Catarina.lista.pronostico.RData")){
    load("tablas/pronosticos/PM10Catarina.lista.pronostico.RData")
    system(paste("cp '", SPCA_HOME, "/tablas/pronosticos/PM10Catarina.lista.pronostico.RData' '", SPCA_HOME, "/tablas/pronosticos/PM10Catarina.lista.pronostico.RData.bkp'", sep = ""), intern = T)
    PM10Catarina.lista.pronostico[[length(PM10Catarina.lista.pronostico) + 1]] <- PM10Catarina.tabla.pronostico
    save(PM10Catarina.lista.pronostico, file = "tablas/pronosticos/PM10Catarina.lista.pronostico.RData")
  }
  else{
   PM10Catarina.lista.pronostico <- list()
        PM10Catarina.lista.pronostico[[1]] <- PM10Catarina.tabla.pronostico
    save(PM10Catarina.lista.pronostico, file = "tablas/pronosticos/PM10Catarina.lista.pronostico.RData")
  } 

  if(file.exists("tablas/pronosticos/PM10Nicolas.lista.pronostico.RData")){
    load("tablas/pronosticos/PM10Nicolas.lista.pronostico.RData")
    system(paste("cp '", SPCA_HOME, "/tablas/pronosticos/PM10Nicolas.lista.pronostico.RData' '", SPCA_HOME, "/tablas/pronosticos/PM10Nicolas.lista.pronostico.RData.bkp'", sep = ""), intern = T)
    PM10Nicolas.lista.pronostico[[length(PM10Nicolas.lista.pronostico) + 1]] <- PM10Nicolas.tabla.pronostico
    save(PM10Nicolas.lista.pronostico, file = "tablas/pronosticos/PM10Nicolas.lista.pronostico.RData")
  }
  else{
   PM10Nicolas.lista.pronostico <- list()
        PM10Nicolas.lista.pronostico[[1]] <- PM10Nicolas.tabla.pronostico
    save(PM10Nicolas.lista.pronostico, file = "tablas/pronosticos/PM10Nicolas.lista.pronostico.RData")
  }
  
  rm(tabla.maestra, pos = .GlobalEnv)
  rm(tabla.maestra.imputada, pos = .GlobalEnv)

  rm(O3Obispado, pos = .GlobalEnv)
  rm(O3Pastora, pos = .GlobalEnv)
  rm(O3Bernabe, pos = .GlobalEnv)
  rm(O3Catarina, pos = .GlobalEnv)
  rm(O3Nicolas, pos = .GlobalEnv)
  rm(PM10Obispado, pos = .GlobalEnv)
  rm(PM10Pastora, pos = .GlobalEnv)
  rm(PM10Bernabe, pos = .GlobalEnv)
  rm(PM10Catarina, pos = .GlobalEnv)
  rm(PM10Nicolas, pos = .GlobalEnv)
  
  rm(O3Obispado.tabla.pronostico, pos = .GlobalEnv)
  rm(O3Pastora.tabla.pronostico, pos = .GlobalEnv)
  rm(O3Bernabe.tabla.pronostico, pos = .GlobalEnv)
  rm(O3Catarina.tabla.pronostico, pos = .GlobalEnv)
  rm(O3Nicolas.tabla.pronostico, pos = .GlobalEnv)
  rm(PM10Obispado.tabla.pronostico, pos = .GlobalEnv)
  rm(PM10Pastora.tabla.pronostico, pos = .GlobalEnv)
  rm(PM10Bernabe.tabla.pronostico, pos = .GlobalEnv)
  rm(PM10Catarina.tabla.pronostico, pos = .GlobalEnv)
  rm(PM10Nicolas.tabla.pronostico, pos = .GlobalEnv)
}
