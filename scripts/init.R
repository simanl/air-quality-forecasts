#### -- Inicialización del shell del proyecto SPCA-- ####
# source("scripts/imputacion_datos.SIMA.R")
# source("scripts/lectura_validacion_colsTemp.R")
# source("scripts/modelos_pronostico.R")
# source("scripts/post.R")
# source("scripts/SPCA.R")
# source("scripts/stSIMA.R")
# source("scripts/tablas.R")
##Depende de la declaración explícita de los nombres del scripts y pueden cambiar o agregarse más scripts (de hecho)
##Se cambia a esto:

if( any( search()=="rutinas" )) detach(rutinas)
rutinas <- new.env()
sourceDir <- function(path, trace = TRUE){
  for (nm in list.files(path, pattern = "[.][R]$")){
    if(nm!="init.R"){
      if(trace) cat("INIT: -->", file.path(path, nm))
	sys.source(file.path(path, nm), rutinas)}
      if(trace) cat("\n")
  }
}

sourceDir("scripts")
attach(rutinas)
rm(list=ls(.GlobalEnv))


