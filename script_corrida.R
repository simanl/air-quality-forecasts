rm(list=ls())
sourceDir <- function(path, trace = TRUE, ...){
  for (nm in list.files(path, pattern = "[.][R]$")){
    if(trace) cat(nm,":")
    source(file.path(path, nm), ...)
    if(trace) cat("\n")
  }
}

sourceDir("scripts/")
SPCA(entrada = "~/SPCA/air-quality-forecast-engine/ejemplos/entrada.csv")
post()
