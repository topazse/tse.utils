.onAttach <- function(libname, pkgname){
  packageStartupMessage("Biblioteca de Utensilios Generales para R (v0.1) - Topaz Soluciones Educativas")
  # variables a cargar
  token_inegi <<-"502ae69d-a835-8bd5-0e73-2173cc35c83d"
  token_inegi_denue <<-"a3c02469-d06d-421a-95e5-8401047c6786"
  # cargar paquetes...
  library(dplyr)
  library(inegiR)
  library(ggplot2)
  library(magrittr)
  # library(xlsx)
  library(foreign)
}
