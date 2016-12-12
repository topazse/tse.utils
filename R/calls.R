#' Actualizar todos los paquetes
#'
#' Actualiza de github todos los paquetes de topazse.
#' Importante: se agregan dentro de la funcion de manera manual.
#' @export
t_actualizartodo <- function(){
  devtools::install_github("topazse/tse.utils")
  devtools::install_github("topazse/tse.sql")
}
