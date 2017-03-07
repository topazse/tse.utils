#' Eliminar outliers
#'
#' Elimina outliers de columna al porcentaje parametrizado
#'
#' @param .data dataset a usar
#' @param por columna (desnuda) por la cual eliminar outliers
#' @param p porcentaje (en decimal) a eliminar de registros
#' @param tipo "ambos", "abajo" o "arriba" depende el caso
#' @examples
#'  df <- data.frame("columna1" = c(1,2,3,4,5,6,1,2,3,4,5,6),
#'                   "columna2" = c(84,102,99,103,101,109,
#'                                  116,121,122,119,131,222))
#' df_sin <- t_eoutliers(df, columna2)
#' @export
t_eoutliers <- function(.data, por, p = 0.01, tipo = "ambos"){
  .data <- as.data.frame(.data)
  e <- length(.data[,1])

  arguments <- as.list(match.call())
  col <- eval(arguments$por, .data)
  d <- with(.data, col)

  #limites
  lim_s <- 1-p
  lim_i <- p
  #quantiles
  qs <- quantile(d, lim_s)
  qi <- quantile(d, lim_i)
  #filtrar
  if(tipo=="ambos"){
    newd <- .data[with(.data, col)>qi & with(.data, col)<qs,]
  }else
    if(tipo=="abajo"){
      newd <- .data[with(.data, col)>qi,]
    }else{
      if(tipo=="arriba"){
        newd <- .data[with(.data, col)<qs,]
      }else{
        stop("No reconocido... Poner arriba, abajo o ambos en tipo.")
      }
    }

  f <- length(newd[,1])
  msg <- paste0("Eliminando... ", ifelse(e-f>10000,
                                         paste0(round((e-f)/1000, digits = 0), "k"),
                                         e-f),
                " renglones (", round((e-f)/e*100, digits = 2), "%)")
  print(msg)
  return(newd)
}
#' Retorna strings de conteo
#'
#' Al ingresar un numero, rendondea a miles o millones, de acuerdo al dato. Usado en prints para resumir informacion.
#'
#' @param n numero
#' @param aprox Si TRUE redondea, si FALSE pone todos los digitos.
#' @export
t_printnum <- function(n, aprox = TRUE){
  if(n<1000){
    nn <- n
  }else{
    if(n<999999){
      if(aprox){
        nn <- paste0("aprox. ", round(n/1000, digits = 1)," mil")
      }else{
        nn <- paste0(n/1000, " mil")
      }
      }else{
      if(aprox){
        nn <- paste0("aprox. ", round(n/1000000, digits = 0)," millones")
      }else{
        nn <- paste0(n/1000000, " millones")
      }
    }
    }
  nn
}
#' Metodo para convertir nan's de data.frames a ceros
#'
#' @param x data.frame
#' @export
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))
#' Metodo para resumir porcentajes
#'
#' @param v vector de numeros
#' @param base100 si TRUE cincuenta porciento es 50. Si FALSE multiplica por 100.
#' @export
pcts <- function(v, base100 = TRUE){
  if(base100){
    v <- round(v, digits = 1)
  }else{
    v <- round(v*100, digits = 1)
  }
  v <- paste0(v,"%")
  v
}

#' Funcion para decirme rapidamente unicos por columna
#'
#' @param .data data.frame
#' @param col numero de columa o nombre de columna
#' @export
t_lu <- function(.data, col = 1, all = FALSE){

  .data <- as.data.frame(.data)

  if(is.null(col)){
    all <- TRUE
    }
  
  if(all){
    # count all cols
    n <- length(colnames(.data))
    for(i in 1:n){
      r <- .data[,i]
      n[i] <- length(unique(r))
    }
    return(n)

  }else{
  r <- .data[, col]
  n <- length(unique(r))
  return(n)
  }
}