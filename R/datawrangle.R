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
#' @param all si TRUE da de todas las columnas. Lo mismo que dejar col en NULL.
#' @param names si TRUE, me genera un data.frame con nombres y unicos, no un vector.
#' @export
t_lu <- function(.data, col = NULL, all = FALSE, names = TRUE){

  .data <- as.data.frame(.data)

  if(is.null(col)){
    all <- TRUE
    }
  
  if(all){
    # count all cols
    n <- length(colnames(.data))
    p <- NULL
    for(i in 1:n){
      r <- .data[,i]
      n[i] <- length(unique(r))
      p[i] <- colnames(.data)[i]
    }
      if(names){
        d <- data.frame("COLUMNA" = p, 
                        "UNICOS" = n)
        return(d)
        }else{
          return(n)
        }
  }else{
  r <- .data[, col]
  n <- length(unique(r))
    
    if(names){
      d <- data.frame("COLUMNA" = col, "UNICOS" = n)
      return(d)
      }else{
      return(n)
      }
  }
}
#' Hacer llavegeo
#'
#' Toma ids de estado y municipio (en numeros o como esten) y regresa llavegeo usada en SQL.
#' @param estado estado en formato 00 o 0
#' @param municipio en formato 000, 00 o 0
#' @export
t_llavegeo <- function(estado, municipio){
  estado <- as.character(estado)
  municipio <- as.character(municipio)
  
  if(nchar(estado) == 1){
    estado <- paste0("0", estado)
  }else{ 
    if(nchar(estado) == 2){
      # nada
    }else{
      stop("Estado tiene cero o mas de 2 letras/numeros")
    }
    }
  if(nchar(municipio) == 3){
    # nada
  }else{
    if(nchar(municipio) == 2){
      municipio <- paste0("0", municipio)
    }else{
      if(nchar(municipio) == 1){
        municipio <- paste0("00", municipio)
      }else{
        stop("Municipio tiene cero o mas de 4 letras/numeros")
      }
    }
  }
  
  llave <- paste0(estado, "-", municipio)
  return(llave)
}
#' Hacer nueva columna en base a catalogo
#'
#' Toma dos vectores: el primero es la columna que esta en la base de datos y el segundo es el nuevo, en el mismo orden de unique() de esa columna. 
#' Ejemplo: unique(d$columna) es 1,5,2 y vector dos sería: c("uno","cinco","dos). Se puede poner tambien en otro orden, puesto que se junta con un left_join de dplyr.
#' Returns un vector con las nuevas categorias en el orden de la base de datos.
#' @param .data base de datos
#' @param col vector de unicos de columna a catalogar
#' @param v2 vector de nuevos nombres o categorias
#' @export
t_catalog <- function(.data, col, v2, verbose = TRUE){
  .data <- as.data.frame(.data)
  
  if(col %in% colnames(.data)){ }else{ stop("Nombre de columna no esta en data") }
  
  v1 <- .data[, col]
  v1 <- unique(v1)
  
  catalog <- data.frame("c1" = v1, 
                        "c2" = v2)
  
  names(catalog) <- c(col, paste0(col,"_2"))
  if(verbose){
    print(head(catalog))
    }
  dn <- left_join(.data, catalog)
  e <- dn[, paste0(col,"_2")]
return(e)
}
#' Imputar la media en la columna
#' 
#' Si el valor es NA, se agrega la media de la columna. Esto se puede usar dentro de un summarise (dplyr).
#' Si el parametro de todo = TRUE, entonces se cambian todos los valores infinitos y NaN's.
#'
#' @param x nombre de columna (bare si estas dentro de dplyr)
#' @param todo si FALSE solo revisa NA's
#' @export
t_imp_media <- function(x, todo = FALSE){ 
  if(todo){
    replace(x, 
            is.na(x)| is.infinite(x) | is.nan(x), 
            mean(x, na.rm = TRUE))
    mm <- x[!is.infinite(x)]
    x <- ifelse(is.infinite(x), x, mm)
    
    }else{
    replace(x, is.na(x), mean(x, na.rm = TRUE))
  }
}
