#' Genera un indicador estandar de tendencia ciclo
#'
#' Genera un indicador estandar las tendencias ciclo (4 si esta creciendo y ademas esta por encima de la mediana de la tendencia definida en el parametro, 
#' 3 si esta por encima de la mediana de tendencia pero decreciendo, 2 si esta por debajo pero creciendo y 1 si esta decreciendo y por debajo). 
#' Retorna un objeto en forma de lista, donde "modelo_sum" es el resumen del modelo ARIMA-X13, "modelo" es el objeto de clase seas, 
#' "d" es el data.frame con las estimaciones y "tendencia" es el indicador de tendencia estandar.
#' Si fast = TRUE, no se genera un modelo de descomposicion estacional (usa el paquete seasonal y el algoritmo X13).
#' 
#' @param m vector de meses como fecha (en orden de menor a mayor)
#' @param v vector numerico
#' @param tendencia meses para comparativa de tendencia. Default = 12.
#' @param fast se debe saltar el modelo?
#' @export
t_tendencia_ciclo <- function(m, v, tendencia = 12, fast = FALSE){
  m <- as.Date(m)
  
if(!fast){
  # hacemos arima
  require(seasonal)
  require(zoo)
  require(lubridate)

# modelo ARIMA  
tt <- ts(data = v, 
         start = c(lubridate::year(min(as.Date(m))),
                   lubridate::month(min(as.Date(m)))), 
         frequency = 12)
mod <- seas(tt)

# data.frame
df_seas <- as.data.frame(mod$data)
df_seas$M <- m
}else{
  mod <- "sin modelo"
}

# -----
# Decomposicion logica de serie

media <- mean(tail(v, tendencia))
mediana <- median(tail(v, tendencia))

mma <- inegiR::YoY(v, 12)

g <- sum(tail(v, 3))/sum(v[(length(v)-14):(length(v)-12)])-1
if(is.nan(g)){ g <- 0}

if(mean(tail(v, 3)) > mediana){
  # arriba de la tendencia
  if(g>0){
    # creciendo
    t <- 4
  }else{
    t <- 3
  }
}else{
  # abajo de la tendencia
  if(g>0){
    # creciendo
    t <- 2
  }else{
    t <- 1
  }
}

# Medidas generales
l <- list("modelo_sum" = summary(m), 
          "modelo" = ifelse(fast, "Sin Modelo", mod),
          "d" = ifelse(fast, "Sin datos", as.data.frame(mod$data)),
          "tendencia" = t
          )
 return(l) 
}
