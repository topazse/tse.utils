#' Funcion para desagregar a nivel AGEB
#'
#' Implementado solamente para crimen (V1)
#' @param v_marg variable de marginacion del ageb (Alto, Medio o Bajo)
#' @param v_derechs variable derecho social. Porcentaje de derechohabiente de seguridad social (excepto seguro popular) respecto a la población total
#' @param v_relhm variable relacion hombre-mujer. Razón del total de hombres respecto al total de mujeres
#' @param v_edadmed variable edad media. Edad promedio de los habitantes mayores de 14 años
#' @param v_graproes variable grado promedio escolaridad. Grado Promedio de Escolaridad en años
#' @param v_hjf variable hogares jefa de familia mujer. Porcentaje de hogares censales con jefatura femenina respecto al total de hogares
#' @param version version a estimar.
t_desagreg_ageb <- function(v_marg,
                            v_derechs,
                            v_relhm,
                            v_edadmed,
                            v_graproes,
                            v_hjf,
                            version = "crimen_v1"){
  if(version == "crimen_v1"){
    # Modelo de desagregacion por crimen. 
    ### Usando modelo de Medellin, Mendoza (UANL): http://eprints.uanl.mx/3950/1/1080253615.pdf
    ### Para evitar nulos pongo promedio del pais.
    marginacion <- ifelse(v_marg == "Alto",0.049, ifelse(v_marg=="Medio", 0.033, 0))
    r <- ifelse(is.na(v_derechs), 0.7122, v_derechs)*(-0.064)+
          ifelse(is.na(v_relhm), 1.011, v_relhm)*(-0.0001)+
           ifelse(is.na(v_edadmed), 26.9912, v_edadmed)*(-0.002)+
            ifelse(is.na(v_graproes), 8.112, v_graproes)*(-0.008)+
             ifelse(is.na(v_hjf), 0.3511, v_hjf)*(-0.279)+
            marginacion
    
    r <- abs(r)
    
  }else{
    
  }
  
  r
}
