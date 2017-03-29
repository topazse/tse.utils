#' Importar varios archivos
#'
#' Metodo para importar uno o varios archivos. Regex y extension se ponen en dos parametros.
#' Regresa ademas un resumen.
#'
#' @param regex nombre de archivo o archivos en formato regex
#' @param ext nombre de extension con punto
#' @param mismas si TRUE, todos los archivos tienen el mismo formato (columnas). Si FALSE retorna solo el último.
#'
#' @export
t_i <- function(regex, ext, mismas=FALSE){
  ext <- tolower(ext)
  
  a <- list.files(pattern = regex)
  n <- length(a)
  print(paste0("Importando... ", n, " archivos"))
  
  if(ext==".xlsx"){

  }else{
    if(ext==".dbf"){

    }else{
      if(ext==".sav"){
        
        r <- NULL
        for(i in 1:n){
          
          tmp <- memisc::as.data.set(spss.system.file(a[i]))
          tmp <- as.data.frame(tmp)
          
          print(paste0("Archivo: ", i, ":",a[i]," (", nrow(tmp), " renglones)"))
          if(mismas){
            r <- rbind.data.frame(tmp, r)
          }else{
            r <- as.data.frame(tmp)
          }
        }
      }
    }
  }
r
}
#' Agregar string de conexion a codigo INEGI
#'
#' Al pasar un codigo inegi, agrega lo necesario para conectar con inegiR
#'
#' @param codigo codigo de indicador (i.e. 251577)
#'
#' @export

t_hacer_str_inegi <- function(codigo){
  s <- paste0("http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/", codigo, "/00000/en/false/xml/")
  s
}
