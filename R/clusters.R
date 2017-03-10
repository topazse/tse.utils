#' Revisar clusters
#'
#' Contiene visualizaciones mas sencillas para visualizar clusters optimos
#' @param .data data.frame de datos
#' @param id columna (en numero) con ids de individuos
#' @param tipo visualizacion ("scree", "optimos" o "contrib")
#' @param verbose si TRUE genera mensajes de avance
#' @export
t_cluster_revisar <- function(.data, id = 1, tipo = "scree", verbose = TRUE){
  require(zoo)
  require(factoextra)
  require(FactoMineR)
  
  ids <- as.vector(.data[, id])
  d <- .data[, -id]
  
  nums <- sapply(.data, is.numeric)
  d <- .data[, nums]
  
  if(verbose){
    print(paste0("Usando columnas: ", paste0(names(d), collapse = " *** ")))
  }
  
  d <- scale(d)
  d <- na.aggregate(d)
  
  if(tipo == "scree"){
    r <- PCA(d, graph = FALSE)
    fviz_screeplot(r, addlabels = TRUE)  
    
  }else{
    if(tipo == "optimos"){
      fviz_nbclust(d, kmeans)
      
    }else{
      if(tipo == "contrib"){
        get_pca_var(r)$contrib
        
      }else{
        stop("Tipo debe ser optimos, scree o contrib")  
      }
    }
  }
  # end
}
#' Hacer clusters
#'
#' Genera clusters de manera sencilla
#' @param .data data.frame de datos
#' @param id columna (en numero) con ids de individuos
#' @param clusters numero de clusters a hacer
#' @param tipo solamente "kmeans" implementada actualmente
#' @param distancias tipo de distancias, a pasar a cls.scatt.data
#' @param verbose si TRUE genera mensajes de avance
#' @param juntar_dfs si TRUE genera exporta data.frame con todas las columnas usadas para clusterizar. Cuando FALSE solamente id y cluster 
#' @export
t_cluster <- function(.data, id, clusters = 2, 
                      tipo = "kmeans", 
                      distancias = "euclidean", verbose = TRUE, juntar_dfs = FALSE){
  require(zoo)
  require(factoextra)
  require(FactoMineR)
  require(clv)
  
  ids <- as.vector(.data[, id])
  d <- .data[, -id]
  
  nums <- sapply(.data, is.numeric)
  d <- .data[, nums]
  
  if(verbose){
    print(paste0("Usando columnas: ", paste0(names(d), collapse = " *** ")))
  }
  
  d <- scale(d)
  d <- na.aggregate(d)
  
  if(tipo == "kmeans"){
    print(paste0("Clusterizando en ", clusters, " clusters usando ", tipo))
    r <- kmeans(x = d, centers = clusters)
    cluster_ids <- r$cluster
    
    # indicadores 
    w <- r$betweenss/r$totss # razon diferencias
    s <- r$size # tamanos
    
    # frame matriz
    m <- cls.scatt.data(data = d, cluster_ids, dist = distancias)
    m2 <- data.frame("CLUSTER" = unique(cluster_ids), 
                     "INTRA_PROMEDIO" = as.vector(m$intracls.average), 
                     "INTRA_CENTROIDE" = as.vector(m$intracls.centroid), 
                     "INTER_PROMEDIO" = as.vector(m$intercls.average), 
                     "INTER_CENTROIDE" = as.vector(m$intercls.centroid))
    # frame datos
    m3 <- data.frame("ID" = ids, 
                     "CLUSTER" = cluster_ids)
    if(juntar_dfs){
      m3 <- cbind.data.frame(d, m3)
    }
    
    e <- list("razon_dif" = w, 
              "tamanios" = s, 
              "mediciones" = m2, 
              "data" = m3)
    
  }
  e
}
