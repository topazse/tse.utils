#' Modelo de Empleos-Alumnos
#'
#' Estima alumnos en base a crecimientos por industria de la economía
#'
#' @param s_ind11 Personal ocupado inicial en industria 11
#' @param s_ind53 Personal ocupado inicial en industria 53
#' @param s_ind21 Personal ocupado inicial en industria 21 
#' @param s_ind54 Personal ocupado inicial en industria 54
#' @param s_ind22 Personal ocupado inicial en industria 22
#' @param s_ind55 Personal ocupado inicial en industria 55
#' @param s_ind43 Personal ocupado inicial en industria 43
#' @param s_ind56 Personal ocupado inicial en industria 56
#' @param s_ind23 Personal ocupado inicial en industria 23
#' @param s_ind46 Personal ocupado inicial en industria 46
#' @param s_ind52 Personal ocupado inicial en industria 52 
#' @param s_ind51 Personal ocupado inicial en industria 51
#' @param s_ind71 Personal ocupado inicial en industria 71 
#' @param s_indSC Personal ocupado inicial en industria SC 
#' @param s_ind72 Personal ocupado inicial en industria 72 
#' @param s_ind81 Personal ocupado inicial en industria 81 
#' @param s_ind61 Personal ocupado inicial en industria 61 
#' @param s_ind62 Personal ocupado inicial en industria 62 
#' @param s_ind31 Personal ocupado inicial en industria 31
#' @param c_ind11 Porcentaje de crecimiento lineal de industria 11 
#' @param c_ind53 Porcentaje de crecimiento lineal de industria 53, 
#' @param c_ind21 Porcentaje de crecimiento lineal de industria 21, 
#' @param c_ind54 Porcentaje de crecimiento lineal de industria 54, 
#' @param c_ind22 Porcentaje de crecimiento lineal de industria 22, 
#' @param c_ind55 Porcentaje de crecimiento lineal de industria 55, 
#' @param c_ind43 Porcentaje de crecimiento lineal de industria 43, 
#' @param c_ind56 Porcentaje de crecimiento lineal de industria 56, 
#' @param c_ind23 Porcentaje de crecimiento lineal de industria 23, 
#' @param c_ind46 Porcentaje de crecimiento lineal de industria 46, 
#' @param c_ind52 Porcentaje de crecimiento lineal de industria 52, 
#' @param c_ind51 Porcentaje de crecimiento lineal de industria 51
#' @param c_ind71 Porcentaje de crecimiento lineal de industria 71
#' @param c_indSC Porcentaje de crecimiento lineal de industria SC
#' @param c_ind72 Porcentaje de crecimiento lineal de industria 72
#' @param c_ind81 Porcentaje de crecimiento lineal de industria 81
#' @param c_ind61 Porcentaje de crecimiento lineal de industria 61
#' @param c_ind62 Porcentaje de crecimiento lineal de industria 62
#' @param c_ind31 Porcentaje de crecimiento lineal de industria 31
#' @param d_inxgue Cambio en ingreso por unidad economica en industria 55
#' @param tpt Total previo de trabajadores
#' @param dif_hh diferencial en indice herfindahl
#' @param p_joven porcentaje (en decimal) de personas jovenes en el municipio
#' @param zm 1 si es zona metropolitana
#' @param version version del modelo
#' @export
t_modelo_empleos <- function(s_ind11, s_ind53, s_ind21, s_ind54, 
                             s_ind22, s_ind55, s_ind43, s_ind56, 
                             s_ind23, s_ind46, s_ind52, s_ind51,  
                             s_ind71, s_indSC, s_ind72, s_ind81, 
                             s_ind61, s_ind62, s_ind31, 
                             c_ind11, c_ind53, c_ind21, c_ind54, 
                             c_ind22, c_ind55, c_ind43, c_ind56, 
                             c_ind23, c_ind46, c_ind52, c_ind51, 
                             c_ind71, c_indSC, c_ind72, c_ind81, 
                             c_ind61, c_ind62, c_ind31, 
                             d_ingxue_55, 
                             tpt, 
                             dif_hh, 
                             p_joven, 
                             zm, 
                             version = 1 
                             ){
  
  a_est <- s_ind11*(1 + c_ind11)+
    s_ind53*(1 + c_ind53)+
    s_ind21*(1 + c_ind21)+
    s_ind54*(1 + c_ind54)+
    s_ind22*(1 + c_ind22)+
    s_ind55*(1 + c_ind55)+
    s_ind43*(1 + c_ind43)+
    s_ind56*(1 + c_ind56)+
    s_ind23*(1 + c_ind23)+
    s_ind46*(1 + c_ind46)+
    s_ind52*(1 + c_ind52)+
    s_ind51*(1 + c_ind51)+
    s_ind71*(1 + c_ind71)+
    s_indSC*(1 + c_indSC)+
    s_ind72*(1 + c_ind72)+
    s_ind81*(1 + c_ind81)+
    s_ind61*(1 + c_ind61)+
    s_ind31*(1 + c_ind31)
  r <- a_est - tpt
  
  if(version==1){
    # prepa
  s <- 6.921 + r*(1.398)+zm*(1.349)+p_joven*(-1.087)  
  }
  if(version==2){
    # profesional
  s <- r*(3.5130)+zm*(1.452)+dif_hh*(2.104)+p_joven*(9.114)    
  }
  if(version==3){
    # posgrado
  s <- r*(5.019)+zm*(4.0134)+d_ingxue_55*(5.151)+p_joven*(4.2133)    
  }
  
  s
}
