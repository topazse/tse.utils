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
#' Modelo de Pesos de Crecimiento por Edad
#' 
#' Usado en el modulo de demografia en el modelo de expansion. 
#' El objeto de df_conapo debe seguir mismo formato detallado en ese documento.
#' @param df_conapo data.frame de conapo
#' @export
t_mod_edad_conapo <- function(df_conapo, version = 1){
  if(version == 1){
    pesos <- data.frame("RANGO_EDAD" = c("0-14", "15-29", "30-44", "45-64", "65+"), 
                        "PESO" = c(2.155049, 3.708537,
                                   2.155049, 1.200493, 
                                   0.001998)) %>%
      mutate("PESO_REL" = PESO/9.221126)
    
    d <- df_conapo %>% 
      inner_join(., pesos) %>%
      mutate("POBPOND" = PESO_REL*POB) %>%
      group_by(Y, LLAVEGEO) %>% 
      summarise("POB" = sum(POBPOND)) %>%
      ungroup() %>%
      group_by(LLAVEGEO) %>% 
      arrange(Y) %>%
      mutate("DELTA" =  diff(c(NA,POB)), 
             "CREC_PORCENTUAL" = ((POB/lag(POB,1))-1)) %>% 
      ungroup() %>%
      dplyr::filter(Y != "2010") %>%
      group_by(LLAVEGEO) %>%
      summarise("C_TEORICO" = mean(CREC_PORCENTUAL))
    
    d
  }
}
#' Funcion para hacer zonas
#' 
#' Toma un data.frame obtenido mediante t_datos_modelo (ver tse.sql) y retorna uno agrupado por zonas, de acuerdo a las reglas de agrupacion. 
#' @param d data.frame
#' @export
t_zonificar_expansion <- function(d){
  # primero, obtener llavegeos unicas y ageb-clusters unicos
  llave_mun <- d %>% 
    dplyr::group_by(ZONA_TOPAZ_ID, LLAVEGEO) %>% 
    dplyr::summarise("p_pxmun" = sum(POBLACION)) %>% 
    dplyr::arrange(-p_pxmun) %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by(ZONA_TOPAZ_ID) %>% 
    dplyr::slice(1) %>% 
    dplyr::select(-p_pxmun)
  
  llave_cluster <- d %>% 
    dplyr::group_by(ZONA_TOPAZ_ID, CLUSTER_AGEB) %>% 
    dplyr::summarise("p_pxc" = sum(POBLACION)) %>% 
    dplyr::arrange(-p_pxc) %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by(ZONA_TOPAZ_ID) %>% 
    dplyr::slice(1) %>% 
    dplyr::select(-p_pxc)
    
  clusters_xmun <- d %>%
    dplyr::select(c(LLAVEGEO, CLUSTER)) %>% 
    unique(.)
  
  mini_mercados <- d %>% 
    dplyr::group_by(CLUSTER_AGEB) %>% 
    dplyr::summarise("POBMERCADO" = sum(POBLACION))
  
  nombres_zonas <- d %>% 
    dplyr::group_by(ZONA_TOPAZ_ID, ZONA) %>%
    dplyr::summarise("n" = n()) %>% 
    dplyr::select(-n) %>% 
    unique(.)
  
    
  # hacemos un catalogo, para poner al final 
  catalogo <- llave_cluster %>% 
    left_join(., llave_mun) %>%
    left_join(., clusters_xmun) %>% 
    left_join(., mini_mercados) %>%
    left_join(., nombres_zonas)
    
  ### Quitamos los NA's, reemplazando por el promedio
  da <- d %>% mutate_if(is.numeric, function(x){t_imp_media(x, todo = TRUE)})
  
  # calculamos promedios ponderados
  
  dd <- da %>% 
    group_by(MACRO_ZONA, ZONA_AGRUPADA, ZONA_TOPAZ_ID) %>% 
    summarise("CERCANIA" = t_ppond(POBLACION, CERCANIA)/100000,
              "CENTRALIDAD_PODER" = t_ppond(POBLACION, CENTRALIDAD_PODER),
              "CERCANIA_PROM" = t_ppond(POBLACION, CERCANIA_PROM),
              "CERCANIA_REL" = t_ppond(POBLACION, CERCANIA_REL)/100000,
              "N2" = t_ppond(POBLACION, N2),
              "N3" = t_ppond(POBLACION, N3),
              "ES_SOLO" = t_ppond(POBLACION, ES_SOLO),
              "AM_GRAPROES" = t_ppond(POBLACION, AM_GRAPROES),
              "AM_PERSXHOGAR" = t_ppond(POBLACION, AM_PERSXHOGAR),
              "AM_AUTOS" = t_ppond(POBLACION, AM_AUTOS),
              "AM_EDOCIVIL" = t_ppond(POBLACION, AM_EDOCIVIL),
              "TASA_EST" = t_ppond(POBLACION, TASA_EST),
              "CRIMENES" = t_ppond(POBLACION, CRIMENES),
              "TENDENCIA" = t_ppond(POBLACION, TENDENCIA),
              "COB_NET_PRIM" = t_ppond(POBLACION, COB_NET_PRIM),
              "COB_NET_SEC" = t_ppond(POBLACION, COB_NET_SEC),
              "COB_NET_PREP" = t_ppond(POBLACION, COB_NET_PREP),
              "COB_NET_PROF" = t_ppond(POBLACION, COB_NET_PROF),
              "REZ_TOTAL_ADULTO" = t_ppond(POBLACION, REZ_TOTAL_ADULTO),
              "REZ_PREPA_ADULTO" = t_ppond(POBLACION, REZ_PREPA_ADULTO),
              "ESCOLARIDAD" = t_ppond(POBLACION, ESCOLARIDAD),
              "PEA" = t_ppond(POBLACION, PEA),
              "CATOLICOS" = t_ppond(POBLACION, CATOLICOS),
              "CENS_PERSXHOGAR" = t_ppond(POBLACION, CENS_PERSXHOGAR),
              "CENS_AUTO" = t_ppond(POBLACION, CENS_AUTO),
              "CENS_PC" = t_ppond(POBLACION, CENS_PC),
              "P_EDAD_EDU" = t_ppond(POBLACION, P_EDAD_EDU),
              "P_EDAD_EDU_PROF" = t_ppond(POBLACION, P_EDAD_EDU_PROF),
              "P_EDAD_EDUF_PREPA" = t_ppond(POBLACION, P_EDAD_EDUF_PREPA),
              "P_EDAD_EDUF_PROF" = t_ppond(POBLACION, P_EDAD_EDUF_PROF),
              "HACINAMIENTO" = t_ppond(POBLACION, HACINAMIENTO),
              "DENSIDAD" = t_ppond(POBLACION, DENSIDAD),
              "GEN_REZ_PREPA" = t_ppond(POBLACION, GEN_REZ_PREPA),
              "GEN_PART_PEA" = t_ppond(POBLACION, GEN_PART_PEA),
              "MIGRACION" = t_ppond(POBLACION, MIGRACION),
              "C_TEORICO" = t_ppond(POBLACION, C_TEORICO),
              "C_PROM" = t_ppond(POBLACION, C_PROM),
              "HH_UE_2014" = t_ppond(POBLACION, HH_UE_2014),
              "LQ_EMP_2014_11" = t_ppond(POBLACION, LQ_EMP_2014_11),
              "LQ_EMP_2014_21" = t_ppond(POBLACION, LQ_EMP_2014_21),
              "LQ_EMP_2014_22" = t_ppond(POBLACION, LQ_EMP_2014_22),
              "LQ_EMP_2014_23" = t_ppond(POBLACION, LQ_EMP_2014_23),
              "LQ_EMP_2014_43" = t_ppond(POBLACION, LQ_EMP_2014_43),
              "LQ_EMP_2014_46" = t_ppond(POBLACION, LQ_EMP_2014_46),
              "LQ_EMP_2014_51" = t_ppond(POBLACION, LQ_EMP_2014_51),
              "LQ_EMP_2014_52" = t_ppond(POBLACION, LQ_EMP_2014_52),
              "LQ_EMP_2014_53" = t_ppond(POBLACION, LQ_EMP_2014_53),
              "LQ_EMP_2014_54" = t_ppond(POBLACION, LQ_EMP_2014_54),
              "LQ_EMP_2014_55" = t_ppond(POBLACION, LQ_EMP_2014_55),
              "LQ_EMP_2014_56" = t_ppond(POBLACION, LQ_EMP_2014_56),
              "LQ_EMP_2014_61" = t_ppond(POBLACION, LQ_EMP_2014_61),
              "LQ_EMP_2014_62" = t_ppond(POBLACION, LQ_EMP_2014_62),
              "LQ_EMP_2014_71" = t_ppond(POBLACION, LQ_EMP_2014_71),
              "LQ_EMP_2014_72" = t_ppond(POBLACION, LQ_EMP_2014_72),
              "LQ_EMP_2014_81" = t_ppond(POBLACION, LQ_EMP_2014_81),
              "LQ_EMP_2014_SC" = t_ppond(POBLACION, LQ_EMP_2014_SC),
              "LQ_EMP_2014_OTROS" = t_ppond(POBLACION, LQ_EMP_2014_OTROS),
              "LQ_ING_2014_OTROS" = t_ppond(POBLACION, LQ_ING_2014_OTROS),
              "LQ_EMP_2014_PRIMARIA" = t_ppond(POBLACION, LQ_EMP_2014_PRIMARIA),
              "LQ_ING_2014_PRIMARIA" = t_ppond(POBLACION, LQ_ING_2014_PRIMARIA),
              "LQ_EMP_2014_SECUNDARIA" = t_ppond(POBLACION, LQ_EMP_2014_SECUNDARIA),
              "LQ_ING_2014_SECUNDARIA" = t_ppond(POBLACION, LQ_ING_2014_SECUNDARIA),
              "LQ_EMP_2014_TERCIARIA" = t_ppond(POBLACION, LQ_EMP_2014_TERCIARIA),
              "LQ_ING_2014_TERCIARIA" = t_ppond(POBLACION, LQ_ING_2014_TERCIARIA),
              "TMR_SEC" = t_ppond(POBLACION, TMR_SEC),
              "TMR_PREP" = t_ppond(POBLACION, TMR_PREP),
              "TMR_PROF" = t_ppond(POBLACION, TMR_PROF),
              "TA_PREP" = t_ppond(POBLACION, TA_PREP),
              "TA_PRO" = t_ppond(POBLACION, TA_PRO),
              "L_ADMINISTRACION" = t_ppond(POBLACION, L_ADMINISTRACION),
              "L_ARQUITECTURA_Y_CONSTRUCCION" = t_ppond(POBLACION, L_ARQUITECTURA_Y_CONSTRUCCION),
              "L_CIENCIAS_DE_LA_INFORMACION" = t_ppond(POBLACION, L_CIENCIAS_DE_LA_INFORMACION),
              "L_CIENCIAS_SOCIALES_Y_ESTUDIOS_DEL_COMPORTAMIENTO" = t_ppond(POBLACION, L_CIENCIAS_SOCIALES_Y_ESTUDIOS_DEL_COMPORTAMIENTO),
              "L_DERECHO" = t_ppond(POBLACION, L_DERECHO),
              "L_HUMANIDADES" = t_ppond(POBLACION, L_HUMANIDADES),
              "L_INGENIERIA_INDUSTRIAL__MECANICA_Y_ELECTRICA" = t_ppond(POBLACION, L_INGENIERIA_INDUSTRIAL__MECANICA_Y_ELECTRICA),
              "L_MANUFACTURAS_Y_PROCESOS" = t_ppond(POBLACION, L_MANUFACTURAS_Y_PROCESOS),
              "L_NEGOCIOS" = t_ppond(POBLACION, L_NEGOCIOS),
              "L_SALUD" = t_ppond(POBLACION, L_SALUD),
              "C_ADMINISTRACION" = t_ppond(POBLACION, C_ADMINISTRACION),
              "C_ARQUITECTURA_Y_CONSTRUCCION" = t_ppond(POBLACION, C_ARQUITECTURA_Y_CONSTRUCCION),
              "C_CIENCIAS_DE_LA_INFORMACION" = t_ppond(POBLACION, C_CIENCIAS_DE_LA_INFORMACION),
              "C_CIENCIAS_SOCIALES_Y_ESTUDIOS_DEL_COMPORTAMIENTO" = t_ppond(POBLACION, C_CIENCIAS_SOCIALES_Y_ESTUDIOS_DEL_COMPORTAMIENTO),
              "C_DERECHO" = t_ppond(POBLACION, C_DERECHO),
              "C_HUMANIDADES" = t_ppond(POBLACION, C_HUMANIDADES),
              "C_INGENIERIA_INDUSTRIAL__MECANICA_Y_ELECTRICA" = t_ppond(POBLACION, C_INGENIERIA_INDUSTRIAL__MECANICA_Y_ELECTRICA),
              "C_MANUFACTURAS_Y_PROCESOS" = t_ppond(POBLACION, C_MANUFACTURAS_Y_PROCESOS),
              "C_NEGOCIOS" = t_ppond(POBLACION, C_NEGOCIOS),
              "C_SALUD" = t_ppond(POBLACION, C_SALUD), 
              #### fin de los promedios ponderados, ahora las sumas... 
              "POBLACION" = sum(POBLACION),
              "EDAD_EDU" = sum(EDAD_EDU),
              "EDAD_EDU_PROF" = sum(EDAD_EDU_PROF),
              "EDAD_EDUF_PREPA" = sum(EDAD_EDUF_PREPA),
              "EDAD_EDUF_PROF" = sum(EDAD_EDUF_PROF),
              "EST_MODELO" = sum(EST_MODELO),
              "HH_COMP_GDE" = t_ppond(HH_COMP_GDE),
              "HH_GENERAL" = t_ppond(HH_GENERAL), 
              "SHR_PUB" = t_ppond(SHR_PUB), 
              "SHR_GDE" = t_ppond(SHR_GDE), 
              "SHR_MAYOR" = t_ppond(SHR_MAYOR),
              "TEND_SHR_PUB" = t_ppond(TEND_SHR_PUB)
              )
    
  # solamente las columnas numericas...
  nums <- sapply(dd, is.numeric)
  df_nums <- dd[, nums]
  # el resto de columnas... 
  df_car <- dd[,!nums]

  # escalamos todos los numericos...
  df_scale <- scale(df_nums)
  
  # unimos los datos
  df_n <- cbind.data.frame(df_car, df_scale)
  
  # unimos el catalogo
  df_n <- df_n %>% 
    left_join(., catalogo)
  
  # exp
  df_n <- df_n %>% dplyr::ungroup()
  df_n
}

