library(lmtest)
library(sandwich)
library(gmodels)
library(dplyr)



load("../explore/cv_resultados.RData")  

df_convenio$liqtotal_dias <- df_convenio$liq_total_tope/df_convenio$sueldo
df_convenio_1 <- dplyr::filter(df_convenio, duracion<1)

factor2num <- function(var){
  as.numeric(as.character(var))
}

df_convenio_1$hextra <- factor2num(df_convenio_1$hextra)
df_convenio_1$gen <- factor2num(df_convenio_1$gen)
df_convenio_1$rec20 <- factor2num(df_convenio_1$rec20)

df_convenio_1_menor_menor <- dplyr::filter(df_convenio_1, sueldo<quantile(sueldo, probs = .5, na.rm = T) & c_antiguedad<quantile(c_antiguedad, probs = .5, na.rm = T) )
df_convenio_1_menor_mayor <- dplyr::filter(df_convenio_1, sueldo<quantile(sueldo, probs = .5, na.rm = T) & c_antiguedad>quantile(c_antiguedad, probs = .5, na.rm = T) )
df_convenio_1_mayor_menor <- dplyr::filter(df_convenio_1, sueldo>quantile(sueldo, probs = .5, na.rm = T) & c_antiguedad<quantile(c_antiguedad, probs = .5, na.rm = T) )
df_convenio_1_mayor_mayor <- dplyr::filter(df_convenio_1, sueldo>quantile(sueldo, probs = .5, na.rm = T) & c_antiguedad>quantile(c_antiguedad, probs = .5, na.rm = T) )


# variables <- c("liqtotal_dias", "liq_total_tope", "c_antiguedad", "hextra", "sueldo","rec20","c_indem",
#           "gen", "top_dem", "giro_empresa00","giro_empresa22", "giro_empresa31", "giro_empresa32",
#           "giro_empresa33","giro_empresa43", "giro_empresa46", "giro_empresa48", "giro_empresa49",
#           "giro_empresa51", "giro_empresa52", "giro_empresa53", "giro_empresa54", "giro_empresa55",
#           "giro_empresa56", "giro_empresa61", "giro_empresa62", "giro_empresa71", "giro_empresa72")

variables <- c("liqtotal_dias", "liq_total_tope", "c_antiguedad", "hextra", "sueldo","rec20","c_indem",
               "gen", "top_dem", "giro_empresa00","giro_empresa22", "giro_3", "giro_4", "giro_5", 
               "giro_6", "giro_7")

variables2 <- c("liqtotal_dias", "liq_total_tope", "c_antiguedad", "sueldo")

#df_modelo_convenio <- dplyr::select(df_convenio, one_of(vars))

# reg_conv11 <- glm(liq_total_tope ~  c_antiguedad + hextra + sueldo + rec20 + c_indem + 
#             gen+ top_dem + giro_empresa00 + giro_empresa22+ giro_empresa31 + giro_empresa32 +
#             giro_empresa33 + giro_empresa43 + giro_empresa46 + giro_empresa48 + giro_empresa49 +
#             giro_empresa51 + giro_empresa52 + giro_empresa53 + giro_empresa54 + giro_empresa55 +
#             giro_empresa56 + giro_empresa61 + giro_empresa62 + giro_empresa71 + giro_empresa72  , data=df_convenio_1_menor_menor)

reg_conv11 <- glm(liq_total_tope ~  c_antiguedad + hextra + sueldo + rec20 + c_indem + 
                    gen+ top_dem + giro_empresa00 + giro_empresa22+ giro_3 + giro_4 + giro_5 + 
                    giro_6 + giro_7, data=df_convenio_1_menor_menor)

# reg_conv12 <- glm(liq_total_tope ~  c_antiguedad +sueldo, data=df_convenio_1_menor_menor)

# reg_conv21 <- glm(liq_total_tope ~  c_antiguedad + hextra + sueldo + rec20 + c_indem + 
#         gen+ top_dem + giro_empresa00 + giro_empresa22+ giro_empresa31 + giro_empresa32 +
#         giro_empresa33 + giro_empresa43 + giro_empresa46 + giro_empresa48 + giro_empresa49 +
#         giro_empresa51 + giro_empresa52 + giro_empresa53 + giro_empresa54 + giro_empresa55 +
#         giro_empresa56 + giro_empresa61 + giro_empresa62 + giro_empresa71 + giro_empresa72  , data=df_convenio_1_mayor_menor)

reg_conv21 <- glm(liq_total_tope ~  c_antiguedad + hextra + sueldo + rec20 + c_indem + 
                    gen+ top_dem + giro_empresa00 + giro_empresa22+ giro_3 + giro_4 + giro_5 + 
                    giro_6 + giro_7 , data=df_convenio_1_mayor_menor)

# reg_conv22 <- glm(liq_total_tope ~  c_antiguedad +sueldo, data=df_convenio_1_mayor_menor)

# reg_conv31 <- glm(liq_total_tope ~  c_antiguedad + hextra + sueldo + rec20 + c_indem + 
#         gen+ top_dem + giro_empresa00 + giro_empresa22+ giro_empresa31 + giro_empresa32 +
#         giro_empresa33 + giro_empresa43 + giro_empresa46 + giro_empresa48 + giro_empresa49 +
#         giro_empresa51 + giro_empresa52 + giro_empresa53 + giro_empresa54 + giro_empresa55 +
#         giro_empresa56 + giro_empresa61 + giro_empresa62 + giro_empresa71 + giro_empresa72  , data=df_convenio_1_menor_mayor)

reg_conv31 <- glm(liq_total_tope ~  c_antiguedad + hextra + sueldo + rec20 + c_indem + 
                    gen+ top_dem + giro_empresa00 + giro_empresa22+ giro_3 + giro_4 + giro_5 + 
                    giro_6 + giro_7  , data=df_convenio_1_menor_mayor)

# reg_conv32 <- glm(liq_total_tope ~  c_antiguedad +sueldo, data=df_convenio_1_menor_mayor)

# reg_conv41 <- glm(liq_total_tope ~  c_antiguedad + hextra + sueldo + rec20 + c_indem + 
#         gen+ top_dem + giro_empresa00 + giro_empresa22+ giro_empresa31 + giro_empresa32 +
#         giro_empresa33 + giro_empresa43 + giro_empresa46 + giro_empresa48 + giro_empresa49 +
#         giro_empresa51 + giro_empresa52 + giro_empresa53 + giro_empresa54 + giro_empresa55 +
#         giro_empresa56 + giro_empresa61 + giro_empresa62 + giro_empresa71 + giro_empresa72  , data=df_convenio_1_mayor_mayor)

reg_conv41 <- glm(liq_total_tope ~  c_antiguedad + hextra + sueldo + rec20 + c_indem + 
                    gen+ top_dem + giro_empresa00 + giro_empresa22+ giro_3 + giro_4 + giro_5 + 
                    giro_6 + giro_7   , data=df_convenio_1_mayor_mayor)

# reg_conv42 <- glm(liq_total_tope ~  c_antiguedad +sueldo, data=df_convenio_1_mayor_mayor)

coef11 <- as.vector(reg_conv11$coefficients)
coef11[is.na(coef11)] <- 0
# coef12 <- as.vector(reg_conv12$coefficients)
# coef12[is.na(coef12)] <- 0
coef21 <- as.vector(reg_conv21$coefficients)
coef21[is.na(coef21)] <- 0
# coef22 <- as.vector(reg_conv22$coefficients)
# coef22[is.na(coef22)] <- 0
coef31 <- as.vector(reg_conv31$coefficients)
coef31[is.na(coef31)] <- 0
# coef32 <- as.vector(reg_conv32$coefficients)
# coef32[is.na(coef32)] <- 0
coef41 <- as.vector(reg_conv41$coefficients)
coef41[is.na(coef41)] <- 0
# coef42 <- as.vector(reg_conv42$coefficients)
# coef42[is.na(coef42)] <- 0

# Cross Validation
cross_validation_intervalo <- function(df,porcentaje_entrena, quintil_low, quintil_high, vars, coeficientes){
  
  # Variables de captura de resultados (nombrar con otra cosa)
  medias<- NULL
  medianas <- NULL
  porcentaje_intervalo <- NULL
  intervalo_high <- NULL
  intervalo_low <- NULL
  media_dentro_intervalo <- NULL
  
  for( i in 1:1000){ #cambiar a 1000
    
    set.seed(i)
    N <- floor(nrow(df) * porcentaje_entrena)
    set.seed(i)
    indices_entrena <- sample(df$id_actor, N) 
    df.entrena <- dplyr::filter(df, id_actor %in% indices_entrena) 
    df.prueba <- dplyr::filter(df, !(id_actor %in% indices_entrena)) 
    
    df_modelo_convenio <- dplyr::select(df.entrena, one_of(vars))
    
    matriz_modelo <- as.matrix(df_modelo_convenio[3:length(vars)])
    valores_entrenados <- (matriz_modelo %*% coeficientes[2:(length(vars)-1)] + coeficientes[1])/df.entrena$sueldo
    
    porcentaje_intervalo[i] <- sum(valores_entrenados>quantile(df.prueba$liqtotal_dias, probs = quintil_low, 
                               na.rm = T)&valores_entrenados<quantile(df.prueba$liqtotal_dias, probs = quintil_high , 
                               na.rm = T), na.rm = T)/N
    
    medias[i]<- mean(df.prueba$liqtotal_dias, na.rm=T)
    medianas[i] <- quantile(df.prueba$liqtotal_dias, probs = .5 , na.rm = T)
    intervalo_high[i] <- quantile(df.prueba$liqtotal_dias, probs = quintil_high, na.rm = T)
    intervalo_low[i] <- quantile(df.prueba$liqtotal_dias, probs = quintil_low, na.rm = T)
    media_dentro_intervalo[i] <- mean(df.prueba$liqtotal_dias[df.prueba$liqtotal_dias>quantile(df.prueba$liqtotal_dias, probs = quintil_low, 
    na.rm = T)&df.prueba$liqtotal_dias<quantile(df.prueba$liqtotal_dias, probs = quintil_high , na.rm = T)] ,na.rm=T)
  }
  
  res <- data.frame(cbind(porcentaje_intervalo, medias, medianas, intervalo_low,intervalo_high, media_dentro_intervalo))
  #res <- data.frame(cbind(porcentaje_intervalo, intervalo_low,intervalo_high, media_dentro_intervalo))
  
  return(res)
}  

# df,porcentaje_entrena, quintil_low, quintil_high, variables, coeficientes
cvi_11 <- cross_validation_intervalo(df_convenio_1_menor_menor, .5, .245, .755, variables, coef11) # 51% de la distribución
cvi_21 <- cross_validation_intervalo(df_convenio_1_mayor_menor, .5, .245, .755, variables, coef21) # 51% de la distribución
cvi_31 <- cross_validation_intervalo(df_convenio_1_menor_mayor, .5, .245, .755, variables, coef31) # 51% de la distribución
cvi_41 <- cross_validation_intervalo(df_convenio_1_mayor_mayor, .5, .245, .755, variables, coef41) # 51% de la distribución

