
library(lmtest)
library(sandwich)
library(gmodels)
library(dplyr)



load("../explore/cv_resultados.RData")  

df_convenio$liqtotal_dias <- df_convenio$liq_total_tope/df_convenio$sueldo

vars <- c("liqtotal_dias", "liq_total_tope", "c_antiguedad", "hextra", "sueldo","rec20","c_indem", 
          "gen", "top_dem", "giro_empresa00","giro_empresa22", "giro_empresa31", "giro_empresa32",
          "giro_empresa33","giro_empresa43", "giro_empresa46", "giro_empresa48", "giro_empresa49",
          "giro_empresa51", "giro_empresa52", "giro_empresa53", "giro_empresa54", "giro_empresa55",
          "giro_empresa56", "giro_empresa61", "giro_empresa62", "giro_empresa71", "giro_empresa72")


#df_modelo_convenio <- dplyr::select(df_convenio, one_of(vars))

reg_conv <- filter(df_exp2, modo_termino==1) %>%  
  glm(liq_total_tope ~  c_antiguedad + hextra + sueldo + rec20 + c_indem + 
        gen+ top_dem + giro_empresa00 + giro_empresa22+ giro_empresa31 + giro_empresa32 +
        giro_empresa33 + giro_empresa43 + giro_empresa46 + giro_empresa48 + giro_empresa49 +
        giro_empresa51 + giro_empresa52 + giro_empresa53 + giro_empresa54 + giro_empresa55 +
        giro_empresa56 + giro_empresa61 + giro_empresa62 + giro_empresa71 + giro_empresa72  , data=.)

coef <- as.vector(reg_conv$coefficients)

factor2num <- function(var){
  as.numeric(as.character(var))
}

df_convenio$hextra <- factor2num(df_convenio$hextra)
df_convenio$gen <- factor2num(df_convenio$gen)
df_convenio$rec20 <- factor2num(df_convenio$rec20)


# Cross Validation
cross_validation_intervalo <- function(df,porcentaje_entrena, quintil_low, quintil_high){
  
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
    
    matriz_modelo <- as.matrix(df_modelo_convenio[3:28])
    valores_entrenados <- (matriz_modelo %*% coef[2:27] + coef[1])/df.entrena$sueldo
    
    porcentaje_intervalo[i] <- sum(valores_entrenados>quantile(df.prueba$liqtotal_dias, probs = quintil_low, 
                               na.rm = T)&valores_entrenados<quantile(df.prueba$liqtotal_dias, probs = quintil_high , 
                                na.rm = T), na.rm = T)/N
    
    medias[i]<- mean(df.prueba$liqtotal_dias, na.rm=T)
    medianas[i] <- quantile(df.prueba$liqtotal_dias, probs = .5 , na.rm = T)
    intervalo_high[i] <- quantile(df.prueba$liqtotal_dias, probs = quintil_high, na.rm = T)
    intervalo_low[i] <- quantile(df.prueba$liqtotal_dias, probs = quintil_low, na.rm = T)
    media_dentro_intervalo[i] <- mean(df.prueba$liqtotal_dias[df.prueba$liqtotal_dias>quantile(df.prueba$liqtotal_dias, probs = quintil_low, 
     na.rm = T)&df.prueba$liqtotal_dias<quantile(df.prueba$liqtotal_dias, probs = quintil_high , 
     na.rm = T)] ,na.rm=T)
  }
  
  res <- data.frame(cbind(porcentaje_intervalo, medias, medianas, intervalo_low,intervalo_high, media_dentro_intervalo))
  return(res)
}  

cvi_1 <- cross_validation_intervalo(df_convenio, .5, .125, .875) # 75% de la distribución
cvi_2 <- cross_validation_intervalo(df_convenio, .5, .165, .835) # 67% de la distribución
cvi_3 <- cross_validation_intervalo(df_convenio, .5, .2, .8)    # 60% de la distribución
cvi_4 <- cross_validation_intervalo(df_convenio, .5, .225, .775) # 55% de la distribución
cvi_5 <- cross_validation_intervalo(df_convenio, .5, .275, .725) # 51% de la distribución
