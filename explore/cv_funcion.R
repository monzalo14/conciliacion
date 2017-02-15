# Función para calcular CV: convenio y laudo>0
# Inputs: 1) modelo (formula)
#         2) data frame (df_convenio o df_laudo)
#         3) convenio o laudo>0

df_exp2 <- readRDS("../clean_data/observaciones.RDS")

# df_exp2$liq_total_tope[df_exp2$modo_termino==1 & df_exp2$liq_total_tope>quantile(df_exp2$liq_total_tope[df_exp2$modo_termino==1], 
#                     probs = .99, na.rm=T)] <- quantile(df_exp2$liq_total_tope[df_exp2$modo_termino==1], probs = .99, na.rm=T)

# Generamos dos data frames, cada uno depende del modo de termino
df_convenio <- dplyr::filter(df_exp2, modo_termino==1)
df_laudo <- dplyr::filter(df_exp2, modo_termino==3 & liq_total_tope>1)



# Fórmulas de los modelos:
# Convenio

formula_convenio <- c("liq_total_tope ~  c_antiguedad + hextra + sueldo + rec20 + c_indem + gen + top_dem + 
                       giro_empresa00 + giro_empresa22+ giro_empresa31 + giro_empresa32 + giro_empresa33 + 
                       giro_empresa43 + giro_empresa46 + giro_empresa48 + giro_empresa49 + giro_empresa51 + 
                       giro_empresa52 + giro_empresa53 + giro_empresa54 + giro_empresa55 + giro_empresa56 + 
                       giro_empresa61 + giro_empresa62 + giro_empresa71 + giro_empresa72")

formula_convenio_log <- c("ln_liq_total ~  ln_c_antiguedad+ hextra + ln_sueldo+ rec20 +ln_c_indem + gen + 
                          top_dem + giro_empresa00 + giro_empresa22 + giro_empresa31 + giro_empresa32 +
                          giro_empresa33 + giro_empresa43 + giro_empresa46 + giro_empresa48 + giro_empresa49 +
                          giro_empresa51 + giro_empresa52 + giro_empresa53 + giro_empresa54 + giro_empresa55 +
                          giro_empresa56 + giro_empresa61 + giro_empresa62 + giro_empresa71 + giro_empresa72")

# Laudo

l11 <- c("liq_total_tope ~ c_antiguedad +c_indem +c_rec20 +c_utilidades +top_despacho_ac +c_recsueldo + gen +codem +
         junta7 +junta9 +junta11 + junta16")

l21 <- c("liq_total_tope ~ c_antiguedad +c_indem +c_rec20 + top_dem +c_recsueldo + gen +codem +
         junta7 +junta9 +junta11 + junta16")

l31 <- c("liq_total_tope ~ c_antiguedad +c_indem +c_rec20 + top_dem +c_recsueldo + gen +codem +
         giro_4 + giro_5 + giro_6+ giro_7")

# Función de predicción
y_hat <- function(modelo, nuevos.datos) {
  prediccion <- predict(modelo, newdata = nuevos.datos)
  prediccion
}

# Cross Validation
cross_validation <- function(df, modelo, tope, porcentaje_entrena){
  
  liq_total<- NULL
  pred_liq_total <- NULL
  
  
  for( i in 1:1000){ #cambiar a 1000
    
    set.seed(i)
    N <- floor(nrow(df) * porcentaje_entrena)
    set.seed(i)
    indices_entrena <- sample(df$id_actor, N) 
    df.entrena <- dplyr::filter(df, id_actor %in% indices_entrena) 
    df.prueba <- dplyr::filter(df, !(id_actor %in% indices_entrena)) 
    
    modelo_entrenado <- glm(modelo, data=df.entrena)
    
    df.prueba$pred_liq_total <- y_hat(modelo_entrenado, df.prueba)
    
    if(tope==0){
      aux1 <- df.prueba$liq_total
    }else{
      aux1 <- df.prueba$liq_total_tope
    }
    
    liq_total <- cbind(liq_total, aux1)
    
    aux2 <- df.prueba$pred_liq_total
    
    pred_liq_total <- cbind(pred_liq_total, aux2)
    
  }
  
  # Pasar a un vector
  
  liq_total <- c(liq_total)
  pred_liq_total <- c(pred_liq_total)
  
  res <- data.frame(cbind(liq_total, pred_liq_total))
  return(res)
}  

#   # Calcular resultados
#   residual <- liq_total - pred_liq_total
#   residual2 <- residual^2
#   promedio_residual <- mean(residual, na.rm=T)
#   promedio_residual2 <- mean(residual2, na.rm=T)
#   corr <- cor(liq_total, pred_liq_total, use = "pairwise.complete.obs")
#   

# Modelos convenio
cv_convenio <- cross_validation(df_convenio, formula_convenio, 1, .7)
cv_convenio_log <- cross_validation(df_convenio, formula_convenio_log, 0, .7)

# Pasamos los valores de liq_total a logarítmos en el Cross Validation de convenio log
cv_convenio_log$liq_total <- log(cv_convenio_log$liq_total)

# Modelo laudo
cv_l11 <- cross_validation(df_laudo, l11, 2, .8)

cv_l21 <- cross_validation(df_laudo, l21, 2, .8)

cv_l31 <- cross_validation(df_laudo, l31, 2, .8)
