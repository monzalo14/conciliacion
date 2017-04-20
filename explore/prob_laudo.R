prob_laudo <- function(df){
  load('probabilidad_laudo.RData')
  prediccion_RF <- predict(RF_best, df, 'prob')
  prediccion_RF[, '0']
}

prob_laudo_vars <- function(array){
  load('probabilidad_laudo.RData')
  names(array) <- c('reclutamiento', 'gen',
                    'antig', 'reinst', 'hextra',
                    'sarimssinf', 'horas_sem', 'hextra_sem',
                    'rec20', 'prima_dom', 'desc_sem',
                    'desc_ob', 'c_indem', 'min_ley', 'giro')
  
df <- as.data.frame(array)
  giros <- c('00', '31', '43', '52', '54', '56', '61', '62', '81')
  
  for (giro in giros){
    df[paste('giro', giro)] <- ifelse(df$giro == giro, 1, 0)
  }

library(dplyr)
giros <- giros[-1]
df<- df %>% 
	mutate(giro00 = ifelse(giro %in% giros, 0, 1)) %>%
	select(-giro) 
	
  
  prediccion_RF <- predict(RF_best, df, 'prob')
  prediccion_RF[, '0']
}