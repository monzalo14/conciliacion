# Correr expand implicaba demasiadas diferencias en la distribución. Mejor sólo tiramos giros

df <- readRDS('../clean_data/observaciones.RDS') %>% 
  filter(!giro_empresa93, 
         !giro_empresa11,
         !giro_empresa64,
         !giro_empresa81,
         !giro_empresa23) #typo

saveRDS(df_exp, '../clean_data/observaciones_expandido.RDS')
rm(list=ls())
