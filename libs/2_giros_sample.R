# Correr expand implicaba demasiadas diferencias en la distribución. Mejor sólo tiramos giros

df <- readRDS('../clean_data/sample_added.RDS')

giros <- c('31', '43', '46', '52', '54', '56', '61', '62', '72', '81')
giros_agregado <- c('3', '4', '5', '6', '7', '8')

for (num in giros_agregado){
	df[paste0('giro_', num)] <- ifelse(substring(df$giro_empresa, 2) == num)
}

for (giro in giros){
	df[paste0('giro_empresa', giro)] <- ifelse(df$giro_empresa == giro, 1, 0)
}

df <- df %>%
		mutate(giro_empresa00 = rowSums(df[grepl('giro_empresa', names(df))])) %>%
		select(-giro_empresa)

df$giro_empresa00 <- as.numeric(df$giro_empresa00>0)

saveRDS(df_exp, '../clean_data/sample_expandido.RDS')
rm(list=ls())
