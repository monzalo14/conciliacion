library(unbalanced)

factor2num <- function(x){as.numeric(as.character(x))}

# Leemos los datos, filtramos para laudos y creamos la dummy de gana/pierde

df_laudo <- readRDS('../clean_data/observaciones_selected.RDS') %>% 
            filter(modo_termino == 3) %>% 
            mutate(laudo_gana = liq_total>0) %>%
            select(-liq_total)

clases <- sapply(df_laudo, class)
factores <- clases[clases == 'factor'] %>% names()

# Volvemos todas numéricas

df_laudo <- df_laudo %>%
            mutate_each(funs(factor2num), one_of(factores)) %>%
            mutate(laudo_gana = as.factor(as.numeric(laudo_gana)))

saveRDS(df_laudo, '../clean_data/observaciones_selected_laudos.RDS')




