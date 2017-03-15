library(unbalanced)

factor2num <- function(x){as.numeric(as.character(x))}

# Leemos los datos, filtramos para laudos y creamos la dummy de gana/pierde

df_laudo <- readRDS('../clean_data/observaciones_selected.RDS') %>% 
            filter(modo_termino == 3) %>% 
            mutate(laudo_gana = liq_total>0) %>%
            select(-liq_total, -exp, -anio) %>%
            rename(nombre_actor = nombre_ac)

clases <- sapply(df_laudo, class)
factores <- clases[clases == 'factor'] %>% names()

# Jalamos excepciones principales

renuncia_voluntaria <- function(x){
  x == '3'
}

df_ep <- read_excel('../data/laudos_excepcion_principal.xlsx') %>% 
  filter(row_number() > 2) %>%
  select(-exp, -anio, -junta) %>%
  mutate_at(vars(starts_with('excepcion')), renuncia_voluntaria)

df_ep %>% 
  select(starts_with('excepcion')) %>%
  rowSums(., na.rm = T) -> df_ep$renuncia_voluntaria

# Volvemos todas numéricas

df_laudo <- df_laudo %>%
            mutate_each(funs(factor2num), one_of(factores)) %>%
            mutate(laudo_gana = as.factor(as.numeric(laudo_gana))) %>%
            right_join(df_ep) %>%
            select(-starts_with('excepcion')) %>%
            !is.na(modo_termino)

saveRDS(df_laudo, '../clean_data/observaciones_selected_laudos.RDS') 
