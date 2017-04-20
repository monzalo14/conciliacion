library(unbalanced)

factor2num <- function(x){as.numeric(as.character(x))}

# Leemos los datos, filtramos para laudos y creamos la dummy de gana/pierde

df_laudo <- readRDS('../clean_data/observaciones_selected.RDS') %>% 
            filter(modo_termino == 3) %>% 
            mutate(laudo_gana = liq_total>0) %>%
            select(-liq_total, -exp, -anio) %>%
            dplyr::rename(nombre_actor = nombre_ac)

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


df_ep$renuncia_voluntaria <- df_ep %>% 
  select(starts_with('excepcion')) %>%
  rowSums(., na.rm = T)>0 

df_ep$renuncia_voluntaria <- as.numeric(df_ep$renuncia_voluntaria)

# Volvemos todas numÃ©ricas

df_laudo <- df_laudo %>%
            mutate_each(funs(factor2num), one_of(factores)) %>%
            mutate(laudo_gana = as.factor(as.numeric(laudo_gana)),
                   hextra_sem = as.numeric(hextra_sem)) %>%
            right_join(df_ep) %>%
            select(-starts_with('excepcion'),
                   -nombre_actor, -id_exp) %>%
            filter(!is.na(modo_termino))

saveRDS(df_laudo, '../clean_data/observaciones_selected_laudos.RDS')

rm(list = ls())
#Agrego una parte de código  necesaria de limpieza,
# que estaba en el reporte que mando a Joyce sobre el modelo

df_laudo <- readRDS('../clean_data/observaciones_selected_laudos.RDS') %>% 
  select(-modo_termino, -starts_with('junta'), -codem, -c_sal_caidos, -sueldo) 

df_laudo %>%
select(starts_with('giro')) %>%
sapply(sum) -> giros

quita <- giros[giros < 10] %>% names()
df_laudo <- df_laudo %>%
            mutate(giro_empresa_00 = rowSums(df_laudo[names(df_laudo) %in% quita])) %>%
            select(-one_of(quita[-1])) %>%
			mutate(giro_empresa00 = ifelse(giro_empresa00 == 1 | giro_empresa_00 == 1, 1, 0)) %>%
      select(-giro_empresa_00) 

saveRDS(df_laudo, '../clean_data/observaciones_selected_laudos.RDS')