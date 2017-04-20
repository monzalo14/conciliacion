df_laudo <- readRDS('../clean_data/sample_selected.RDS') %>% 
  select(-starts_with('junta'), -codem, -c_sal_caidos, -sueldo, -exp, -anio) 

clases <- sapply(df_laudo, class)
factores <- clases[clases == 'factor'] %>% names()

df_laudo <- df_laudo %>%
            mutate_each(funs(factor2num), one_of(factores)) %>%
            mutate(hextra_sem = as.numeric(hextra_sem)) %>%
            select(-nombre_actor, -id_exp)

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