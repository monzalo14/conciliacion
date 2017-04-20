df_laudo <- readRDS('../clean_data/sample_selected.RDS') %>% 
  select(-starts_with('junta'), -codem, -c_sal_caidos, -sueldo, -exp, -anio) 

clases <- sapply(df_laudo, class)
factores <- clases[clases == 'factor'] %>% names()

df_laudo <- df_laudo %>%
            mutate_each(funs(factor2num), one_of(factores)) %>%
            mutate(hextra_sem = as.numeric(hextra_sem)) %>%
            select(-nombre_actor, -id_exp)

saveRDS(df_laudo, '../clean_data/sample_selected_laudos.RDS')