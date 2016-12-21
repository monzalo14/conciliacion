# Select features

df <- readRDS('../clean_data/observaciones_expandido.RDS')

control_vars <- c("id_exp","exp", "anio")
label_vars <- c("modo_termino", "liq_total") 

train_vars <- c("reclutamiento",
                      "giro_empresa_00",
                      "giro_empresa_22",
                      "giro_empresa_31",
                      "giro_empresa_32",
                      "giro_empresa_33",
                      "giro_empresa_43",
                      "giro_empresa_46",
                      "giro_empresa_48",
                      "giro_empresa_49",
                      "giro_empresa_51",
                      "giro_empresa_52",
                      "giro_empresa_53",
                      "giro_empresa_54",
                      "giro_empresa_55",
                      "giro_empresa_56",
                      "giro_empresa_61",
                      "giro_empresa_62",
                      "giro_empresa_71",
                      "giro_empresa_72",
                      "sueldo",         
                      "gen",
                      "antig",
                      "reinst",  
                      "hextra", 
                      "sarimssinf",   
                      "codem",          
                      "modo_termino",
                      "liq_total")

vars_joyce <- c('sueldo',
                'gen',
                'horas_sem',
                'hextra',
                'hextra_sem',
                'rec20', #imputado como trabajador de confianza
                'prima_dom',
                'desc_sem',
                'desc_ob',
                'sarimssinf', # Partir en tres preguntas (componentes)
                'c_indem',
                'min_ley', # Darle prioridad sobre otros c_*; meteríamos otras prestaciones
                #'top_dem',
                'c_sal_caidos')
                #'antig>15'*'prima_antig'

#probabilidad de ganar: multiplicar por x<1 en caso de carta de renuncia

df_exp <- df
giros <- c(22, 31, 32, 33, 43, 46, 48, 49, 51, 52, 53, 54, 55, 56, 61, 62, 71, 72)
for(level in unique(giros)){
  df_exp[paste("giro_empresa", level, sep = "_")] <- ifelse(df_exp$giro_empresa == level, 1, 0)
}

df_exp$giro_empresa_00 <- ifelse(df_exp$giro_empresa == '00', 1, 0)

df_exp <- df_exp %>% select(one_of(train_vars), one_of(vars_joyce))

saveRDS(df_exp, '../clean_data/observaciones_selected.RDS')
rm(list=ls())
