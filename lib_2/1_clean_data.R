#Librerías:

library(tidyr)
library(stringr)
library(stringdist)

Sys.setlocale('LC_ALL','C') 

# Revisemos los tipos de variables:
# clases <- sapply(df, class)
# revisa <- paste0(names(df), "-" ,clases)

load("utils.RData")

# Dummies de aguinaldo y vacaciones

genera_dummy <- function(fecha1, fecha2, cuantif){
  if (is.na(as.numeric(cuantif))){
    if(!is.na(limpia_fechas(fecha1)) | !is.na(limpia_fechas(fecha2))){
      dummy <- T 
    } else if (grepl("MENC", fecha1)){
      dummy <- F
    } else if (grepl("ESPE", fecha1)){
      dummy <- 1
    } else {
      dummy <- NA
    }
  } else {
    dummy <- as.numeric(cuantif)>0
  }
  as.factor(as.numeric(dummy))
}

df <- mutate(df, dummy_vac = genera_dummy(fecha_inic_vac, fecha_fin_vac, c_vac),
             dummy_ag = genera_dummy(fecha_inic_ag, fecha_fin_ag, c_ag)) %>%
      mutate_each(., funs(limpia_fechas), contains("inic"), contains("fin"))


# Codemanda IMSS:

dems_imss <- select(df, id_exp, starts_with("nombre_d"), -nombre_despido) %>%
    gather(., key=dem, value=nombre, -id_exp) %>%
    mutate(., codem_imss = grepl("IMSS", nombre) | (grepl("INSTITUTO", nombre) &
                                         (grepl("SEGURO", nombre) | grepl("FONDO", nombre))) |
                                    grepl("INFONAVIT", nombre)) %>%
    group_by(., id_exp) %>%
    dplyr::summarise(., codem = sum(codem_imss)) %>%
    mutate(., dummy_codem = ifelse(codem>0, 1,0)) %>%
    select(., -codem)

df <- left_join(df,dems_imss)

# Terminaciones y cuantificaciones:

df$modo_termino[!(df$modo_termino %in% c("1","2","3","4"))] <- NA

df <- mutate_each(df, funs(as.numeric),
                 c_antiguedad:min_ley, antig) %>%
      mutate(., grado_exag = c_total/min_ley)

df$liq_total <- gsub("\\$","", df$liq_total) %>%
                gsub(",","", .) %>% as.numeric(.)
df$liq_total[is.na(df$liq_total)] <- 0
df$c_sal_caidos <- as.numeric(df$c_sal_caidos)


# Repeat players: despachos
# Limpia nombres de despachos y genera dummy de "frecuentemente demandante", para el despacho

# df$despacho_ac[union(grep("MENC", df$despacho_ac), grep("PROC", df$despacho_ac))] <- NA
# df$despacho_ac[df$despacho_ac==""]<-NA
# df$despacho_ac[grep("PZ", df$despacho_ac)] <- "PZYD"
# 
# dems <- plyr::count(df$despacho_ac) %>% mutate(., x=as.character(x))
# dems$top_despacho_ac <- dems$freq>10
# names(dems)[1] <- "despacho_ac"
# 
# df <- left_join(df, dems) %>% select(., -freq)
# df$top_despacho_ac[is.na(df$despacho_ac)] <- F
# 
# top_despachos <- unique(df$despacho_ac[df$top_despacho_ac])

df$top_despacho_ac <- df$despacho_ac %in% top_despachos

# Genera variables de liq_total_positiva, abogado público y resuelve giro de empresa
df$liq_total_pos <- as.numeric(df$liq_total_tope>0) %>% as.factor(.)
df$abogado_pub <- ifelse(df$tipo_abogado_ac=="3",1,0) %>% as.factor(.)
df$giro_empresa <- ifelse(df$giro_empresa=="NO MENCIONA" | df$giro_empresa=="NO ESPECIFICA", "00",
                          df$giro_empresa) 


# dems <- select(df, id_exp, starts_with("nombre_d"), -nombre_despido) %>%
#     gather(., key=dem, value=nombre, -id_exp) %>%
#     group_by(., nombre) %>%
#     dplyr::summarise(., rep = n())
#
# revisa_nombres <- paste0(dems$nombre, "-", dems$rep)

# Arregla factores

# ¿Hacer algo con delegaciones?
# Revisar si los giros pueden ser cruzados con alguna base de INEGI
