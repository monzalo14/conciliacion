# Leemos nuestros datos y la ENOE, seleccionando sólo las encuestas de la Ciudad de México

df <- readRDS('../clean_data/observaciones.RDS') %>% 
      filter(giro_empresa != '93', 
             giro_empresa != '11',
             giro_empresa != '21',
             giro_empresa != '64') #typo

sdem <- dir("../data", pattern = "sdemt1.*\\.dbf$", 
            full.names = TRUE, recursive = T) %>% 
            read.dbf() %>%
            select(CLASE1, SCIAN, ENT) %>%
            filter(CLASE1 == 1, 
                   SCIAN != '20',
                   SCIAN != '1',
                   SCIAN != '2') %>%
            select(-CLASE1)

# Tomamos la codificación de la ENOE para los giros, de acuerdo a SCIAN
codif_giros <- list('11', '21', '22', '23', '33', '43', '46', '49', 
                    '51', '52', '53', '54', '55', '56', '61', '62', 
                    '71', '72', '81', '00')
names(codif_giros) <- c(as.character(1:19), '21')

#Armamos funcioncitas auxiliares para decodificar
get_code <- function(x){
  codif_giros[[as.character(x)]]
}

decode <- function(variable){
  sapply(variable, get_code) %>%
  unname()
}

# Necesitamos tomar una muestra de la población flotante de EDOMEX, y ahora unirla con la del DF:

set.seed(140693)
pob_flotante <- sdem %>% filter(ENT == '15') %>% nrow()*.2089 
pob_flotante <- round(pob_flotante)

sdem_edomex <- sdem %>% 
              filter(ENT == '15') %>%
              dplyr::sample_n(size=pob_flotante) 

sdem <- sdem %>% filter(ENT == '09') %>%
        rbind(sdem_edomex) %>%
        select(-ENT)

# Obtenemos así la proporción de personas de la muestra ocupadas en cada giro, 
# ya con los nombres que buscamos:

# Usé la suma de frecuencias en lugar del total de la población ocupada

enoe_giros <- plyr::count(sdem$SCIAN) %>%
            filter(x!= 0) %>%
            mutate(giro = decode(x),
            freq = freq/nrow(sdem)) %>%
            select(-x) 

# Al omitir los 'No aplica' y a los trabajadores de gobierno de los giros (x=0), 
# estamos quitando una fracción de la población ocupada.
# (trabajadores domésticos o bajo algún régimen distinto, por ejemplo). 
# Compensemos agregándolos a la categoría de 'No especificado', 
# que es '00' (NO MENCIONA/NO ESPECIFICA, en nuestra base de datos)

enoe_giros$freq[nrow(enoe_giros)] <-  enoe_giros$freq[nrow(enoe_giros)] 
                                      + (1-sum(enoe_giros$freq))

# Obtenemos, para nuestra muestra, el número de observaciones que necesitamos por giro:
enoe_giros$freq <- enoe_giros$freq*nrow(df)

# Sin embargo, necesitamos alterar el tamaño de la base:
df$giro_empresa[df$giro_empresa == '31'] <- '33'
df$giro_empresa[df$giro_empresa == '32'] <- '33'
df$giro_empresa[df$giro_empresa == '48'] <- '49'
df_giros <- plyr::count(df$giro_empresa) %>%
            mutate(x = as.character(x))
names(df_giros) <- c('giro','df_freq')

df_giros <- df_giros %>% left_join(enoe_giros)

# Demasiadas diferencias en la distribución; no haremos nada con giros. 
# Sólo tiramos algunas industrias 

df_exp <- df %>% filter(giro_empresa != '81',
                        giro_empresa != '23')

saveRDS(df_exp, '../clean_data/observaciones_expandido.RDS')
rm(list=ls())
