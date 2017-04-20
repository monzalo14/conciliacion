library(dplyr)
library(reshape2)
library(dummies)
library(lmtest)
library(gmodels)
library(sandwich)

base <- readRDS("../clean_data/sample.RDS")

razones <- list(
'WALMART' = c('WALMART', 'WAL MART'),
'COMERCIAL MEXICANA' = c('COMER', 'COMERCIAL MEXICANA', 
'SUMESA', 'FRESKO'),
'ELEKTRA' = c('ELMEX', 'ELEKTRA', 'TEZONTLE'),
'SANBORNS' = c('SANBORN', 'SANBORNS'),
'MANPOWER' = c('MAN POWER', 'MANPOWER'),
'WINGS' = 'WINGS',
'VIPS'= 'VIPS',
'SUBURBIA' ='SUBURBIA',
'PALACIO DE HIERRO' = 'PALACIO DE HIERRO',
'CHEDRAUI' = 'CHEDRAUI',
'ATENTO' = 'ATENTO',
'OXXO' = 'OXXO',
# Gobierno
'QUITAR' = c('IMSS',
  'INFONAVIT',
  'INSTITUTO MEXICANO DEL SEGURO SOCIAL',
  'INSTITUTO NACIONAL DEL FONDO PARA LA VIVIENDA',
  'SHCP', 'SECRETARIA DE HACIENDA',
  'GORDILLO',
  'SNTE', 'SINDICATO NACIONAL DE TRABAJADORES DE LA EDUCACION',
  'GOBIERNO DEL', 'DELEGACION POLITICA', 
  'CONSAR',
  #Físicos
  'QUIEN', 'RESULTE',
  'AGUIRRE', 'KUTZ', 'ISAIAS', 'ESCARPITA')
)

# Funciones auxiliares para juntar razones sociales de empresas
single_match <- function(variable, expresion, nombre){
  variable[grepl(expresion, variable)] <- nombre
  variable
}

element_match <- function(elemento_lista, nombre, variable){
 for (expresion in elemento_lista){
  variable <- single_match(variable, expresion, nombre)
 }
variable
}

limpia_razones <- function(lista, variable){
  for (i in seq_along(lista)){
    variable <- element_match(lista[[i]], names(lista)[i], variable)
  }
variable[variable == 'QUITAR'] <- NA
as.character(variable)
}

base <- base %>%
        mutate_at(vars(starts_with('nombre_d'), -nombre_despido), limpia_razones)

load('../data/top_demandados.RData')

top_dem <- function(x){
  x %in% top_dems
}

base <- base %>%
        mutate_at(vars(starts_with('nombre_d'), -nombre_despido), top_dem) %>%
        mutate(top_demandado = select(., starts_with('nombre_d'), -nombre_despido) %>% rowSums())

base$top_demandado <- as.numeric(base$top_demandado>0)

# Dummy antigüedad mayor a 15 años

base_exp$prima_antig <- as.numeric(as.character(base_exp$prima_antig))
base_exp$antig_15 <- ifelse(base_exp$c_antiguedad>15, 1, 0)

###########################################################################################

trunca99 <- function(x){
  cuantil99 <- quantile(x, .99, na.rm=T, type=1)
  x [x>cuantil99] <- cuantil99
  x
}

quita_negativos <- function(x){
  x[x<0] <- 0
  x
}

df_exp <- group_by(base_exp, modo_termino) %>% 
  mutate_each(funs(trunca99), liq_total, liq_total_tope, starts_with("c_")) %>%
  data.frame(.) %>%
  dummy.data.frame(names=c("junta")) %>%
  mutate_each(., funs(quita_negativos), starts_with("c_"))

logs <- c("c_antiguedad", "c_indem")
suma <- function(x){x+1}

df_exp2 <- mutate_each(df_exp, funs(suma), one_of(logs)) %>%
  mutate(., ln_c_antiguedad = log(c_antiguedad),
         ln_c_indem = log(c_indem))

# Modificación 25/01/2017: para utilizarlos en el modelo de laudo, se agrupan a un dígito los giros de empresa
# 3, 4, 5, 6, 7, 8



# df_exp2$giro_3 <- df_exp2$giro_empresa31 + df_exp2$giro_empresa32 + df_exp2$giro_empresa33
# df_exp2$giro_3[df_exp2$giro_3>0] <- 1

# df_exp2$giro_4 <- df_exp2$giro_empresa43 + df_exp2$giro_empresa46 + df_exp2$giro_empresa48 +df_exp2$giro_empresa49
# df_exp2$giro_4[df_exp2$giro_4>0] <- 1

# df_exp2$giro_5 <- df_exp2$giro_empresa51 + df_exp2$giro_empresa52 + df_exp2$giro_empresa53 +
#                   df_exp2$giro_empresa54 + df_exp2$giro_empresa55 + df_exp2$giro_empresa56
# df_exp2$giro_5[df_exp2$giro_5>0] <- 1

# df_exp2$giro_6 <- df_exp2$giro_empresa61 + df_exp2$giro_empresa62 + df_exp2$giro_empresa64 
# df_exp2$giro_6[df_exp2$giro_6>0] <- 1

# df_exp2$giro_7 <- df_exp2$giro_empresa71 + df_exp2$giro_empresa72
# df_exp2$giro_7[df_exp2$giro_7>0] <- 1

# df_exp2$giro_8 <- df_exp2$giro_empresa81 
# df_exp2$giro_8[df_exp2$giro_8>0] <- 1

saveRDS(df_exp2, "../clean_data/sample_added.RDS")