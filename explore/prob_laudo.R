prob_laudo <- function(df){
  load('probabilidad_laudo.RData')
  prediccion_RF <- predict(RF_best, df, 'prob')
  prediccion_RF[, '0']
}

prob_laudo_vars <- function(array){
  load('probabilidad_laudo.RData')
  names(array) <- c('reclutamiento', 'gen',
                    'antig', 'reinst', 'hextra',
                    'sarimssinf', 'horas_sem', 'hextra_sem',
                    'rec20', 'prima_dom', 'desc_sem',
                    'desc_ob', 'c_indem', 'min_ley', 'giro', 
                    'nombre_d1', 'nombre_d2', 
                    'nombre_d3', 'nombre_d4')
  
df <- as.data.frame(array)
  
giros <- c('31', '43', '46', '52', '54', '56', '61', '62', '72', '81')
giros_agregado <- c('3', '4', '5', '6', '7', '8')

# Esto es para la que lo tome la regresión lineal
for (num in giros_agregado){
  df[paste0('giro_', num)] <- ifelse(substring(df$giro_empresa, 2) == num)
}

# Esto es para el RF
for (giro in giros){
  df[paste0('giro_empresa', giro)] <- ifelse(df$giro_empresa == giro, 1, 0)
}

df <- df %>%
    mutate(giro_empresa00 = rowSums(df[grepl('giro_empresa', names(df))])) %>%
    select(-giro_empresa)

df$giro_empresa00 <- as.numeric(df$giro_empresa00>0)	


########################## Código para variable de "top demandado" #########

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

df <- df %>%
        mutate_at(vars(starts_with('nombre_d')), limpia_razones)

load('../data/top_demandados.RData')

top_dem <- function(x){
  x %in% top_dems
}

df <- df %>%
        mutate_at(vars(starts_with('nombre_d')), top_dem) %>%
        mutate(top_demandado = select(., starts_with('nombre_d')) %>% rowSums())

df$top_demandado <- as.numeric(df$top_demandado>0)

  prediccion_RF <- predict(RF_best, df, 'prob')
  prediccion_RF[, '0']
}