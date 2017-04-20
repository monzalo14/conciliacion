# Módulo de conciliación prejudicial: Limpieza y de datos para calculadora 

library(readr)
library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)
library(stringdist)
library(reshape2)
library(dummies)
library(lmtest)
library(gmodels)
library(sandwich) 

data_path <- paste0('../data/BASE_INICIALES_AUDIENCIAS_MC_append_', fecha, '.csv')

limpia_fechas <- function(date){
  fecha <- as.Date(as.numeric(date), origin="1899-12-30")
  fecha[grepl("\\/", date)] <- dmy(date[grepl("\\/", date)])
  fecha
}

df <- read.csv(data_path) %>%
		mutate_at(vars(contains('fecha'), -starts_with('vac'), -starts_with('ag')))

load("utils.RData")
load("top_despachos.RData")

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

# Cuantificaciones:

df <- mutate_each(df, funs(as.numeric),
                 c_antiguedad:min_ley, antig, c_sal_caidos) %>%
      mutate(., grado_exag = c_total/min_ley)

df$top_despacho_ac <- df$despacho_ac %in% top_despachos

# Genera variables de liq_total_positiva, abogado público y resuelve giro de empresa
df$abogado_pub <- ifelse(df$tipo_abogado_ac=="3",1,0) %>% as.factor(.)
df$giro_empresa <- ifelse(df$giro_empresa=="NO MENCIONA" | df$giro_empresa=="NO ESPECIFICA", "00",
                          df$giro_empresa) 


# Prestaciones
# Limpieza dummys (aplica para prestaciones y algunas variables del apartado de hechos)
limpia_dummy <- function(var){
  replace(var, var!=0&var!=1, NA)
}

df <- mutate_each(df, funs(limpia_dummy), contains("dummy"))
df <- mutate_each(df, funs(as.factor), contains("dummy"))

# Quitamos el prefijo "dummy" para tener nombres de variables más cortos
names(df) <- gsub("dummy_", "", names(df))

# Limpieza montos

factor2num <- function(var){
  as.numeric(as.character(var))
}

limpia_negativos <- function(var){
  replace(var, var<0, NA)
}

# Generación de variables sobre nivel de exageración
# Proporción de la cuantificación que corresponde a horas extras
df$c_hextra[is.na(df$c_hextra)] <- 0
df$prop_hextra <- df$c_hextra/df$c_total

# Exageración general
df$exag_general <- df$c_total/df$min_ley


df <- mutate_each(df, funs(factor2num), contains("monto")) %>%
  mutate_each(., funs(limpia_negativos), contains("monto"))


# Limpieza del apartado de hechos

# Acción Principal
# Solo se utilizarán expedientes con accion principal: indemnización constitucional, reinstalación y rescisión
df <- df[union(grep("INDEM", df$accion_principal), union(grep("REINST", df$accion_principal), grep("RESC", df$accion_principal))),]

# Antigüedad
df$c_antiguedad <- as.numeric(as.character(df$c_antiguedad))
df$c_antiguedad[df$c_antiguedad<0] <- NA


# Sueldos
df <- mutate_each(df, funs(factor2num), contains("sueldo")) %>%
      mutate_each(., funs(limpia_negativos), contains("sueldo"))


# Periodicidades
factor2int <- function(var){
  as.integer(as.character(var))
}

limpia_per <- function(var){
  replace(var, var>3|var<0, NA)
}

df <- mutate_each(df, funs(factor2int), starts_with("per_")) %>%
      mutate_each(., funs(limpia_per), starts_with("per_"))

# Jornada
limpia_jornada <- function(jornada){
  aux <- ifelse(jornada!=1&jornada!=2&jornada!=3&jornada!=4, NA ,jornada)
  aux
}

df$tipo_jornada <- limpia_jornada(df$tipo_jornada)

# Horas
df <- mutate_each(df, funs(factor2num), starts_with("horas_")) %>%
      mutate_each(., funs(limpia_negativos), starts_with("horas_"))


# Prestaciones
# Limpieza dummys (aplica para prestaciones y algunas variables del apartado de hechos)

limpia_dummy <- function(var){
  replace(var, var!=0&var!=1, NA)
}

df <- mutate_each(df, funs(limpia_dummy), contains("dummy"))
df <- mutate_each(df, funs(as.factor), contains("dummy"))

# Quitamos el prefijo "dummy" para tener nombres de variables más cortos
names(df) <- gsub("dummy_", "", names(df))

# Limpieza montos
factor2num <- function(var){
  as.numeric(as.character(var))
}

limpia_negativos <- function(var){
  replace(var, var<0, NA)
}

# Generación de variables sobre nivel de exageración
# Proporción de la cuantificación que corresponde a horas extras
df$c_hextra[is.na(df$c_hextra)] <- 0
df$prop_hextra <- df$c_hextra/df$c_total

# Exageración general
df$exag_general <- df$c_total/df$min_ley

df <- mutate_each(df, funs(factor2num), contains("monto")) %>%
  mutate_each(., funs(limpia_negativos), contains("monto"))

# Tratamiento especial a variable de reclutamiento y giro de empresa
# Reclutamiento
df$reclutamiento[df$id_exp=='2/794/2011'] <- 1
df$reclutamiento[df$giro_empresa=='89'] <- 1

# Casos generales
limpia_giros <- function(giro){
  nuevo_giro <- ifelse(giro==10, 31,
                       ifelse(giro==12, 46,
                              ifelse(giro==25, 52,
                                     ifelse(giro==63|giro==87|giro==89, 72, giro))))
  nuevo_giro
}

df$giro_empresa <- limpia_giros(df$giro_empresa)
df <- mutate_each(df, funs(factor), gen, starts_with("tipo"), modo_termino,
                  starts_with("per"), junta, exp, anio, giro_empresa)

saveRDS('../clean_data/sample.RDS')