
# Prestaciones
# Limpieza dummys (aplica para prestaciones y algunas variables del apartado de hechos)

limpia_dummy <- function(var){
  replace(var, var!=0&var!=1, NA)
}

df <- dplyr::mutate_each(df, funs(limpia_dummy), contains("dummy"))
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


df <- dplyr::mutate_each(df, funs(factor2num), contains("monto")) %>%
  dplyr::mutate_each(., funs(limpia_negativos), contains("monto"))



# Limpieza del apartado de hechos

# Acción Principal
# Solo se utilizarán expedientes con accion principal: indemnización constitucional, reinstalación y rescisión
df <- df[union(grep("INDEM", df$accion_principal), union(grep("REINST", df$accion_principal), grep("RESC", df$accion_principal))),]



# length(df$accion_principal[dplyr::union(grep("INDEM", df$accion_principal), 
#                                  union(grep("REINST", df$accion_principal),
#                                  grep("RESCI", df$accion_principal)))])
# 
# length(mc$accion_principal[dplyr::union(grep("INDEM", mc$accion_principal), 
#                                         union(grep("REINST", mc$accion_principal),
#                                               grep("RESCI", mc$accion_principal)))])
# 
# length(mc7$accion_principal[dplyr::union(grep("INDEM", mc7$accion_principal), 
#                                         union(grep("REINST", mc7$accion_principal),
#                                               grep("RESCI", mc7$accion_principal)))])

# Antigüedad

df$c_antiguedad <- as.numeric(as.character(df$c_antiguedad))
df$c_antiguedad[df$c_antiguedad<0] <- NA


# Sueldos

df <- dplyr::mutate_each(df, funs(factor2num), contains("sueldo")) %>%
      dplyr::mutate_each(., funs(limpia_negativos), contains("sueldo"))


# Periodicidades
factor2int <- function(var){
  as.integer(as.character(var))
}

limpia_per <- function(var){
  replace(var, var>3|var<0, NA)
}

df <- dplyr::mutate_each(df, funs(factor2int), starts_with("per_")) %>%
      dplyr::mutate_each(., funs(limpia_per), starts_with("per_"))

# Año de nacimiento

limpia_anio_nacimiento <- function(date){
  date <- factor2int(date)

  anio <- ifelse(date>2016, year(as.Date(date, origin="1899-12-30")),
          ifelse(date<1900, NA, date))
  anio
}


df$anio_nac <- limpia_anio_nacimiento(df$anio_nac)

# Generamos la variable de Edad
df$edad <- df$anio-df$anio_nac

# Jornada
limpia_jornada <- function(jornada){
  aux <- ifelse(jornada!=1&jornada!=2&jornada!=3&jornada!=4, NA ,jornada)
  aux
}

df$tipo_jornada <- limpia_jornada(df$tipo_jornada)

# Horas
df <- dplyr::mutate_each(df, funs(factor2num), starts_with("horas_")) %>%
      dplyr::mutate_each(., funs(limpia_negativos), starts_with("horas_"))


