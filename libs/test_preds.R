# Librerías

library(readr)
library(plyr)
library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)
library(stringdist)

Sys.setlocale('LC_ALL','C') 

# Importar base iniciales

fecha <- "17_03_17"

path <- paste("../data/BASE_INICIALES_AUDIENCIAS_MC_append_",fecha,sep="")
path_mc <-paste(path,".csv", sep = "")

limpia_fechas <- function(date){
  fecha <- as.Date(as.numeric(date), origin="1899-12-30")
  fecha[grepl("\\/", date)] <- dmy(date[grepl("\\/", date)])
  fecha
}

#Leemos las bases de Megacalculadora, el acumulado de J7 y la de recaptura (en ese orden),
# limpiamos fechas.
mc <- read.csv(path_mc, as.is = T) %>%
    dplyr::select(., clave:dummy_nulidad, fecha_termino:liq_total_tope) %>%
    mutate_each(., funs(limpia_fechas), contains("fecha"), -contains("vac"), -contains("ag"))

df <- mc

######################  Aquí empieza lo que era 1_clean_data.R ##################

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

#df$top_despacho_ac <- as.numeric(df$despacho_ac %in% top_despachos)

# Genera variables de liq_total_positiva, abogado público y resuelve giro de empresa
# df$liq_total_pos <- as.numeric(df$liq_total_tope>0) %>% as.factor(.)
df$abogado_pub <- ifelse(df$tipo_abogado_ac=="3",1,0) %>% as.factor(.)
df$giro_empresa <- ifelse(df$giro_empresa=="NO MENCIONA" | df$giro_empresa=="NO ESPECIFICA", "00",
                          df$giro_empresa) 

########################################Aquí empieza lo que era 2_clean_data.R ######################################################################

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


juntas<-c(2,7,9,11,16)
for(level in unique(juntas)){
  df[paste("junta", level, sep = "_")] <- ifelse(df$junta == level, 1, 0)
}

giros<-c(11,21,22,23,31,32,33,43,46,48,49,51,52,53,54,55,56,61,62,71,72,81,93)
for(level in unique(giros)){
  df[paste("giro_empresa", level, sep = "_")] <- ifelse(df$giro_empresa == level, 1, 0)
}

jornadas<-c(1,2,3,4)
for(level in unique(jornadas)){
  df[paste("tipo_jornada", level, sep = "_")] <- ifelse(df$tipo_jornada == level, 1, 0)
}

df <- select(df,-giro_empresa,-tipo_jornada,-junta)

df <- as.data.frame(lapply(df, function(x) {if (class(x) == "factor") as.numeric(levels(x))[x] else as.numeric(x)}))
df <- na.roughfix(df)