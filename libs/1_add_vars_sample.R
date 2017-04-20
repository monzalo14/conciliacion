library(dplyr)
library(reshape2)
library(dummies)
library(lmtest)
library(gmodels)
library(sandwich)

base <- readRDS("../clean_data/sample.RDS")

dems <- base %>%
        select(starts_with(nombre_d), -nombre_despido, id_exp) %>%
        gather(key = numero, value = nombre, -id_exp) 

razones <- list(
'WALMART' = 'WALMART'
'WAL MART' = 'WALMART'
'SUMESA' = 'COMER'
)

# Englobamos los nombres de ciertas empresas con varias razones sociales. 
for (i in match(names(dplyr::select(base_exp, starts_with("nombre_d"), -nombre_despido)), names(base_exp))){
  base_exp[, i][union(grep("WALMART", base_exp[, i]), grep("WAL MART", base_exp[, i]))] <- "WALMART"
  base_exp[, i][union(grep("SUMESA", base_exp[, i]), grep(" COMER ", base_exp[, i]))] <- "COMERCIAL MEXICANA"
  base_exp[, i][union(grep("COMERCIAL MEXICANA", base_exp[, i]), grep("FRESKO", base_exp[, i]))] <- "COMERCIAL MEXICANA"
  base_exp[, i][union(grep("ELMEX", base_exp[, i]), grep("ELEKTRA", base_exp[, i]))] <- "ELEKTRA"
  base_exp[, i][union(grep("SANBORN", base_exp[, i]), grep("SANBORNS", base_exp[, i]))] <- "SANBORNS"
  base_exp[, i][union(grep("MANPOWER", base_exp[, i]), grep("MAN POWER", base_exp[, i]))] <- "MANPOWER"
  base_exp[, i][grep("WINGS", base_exp[, i])] <- "WINGS"
  base_exp[, i][grep("VIPS", base_exp[, i])] <- "VIPS"
  base_exp[, i][grep("SUBURBIA", base_exp[, i])] <- "SUBURBIA"
  base_exp[, i][grep("PALACIO DE HIERRO", base_exp[, i])] <- "PALACIO DE HIERRO"
  base_exp[, i][grep("CHEDRAUI", base_exp[, i])] <- "CHEDRAUI"
  base_exp[, i][grep("ATENTO", base_exp[, i])] <- "ATENTO"
  base_exp[, i][grep("7 ELEVEN", base_exp[, i])] <- "7 ELEVEN"
  base_exp[, i][grep("OXXO", base_exp[, i])] <- "OXXO"
  base_exp[, i][grep("TEZONTLE", base_exp[, i])] <- "ELEKTRA"
}



nombres_dems <- dplyr::select(base_exp, clave, starts_with("nombre_d"), -nombre_despido) %>%
                melt(., id=c("clave")) 

# Dependencias de gobierno/ sindicatos
nombres_dems$value[union(grep("IMSS", nombres_dems$value), grep("INSTITUTO MEXICANO DEL SEGURO SOCIAL", nombres_dems$value))] <- NA
nombres_dems$value[union(grep("INFONAVIT", nombres_dems$value), grep("INSTITUTO DEL FONDO NACIONAL DE LA VIVIENDA PARA LOS TRABAJADORES", nombres_dems$value))] <- NA
nombres_dems$value[union(grep("RESULTE", nombres_dems$value), grep("RESPONSABLE", nombres_dems$value))] <- NA
nombres_dems$value[union(grep("GOBIERNO DEL DISTRITO FEDERAL", nombres_dems$value),grep("DELEGACION POLITICA", nombres_dems$value))] <- NA
nombres_dems$value[union(grep("SHCP", nombres_dems$value), grep("SECRETARIA DE HACIENDA Y CREDITO PUBLICO", nombres_dems$value))] <- NA
nombres_dems$value[union(grep("GORDILLO", nombres_dems$value),grep("SINDICATO NACIONAL DE TRABAJADORES DE LA EDUCACION", nombres_dems$value))] <- NA
nombres_dems$value[grep("CONSAR", nombres_dems$value)] <- NA
nombres_dems$value[nombres_dems$value==""]<- NA

# Casos muy particulares (demandados físicos)
nombres_dems$value[union(grep("AGUIRRE", nombres_dems$value),grep("KUTZ", nombres_dems$value))] <- NA
nombres_dems$value[union(grep("ESCARPITA", nombres_dems$value),grep("ISAIAS", nombres_dems$value))] <- NA


# Variable top_dem
demandados <- plyr::count(nombres_dems$value) %>% mutate(., x=as.character(x)) 

top_demandados <- demandados$x[demandados$freq>10&demandados$freq<max(demandados$freq)] # El último operador lógico elimina el NA


base_exp$top_dem1 <- ifelse(base_exp$nombre_d1 %in% top_demandados, 1,0)
base_exp$top_dem2 <- ifelse(base_exp$nombre_d2 %in% top_demandados, 1,0)
base_exp$top_dem3 <- ifelse(base_exp$nombre_d3 %in% top_demandados, 1,0)
base_exp$top_dem4 <- ifelse(base_exp$nombre_d4 %in% top_demandados, 1,0)
base_exp$top_dem5 <- ifelse(base_exp$nombre_d5 %in% top_demandados, 1,0)
base_exp$top_dem6 <- ifelse(base_exp$nombre_d6 %in% top_demandados, 1,0)
base_exp$top_dem <- base_exp$top_dem1 + base_exp$top_dem2 + base_exp$top_dem3 + base_exp$top_dem4 +
                    base_exp$top_dem5 + base_exp$top_dem6

drops <- c("top_dem1","top_dem2","top_dem3","top_dem4","top_dem5","top_dem6")
base_exp <- base_exp[ , !(names(base_exp) %in% drops)]


base_exp$top_dem[base_exp$top_dem>1] <- 1

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