# Librerías

library(readr)
library(plyr)
library(dplyr)
library(lubridate)

# Importar base iniciales

path_mc <- "../data/base_mc_tope.csv"
path_ac7 <- "../data/base_ac7_tope.csv"
path_recap <- "../data/base_recap.csv"

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

### Vamos con las de J7
ac7 <- read.csv(path_ac7, stringsAsFactors = F) %>%
       select(., -giro_empresa, -anio_nac)
recap <- read_csv(path_recap, col_names=T) %>%
         select(., -starts_with("X"))

#Juntamos la de recaptura con la de J7 y creamos la variable duración
mc7 <- left_join(recap, ac7) %>%
    mutate_each(., funs(limpia_fechas), contains("fecha"), -contains("vac"), -contains("ag"))

df <- rbind(mc, mc7)
