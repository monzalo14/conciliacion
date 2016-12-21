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

# Casos particulares

df$giro_empresa[df$id_exp=='2/1183/2011'|df$id_exp=='2/794/2011'] <- 72
df$giro_empresa[df$id_exp=='2/475/2011'|df$id_exp=='2/633/2011'|df$id_exp=='2/634/2011'|df$id_exp=='2/797/2011'] <- 31
df$giro_empresa[df$id_exp=='2/1255/2011'|df$id_exp=='9/548/2011'] <- 43
df$giro_empresa[df$id_exp=='11/342/2011'] <- 32
df$giro_empresa[df$id_exp=='2/473/2011'] <- 46

df <- mutate_each(df, funs(factor), gen, starts_with("tipo"), modo_termino,
                  starts_with("per"), junta, exp, anio, giro_empresa)

saveRDS(df, "../clean_data/observaciones.RDS")
write.csv(df, "../clean_data/observaciones_tope.csv", na="")



