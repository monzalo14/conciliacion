df <- readRDS("../data/enoe_ampliado.rds")


### Por hogar
df$folioh <- paste0(df$cd_a, df$ent, df$con, df$v_sel, df$n_hog, df$h_mud)
df.hogar <- data.frame(df) %>% 
  dplyr::mutate(
    ingocup.aux = ifelse(is.na(ingocup), 1, 0)
    , ingreso.mensual.aux = ifelse(is.na(ingreso.mensual) & ocupado == 1, 1, 0)
    , ingreso.mensual.recup.aux = ifelse(is.na(ingreso.mensual.recup), 1, 0)
    #    , ingreso.secun.aux = ifelse(is.na(ingreso.secun), 1, 0)
    #    , ingreso.secun.efectivo.aux = ifelse(is.na(ingreso.secun.efectivo), 1, 0)
  ) %>%
  dplyr::group_by(cd_a, ent, con, v_sel, n_hog, h_mud, upm, fac, ent, per, upm_14, t_loc, est) #%>%
#dplyr::group_by(., folioh) 

## NOs aseguramos que la cuenta de NAs /que tanto problema causo/ sea correcta
assertthat::assert_that(sum(df.hogar$ingreso.mensual.aux) == sum(is.na(df$ingreso.mensual[df$ocupado == 1])))

df.h <- 
  dplyr::summarise(df.hogar,
                   n = n(),
                   num.ocupados = sum(ocupado, na.rm = T)
                   , ingocup = sum(ingocup, na.rm = T)
                   , ingocup.na = ifelse(sum(ingocup.aux) > 0, NA, sum(ingocup))
                   , ingreso.mensual = sum(ingreso.mensual, na.rm = T)
                   , ingreso.mensual.na = ifelse(sum(ingreso.mensual.aux) > 0, NA, sum(ingreso.mensual))
                   , ingreso.mensual.recup = sum(ingreso.mensual.recup, na.rm = T)
                   , ingreso.mensual.recup.na = ifelse(sum(ingreso.mensual.recup.aux) > 0, NA, sum(ingreso.mensual.recup))
                   , ingreso.secun = sum(ingreso.secun, na.rm = T)
                   #, ingreso.secun.na = ifelse(sum(ingreso.secun.aux) > 0, NA, sum(ingreso.secun))
                   , ingreso.secun.efectivo = sum(ingreso.secun.efectivo, na.rm = T)
                   #, ingreso.secun.efectivo.na = ifelse(sum(ingreso.secun.efectivo.aux) > 0, NA, sum(ingreso.secun.efectivo))
  )

df.h <- data.frame(df.h) %>% 
  dplyr::mutate(fac = as.numeric(fac),
                ingreso.mensual.total = ingreso.mensual.na + ingreso.secun,
                ingreso.mensual.recup.total = ingreso.mensual.recup.na + ingreso.secun) %>%
  data.frame() 


saveRDS(df.h, "../data/enoe_ampliado_hogares.rds")
