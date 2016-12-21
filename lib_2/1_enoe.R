library(foreign)
library(rformat)
library(dplyr)

## sobre el ampliado
part1 <- dir("../data", pattern = "coe1t1.*\\.dbf$", full.names = TRUE, recursive = T)
part2 <- dir("../data", pattern = "coe2t1.*\\.dbf$", full.names = TRUE, recursive = T)
hogs <- dir("../data", pattern = "hogt1.*\\.dbf$", full.names = TRUE, recursive = T)
sdem <- dir("../data", pattern = "sdemt1.*\\.dbf$", full.names = TRUE, recursive = T)
fac2chr <- function(x){as.character(x)}

hogs_df <- read.dbf(hogs)
sdem_df <- read.dbf(sdem)

names(part1_df)

df.all <- NULL
df.h.all <- NULL
for (i in seq(part1)) {
  print(paste0("Anio procesado: ", i))
  p <- read.dbf(part1[i]) %>%
    dplyr::filter(R_DEF == "00")
  p2 <- read.dbf(part2[i])
  h <- read.dbf(hogs[i])
  sd <- read.dbf(sdem[i]) %>%
    dplyr::filter(
      R_DEF == "00" # entrevistas incompletas o eliminadas
      , C_RES != "2" # gente que ya no habita en la vivienda
      # , EDA != "00", EDA != "11", EDA != "99" # eliminamos a menores de 12
    )
  # por cada periodo, hay que hacer el read y el join y luego pegamos todo

  hh <- dplyr::mutate_each(h, funs(fac2chr), CD_A, ENT, CON, UPM, D_SEM, 
                           N_PRO_VIV, V_SEL, N_HOG, H_MUD, N_ENT, EST, T_LOC, PER) %>%
    dplyr::select(., CD_A, ENT, CON, UPM, D_SEM, N_PRO_VIV, V_SEL, N_HOG, 
                  H_MUD, N_ENT, PER, EST, T_LOC)
  print("hh cool")
  sd <- dplyr::mutate_each(sd, funs(fac2chr), 
                           R_DEF, CD_A, ENT, CON, UPM, D_SEM, N_PRO_VIV, 
                           V_SEL, N_HOG, H_MUD, N_ENT, PER, N_REN, EDA, EST, T_LOC, FAC) %>%
    dplyr::select(.,
                  R_DEF, CD_A, ENT, CON, UPM, D_SEM, N_PRO_VIV, V_SEL, N_HOG, 
                  H_MUD, N_ENT, PER, N_REN, EDA, EST, EST_D, T_LOC, FAC, INGOCUP, SALARIO, CLASE1, CLASE2) %>%
    dplyr::mutate(
      FAC = as.integer(FAC)
    )
  print("sd cool")
  ## Creo tabla de cuestionarios pegandole demograficos y carac del hogar
  df <- dplyr::inner_join(p, p2) %>% 
    dplyr::mutate_each(., funs(fac2chr), 
                       R_DEF, CD_A, ENT, CON, UPM, D_SEM, N_PRO_VIV, V_SEL, N_HOG, 
                       H_MUD, N_ENT, PER, N_REN, EDA)
  df <- inner_join(df, hh)
  df <- inner_join(df, sd)
  names(df) <- normalize_names(names(df))
  print("df cool")
  ## Creo una base por hogar
  df.h <- sd %>% 
    dplyr::group_by(ENT, CON, UPM, D_SEM, N_PRO_VIV, V_SEL, N_HOG, H_MUD, N_ENT, PER, EST, EST_D, T_LOC, FAC) %>%
    dplyr::summarise(
      integrantes = n(),
      integrantes.m12 = sum(ifelse(EDA %in% c("00", "11", "99"), 1, 0)),
      perceptores = sum(ifelse(INGOCUP > 0, 1, 0)),
      ingocup = sum(INGOCUP, na.rm = T)
    )
  names(df.h) <- normalize_names(names(df.h))
  print(paste0("dim de df.h: ", nrow(df.h)))
  rm(p, p2, h, hh)
  rm(sd)
  
  ## Junto
  df.all <- plyr::rbind.fill(df.all, df)
  df.h.all <- plyr::rbind.fill(df.h.all, df.h)
  rm(df)
  rm(df.h)
}

saveRDS(df.h.all, "../data/sdem_hogares.rds")
saveRDS(df.all, "../data/enoe_ampliado_raw.rds")

rm(list = ls())

# NO juegues con todos, la base esta grande, juega con estos para limpiar mas vars!
# p <- read.dbf("2014trim1/coe1t114.dbf") %>%
#   dplyr::filter(R_DEF == "00")
# p2 <- read.dbf("2014trim1/coe2t114.dbf") 
# h <- read.dbf("2014trim1/hogt114.dbf")
# sd <- read.dbf("2014trim1/sdemt114.dbf") %>%
#   dplyr::filter(
#     R_DEF == "00" # entrevistas incompletas o eliminadas
#     , C_RES != "2" # gente que ya no habita en la vivienda
#     # , EDA != "00", EDA != "11", EDA != "99" # eliminamos a menores de 12
#     )
# # por cada periodo, hay que hacer el read y el join y luego pegamos todo
# 
# fac2chr <- function(x){as.character(x)}
# hh <- dplyr::mutate_each(h, funs(fac2chr), CD_A, ENT, CON, UPM, D_SEM, 
#                          N_PRO_VIV, V_SEL, N_HOG, H_MUD, N_ENT, EST, T_LOC, PER) %>%
#   dplyr::select(., CD_A, ENT, CON, UPM, D_SEM, N_PRO_VIV, V_SEL, N_HOG, 
#                 H_MUD, N_ENT, PER, EST, T_LOC)
# sd <- dplyr::mutate_each(sd, funs(fac2chr), 
#                          R_DEF, CD_A, ENT, CON, UPM, D_SEM, N_PRO_VIV, 
#                          V_SEL, N_HOG, H_MUD, N_ENT, PER, N_REN, EDA, EST, T_LOC, FAC) %>%
#   dplyr::select(.,
#                 R_DEF, CD_A, ENT, CON, UPM, D_SEM, N_PRO_VIV, V_SEL, N_HOG, 
#                 H_MUD, N_ENT, PER, N_REN, EDA, EST, T_LOC, FAC, INGOCUP) %>%
#   dplyr::mutate(
#     FAC = as.integer(FAC)
#   )
# 
# ## Creo tabla de cuestionarios pegandole demograficos y carac del hogar
# df <- dplyr::inner_join(p, p2) %>% 
#   dplyr::mutate_each(., funs(fac2chr), 
#                      R_DEF, CD_A, ENT, CON, UPM, D_SEM, N_PRO_VIV, V_SEL, N_HOG, 
#                      H_MUD, N_ENT, PER, N_REN, EDA)
# df <- inner_join(df, hh)
# df <- inner_join(df, sd)
# names(df) <- normalize_names(names(df))
# 
# ## Creo una base por hogar
# df.h <- sd %>% 
#   dplyr::group_by(CD_A, ENT, CON, UPM, D_SEM, N_PRO_VIV, V_SEL, N_HOG, H_MUD, N_ENT, PER, EST, T_LOC, FAC) %>%
#   dplyr::summarise(
#     integrantes = n(),
#     integrantes.m12 = sum(ifelse(EDA %in% c("00", "11", "99"), 1, 0)),
#     perceptores = sum(ifelse(INGOCUP > 0, 1, 0)),
#     ingocup = sum(INGOCUP, na.rm = T)
#   )
# names(df.h) <- normalize_names(names(df.h))
# 
# rm(p, p2, h, hh)
# rm(sd)