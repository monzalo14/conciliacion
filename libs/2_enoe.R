library(rformat)

df <- readRDS("../data/enoe_ampliado_raw.rds") 

df <- df %>%
  dplyr::mutate(
    p3a = as.character(p3a),
    tiene_jefe = ifelse(p3a == "1", "si", ifelse(p3a == "2", "no", NA)),
    p3h = as.character(p3h),
    recibe_pago = ifelse(p3h == "1", "si", ifelse(p3h == "9", "ns", "no")),
    p3k1 = as.character(p3k1),
    p3k2 = as.character(p3k2),
    contrato = ifelse(p3k1 == "2", "base", ifelse(p3k1 == "9", "ns",
                                                  ifelse(p3k2 == "1", "temporal.0a2m",
                                                         ifelse(p3k2 == "2", "temporal.2a6m",
                                                                ifelse(p3k2 == "3", "temporal.6a12m",
                                                                       ifelse(p3k2 == "4", "a.termino", NA))))
    )),
    contrato.base = ifelse(p3k1 == "2", "si", NA),
    contrato.temporal = ifelse(p3k1 == "1", "si", NA),
    temporal.duracion = ifelse(p3k2 == "1", "temporal.0a2m",
                               ifelse(p3k2 == "2", "temporal.2a6m",
                                      ifelse(p3k2 == "3", "temporal.6a12m",
                                             ifelse(p3k2 == "4", "a.termino", NA)))),
    upm = stringr::str_pad(as.character(upm), width = 7, pad = "0", side = "left")
  ) %>%
  dplyr::mutate_each_(., funs(as.character), vars = c(paste0("p3l", c(1:5, 9)), paste0("p3m", c(1:9)))) %>%
  dplyr::mutate(
    r3l.aguinaldo = ifelse(p3l1 == "1", "si", "no"),
    r3l.vacaciones.pagadas = ifelse(p3l2 == "2", "si", "no"),
    r3l.utilidades = ifelse(p3l3 == "3", "si", "no"),
    r3l.ninguna.anteriores = ifelse(p3l4 == "4", "si", "no"),
    r3l.nada = ifelse(p3l5 == "5", "si", "no"),
    r3m.credito.viv = ifelse(p3m1 == "1", "si", "no"),
    r3m.guarderia = ifelse(p3m2 == "2", "si", "no"),
    r3m.maternidad.pat = ifelse(p3m3 == "3", "si", "no"),
    r3m.fondo.retiro = ifelse(p3m4 == "4", "si", "no"),
    r3m.seguro.vida = ifelse(p3m5 == "5", "si", "no"),
    r3m.seguro.gmm = ifelse(p3m6 == "6", "si", "no"),
    r3m.prestamos.caja = ifelse(p3m7 == "7", "si", "no"),
    r3m.ninguna.anteriores = ifelse(p3m8 == "8", "si", "no"),
    r3m.no.sabe = ifelse(p3m9 == "9", "si", "no"),
    p3q = as.character(p3q),
    r3q.num.trab = ifelse(p3q == "01", "t001",
                          ifelse(p3q == "02", "t002a005",
                                 ifelse(p3q == "03", "t006a010",
                                        ifelse(p3q == "04", "t011a015",
                                               ifelse(p3q == "05", "t016a020",
                                                      ifelse(p3q == "06", "t021a030",
                                                             ifelse(p3q == "07", "t031a050",
                                                                    ifelse(p3q == "08", "t051a100",
                                                                           ifelse(p3q == "09", "t101a250",
                                                                                  ifelse(p3q == "10", "t251a500",
                                                                                         ifelse(p3q == "11", "t501omas", "no.sabe"
                                                                                         ))))))))))),
    r3q.num.trab.chico = ifelse(p3q %in% paste0("0", 1:4), "t01a15",
                                ifelse(p3q %in% c(paste0("0", 5:9), "10", "11"), "t16omas", NA)),
    p3r = as.character(p3r),
    r3r.inicio = ifelse(p3r == "1", "este.año",
                        ifelse(p3r == "2", "año.pasado",
                               ifelse(p3r == "3", "antes.año.pasado", "no.sabe")))
  ) %>% # me salto a subordinado
  dplyr::mutate(
    p7 = as.character(p7),
    r7.simple = ifelse(p7 %in% c("7", "9"), "no", "si"),
    r7.toda = ifelse(p7 == "1", "vender",
                     ifelse(p7 == "2", "presta.servicios",
                            ifelse(p7 == "3", "tierra.o.cria",
                                   ifelse(p7 == "4", "propinas.comision.destajo",
                                          ifelse(p7 == "5", "asalariado",
                                                 ifelse(p7 == "6", "ayuda.negocio", NA
                                                 ))))))
  ) %>% # me salto a antescedentes laborales
  dplyr::mutate_each_(., funs(as.character), vars = c(paste0("p9n", c(1:6, 9)), paste0("p10_", c(1:4, 9)), paste0("p10a", c(1:4, 9)))) %>%
  dplyr::mutate(
    r9n1 = ifelse(p9n1 == "1", "si", "no"),
    r9n2 = ifelse(p9n2 == "2", "si", "no"),
    r9n3 = ifelse(p9n3 == "3", "si", "no"),
    r9n4 = ifelse(p9n4 == "4", "si", "no"),
    r9n5 = ifelse(p9n5 == "5", "si", "no"),
    r9n6 = ifelse(p9n6 == "6", "si", "no"),
    r10_1 = ifelse(p10_1 == "1", "si", "no"),
    r10_2 = ifelse(p10_2 == "2", "si", "no"),
    r10_3 = ifelse(p10_3 == "3", "si", "no"),
    r10_4 = ifelse(p10_4 == "4", "si", "no"),
    r10a1 = ifelse(p10a1 == "1", "si", "no"),
    r10a2 = ifelse(p10a2 == "2", "si", "no"),
    r10a3 = ifelse(p10a3 == "3", "si", "no"),
    r10a4 = ifelse(p10a4 == "4", "si", "no")
  ) %>%
  dplyr::mutate(
    p6b1 = as.character(p6b1),
    p6b2 = as.character(p6b2),
    p6b2 = ifelse(p6b2 != "999999" & p6b2 != "999998", as.numeric(p6b2), NA),
    ingreso.mensualizado = ifelse(p6b1 == "1", p6b2, 
                                  ifelse(p6b1 == "2", p6b2 * 2,
                                         ifelse(p6b1 == "3", p6b2 * 4,
                                                ifelse(p6b1 == "4", p6b2 * 30, NA)))),
    ingreso.mensual = p6b2,
    ocupado = ifelse(clase1 == 1 & clase2 == 1, 1, 0),
    p6_9 = as.character(p6_9),
    p6a3 = as.character(p6a3),
    p6c = as.numeric(as.character(p6c)),
    r6c = p6c,
    r6b1 = p6b1,
    r6b2 = p6b2,
    ingreso.mensual.recup1  = ifelse(!is.na(p6b2), p6b2,
                                     ifelse(ocupado == 0, 0,
                                            ifelse(p6_9 == "09" | p6a3 == "3", 0)))) %>%
  data.frame(.) %>%
  dplyr::mutate(
    ingreso.mensual.recup2 = ifelse(p6c == "1", 0.5 * salario,
                                    ifelse(p6c == "2", 1 * salario,
                                           ifelse(p6c == "3", 1.5 * salario,
                                                  ifelse(p6c == "4", 2.5 * salario,
                                                         ifelse(p6c == "5", 4 * salario,
                                                                ifelse(p6c == "6", 7.5 * salario,
                                                                       ifelse(p6c == "7", 10 * salario, NA
                                                                       ))))))),
    ingreso.mensual.recup = ifelse(is.na(ingreso.mensual.recup1), ingreso.mensual.recup2, ingreso.mensual.recup1),
    sm = ifelse(p6c %in% c("1", "2"), "sm00a01", 
                ifelse(p6c == "3", "sm01a02",
                       ifelse(p6c == "4", "sm02a03",
                              ifelse(p6c == "5", "sm03a05",
                                     ifelse(p6c == "6", "sm05a10",
                                            ifelse(p6c == "7", "sm10ymas", NA)))))),
    p7g1 = as.character(p7g1),
    p7g2 = as.character(p7g2),
    p7gcan = as.character(p7gcan),
    ingreso.secun.efectivo = ifelse(p7g1 == "1", as.numeric(p7gcan), NA),
    ingreso.secun = as.numeric(p7gcan)
  )

#sum(is.na(df2$p6b2))/nrow(df2) + sum(df2$p6b2 == df2$ingocup, na.rm =T)/nrow(df2) == 1

#source("../../lib/crea_cortes_eco.R")
#df <- corte.economico.gpo(datos = dplyr::rename(df, factor = fac), grupo = "per", variable.ref = "ingreso.mensual", pesos = "factor", cortes = 10, nombre = "dec", prefijo = "d")
#print(names(df))

#df <- df[, -grep("^p[0-9]", names(df))]

#
load("../intercensal/datos/upms.rdata")
df$upm_panel <- F
df$upm_panel[df$upm %in% upms$panel] <- T

df$upm_14 <- F
df$upm_14[df$upm %in% upms$a2014] <- T
df$upm_15 <- F
df$upm_15[df$upm %in% upms$a2015] <- T

fac2char <- function(x){as.character(x)}
df <- dplyr::mutate_each(df, funs(fac2char), est, t_loc)
df$fac <- as.numeric(df$fac)

library(Hmisc)
df <- data.frame(df) %>% 
  dplyr::mutate(fac = as.numeric(fac),
                ingreso.mensual.total = ifelse(is.na(ingreso.secun), ingreso.mensual, ingreso.mensual + ingreso.secun),
                ingreso.mensual.recup.total = ifelse(is.na(ingreso.secun), ingreso.mensual.recup, ingreso.mensual.recup + ingreso.secun)) %>%
  data.frame() 

saveRDS(df, "../data/enoe_ampliado_todo.rds")

df <- df[, -grep("^p[0-9]", names(df))]
saveRDS(df, "../data/enoe_ampliado.rds")

