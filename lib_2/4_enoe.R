ea <- readRDS("../data/enoe_ampliado_hogares.rds")
sd <- readRDS("../data/sdem_hogares.rds") %>% dplyr::select(-ingocup)

hogares <- dplyr::inner_join(
  ea, sd,
  by = c("ent", "con", "v_sel", "n_hog", "h_mud", "upm", "fac", "per", "t_loc", "est")
)

saveRDS(hogares, "../data/enoe_hogares.rds")


names(df)
