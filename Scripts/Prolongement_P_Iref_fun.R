
names <- c("ABH", "AEB", "CDH", "CDS", "COF1", "COF2", "EDS", "PRP")

path_input <- "C:/Users/BERNARD-REYMONDGuill/ENGIE/Pôle Ressource et productible - 4-Stage/2025 - Bernard-Reymond/2-Inputs"
path_result <- "C:/Users/BERNARD-REYMONDGuill/ENGIE/Pôle Ressource et productible - 4-Stage/2025 - Bernard-Reymond/5-Résultats"

for (name in names) {
  print(name)
  env_tmp <- new.env()  # Crée un environnement temporaire
  
  load(glue("{path_result}/{name}/data_{name}.Rdata"), envir = env_tmp)
  
  # Appelle la fonction avec les données chargées
  res_M1 <- with(env_tmp, res_M1)
  
  df_bin <- res_M1$df_bin
  P_Iref_fun <- approxfun(
    x = df_bin$ws,
    y = df_bin$P_Iref,
    rule = 2,
    f = 0
  )
  
  # Ajoute les nouveaux résultats dans l'environnement
  env_tmp$P_SLT_TI <- sapply(env_tmp$df_SLT_CMV$ws_hub_norm, P_Iref_fun)
  env_tmp$P_Rea_TI <- sapply(env_tmp$df_wrf_norm$ws_norm, P_Iref_fun)
  
  # Sauvegarde l'environnement mis à jour
  save(list = ls(env_tmp), file = glue("{path_result}/{name}/data_{name}.Rdata"), envir = env_tmp)
}


for (name in names) {
  print(name)
  env_tmp <- new.env()  # Crée un environnement temporaire
  
  load(glue("{path_result}/{name}/data_{name}2.Rdata"), envir = env_tmp)
  
  # Appelle la fonction avec les données chargées
  res_M1 <- with(env_tmp, res_M1)
  
  df_bin <- res_M1$df_bin
  P_Iref_fun <- approxfun(
    x = df_bin$ws,
    y = df_bin$P_Iref,
    rule = 2,
    f = 0
  )
  
  # Ajoute les nouveaux résultats dans l'environnement
  env_tmp$P_SLT_TI2 <- sapply(env_tmp$df_SLT_norm$ws_hub_norm, P_Iref_fun)
  
  # Sauvegarde l'environnement mis à jour
  save(list = ls(env_tmp), file = glue("{path_result}/{name}/data_{name}2.Rdata"), envir = env_tmp)
}
