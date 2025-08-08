rm(list = ls())

# tri des données
library(tidyverse)
library(glue)

# tableau ppt
library(flextable)
set_flextable_defaults(
  digits = 2, # deux chiffres significatifs
  decimal.mark = ".", # séparation décimale
  big.mark = " ", # tranche de 3 chiffres
  na_str = "<na>"
)

# tableau latex
library(xtable)

# graphique
library(ggplot2)
library(ggpmisc)

# machine learning
library(ranger)
library(caret)

# name
name <- "EDS" # choisir parmi les names
names <- c("ABH", "AEB", "CDH", "CDS", "COF1", "COF2", "EDS", "PRP")

setwd("C:/Users/BERNARD-REYMONDGuill/ENGIE/Pôle Ressource et productible - 4-Stage/2025 - Bernard-Reymond")

# import du rdata : peut ne pas exister
if (file.exists(glue("./5-Résultats/{name}/data_{name}.Rdata"))) {
  load(glue("./5-Résultats/{name}/data_{name}.Rdata"), envir = .GlobalEnv)
}


# import des differentes fonctions
source("./5-Résultats/MCP_CP/fonctions_prev_SLT.R")
source("./5-Résultats/AEP_ref/fonctions_data_cm.R")
source("./5-Résultats/AEP_ref/fonctions_AEP_ref.R")
source("./5-Résultats/Lasso_clean/fonctions_split_recomp.R")
source("./5-Résultats/TI_norm/fonctions_TI_norm.R")

###################################
# Import des dataframe filtrés - nettoyés
####################################

# courbe de puissance constructeur
df_cons <- readRDS(glue("./2-Inputs/{name}/df_cons.rds"))

# données lidar filtrée
df_lidar <- readRDS(glue("./2-Inputs/{name}/df_lidar_filtered.rds"))

# données wrf filtrée et shear
df_wrf <- readRDS(glue("./2-Inputs/{name}/df_wrf_filtered.rds"))
nb_year <- max(year(df_wrf$TimeStamp)) - min(year(df_wrf$TimeStamp)) + 1

# on normalise la vitesse du vent
df_wrf_norm <- df_wrf %>%
  mutate(ws = ws * (288 / (temp + 273))^(1 / 3)) %>%
  select(-temp)

# données contenant les données scada néttoyées et les différentes prédictions de ws et P : peut-être amené à être enrichi
df_cm <- readRDS(glue("./2-Inputs/{name}/df_{name}_cleaned_merge.rds"))

# données de mat
df_mat <- readRDS(glue("./2-Inputs/{name}/df_mat_filtered.rds"))

# données scada cleané et filtré
df_clean_filt <- clean_filt(name = name)

# caractéristique éoliennes
df_carac <- readRDS(glue("./2-Inputs/df_carac.rds"))

# type de variables
variables_lidar <- c("ws_hub_norm", "ti_hub", "v_shear", "v_veer")

if (name %in% c("COF1", "COF2")) {
  variables_mat <- c("ws_mat", "ti_mat", "wa_mat")
} else {
  variables_mat <- c("ws_mat", "ti_mat", "shear_mat", "wa_mat")
}



save.image(glue("./5-Résultats/{name}/data_{name}.Rdata"))

##########################################
# scatter plot + courbe de puissance binning
###########################################

binning_curve(df = df_lidar, df_cons = df_cons)

##################################
# moyennage, filtrage et jointure
##################################

df_moy <- moy_filt(name, type = "lidar")

df_merge_wrf_lidar <- df_wrf%>%
  inner_join(df_moy,by="TimeStamp")


########################
# caclul AEP_CMV
#######################

AEP_CMV_fun(name = name)

#########################################
# vérification synchronisation si le moyennage n'a pas tout décalé
#########################################

# Nuage de points
p <- ggplot(data = df_merge_wrf_lidar, aes(x = ws, y = ws_hub_norm)) +
  geom_point() +
  geom_smooth(method = lm) +
  stat_poly_eq() +
  xlab("WS Réa 100 m") +
  ylab("WS CMV")

p

# Vérif synchronisation : TS
# valeur 200 à changer pour vérifier sur une plus grande plage horaire
df_merge_wrf_lidar_subset <- df_merge_wrf_lidar[1:200, ] %>%
  dplyr::select(TimeStamp, ws, ws_hub_norm) %>%
  tidyr::pivot_longer(cols = c(ws, ws_hub_norm), names_to = "source", values_to = "value")

ts <- ggplot(data = df_merge_wrf_lidar_subset, aes(x = TimeStamp, y = value, color = source)) +
  geom_line() +
  xlab("Temps") +
  ylab("Vitesse du vent (m/s)") +
  scale_color_manual(
    values = c("ws" = "blue", "ws_hub_norm" = "red"),
    labels = c("LiDAR", "WRF"),
    name = "Source"
  ) +
  theme_minimal()


ts



#############################################
# MCP ++ Lidar
##############################################


######################################
# Prédiction CT sans et avec shear
#####################################
for (var in variables_lidar) {
  print(var)
  train_and_fit(var, df_merge_wrf_lidar, df_wrf, shear = "no")
  test_and_result(var, shear = "no")
}

rm(var)

df_perf <- as.data.frame(t(sapply(variables_lidar, function(var) get(glue("resultat_{var}")))))
df_perf <- df_perf %>%
  mutate(across(everything(), ~ if (is.list(.x)) map_dbl(.x, ~ .x[[1]]) else .x))
flextable(df_perf)

for (var in variables_lidar) {
  print(var)
  train_and_fit(var, df_merge_wrf_lidar, df_wrf, shear = "yes")
  test_and_result(var, shear = "yes")
}

df_perf_shear <- as.data.frame(t(sapply(variables_lidar, function(var) get(glue("resultat_{var}_shear")))))
df_perf_shear <- df_perf_shear %>%
  mutate(across(everything(), ~ if (is.list(.x)) map_dbl(.x, ~ .x[[1]]) else .x))

flextable(df_perf_shear)


########################################
# Prédiction LT et data_frame LT pour prédiction à partir données lidar
########################################

df_SLT <- data.frame(TimeStamp = df_wrf$TimeStamp)

df_SLT_shear <- data.frame(TimeStamp = df_wrf$TimeStamp)
for (var in variables_lidar) {
  print(var)
  df_SLT_shear[[var]] <- predict(get(glue("fit_{var}_shear")), newdata = df_wrf)
}

save.image(glue("./5-Résultats/{name}/data_{name}.Rdata"))


#############################################
# MCP ++ Mat
##############################################

######################################################### "
# moyennage pour le mat pour les heures contenant 6 données et jointure avec df_wrf
#######################################################
df_moy_mat <- moy_filt(name = name, type = "mat")

df_merge_wrf_lidar <- df_wrf%>%
  inner_join(df_moy_mat,by="TimeStamp")

###################################
# Prédiction MCP avec données de réanalyse et de mat sans et avec shear
##################################
# sans shear
for (var in variables_mat) {
  print(var)
  train_and_fit(var, df_merge_wrf_mat, df_wrf, shear = "no")
  test_and_result(var, shear = "no")
}

#set.seed(1809)
#index <- sample(seq_len(nrow(df_merge_wrf_mat)), size = 0.8 * nrow(df_merge_wrf_mat))
#df_residus_ws <- df_merge_wrf_mat%>%
#  inner_join(df_SLT_mat%>%select(TimeStamp,ws_mat)%>%
#               rename(ws_SLT_mat = ws_mat), by="TimeStamp")%>%
#  slice(index)%>%
#  mutate(res_ws = ws_mat-ws_SLT_mat)
#
#hist(df_residus_ws$res_ws,breaks=50,probability=T)
#sd(df_residus_ws$res_ws)

df_perf_mat <- as.data.frame(t(sapply(variables_mat, function(var) get(glue("resultat_{var}")))))
df_perf_mat <- df_perf_mat %>%
  mutate(across(everything(), ~ if (is.list(.x)) map_dbl(.x, ~ .x[[1]]) else .x))

flextable(df_perf_mat)

df_SLT_mat <- data.frame(TimeStamp = df_wrf$TimeStamp)
for (var in variables_mat) {
  print(var)
  df_SLT_mat[var] <- predict(get(glue("fit_{var}")), newdata = df_wrf)
}

# Normalisation par la densité de l'air
df_SLT_mat <- df_SLT_mat %>%
  filter(!is.na(TimeStamp)) %>%
  left_join(df_wrf %>% select(TimeStamp, temp) %>% filter(!is.na(TimeStamp)),
    by = "TimeStamp"
  ) %>%
  mutate(ws_mat = ws_mat * (288 / (temp + 273))^(1 / 3)) %>%
  select(-temp)

# avec shear
for (var in variables_mat) {
  print(var)
  train_and_fit(var, df_merge_wrf_mat, df_wrf, shear = "yes")
  test_and_result(var, shear = "yes")
}

df_perf_mat_shear <- as.data.frame(t(sapply(variables_mat, function(var) get(glue("resultat_{var}_shear")))))
df_perf_mat_shear <- df_perf_mat_shear %>%
  mutate(across(everything(), ~ if (is.list(.x)) map_dbl(.x, ~ .x[[1]]) else .x))

flextable(df_perf_mat_shear)

df_SLT_mat_shear <- data.frame(TimeStamp = df_wrf$TimeStamp)
for (var in variables_mat) {
  print(var)
  df_SLT_mat_shear[var] <- predict(get(glue("fit_{var}_shear")), newdata = df_wrf)
}

# Normalisation par la densité de l'air
df_SLT_mat_shear <- df_SLT_mat_shear %>%
  inner_join(df_wrf %>% select(TimeStamp, temp), by = "TimeStamp") %>%
  mutate(ws_mat = ws_mat * (288 / (temp + 273))^(1 / 3)) %>%
  select(-temp)

save.image(glue("./5-Résultats/{name}/data_{name}.Rdata"))




#--------------------------------------------------------------------------------


################################################################
# Modèles de Courbes de puissance
################################################################

# procédure de cross-validation
fitControl <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 10
)

###########################
# prediction de P_CMV Lidar -> Lidar
###########################
set.seed(1809)

fit_P_CMV <- train(reformulate(variables_lidar, response = "P"),
  data = df_lidar,
  method = "ranger",
  trControl = fitControl,
  verbose = FALSE,
)

pred_P_CMV <- predict(fit_P_CMV, newdata = df_SLT %>% select(-TimeStamp))

df_SLT_CMV <- cbind(df_SLT, P = pred_P_CMV)

plot_P_SLT(df_SLT, pred_P_CMV, df_cons, df_cm = df_cm)

save.image(glue("{path_result}/{name}/data_{name}.Rdata"))

#######################################################
# prediction de P_SLT avec entrainements sur données lidar MCP_lidar -> P_lidar
#######################################################

df_merge_SLT <- df_SLT %>%
  inner_join(df_moy %>%
    dplyr::select(TimeStamp, P), by = "TimeStamp")

set.seed(1809)

fit_P_SLT <- train(P ~ . - TimeStamp,
  data = df_merge_SLT,
  method = "ranger",
  trControl = fitControl,
  verbose = FALSE,
  importance = "impurity"
)

pred_P_SLT <- predict(fit_P_SLT, newdata = df_SLT)

df_SLT_SLT <- cbind(df_SLT, P = pred_P_SLT)

plot_P_SLT(df_SLT, pred_P_SLT, df_cons, df_cm)

#######################################################
# prediction de P_SLT_shear avec entrainements sur données lidar MCP_shear_lidar -> P_lidar
# Plus vraiment d'intérêt en fait
#######################################################

df_merge_SLT_shear <- df_SLT_shear %>%
  inner_join(df_moy %>%
    dplyr::select(TimeStamp, P), by = "TimeStamp")

set.seed(1809)

fit_P_SLT_shear <- train(P ~ . - TimeStamp,
  data = df_merge_SLT_shear,
  method = "ranger",
  trControl = fitControl,
  verbose = FALSE,
  importance = "impurity"
)

pred_P_SLT_shear <- predict(fit_P_SLT_shear, newdata = df_SLT_shear)

df_SLT_SLT_shear <- cbind(df_SLT, P = pred_P_SLT_shear)

plot_P_SLT(df_SLT_shear, pred_P_SLT_shear, df_cons = df_cons, df_cm = df_cm)

########################################################################
# prediction de P_SLT sur données avec entrainements sur P_scada par année : MCP_lidar -> P_scada_best_year
########################################################################

# test de kullback-leibler pour connaître la distrib la plus proche de P_scada complet
best_year <- test_best_year_KL(name = name,var="P_scada",ref="scada")$best_year

fit_P_SLT_best_year <- fit_P_year_scada(year = best_year,type="lidar")

pred_P_SLT_best_year <- predict(fit_P_SLT_best_year, newdata = df_SLT)

df_SLT_best_year <- data.frame(df_SLT,
                               P = pred_P_SLT_best_year)

plot_P_SLT(df_SLT, pred_P_SLT_best_year, df_cons = df_cons, df_cm = df_cm)


########################################################################
# prediction de P_SLT_shear sur données avec entrainements sur P_scada par année MCP_lidar_shear -> P_scada_shear_best_year
########################################################################
df_merge_SLT_shear_scada <- df_SLT_shear %>%
  inner_join(df_cm %>%
    dplyr::select(TimeStamp, P_scada), by = "TimeStamp") %>%
  rename(P = P_scada)

set.seed(1809)

fit_P_SLT_shear_best_year <- fit_P_year_scada(year = best_year,type="lidar",shear="yes")

pred_P_SLT_shear_best_year <- predict(fit_P_SLT_shear_best_year, newdata = df_SLT_shear)

df_SLT_shear_best_year <- data.frame(df_SLT_shear, P = pred_P_SLT_shear_best_year)

plot_P_SLT(df_SLT_shear, pred_P_SLT_shear_best_year, df_cons = df_cons, df_cm = df_cm)


#############################################
# Calcul de P_modele_Cons à partir de MCP_lidar sans et avec shear
###############################################

P_SLT_cons <- power_curve(df_SLT$ws_hub_norm, df_cons)
P_SLT_shear_cons <- power_curve(df_SLT_SLT_shear$ws_hub_norm, df_cons)

#########################################
# Calcul de P_SLT_TI à partir de MCP_lidar
# On utilise les fonctions de TI_norm.
# df_norm désigne un dataframe contenant la vitesse de vent normalisé se nommant ws_hub_norm
# et une variable d'intensité de turbulence nommée ti_hub
########################################

rho <- 1.225
D <- df_carac %>%
  filter(name == name) %>%
  pull(diametre)

A <- pi * D^2 / 4

res_M1 <- figure_M1(df_norm = df_SLT_CMV)

P_Iref_fun <- res_M1$P_Iref_fun

P_SLT_TI <- sapply(df_SLT_CMV$ws_hub_norm, P_Iref_fun)


#######################################################################
# modele CMV dégradé : on modélise P_lidar à partir des données de Réanalyses : ws_hub_norm et ti_hub
###############################################################

df_rea <- df_wrf_norm %>%
  dplyr::select(TimeStamp, ws_norm, ti) %>%
  drop_na()

colnames(df_rea) <- c("TimeStamp", "ws_hub_norm", "ti_hub")

df <- df_lidar
fit_P_CMV_deg <- train(P ~ ws_hub_norm + ti_hub,
  data = df,
  method = "ranger",
  trControl = fitControl,
  verbose = FALSE,
)

pred_P_CMV_deg <- predict(fit_P_CMV_deg, df_rea)

AEP_Rea_CMV <- round(sum(pred_P_CMV_deg, na.rm = T) / nb_year / 1e6, 3)


##################################################################################
# prediction de P_SLT_mat en prenant l'année la plus proche de la distrib de P_scada : MCP_mat -> P_scada_best_year
####################################################################################

df_merge_scada_SLT_mat <- df_SLT_mat %>%
  inner_join(df_cm %>%
    dplyr::select(TimeStamp, P_scada), by = "TimeStamp") %>%
  rename(P = P_scada)

set.seed(1809)

fit_P_SLT_mat_best_year <-  fit_P_year_scada(year = best_year, type = "mat",shear="no")

pred_P_SLT_mat_best_year <- predict(fit_P_SLT_mat_best_year, newdata = df_SLT_mat)

df_SLT_mat_best_year <- data.frame(df_SLT_mat,
                                   P = pred_P_SLT_mat_best_year)

df_cm <- df_cm%>%
  select(-P_SLT_mat_best_year)%>%
  inner_join(df_SLT_mat_best_year%>%select(TimeStamp,P)%>%
               rename(P_SLT_mat_best_year = P),
             by="TimeStamp")


plot_P_SLT(df_cm %>% rename(ws_hub_norm = ws_mat), df_cm$P_SLT_mat_best_year, df_cons = df_cons, df_cm = df_cm)


save.image(glue("./5-Résultats/{name}/data_{name}.Rdata"))

##################################################################################
# prediction de P_SLT_mat en prenant la première année de la distrib de P_scada : MCP_mat -> P_scada_first_year
####################################################################################

df_merge_scada_SLT_mat <- df_SLT_mat %>%
  inner_join(df_cm %>%
               dplyr::select(TimeStamp, P_scada), by = "TimeStamp") %>%
  rename(P = P_scada)

set.seed(1809)

df_first <- readRDS(glue("./2-Inputs/df_first.rds"))
first_year <- df_first%>%
  filter(name==parc)%>%
  pull(first_year)

fit_P_SLT_mat_first <-  fit_P_year_scada(year = first_year, type = "mat",shear="no")

pred_P_SLT_mat_first <- predict(fit_P_SLT_mat_first, newdata = df_SLT_mat)

df_SLT_mat_first <- data.frame(df_SLT_mat,
                                   P_SLT_mat_first = pred_P_SLT_mat_first)

df_cm <- df_cm%>%
  inner_join(df_SLT_mat_first%>%
               select(TimeStamp,P_SLT_mat_first),
             by="TimeStamp")

saveRDS(df_cm,glue("./2-Inputs/{name}/df_{name}_cleaned_merge.rds"))

save.image(glue("./5-Résultats/{name}/data_{name}.Rdata"))

residus_P_mat_first <- df_cm%>%
  select(TimeStamp,P_SLT_mat_first,P_scada)%>%
  filter(year(TimeStamp)==2017)%>%
  mutate(residus = P_SLT_mat_first - P_scada)%>%
  filter(P_scada > 1800)


plot(residus_P_mat_first$P_scada,residus_P_mat_first$P_SLT_mat_first)

plot_P_SLT(df_cm %>% rename(ws_hub_norm = ws_mat), df_cm$P_SLT_mat_best_year, df_cons = df_cons, df_cm = df_cm)



###################################
# Modèle de CP : MCP_mat -> P_SLT_mat_cons
######################################

P_SLT_mat_cons <- power_curve(df_SLT_mat$ws_mat, df_cons)

###################################
# Modèle de CP : MCP_mat -> P_SLT_mat_TI
######################################
df_ti <- df_SLT_mat_best_year %>% rename(ws_hub_norm = ws_mat, ti_hub = ti_mat)
res_M1_mat <- figure_M1(df_norm = df_ti)
P_Iref_fun_mat <- res_M1_mat$P_Iref_fun

P_SLT_mat_TI <- sapply(df_SLT_mat_best_year$ws_mat, P_Iref_fun_mat)/1000

df_temp <- df_wrf%>%
  select(TimeStamp)

df_temp <- cbind(df_temp,P_SLT_mat_cons,P_SLT_mat_TI)

df_cm <- df_cm %>%
  select(-P_SLT_mat_cons,-P_SLT_mat_TI)%>%
  inner_join(df_temp,by="TimeStamp")
  

#######################################################
# Modèle de CP à partir des données de réanalyse directement : df_wrf -> P_scada
########################################################
# Avec la courbe constructeur
P_Rea_Cons <- power_curve(df_wrf_norm$ws, wtg = df_cons)

# AVec normalisation des turbulences
P_Rea_TI <- sapply(df_wrf_norm$ws, P_Iref_fun)

df_merge_shear <- df_wrf_norm %>%
  inner_join(
    df_cm %>%
      select(TimeStamp, P_scada) %>%
      mutate(year = year(TimeStamp)) %>%
      filter(year == best_year) %>%
      select(-year),
    by = "TimeStamp"
  )

df_merge_sans_shear <- df_wrf_norm %>%
  select(-shear) %>%
  inner_join(
    df_cm %>%
      select(TimeStamp, P_scada) %>%
      mutate(year = year(TimeStamp)) %>%
      filter(year == best_year) %>%
      select(-year),
    by = "TimeStamp"
  )

fit_P_rea_shear <- train(P_scada ~ . - TimeStamp,
  data = df_merge_shear,
  method = "ranger",
  trControl = fitControl,
  verbose = FALSE,
  importance = "impurity"
)

fit_P_rea <- train(P_scada ~ . - TimeStamp,
  data = df_merge_sans_shear,
  method = "ranger",
  trControl = fitControl,
  verbose = FALSE,
  importance = "impurity"
)

pred_P_rea_shear <- predict(fit_P_rea_shear, newdata = df_wrf_norm)

pred_P_rea <- predict(fit_P_rea, newdata = df_wrf_norm %>% select(-shear))



##########################
# Calcul des AEP
###########################

# AEP à partir des modèles de CP issus de MCP_lidar
AEP_SLT_Cons <- round(sum(P_SLT_cons) / nb_year / 1e6, 3)
AEP_SLT_TI <- round(sum(P_SLT_TI, na.rm = T) / nb_year / 1e9, 3)
AEP_STL_RF_CMV <- round(sum(pred_P_CMV) / nb_year / 1e6, 3)
AEP_SLT_RF_SLT <- round(sum(pred_P_SLT) / nb_year / 1e6, 3)
AEP_SLT_RF_SLT_bestyear <- round(sum(pred_P_SLT_best_year) / nb_year / 1e6, 3)
AEP_SLT_shear_RF_SLT <- round(sum(pred_P_SLT_shear) / nb_year / 1e6, 3)
AEP_SLT_shear_RF_SLT_best_year <- round(sum(pred_P_SLT_shear_best_year) / nb_year / 1e6, 3)

# AEP à partir des modèles de CP avec données de réanalyse
AEP_Rea_cons <- round(sum(P_Rea_Cons) / nb_year / 1e6, 3)
AEP_Rea_TI <- round(sum(P_Rea_TI, na.rm = T) / nb_year / 1e9, 3)
AEP_Rea_RF_best_year <- round(sum(pred_P_rea) / nb_year / 1e6, 3)
AEP_Rea_RF_shear_best_year <- round(sum(pred_P_rea_shear) / nb_year / 1e6, 3)

# AEP à partir des modèles de CP issus de MCP_mat
AEP_SLT_mat_SLT_best_year <- round(sum(pred_P_SLT_mat_best_year) / nb_year / 1e6, 3)




####################################
# Power Deviation Matrix
################################### "

PowMatDev(df_lidar, df_cons)

df_SLT_CMV <- cbind(df_SLT, P = pred_P_CMV)
PowMatDev(df_SLT_CMV, df_cons)

df_SLT_SLT <- cbind(df_SLT, P = pred_P_SLT)
PowMatDev(df_SLT_SLT, df_cons)

####################################
# PDM par rapport courbe binning
################################### "

df_bin <- binning(df_lidar$ws_hub_norm, df_lidar$P, 4.5, 20, 1)

PowMatDev(df_lidar, df_bin)
PowMatDev(df_SLT_CMV, df_bin)
PowMatDev(df_SLT_SLT, df_bin)

df_bin_scada <- binning(df_cm$ws_scada, df_cm$P_scada, 4.5, 20, 1)[[1]]
PowMatDev(df_SLT_CMV, df_bin_scada)



############################################
# Calculs des biais par tranche : wa ou ws.  ref = mat ou scada
###########################################
biais_tranche(name = "CDH", var = "ws", ref = "scada",pred = "mat")
biais_tranche(name = "CDH", var = "wa", ref = "scada",pred = "mat")

#############################
# biais par an de ws_mat_SLT vs ws_mat
###############################
df_biais_mat_SLT_year <- df_SLT_mat %>%
  inner_join(df_moy_mat %>% select(TimeStamp, ws_mat, wa_mat) %>% rename(ws_mat_moy = ws_mat), by = "TimeStamp") %>%
  mutate(year = year(TimeStamp)) %>%
  group_by(year) %>%
  summarise(
    count = n(),
    biais_ws = mean(ws_mat - ws_mat_moy),
    biais_wa = mean(wa_mat.x - wa_mat.y),
    .groups = "drop"
  ) %>%
  mutate(prop = round(count / sum(count) * 100, 2))

ggplot(df_biais_mat_SLT_year, aes(x = factor(year), y = biais_ws)) +
  geom_col(fill = "skyblue") +
  labs(
    x = "Année",
    y = "Biais (m/s)",
    title = "Biais ws_SLT_mat vs ws_mat",
    subtitle = "Proportion des années en %"
  ) +
  geom_text(aes(x = factor(year), y = 0.1, label = prop)) +
  theme_minimal()

ggplot(df_biais_mat_SLT_year, aes(x = factor(year), y = biais_wa)) +
  geom_col(fill = "skyblue") +
  labs(
    x = "Année",
    y = "Biais (°)",
    title = "Biais wa_SLT_mat vs wa_mat",
    subtitle = "Proportion des années en %"
  ) +
  geom_text(aes(x = factor(year), y = 0.6, label = prop)) +
  theme_minimal()

##############################################################
# biais avec les données scada annuel ou mensuel : possible
#############################################################
biais_to_scada(name = "CDH", var = c("ws_mat_corrige"), type = "year")
biais_to_scada(name = "CDH", var = c("P_SLT_mat_corr_first",'P_SLT_mat_best_year'), type = "year")

###################################
# Sauvegarde de l'environnement global
####################################

save.image(glue("./5-Résultats/{name}/data_{name}.Rdata"))
