rm(list=ls())

# tri des données
library(tidyverse)
library(glue)
library(rlang)

# pour avoir des tableau exportable ppt
library(flextable)
set_flextable_defaults(
  digits = 2, # deux chiffres significatifs
  decimal.mark = ".", # séparation décimale
  big.mark = " ", # tranche de 3 chiffres
  na_str = "<na>"
)

path_input <- "C:/Users/BERNARD-REYMONDGuill/ENGIE/Pôle Ressource et productible - 4-Stage/2025 - Bernard-Reymond/2-Inputs"
path_result <- "C:/Users/BERNARD-REYMONDGuill/ENGIE/Pôle Ressource et productible - 4-Stage/2025 - Bernard-Reymond/5-Résultats"
names <- c("ABH", "AEB", "CDH", "CDS", "COF1", "COF2", "EDS", "PRP")
P_nom <- c(3000, 2000, 2050, 2200, 2050, 2050, 2000, 2050)

df_P_moy <- data.frame(parc=names)
for (i in seq_along(names)){
  name <- names[i]
  print(name)
  load(glue("{path_result}/{name}/data_{name}.rdata"))
  df_P_moy[["P_moy"]][i] <- mean(df_cm$P_scada,na.rm=T)
}

df_result_pred_P_nrmse$P_nom <- P_nom

# 2. Fusionner avec les moyennes
df_corr <- df_result_pred_P_nrmse %>%
  left_join(df_P_moy, by = "parc")

# 3. Appliquer la transformation sur toutes les colonnes numériques sauf 'parc', 'P_nom' et 'moy'
df_corr <- df_corr %>%
  mutate(across(
    .cols = where(is.numeric) & !any_of(c("P_nom", "P_moy")),
    .fns = ~ .x * P_nom / P_moy
  ))

df_result_pred_P_bias$P_nom <- P_nom
# 2. Fusionner avec les moyennes
df_corr_biais <- df_result_pred_P_bias %>%
  left_join(df_P_moy, by = "parc")

# 3. Appliquer la transformation sur toutes les colonnes numériques sauf 'parc', 'P_nom' et 'moy'
df_corr_biais <- df_corr_biais %>%
  mutate(across(
    .cols = where(is.numeric) & !any_of(c("P_nom", "P_moy")),
    .fns = ~ .x * P_nom / P_moy
  ))

load(glue("{path_result}/bench_P.Rdata"))

# fitControl
fitControl <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 10
)

df_result_pred_P_nrmse <- df_result_pred_P_nrmse%>%
  mutate(across(where(is.numeric())))

#
df_result_pred_P_nrmse <- data.frame(parc=names)
df_result_pred_P_R2 <- data.frame(parc=names)
df_result_pred_P_bias <- data.frame(parc=names)

#############################
# LM
#############################
for (i in seq_along(names)){
  print(names[i])
  #chargement des données dans environnement temporaire
  env_temp=new.env()
  load(glue("{path_result}/{names[i]}/data_{names[i]}.Rdata"),envir=env_temp)
  
  df_merge_SLT <- get("df_merge_SLT",envir=env_temp)
  fit <- train(P ~ . - TimeStamp,
                     data = df_merge_SLT,
                     method = "lm",
                     trControl = fitControl,
                     verbose = FALSE,
                     importance = "impurity"
  )
  
  # Calcul des prédictions
  pred <- predict(fit, newdata = df_merge_SLT)
  
  # Calcul du biais
  obs <- df_merge_SLT$P
  bias <- mean(pred - obs, na.rm = TRUE)
  
  df_result_pred_P_nrmse[['lm']][i] <- fit$results$RMSE[which.min(fit$results$RMSE)]/P_nom[i]*100
  df_result_pred_P_R2[['lm']][i] <- fit$results$Rsquared[which.min(fit$results$RMSE)]
  df_result_pred_P_bias[['lm']][i] <- 100 * bias / P_nom[i]
}


#####################
#pls
#####################

library(pls)

for (i in seq_along(names)){
  print(names[i])
  #chargement des données dans environnement temporaire
  env_temp=new.env()
  load(glue("{path_result}/{names[i]}/data_{names[i]}.Rdata"),envir=env_temp)
  
  df_merge_SLT <- get("df_merge_SLT",envir=env_temp)
  fit <- train(P ~ . - TimeStamp,
               data = df_merge_SLT,
               method = "pls",
               trControl = fitControl,
               verbose = FALSE,
               importance = "impurity",
               preProcess = c("center", "scale"),
               tuneGrid = expand.grid(ncomp = 1:3)
  )
  
  # Calcul des prédictions
  pred <- predict(fit, newdata = df_merge_SLT)
  
  # Calcul du biais
  obs <- df_merge_SLT$P
  bias <- mean(pred - obs, na.rm = TRUE)
  
  df_result_pred_P_nrmse[['pls']][i] <- fit$results$RMSE[which.min(fit$results$RMSE)]/P_nom[i]*100
  df_result_pred_P_R2[['pls']][i] <- fit$results$Rsquared[which.min(fit$results$RMSE)]
  df_result_pred_P_bias[['pls']][i] <- 100 * bias / P_nom[i]
}


#####################
#knn
#####################
library(rsample)
library(recipes)

for (i in seq_along(names)){
  print(names[i])
  #chargement des données dans environnement temporaire
  env_temp=new.env()
  load(glue("{path_result}/{names[i]}/data_{names[i]}.Rdata"),envir=env_temp)
  
  df_merge_SLT <- get("df_merge_SLT",envir=env_temp)
  fit <- train(P ~ . - TimeStamp,
               data = df_merge_SLT,
               method = "knn",
               trControl = fitControl,
               verbose = FALSE,
               importance = "impurity",
               preProcess = c("center", "scale"),
               tuneGrid = expand.grid(
                 k = seq(5, 9, by = 1)
               )
  )
  
  # Calcul des prédictions
  pred <- predict(fit, newdata = df_merge_SLT)
  
  # Calcul du biais
  obs <- df_merge_SLT$P
  bias <- mean(pred - obs, na.rm = TRUE)
  
  df_result_pred_P_nrmse[['knn']][i] <- fit$results$RMSE[which.min(fit$results$RMSE)]/P_nom[i]*100
  df_result_pred_P_R2[['knn']][i] <- fit$results$Rsquared[which.min(fit$results$RMSE)]
  df_result_pred_P_bias[['knn']][i] <- 100 * bias / P_nom[i]
}



##############################
#gbm
################################

for (i in seq_along(names)){
  print(names[i])
  #chargement des données dans environnement temporaire
  env_temp=new.env()
  load(glue("{path_result}/{names[i]}/data_{names[i]}.Rdata"),envir=env_temp)
  
  df_merge_SLT <- get("df_merge_SLT",envir=env_temp)
  fit <- train(P ~ . - TimeStamp,
               data = df_merge_SLT,
               method = "gbm",
               preProcess = c("center", "scale"),
               trControl = fitControl,
               tuneGrid = expand.grid(
                 n.trees = c(500, 1000),
                 interaction.depth = 2,
                 shrinkage = 0.1,
                 n.minobsinnode = c(5, 10)
               )
  )
  
  # Calcul des prédictions
  pred <- predict(fit, newdata = df_merge_SLT)
  
  # Calcul du biais
  obs <- df_merge_SLT$P
  bias <- mean(pred - obs, na.rm = TRUE)
  
  df_result_pred_P_nrmse[['gbm']][i] <- fit$results$RMSE[which.min(fit$results$RMSE)]/P_nom[i]*100
  df_result_pred_P_R2[['gbm']][i] <- fit$results$Rsquared[which.min(fit$results$RMSE)]
  df_result_pred_P_bias[['gbm']][i] <- 100 * bias / P_nom[i]
  
  
  
}

##############################
#xgboost
################################

for (i in seq_along(names)){
  print(names[i])
  #chargement des données dans environnement temporaire
  env_temp=new.env()
  load(glue("{path_result}/{names[i]}/data_{names[i]}.Rdata"),envir=env_temp)
  df_merge_SLT <- get("df_merge_SLT",envir=env_temp)
  
  set.seed(1809)
  idx <- sample(seq_len(nrow(df_merge_SLT)), size = 0.8 * nrow(df_merge_SLT))
  train <- df_merge_SLT[idx,]
  test <- df_merge_SLT[-idx,]
  
  x_train <- as.matrix(train[, setdiff(names(train), c("TimeStamp","P"))])
  y_train <- train$P
  x_test <- as.matrix(test[, setdiff(names(train), c("TimeStamp","P"))])
  y_test <- test$P
                           
  grid <- expand.grid(
    max_depth = c(4, 6, 8),
    eta = c(0.05, 0.1, 0.25),
    nrounds = c(500, 1000, 2000)
  )
  
  best_rmse <- Inf
  best_model <- NULL
  best_pred <- NULL
  best_R2 <- NULL
  
  for (j in 1:nrow(grid)) {
    params <- grid[j, ]
    
    fit <- xgboost(
      data = x_train,
      label = y_train,
      nrounds = 2000,
      objective = "reg:squarederror",
      max_depth = params$max_depth,
      eta = params$eta,
      verbose = 0
    )
    
    pred <- predict(fit, newdata = x_test)
    rmse_val <- sqrt(mean((pred - y_test)^2, na.rm = TRUE))
    R2 <- 1 - mean((pred - y_test)^2, na.rm = TRUE) / mean((y_test - mean(y_test, na.rm = TRUE))^2, na.rm = TRUE)
    
    if (rmse_val < best_rmse) {
      best_rmse <- rmse_val
      best_model <- fit
      best_pred <- pred
      best_R2 <- R2
    }
  }
  
  pred <- predict(best_model, newdata = df_merge_SLT)
  
  # Calcul du biais
  obs <- df_merge_SLT$P
  bias <- mean(pred - obs, na.rm = TRUE)
    
  df_result_pred_P_nrmse[['xgboost']][i] <- best_rmse/P_nom[i]*100
  df_result_pred_P_R2[['xgboost']][i] <- best_R2
  df_result_pred_P_bias[['xgboost']][i] <- 100 * bias / P_nom[i]
}

##############################
#rf
################################

for (i in seq_along(names)){
  print(names[i])
  #chargement des données dans environnement temporaire
  env_temp=new.env()
  load(glue("{path_result}/{names[i]}/data_{names[i]}.Rdata"),envir=env_temp)
  
  df_merge_SLT <- get("df_merge_SLT",envir=env_temp)
  fit <- train(P ~ . - TimeStamp,
               data = df_merge_SLT,
               method = "ranger",
               trControl = fitControl,
  )
  
  # Calcul des prédictions
  pred <- predict(fit, newdata = df_merge_SLT)
  
  # Calcul du biais
  obs <- df_merge_SLT$P
  bias <- mean(pred - obs, na.rm = TRUE)
  
  df_result_pred_P_nrmse[['rf']][i] <- fit$results$RMSE[which.min(fit$results$RMSE)]/P_nom[i]*100
  df_result_pred_P_R2[['rf']][i] <- fit$results$Rsquared[which.min(fit$results$RMSE)]
  df_result_pred_P_bias[['rf']][i] <- 100 * bias / P_nom[i]
}


# 1. Transformation au format long
df_nrmse_long <- df_corr %>%
  dplyr::select(-xgboost,-lasso,-pls,-P_nom,-P_moy)%>%
  pivot_longer(-parc, names_to = "modele", values_to = "nrmse")

# 2. Tracé ggplot
ggplot(df_nrmse_long, aes(x = parc, y = nrmse, color = modele, group = modele)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(
    x = "Parc",
    y = "NRMSE (%)",
    color = "Modèle"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# 1. Transformation au format long
df_biais_long <- df_corr_biais %>%
  dplyr::select(-pls,-P_nom,-P_moy)%>%
  mutate(across(where(is.numeric),~round(.,2)))%>%
  pivot_longer(-parc, names_to = "modele", values_to = "biais")

# 2. Tracé ggplot
ggplot(df_biais_long, aes(x = parc, y = biais, color = modele, group = modele)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(
    x = "Parc",
    y = "Biais normalisé (%)",
    color = "Modèle"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save.image(glue("{path_result}/bench_P.Rdata"))

for (i in seq_along(names)){
  print(names[i])
  #chargement des données dans environnement temporaire
  df <- readRDS(glue("{path_input}/{names[i]}/df_{names[i]}_cleaned_merge.rds"))
  cat(min(year(df$TimeStamp)),max(year(df$TimeStamp)))
}

ggplot(env_temp$df_wrf%>%
         mutate(year=year(TimeStamp))%>%
         filter(year==2020),aes(x=ti,y=after_stat(density)))+
  geom_histogram()+
  xlim(c(0,0.3))

ggplot(env_temp$df_wrf%>%
         mutate(year=year(TimeStamp))%>%
         filter(year==2022),aes(x=ti,y=after_stat(density)))+
  geom_histogram()+
  xlim(c(0,0.3))

ggplot(env_temp$df_wrf%>%
         mutate(year=year(TimeStamp))%>%
         filter(year==2021),aes(x=ti,y=after_stat(density)))+
  geom_histogram()+
  xlim(c(0,0.3))

mean_ti <- env_temp$df_wrf%>%
  mutate(year=year(TimeStamp))%>%
  group_by(year)%>%
  summarise(mean_ti = mean(ti,na.rm=T),
            mean_shear = mean(v_shear,na.rm=T))


mean_shear <- env_temp$df_wrf%>%
  mutate(year=year(TimeStamp))%>%
  group_by(year)%>%
  summarise(mean_shear = mean(ti,na.rm=T))