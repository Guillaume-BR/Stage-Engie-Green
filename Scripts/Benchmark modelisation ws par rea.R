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

#pour avoir des tableaux en latex
library(xtable)

names <- c("ABH", "AEB", "CDH", "CDS", "COF1", "COF2", "EDS", "PRP")

path_input <- "C:/Users/BERNARD-REYMONDGuill/ENGIE/Pôle Ressource et productible - 4-Stage/2025 - Bernard-Reymond/2-Inputs"
path_result <- "C:/Users/BERNARD-REYMONDGuill/ENGIE/Pôle Ressource et productible - 4-Stage/2025 - Bernard-Reymond/5-Résultats"


load(glue("{path_result}/bench_MCP.RData"))
#####################"
#fontion pour créer train/test
######################
train.test <- function(variable, df_merge, df_rea){
  set.seed(1809) # Fixe la graine pour reproductibilité
  df_rea <- df_rea %>%
    dplyr::select(-temp)
  
  # Séparation des données
  index <- sample(seq_len(nrow(df_merge)), size = 0.8 * nrow(df_merge))
  train <- df_merge[index, ] %>%
    dplyr::select(setdiff(names(df_rea), "TimeStamp"), variable)
  test <- df_merge[-index, ] %>%
    dplyr::select(setdiff(names(df_rea), "TimeStamp"), variable)
  return(list(test=test, train = train))
}


# fitControl
fitControl <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 10
)

df_result_MCP_nrmse <- data.frame(parc=names)
df_result_MCP_R2 <- data.frame(parc=names)

#############################
# LM
#############################
for (i in seq_along(names)){
  print(names[i])
  #chargement des données dans environnement temporaire
  env_temp=new.env()
  load(glue("{path_result}/{names[i]}/data_{names[i]}.Rdata"),envir=env_temp)
  
  df_merge <- get(glue('df_merge_{names[i]}'),envir=env_temp)
  df_rea <- env_temp$df_wrf
  
  #on récupère df_merge_{name} pour former le train/test
  train <- train.test("ws_hub_norm",df_merge,df_rea)$train
  test <- train.test("ws_hub_norm",df_merge,df_rea)$test
  mean_ws = mean(test$ws_hub_norm,na.rm=T)
  #############################
  # LM
  #############################
  fit <- lm(ws_hub_norm ~ ., data = train)
  pred <- predict(fit, newdata = test)
  df_result_MCP_nrmse[['lm']][i] <- sqrt(mean((pred - test$ws_hub_norm)^2,na.rm=T))/mean_ws*100
  df_result_MCP_R2[['lm']][i] <- 1 - mean((pred - test$ws_hub_norm)^2,na.rm=T) / mean((test$ws_hub_norm - mean_ws)^2,na.rm=T)
}

#####################
#knn
#####################

library(rsample)
library(recipes)

tuneGrid <- expand.grid(
  k = seq(5, 9, by = 1)
)
for (i in seq_along(names)){
  print(names[i])
  #chargement des données dans environnement temporaire
  env_temp=new.env()
  load(glue("{path_result}/{names[i]}/data_{names[i]}.Rdata"),envir=env_temp)
  
  df_merge <- get(glue('df_merge_{names[i]}'),envir=env_temp)
  df_rea <- env_temp$df_wrf
  
  #on récupère df_merge_{name} pour former le train/test
  train <- train.test("ws_hub_norm",df_merge,df_rea)$train
  test <- train.test("ws_hub_norm",df_merge,df_rea)$test
  mean_ws = mean(test$ws_hub_norm,na.rm=T)
  
  #entrainement et prédictions
  fit <- train(
    ws_hub_norm ~ .,
    data = train,
    method = "knn",
    preProcess = c("center", "scale"),
    trControl = fitControl,
    tuneGrid = tuneGrid
  )
  pred <- predict(fit, newdata = test)
  
  #calcul des indicateurs
  df_result_MCP_nrmse[['knn']][i] <- sqrt(mean((pred - test$ws_hub_norm)^2,na.rm=T))/mean_ws*100
  df_result_MCP_R2[['knn']][i] <- 1 - mean((pred - test$ws_hub_norm)^2,na.rm=T) / mean((test$ws_hub_norm - mean_ws)^2,na.rm=T)
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
  
  df_merge <- get(glue('df_merge_{names[i]}'),envir=env_temp)
  df_rea <- env_temp$df_wrf
  
  #on récupère df_merge_{name} pour former le train/test
  train <- train.test("ws_hub_norm",df_merge,df_rea)$train
  test <- train.test("ws_hub_norm",df_merge,df_rea)$test
  mean_ws = mean(test$ws_hub_norm,na.rm=T)
  
  #entrainement et prédictions
  fit <- train(
    ws_hub_norm ~ .,
    data = train,
    method = "pls",
    preProcess = c("center", "scale"),
    trControl = fitControl,
    tuneGrid = expand.grid(ncomp = 1:3)
  )
  
  pred <- predict(fit, newdata = test)
  
  #calcul des indicateurs
  df_result_MCP_nrmse[['pls']][i] <- sqrt(mean((pred - test$ws_hub_norm)^2,na.rm=T))/mean_ws*100
  df_result_MCP_R2[['pls']][i] <- 1 - mean((pred - test$ws_hub_norm)^2,na.rm=T) / mean((test$ws_hub_norm - mean_ws)^2,na.rm=T)
} 
#n'apporte rien par rapport à lm


#####################
# GBM : gradient boosting model
#####################

for (i in seq_along(names)){
  print(names[i])
  #chargement des données dans environnement temporaire
  env_temp=new.env()
  load(glue("{path_result}/{names[i]}/data_{names[i]}.Rdata"),envir=env_temp)
  
  df_merge <- get(glue('df_merge_{names[i]}'),envir=env_temp)
  df_rea <- env_temp$df_wrf
  
  #on récupère df_merge_{name} pour former le train/test
  train <- train.test("ws_hub_norm",df_merge,df_rea)$train
  test <- train.test("ws_hub_norm",df_merge,df_rea)$test
  mean_ws = mean(test$ws_hub_norm,na.rm=T)
  
  #entrainement et prédictions
  fit <- train(
    ws_hub_norm ~ .,
    data = train,
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
  
  pred <- predict(fit, newdata = test)
  
  #calcul des indicateurs
  df_result_MCP_nrmse[['gbm']][i] <- sqrt(mean((pred - test$ws_hub_norm)^2,na.rm=T))/mean_ws*100
  df_result_MCP_R2[['gbm']][i] <- 1 - mean((pred - test$ws_hub_norm)^2,na.rm=T) / mean((test$ws_hub_norm - mean_ws)^2,na.rm=T)
}

######################
#Lasso
######################

for (i in seq_along(names)){
  print(names[i])
  #chargement des données dans environnement temporaire
  env_temp=new.env()
  load(glue("{path_result}/{names[i]}/data_{names[i]}.Rdata"),envir=env_temp)
  
  df_merge <- get(glue('df_merge_{names[i]}'),envir=env_temp)
  df_rea <- env_temp$df_wrf
  
  #on récupère df_merge_{name} pour former le train/test
  train <- train.test("ws_hub_norm",df_merge,df_rea)$train
  test <- train.test("ws_hub_norm",df_merge,df_rea)$test
  mean_ws = mean(test$ws_hub_norm,na.rm=T)
  
  #entrainement et prédictions
  fit <- train(
    ws_hub_norm ~ .,
    data = train,
    method = "lasso",
    preProcess = c("center", "scale"),
    trControl = fitControl,
    tuneGrid = expand.grid(
      .fraction = seq(0, 1, by = 0.1)
    )
  )
  
  pred <- predict(fit, newdata = test)
  
  #calcul des indicateurs
  df_result_MCP_nrmse[['lasso']][i] <- sqrt(mean((pred - test$ws_hub_norm)^2,na.rm=T))/mean_ws*100
  df_result_MCP_R2[['lasso']][i] <- 1 - mean((pred - test$ws_hub_norm)^2,na.rm=T) / mean((test$ws_hub_norm - mean_ws)^2,na.rm=T)
}

#############################"
#Xgboost
#############################
library(xgboost)
library(Metrics) 
for (i in seq_along(names)){
  print(names[i])
  #chargement des données dans environnement temporaire
  env_temp=new.env()
  load(glue("{path_result}/{names[i]}/data_{names[i]}.Rdata"),envir=env_temp)
  
  df_merge <- get(glue('df_merge_{names[i]}'),envir=env_temp)
  df_rea <- env_temp$df_wrf
  
  #on récupère df_merge_{name} pour former le train/test
  train <- train.test("ws_hub_norm",df_merge,df_rea)$train
  test <- train.test("ws_hub_norm",df_merge,df_rea)$test
  mean_ws = mean(test$ws_hub_norm,na.rm=T)
  
  # Données
  x_train <- as.matrix(train[, setdiff(names(train), "ws_hub_norm")])
  y_train <- train$ws_hub_norm
  x_test <- as.matrix(test[, setdiff(names(test), "ws_hub_norm")])
  y_test <- test$ws_hub_norm
  
  grid <- expand.grid(
    max_depth = c(4, 6, 8),
    eta = c(0.05, 0.1, 0.25),
    nrounds = c(500, 1000, 2000)
  )
  
  best_rmse <- Inf
  best_model <- NULL
  best_pred <- NULL
  
  # Grid Search
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
    
    if (rmse_val < best_rmse) {
      best_rmse <- rmse_val
      best_model <- fit
      best_pred <- pred
    }
  }
  
  # Calcul des indicateurs à partir du meilleur modèle
  nrmse <- best_rmse / mean_ws * 100
  r2 <- 1 - mean((best_pred - y_test)^2, na.rm = TRUE) / mean((y_test - mean_ws)^2, na.rm = TRUE)
  
  # Sauvegarde des résultats
  df_result_MCP_nrmse[['xgboost']][i] <- nrmse
  df_result_MCP_R2[['xgboost']][i] <- r2
}


#######################"
#random forest
#########################

for (i in seq_along(names)){
  print(names[i])
  #chargement des données dans environnement temporaire
  env_temp=new.env()
  load(glue("{path_result}/{names[i]}/data_{names[i]}.Rdata"),envir=env_temp)
  
  df_merge <- get(glue('df_merge_{names[i]}'),envir=env_temp)
  df_rea <- env_temp$df_wrf
  
  #on récupère df_merge_{name} pour former le train/test
  train <- train.test("ws_hub_norm",df_merge,df_rea)$train
  test <- train.test("ws_hub_norm",df_merge,df_rea)$test
  mean_ws = mean(test$ws_hub_norm,na.rm=T)
  
  #entrainement et prédictions
  fit <- train(
    ws_hub_norm ~ .,
    data = train,
    method = "ranger",
    trControl = fitControl
  )
  pred <- predict(fit, newdata = test)
  
  #calcul des indicateurs
  df_result_MCP_nrmse[['rf']][i] <- sqrt(mean((pred - test$ws_hub_norm)^2,na.rm=T))/mean_ws*100
  df_result_MCP_R2[['rf']][i] <- 1 - mean((pred - test$ws_hub_norm)^2,na.rm=T) / mean((test$ws_hub_norm - mean_ws)^2,na.rm=T)
}


library(tidyverse)

# 1. Transformation au format long
df_nrmse_long <- df_result_MCP_nrmse %>%
  select(-xgboost,-lasso,-pls)%>%
  pivot_longer(-parc, names_to = "modele", values_to = "nrmse")

# 2. Tracé ggplot
ggplot(df_nrmse_long, aes(x = parc, y = nrmse, color = modele, group = modele)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(
    title = "NRMSE par parc et par modèle pour la WS",
    x = "Parc",
    y = "NRMSE (%)",
    color = "Modèle"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# 1. Transformation au format long
df_R2_long <- df_result_MCP_R2 %>%
  pivot_longer(-parc, names_to = "modele", values_to = "nrmse")

# 2. Tracé ggplot
ggplot(df_R2_long, aes(x = parc, y = nrmse, color = modele, group = modele)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(
    title = "R2 par parc et par modèle",
    x = "Parc",
    y = "R2",
    color = "Modèle"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


###################
#on récupère les biais des vitesses
############################
df_biais <- data.frame(parc=names)
for (i in seq_along(names)){
  print(names[i])
  #chargement des données dans environnement temporaire
  env_temp=new.env()
  load(glue("{path_result}/{names[i]}/data_{names[i]}.Rdata"),envir=env_temp)
  df_biais$ws[i] <- env_temp$resultat_ws_hub_norm[['biais']]
  df_biais$ti[i] <- env_temp$resultat_ti_hub[['biais']]*100
  df_biais$shear[i] <- env_temp$resultat_v_shear[['biais']]
  df_biais$veer[i] <- env_temp$resultat_v_veer[['biais']]
}

xtable(df_biais,round=3)
############
#biais avec knn
###########

biais_knn <- data.frame(parc=names)
for (i in seq_along(names)){
  print(names[i])
  #chargement des données dans environnement temporaire
  env_temp=new.env()
  load(glue("{path_result}/{names[i]}/data_{names[i]}.Rdata"),envir=env_temp)
  df_merge <- get(glue("df_merge_{names[i]}"),envir=env_temp)
  train_and_fit("ws_hub_norm",df_merge = df_merge,df_rea=env_temp$df_wrf,method="knn")
  test_and_result("ws_hub_norm",method="knn")
  biais_knn$biais[i] <- resultat_ws_hub_norm[['biais']]
}

flextable(df_biais%>%mutate(across(where(is.numeric),~round(.,3))))

biais_knn_pourcent <- biais_knn%>%
  mutate(across(where(is.numeric),~round(.*100,2)))
flextable(biais_knn_pourcent
          )


# Sauvegarde des résultats
save.image(glue("{path_result}/bench_MCP.RData"))


df_cons <- data.frame(v=seq(0,25,0.1),P=env_temp$power_curve(seq(0,25,0.1),env_temp$data_cons))


ggplot(env_temp$data_cons, aes(x = V, y = P, color = "Données constructeurs")) +
  geom_point() +
  geom_line(data = df_cons, aes(x = v, y = P, color = "Interpolation")) +
  scale_color_manual(
    name = "Légende",  # Titre de la légende
    values = c("Données constructeurs" = "blue", "Interpolation" = "red")
  ) +
  labs(x = "Vitesse du vent (m/s)", y = "Puissance (kW)") +
  theme_minimal()
