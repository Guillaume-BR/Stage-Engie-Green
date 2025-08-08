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

names <- c("ABH", "AEB", "CDH", "CDS", "COF1", "COF2", "EDS", "PRP")

path_input <- "C:/Users/BERNARD-REYMONDGuill/ENGIE/Pôle Ressource et productible - 4-Stage/2025 - Bernard-Reymond/2-Inputs"
path_result <- "C:/Users/BERNARD-REYMONDGuill/ENGIE/Pôle Ressource et productible - 4-Stage/2025 - Bernard-Reymond/5-Résultats"


load(glue("{path_result}/bench_MCP_ti.RData"))
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
  train <- train.test("ti_hub",df_merge,df_rea)$train
  test <- train.test("ti_hub",df_merge,df_rea)$test
  mean_ws = mean(test$ti_hub,na.rm=T)
  #############################
  # LM
  #############################
  fit <- lm(ti_hub ~ ., data = train)
  pred <- predict(fit, newdata = test)
  df_result_MCP_nrmse[['lm']][i] <- sqrt(mean((pred - test$ti_hub)^2,na.rm=T))/mean_ws*100
  df_result_MCP_R2[['lm']][i] <- 1 - mean((pred - test$ti_hub)^2,na.rm=T) / mean((test$ti_hub - mean_ws)^2,na.rm=T)
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
  train <- train.test("ti_hub",df_merge,df_rea)$train
  test <- train.test("ti_hub",df_merge,df_rea)$test
  mean_ws = mean(test$ti_hub,na.rm=T)
  
  #entrainement et prédictions
  fit <- train(
    ti_hub ~ .,
    data = train,
    method = "knn",
    preProcess = c("center", "scale"),
    trControl = fitControl,
    tuneGrid = tuneGrid
  )
  pred <- predict(fit, newdata = test)
  
  #calcul des indicateurs
  df_result_MCP_nrmse[['knn']][i] <- sqrt(mean((pred - test$ti_hub)^2,na.rm=T))/mean_ws*100
  df_result_MCP_R2[['knn']][i] <- 1 - mean((pred - test$ti_hub)^2,na.rm=T) / mean((test$ti_hub - mean_ws)^2,na.rm=T)
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
  train <- train.test("ti_hub",df_merge,df_rea)$train
  test <- train.test("ti_hub",df_merge,df_rea)$test
  mean_ws = mean(test$ti_hub,na.rm=T)
  
  #entrainement et prédictions
  fit <- train(
    ti_hub ~ .,
    data = train,
    method = "pls",
    preProcess = c("center", "scale"),
    trControl = fitControl,
    tuneGrid = expand.grid(ncomp = 1:3)
  )
  
  pred <- predict(fit, newdata = test)
  
  #calcul des indicateurs
  df_result_MCP_nrmse[['pls']][i] <- sqrt(mean((pred - test$ti_hub)^2,na.rm=T))/mean_ws*100
  df_result_MCP_R2[['pls']][i] <- 1 - mean((pred - test$ti_hub)^2,na.rm=T) / mean((test$ti_hub - mean_ws)^2,na.rm=T)
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
  train <- train.test("ti_hub",df_merge,df_rea)$train
  test <- train.test("ti_hub",df_merge,df_rea)$test
  mean_ws = mean(test$ti_hub,na.rm=T)
  
  #entrainement et prédictions
  fit <- train(
    ti_hub ~ .,
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
  df_result_MCP_nrmse[['gbm']][i] <- sqrt(mean((pred - test$ti_hub)^2,na.rm=T))/mean_ws*100
  df_result_MCP_R2[['gbm']][i] <- 1 - mean((pred - test$ti_hub)^2,na.rm=T) / mean((test$ti_hub - mean_ws)^2,na.rm=T)
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
  train <- train.test("ti_hub",df_merge,df_rea)$train
  test <- train.test("ti_hub",df_merge,df_rea)$test
  mean_ws = mean(test$ti_hub,na.rm=T)
  
  #entrainement et prédictions
  fit <- train(
    ti_hub ~ .,
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
  df_result_MCP_nrmse[['lasso']][i] <- sqrt(mean((pred - test$ti_hub)^2,na.rm=T))/mean_ws*100
  df_result_MCP_R2[['lasso']][i] <- 1 - mean((pred - test$ti_hub)^2,na.rm=T) / mean((test$ti_hub - mean_ws)^2,na.rm=T)
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
  train <- train.test("ti_hub",df_merge,df_rea)$train
  test <- train.test("ti_hub",df_merge,df_rea)$test
  mean_ws = mean(test$ti_hub,na.rm=T)
  
  # Données
  x_train <- as.matrix(train[, setdiff(names(train), "ti_hub")])
  y_train <- train$ti_hub
  x_test <- as.matrix(test[, setdiff(names(test), "ti_hub")])
  y_test <- test$ti_hub
  
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
  train <- train.test("ti_hub",df_merge,df_rea)$train
  test <- train.test("ti_hub",df_merge,df_rea)$test
  mean_ws = mean(test$ti_hub,na.rm=T)
  
  #entrainement et prédictions
  fit <- train(
    ti_hub ~ .,
    data = train,
    method = "ranger",
    trControl = fitControl
  )
  pred <- predict(fit, newdata = test)
  
  #calcul des indicateurs
  df_result_MCP_nrmse[['rf']][i] <- sqrt(mean((pred - test$ti_hub)^2,na.rm=T))/mean_ws*100
  df_result_MCP_R2[['rf']][i] <- 1 - mean((pred - test$ti_hub)^2,na.rm=T) / mean((test$ti_hub - mean_ws)^2,na.rm=T)
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
    title = "NRMSE par parc et par modèle pour la TI",
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


df_ws <- df_AEB_cm%>%
  mutate(year=year(TimeStamp))%>%
  group_by(year)%>%
  summarise(count = n(),
            mean_ws=mean(ws_scada,na.rm=T))

ggplot(df_ws, aes(x = year, y = mean_ws),color="skyblue") +
  geom_point(color="skyblue",size=1.5) +
  geom_line(color="skyblue",linewidth=1) +
  scale_x_continuous(breaks = unique(df_ws$year)) +
  labs(x = "Année",
       y = "Vitesse du vent (m/s)") +
  theme_minimal()



