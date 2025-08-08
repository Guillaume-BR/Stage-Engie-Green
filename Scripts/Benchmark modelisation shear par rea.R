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


load(glue("{path_result}/bench_MCP_shear.RData"))
save.image(glue("{path_result}/bench_MCP_shear.RData"))
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
  train <- train.test("v_shear",df_merge,df_rea)$train
  test <- train.test("v_shear",df_merge,df_rea)$test
  mean_ws = mean(test$v_shear,na.rm=T)
  #############################
  # LM
  #############################
  fit <- lm(v_shear ~ ., data = train)
  pred <- predict(fit, newdata = test)
  df_result_MCP_nrmse[['lm']][i] <- sqrt(mean((pred - test$v_shear)^2,na.rm=T))/mean_ws*100
  df_result_MCP_R2[['lm']][i] <- 1 - mean((pred - test$v_shear)^2,na.rm=T) / mean((test$v_shear - mean_ws)^2,na.rm=T)
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
  train <- train.test("v_shear",df_merge,df_rea)$train
  test <- train.test("v_shear",df_merge,df_rea)$test
  mean_ws = mean(test$v_shear,na.rm=T)
  
  #entrainement et prédictions
  fit <- train(
    v_shear ~ .,
    data = train,
    method = "knn",
    preProcess = c("center", "scale"),
    trControl = fitControl,
    tuneGrid = tuneGrid
  )
  pred <- predict(fit, newdata = test)
  
  #calcul des indicateurs
  df_result_MCP_nrmse[['knn']][i] <- sqrt(mean((pred - test$v_shear)^2,na.rm=T))/mean_ws*100
  df_result_MCP_R2[['knn']][i] <- 1 - mean((pred - test$v_shear)^2,na.rm=T) / mean((test$v_shear - mean_ws)^2,na.rm=T)
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
  train <- train.test("v_shear",df_merge,df_rea)$train
  test <- train.test("v_shear",df_merge,df_rea)$test
  mean_ws = mean(test$v_shear,na.rm=T)
  
  #entrainement et prédictions
  fit <- train(
    v_shear ~ .,
    data = train,
    method = "pls",
    preProcess = c("center", "scale"),
    trControl = fitControl,
    tuneGrid = expand.grid(ncomp = 1:3)
  )
  
  pred <- predict(fit, newdata = test)
  
  #calcul des indicateurs
  df_result_MCP_nrmse[['pls']][i] <- sqrt(mean((pred - test$v_shear)^2,na.rm=T))/mean_ws*100
  df_result_MCP_R2[['pls']][i] <- 1 - mean((pred - test$v_shear)^2,na.rm=T) / mean((test$v_shear - mean_ws)^2,na.rm=T)
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
  train <- train.test("v_shear",df_merge,df_rea)$train
  test <- train.test("v_shear",df_merge,df_rea)$test
  mean_ws = mean(test$v_shear,na.rm=T)
  
  #entrainement et prédictions
  fit <- train(
    v_shear ~ .,
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
  df_result_MCP_nrmse[['gbm']][i] <- sqrt(mean((pred - test$v_shear)^2,na.rm=T))/mean_ws*100
  df_result_MCP_R2[['gbm']][i] <- 1 - mean((pred - test$v_shear)^2,na.rm=T) / mean((test$v_shear - mean_ws)^2,na.rm=T)
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
  train <- train.test("v_shear",df_merge,df_rea)$train
  test <- train.test("v_shear",df_merge,df_rea)$test
  mean_ws = mean(test$v_shear,na.rm=T)
  
  #entrainement et prédictions
  fit <- train(
    v_shear ~ .,
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
  df_result_MCP_nrmse[['lasso']][i] <- sqrt(mean((pred - test$v_shear)^2,na.rm=T))/mean_ws*100
  df_result_MCP_R2[['lasso']][i] <- 1 - mean((pred - test$v_shear)^2,na.rm=T) / mean((test$v_shear - mean_ws)^2,na.rm=T)
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
  train <- train.test("v_shear",df_merge,df_rea)$train
  test <- train.test("v_shear",df_merge,df_rea)$test
  mean_ws = mean(test$v_shear,na.rm=T)
  
  # Données
  x_train <- as.matrix(train[, setdiff(names(train), "v_shear")])
  y_train <- train$v_shear
  x_test <- as.matrix(test[, setdiff(names(test), "v_shear")])
  y_test <- test$v_shear
  
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
  train <- train.test("v_shear",df_merge,df_rea)$train
  test <- train.test("v_shear",df_merge,df_rea)$test
  mean_ws = mean(test$v_shear,na.rm=T)
  
  #entrainement et prédictions
  fit <- train(
    v_shear ~ .,
    data = train,
    method = "ranger",
    trControl = fitControl
  )
  pred <- predict(fit, newdata = test)
  
  #calcul des indicateurs
  df_result_MCP_nrmse[['rf']][i] <- sqrt(mean((pred - test$v_shear)^2,na.rm=T))/mean_ws*100
  df_result_MCP_R2[['rf']][i] <- 1 - mean((pred - test$v_shear)^2,na.rm=T) / mean((test$v_shear - mean_ws)^2,na.rm=T)
}


library(tidyverse)

# 1. Transformation au format long
df_nrmse_long <- df_result_MCP_nrmse %>%
  select(-lasso,-pls)%>%
  pivot_longer(-parc, names_to = "modele", values_to = "nrmse")

# 2. Tracé ggplot
ggplot(df_nrmse_long, aes(x = parc, y = nrmse, color = modele, group = modele)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(
    title = "NRMSE par parc et par modèle pour le shear",
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


env_temp=new.env()
load(glue("{path_result}/{names[1]}/data_{names[1]}.Rdata"),envir=env_temp)

names <- c("ABH", "AEB", "CDH", "CDS", "COF1", "COF2", "EDS", "PRP")

path_input <- "C:/Users/BERNARD-REYMONDGuill/ENGIE/Pôle Ressource et productible - 4-Stage/2025 - Bernard-Reymond/2-Inputs"
path_result <- "C:/Users/BERNARD-REYMONDGuill/ENGIE/Pôle Ressource et productible - 4-Stage/2025 - Bernard-Reymond/5-Résultats"

df <- env_temp$data_wrf_ABH

df[, -1] <- lapply(df[, -1], function(x) as.numeric(gsub(",", ".", x)))
df_modif <- df%>%
  mutate(TimeStamp = as.POSIXct(TimeStamp,format = "%d/%m/%Y %H:%M"),
         year=year(TimeStamp),
         ws_rea = MeanWindSpeedUID_100.0m.Vmoy..vent.L.1.00.U75.00,
         ti_rea = TurbIntUID_100.0m.Intensité.turbulence.L0.00,
         shear_rea = log(MeanWindSpeedUID_75.0m.Vmoy..vent.L.1.00.U75.00/MeanWindSpeedUID_100.0m.Vmoy..vent.L.1.00.U75.00)/log(75/100),
         mean_ti_tot = mean(ti_rea,na.rm=T))%>%
  filter(shear_rea>0, shear_rea !=Inf,year<2025)%>%
  mutate(mean_shear_tot = mean(shear_rea,na.rm=T))%>%
  mutate(mean_ws_tot = mean(ws_rea,na.rm=T))%>%
  group_by(year)%>%
  summarise(count=n(),
            mean_ws = mean(ws_rea,na.rm=T),
            ecart_ti=mean(ti_rea-mean_ti_tot,na.rm=T)/mean_ti_tot*100,
            sd_ti=sd(ti_rea,na.rm=T),
            mean_shear=mean(shear_rea,na.rm=T),
            ecart_shear=mean(shear_rea-mean_shear_tot,na.rm=T)/mean_shear_tot*100,
            sd_shear=sd(shear_rea,na.rm=T),
            ecart_ws=mean(ws_rea-mean_ws_tot,na.rm=T)/mean_ws_tot*100
  )

ggplot(df_modif,aes(x=year,y=ecart_ti))+
  geom_point(color="skyblue",size=1.5) +
  geom_line(color="skyblue",linewidth=1) +
  scale_x_continuous(breaks = unique(df_modif$year)) +
  labs(x = "Année",
       y = "Ecart (%)") +
  theme_minimal()+
  theme(axis.text.x = element_text(face = "bold"),  # Bold x-axis tick labels
        axis.text.y = element_text(face = "bold")
        )
  

data_rilly_modif <- data_rilly%>%
  mutate(ws_mat=X44895.Vit.Moyenne.80.1m.125.,
         ti_mat =  X175851.Vit.EcartType.60m.270./X45024.Vit.Moyenne.60m.270.,
         shear_mat = log(X45023.Vit.Moyenne.10m.270./X45024.Vit.Moyenne.60m.270.)/log(10/60),
         TimeStamp= as.POSIXct(Date,format= "%d/%m/%Y %H:%M:%S"),
         year=year(TimeStamp))%>%
  drop_na(ti_mat,shear_mat,ws_mat)%>%
  filter(ti_mat!=Inf,shear_mat!=Inf,shear_mat!=-Inf)%>%
  group_by(year)%>%
  summarise(count=n(),
            mean_ws=mean(ws_mat,na.rm=T),
            mean_ti=mean(ti_mat,na.rm=T),
            sd_ti = sd(ti_mat,na.rm=T),
            mean_shear=mean(shear_mat,na.rm=T),
            sd_shear = sd(shear_mat,na.rm=T)
  )
  
