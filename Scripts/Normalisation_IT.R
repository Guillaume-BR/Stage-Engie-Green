# tri des données
library(tidyverse)
library(glue)

# graphique
library(ggplot2)
library(plotly)

# machine learning
library(ranger)
library(caret)

source("./R/MCP_and_PC/fonctions_prev_SLT.R")
source("./R/TI_norm/fonctions_TI_norm.R")

#nom de l'éolienne
name <- "ABH"

#Données de l'éolienne
df_cons <- readRDS(glue("./data/{name}/df_cons.rds"))
df_lidar <- readRDS(glue("./data/{name}/df_lidar_filtered.rds"))
df_carac <- readRDS(glue("./data/df_carac.rds"))


#sélection des données
#df_norm pour vitesse normalisée
df_norm <- df_lidar%>%
  dplyr::select('timestamp','ws_hub_norm','ti_hub','P')%>%
  drop_na(ws_hub_norm, P)


plot_M3(df_norm = df_norm)

#On récupère les données issues de la normalisation des turbulences
data_rated <- figure_M4(df_norm)

data_rated

plot_M4(df_norm)

cp_th <- data_rated$cp_th
v_rated_th <- data_rated$v_rated_th
P_rated_th <- data_rated$P_rated_th
v_cut_in <- data_rated$v_cut_in_ajust

#Confrontation valeurs théoriques et mesurées
cp_rated <- seq(0.3,0.5,0.01)
v_rated <- ((2*P_rated_th)/(rho*A*cp_rated))^(1/3)
v_cut_out <- 25

res <- result_rmse(cp_rated = cp_rated, v_rated=v_rated,P_rated_th=P_rated_th)
plot_opt(cp_rated = cp_rated,v_rated=v_rated,P_rated_th=P_rated_th)
