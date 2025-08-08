rm(list=ls())

# tri des données
library(tidyverse)
library(glue)

# graphique
library(ggplot2)
library(plotly)

# machine learning
library(ranger)
library(caret)

names = c("ABH","AEB","CDH","CDS","COF1","COF2","EDS","PRP")
setwd("C:/Users/BERNARD-REYMONDGuill/ENGIE/Pôle Ressource et productible - 4-Stage/2025 - Bernard-Reymond")

source(glue("./5-Résultats/Transfert de modele/fonctions_transfert.R"))

##############################################################
#Même parc
###########################################################

# Initialiser le tableau global si ce n'est pas déjà fait
if (!exists("res_pred_transfert")) {
  res_pred_transfert <- data.frame(
    modele = character(),
    donnees = character(),
    data= character(),
    type = character(),
    nrmse = numeric(),
    ecart_to_scada = numeric(),
    stringsAsFactors = FALSE
  )
}

#Chargement des données pour les éoliennes de même type
model_and_data(names=c("CDH","PRP","COF1","COF2"))

#resultat transfert avec données de réanalyses prédisant directement P
pred_transfert(fit_P_rea_first_COF1,df_SLT_rea_first_COF2)
pred_transfert(fit_P_rea_first_COF2,df_SLT_rea_first_COF1)
pred_transfert(fit_P_rea_first_COF1,df_SLT_rea_first_CDH)
pred_transfert(fit_P_rea_first_CDH,df_SLT_rea_first_COF1)
pred_transfert(fit_P_rea_first_COF1,df_SLT_rea_first_PRP)
pred_transfert(fit_P_rea_first_PRP,df_SLT_rea_first_COF1)

pred_transfert(fit_P_rea_first_COF2,df_SLT_rea_first_CDH)
pred_transfert(fit_P_rea_first_CDH,df_SLT_rea_first_COF2)
pred_transfert(fit_P_rea_first_COF2,df_SLT_rea_first_PRP)
pred_transfert(fit_P_rea_first_PRP,df_SLT_rea_first_COF2)

pred_transfert(fit_P_rea_first_PRP,df_SLT_rea_first_CDH)
pred_transfert(fit_P_rea_first_CDH,df_SLT_rea_first_PRP)


#resultat transfert avec données de mat prédisant directement P
pred_transfert(fit_P_SLT_mat_first_COF1,df_SLT_mat_first_COF2)
pred_transfert(fit_P_SLT_mat_first_COF2,df_SLT_mat_first_COF1)
pred_transfert(fit_P_SLT_mat_first_COF1,df_SLT_mat_first_CDH)
pred_transfert(fit_P_mat_corr_first_CDH,df_SLT_mat_first_COF1)
pred_transfert(fit_P_SLT_mat_first_COF1,df_SLT_mat_first_PRP)
pred_transfert(fit_P_mat_corr_first_PRP,df_SLT_mat_first_COF1)

pred_transfert(fit_P_SLT_mat_first_COF2,df_SLT_mat_first_CDH)
pred_transfert(fit_P_mat_corr_first_CDH,df_SLT_mat_first_COF2)
pred_transfert(fit_P_SLT_mat_first_COF2,df_SLT_mat_first_PRP)
pred_transfert(fit_P_mat_corr_first_PRP,df_SLT_mat_first_COF2)

pred_transfert(fit_P_mat_corr_first_PRP,df_SLT_mat_corr_first_CDH)
pred_transfert(fit_P_mat_corr_first_CDH,df_SLT_mat_corr_first_PRP)

pred_transfert(fit_P_SLT_mat_best_year_COF1,df_SLT_mat_best_year_COF2)
pred_transfert(fit_P_SLT_mat_best_year_COF2,df_SLT_mat_best_year_COF1)
pred_transfert(fit_P_SLT_mat_best_year_COF1,df_SLT_mat_corr_best_CDH)
pred_transfert(fit_P_mat_corr_best_CDH,df_SLT_mat_best_year_COF1)
pred_transfert(fit_P_SLT_mat_best_year_COF1,df_SLT_mat_corr_best_PRP)
pred_transfert(fit_P_mat_corr_best_PRP,df_SLT_mat_best_year_COF1)

pred_transfert(fit_P_SLT_mat_best_year_COF2,df_SLT_mat_corr_best_CDH)
pred_transfert(fit_P_mat_corr_best_CDH,df_SLT_mat_best_year_COF2)
pred_transfert(fit_P_SLT_mat_best_year_COF2,df_SLT_mat_corr_best_PRP)
pred_transfert(fit_P_mat_corr_best_PRP,df_SLT_mat_best_year_COF2)

pred_transfert(fit_P_mat_corr_best_PRP,df_SLT_mat_corr_best_CDH)
pred_transfert(fit_P_mat_corr_best_CDH,df_SLT_mat_corr_best_PRP)

res_pred_transfert <- res_pred_transfert %>%
  mutate(combinaison = paste(modele, donnees, sep = "_"),
         combinaison = factor(.data[["combinaison"]],levels = unique(.data[["combinaison"]]))
         )

ggplot(res_pred_transfert, aes(x = combinaison, y = nrmse),fill="skyblue") +
  geom_col(position = "dodge", color = "black",fill="skyblue") +
  geom_text(aes(label = round(nrmse, 1)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3,fontface="bold") +
  labs(title = "",
       x = "Modèle_Données",
       y = "NRMSE (%)") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )  

ggplot(res_pred_transfert, aes(x = combinaison, y = ecart_to_scada)) +
  geom_col(position = "dodge", color = "black",fill="skyblue") +
  geom_text(aes(label = round(ecart_to_scada, 1)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3,fontface="bold") +
  labs(title = "",
       x = "Modèle_Donnée",
       y = "Écart (%)") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )

saveRDS(file = glue("./5-Résultats/res_pred_transfert_rea.rds"),res_pred_transfert)

res_pred_transfert <- res_pred_transfert%>%
  slice(-5)

res_pred_transfert <- res_pred_transfert %>%
  mutate(combinaison = paste(modele, donnees, sep = "_"))

ggplot(res_pred_transfert, aes(x = combinaison, y = rmse, fill = type)) +
  geom_col(position = "dodge", color = "black") +
  geom_text(aes(label = round(rmse, 2)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3) +
  labs(title = "RMSE par combinaison modèle_donnee",
       x = "Modèle_Donnée",
       y = "RMSE") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )

ggplot(res_pred_transfert, aes(x = combinaison, y = ecart_to_scada, fill = type)) +
  geom_col(position = "dodge", color = "black") +
  geom_text(aes(label = round(ecart_to_scada, 1)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3) +
  labs(title = "Écart à SCADA par combinaison modèle_donnee",
       x = "Modèle_Donnée",
       y = "Écart relatif à SCADA") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )
