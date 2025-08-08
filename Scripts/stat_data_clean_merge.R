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

source(glue("{path_result}/AEP_Ref/fonctions_data_cm.R"))

###########################################
# Boxplot des différentes variables : ws, P ou ti
##########################################
boxplot_WT(name= "ABH", var ="ti")


######################################
# Histogramme des variables
#####################################
histo_WT("PRP", vars=c("P_SLT_SLT","P_SLT_CMV","P_scada"))

#######################################################
# Comparaison des distributions des variables pour les différentes années
#################################
histo_year_WT(name = "PRP", var = "P_scada")
histo_year_WT(name = "ABH", var = "ws_scada_norm")


##########################
#Comparaison des distributions des TI par éolienne : lidar, rea et SLT_lidar
###############################"
distrib_ti_name("CDH")

#################################
#Comparaison par parc des distributions d'un type de TI
##################################
distrib_ti_type(type = "ti_SLT", names = c("ABH", "AEB", "EDS"))
distrib_ti_type(type = "ti_SLT", names = c("ABH", "AEB", "EDS", "CDH", "CDS"))
distrib_ti_type(type = "ti_SLT", names = c("COF1", "COF2", "PRP"))

######################################
# plot Erreur= f(TI)
#####################################
plot_err_ti(name = "AEB", var = "P_scada")


##############################
# df résumé des df_name_cm
###########################
name <- "CDH"
df_temp <- readRDS(glue("{path_input}/{name}/df_{name}_cleaned_merge.RDS"))
variables <- sort(colnames(df_temp)[-1], decreasing = T)
name_mean <- c()
name_sd <- c()
for (i in seq_along(variables)) {
  name_mean <- c(name_mean, glue("{variables[i]}_mean"))
  name_sd <- c(name_sd, glue("{variables[i]}_sd"))
}

# Création d'un df vide avec parc, date_min, date_max
df_mean <- data.frame(
  parc = names,
  date_min = as.Date(NA),
  date_max = as.Date(NA),
  stringsAsFactors = FALSE
)

df_sd <- data.frame(
  parc = names,
  date_min = as.Date(NA),
  date_max = as.Date(NA),
  stringsAsFactors = FALSE
)

for (i in seq_along(names)) {
  print(names[i])
  name <- names[i]
  df_temp <- readRDS(glue("{path_input}/{name}/df_{name}_cleaned_merge.RDS"))
  df_mean[["date_min"]][i] <- min(df_temp$TimeStamp)
  df_mean[["date_max"]][i] <- max(df_temp$TimeStamp)
  df_sd[["date_min"]][i] <- min(df_temp$TimeStamp)
  df_sd[["date_max"]][i] <- max(df_temp$TimeStamp)
  for (j in seq_along(variables)) {
    df_mean[[variables[j]]][i] <- round(mean(df_temp[[variables[j]]], na.rm = T), 3)
    df_sd[[variables[j]]][i] <- round(sd(df_temp[[variables[j]]], na.rm = T), 3)
  }
}


flextable(df_mean)

saveRDS(object=df_mean,glue("{path_result}/MCP_CP/df_mean.rds"))
saveRDS(object=df_sd,glue("{path_result}/MCP_CP/df_sd.rds"))

plot_resum <- function(var = "P", df = df_mean) {
  df_long <- df %>%
    dplyr::select(parc, starts_with(var)) %>%
    pivot_longer(cols = -parc, names_to = "Variable", values_to = "Valeur")

  ggplot(df_long, aes(x = parc, y = Valeur, fill = Variable)) +
    geom_bar(stat = "identity", position = "dodge") +
    theme_minimal() +
    labs(title = glue("Valeurs moyennes de {var} sur la durée d'exploitation scada"), y = var)
}

plot_resum("P_SLT_SLT")
plot_resum("ti")
plot_resum("ws")


##################################################
# barplot des ecart en % par rapport à scada
#################################################
plot_mean_error(var_scada = "P_scada", names=names,vars_to_compare = c("P_SLT_mat_TI"))

nrmse_vs_scada(var_scada = "ws_scada_norm", names=names,vars_to_compare = c("ws_SLT","ws_mat"))

plot_nrmse(var_scada = "ws_scada_norm", names=names,vars_to_compare = c("ws_mat","ws_m"))
plot_nrmse(var_scada = "P_scada", names=names,vars_to_compare = c("P_SLT_mat_best_year","P_SLT_CMV","P_SLT_best_year"))\_##############################


########################
#Comparaison annuelle ou mensuelle avec les valeurs Scada
#########################
biais_to_scada(name = "CDS", var = c("P_SLT_mat_best_year", "P_SLT_best_year","P_SLT_CMV"), type = "year")

##################################################
# Variables synthétiques : on ramène ws_SLT vers ws_scada
################################
var_synth("ABH")

name <- "ABH"
variables <- sort(colnames(import_WT(name))[-1], decreasing = T)
name_mean <- c()
for (i in seq_along(variables)) {
  name_mean <- c(name_mean, glue("{variables[i]}_mean"))
}

df_mean_synth <- data.frame(
  parc = names,
  date_min = as.Date(NA),
  date_max = as.Date(NA),
  stringsAsFactors = FALSE
)

for (i in seq_along(names)) {
  df_temp <- import_WT(names[i])
  df_mean_synth[["date_min"]][i] <- min(df_temp$TimeStamp)
  df_mean_synth[["date_max"]][i] <- max(df_temp$TimeStamp)
  for (j in seq_along(variables)) {
    df_mean_synth[[name_mean[j]]][i] <- round(mean(df_temp[[variables[j]]], na.rm = T), 3)
  }
}

plot_resum(var = "P", df = df_mean_synth)


######################
# Erreur après recalage de ws_scada sur ws_SLT
######################
ggplot(df_CDH_cleaned_merge, aes(x = ws_SLT, y = ws_scada_norm)) +
  geom_point() +
  geom_smooth(method = lm)
lm(df_CDH_cleaned_merge$ws_scada_norm ~ df_CDH_cleaned_merge$ws_SLT, df_CDH_cleaned_merge)
ecart <- -mean((df_CDH_cleaned_merge$P_scada - df_CDH_cleaned_merge$P_SLT_SLT_modif), na.rm = T) / mean(df_CDH_cleaned_merge$P_scada, na.rm = T) * 100

ggplot(df_CDH_cm%>%mutate(year=year(TimeStamp)),aes(x=ws_scada,y=P_scada,color=as.factor(year)))+
  geom_point()+
  labs(x="Vitesse de vent (m/s)",
       y="Puissance (kW)",
       )+
  scale_color_discrete(name = "Année")+
  theme_minimal()



