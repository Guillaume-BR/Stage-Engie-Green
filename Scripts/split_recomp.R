rm(list=ls())

# tri des données
library(tidyverse)
library(glue)

names <- c("ABH", "AEB", "CDH", "CDS", "COF1", "COF2", "EDS", "PRP")


df_carac <- readRDS("./data/df_carac.RDS")

#split les df_scada soit en récupérant autant de df que d'année, soit un seul sur une éolienne en particulier
for (name in names){
  split_df(name)
}

#si on veut une éolienne en particulier : exemple
split_df("AEB", "WIT_FRAEB_SS001_WT005")


#rbind les df_name_year par année
#calcule le pourcentage de pertes de données entre le brut et le cleaned
for (name in names){
  recomp_clean(name)
  diff_data(name)
}


diff_data_scada_ABH %>%
  tibble::rownames_to_column() %>%
  flextable()

diff_data_scada_CDH %>%
  flextable() %>%
  colformat_num(big.mark = " ")

###################################"
#Création des fichiers cleaned_merge contenant ws_scada,P_scada
#et les P modélisés avec la fonction merge_save : d'abord vérifier que le fichier
######################################

for (name in names){
  print(name)
  merge_save(name)
}

merge_save("PRP")

###########################################################
#Normalisation de la vitesse scada dans le fichier df_name_cleaned_merge.rds
#stop si déjà présent
#############################################################
for (name in names){
  print(name)
  ws_norm_scada(name)
}


############################################
# Sauvegarde des deux fichiers nettoyés de Paul CDS et CDH
###############################################
df_CDS_cleaned <- readRDS(glue("./data/CDS/data_long_WS_cleaned_completed_CDS.rds")) %>%
  filter(wtg_status == "normal", WIT_ENT_CODE == "WIT_FRCDS_SS002_WT005") %>%
  drop_na()

saveRDS(df_CDS_cleaned, glue("./data/CDS/df_CDS_cleaned.RDS"))

df_CDH_cleaned <- readRDS(glue("./data/CDH/data_long_WS_cleaned_completed_CDH.rds")) %>%
  filter(wtg_status == "normal", WIT_ENT_CODE == "WIT_FRCDH_SS002_WT002") %>%
  drop_na()

saveRDS(df_CDH_cleaned, glue("./data/CDH/df_CDH_cleaned.RDS"))


