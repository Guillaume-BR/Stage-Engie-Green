rm(list=ls())

library(glue)
library(tidyverse)
names <- c("ABH", "AEB", "CDH", "CDS", "COF1", "COF2", "EDS", "PRP")

path_input <- "C:/Users/BERNARD-REYMONDGuill/ENGIE/Pôle Ressource et productible - 4-Stage/2025 - Bernard-Reymond/2-Inputs"
path_result <- "C:/Users/BERNARD-REYMONDGuill/ENGIE/Pôle Ressource et productible - 4-Stage/2025 - Bernard-Reymond/5-Résultats"


source(glue("{path_result}/AEP_Ref/fonction AEP_ref.R"))

#Pour obtenir la distribution pour une éolienne, selon l'année considéré et la variable scada ws ou P
histo_year(name="PRP",2017,'P')

#Pour visualiser les variations d'AEP selon le nombre de mésures retenues par heure
var_AEP(name="CDH")

#variation de l'AEP par rapport à la référence : 6 mesures par heure
var_AEP_ref(name="CDH")

#Pour visualiser l'évolution de l'AEP selon le nombre de mesures considérés par heure
AEP_bar(name=name,mesures=4)


#############################################
# Pourcentage d'écart entre les différentes P calculées et le P_scada par année
#############################################
diff_AEP_to_scada("ABH")
flextable(diff_AEP_to_scada"ABH"))
AEP_WT_moy("ABH")

###########################################
# Calcul des AEP
###########################################
AEP_WT_norm("ABH")
AEP_WT_norm("COF1")


########################################
# Pondération selon la distribution pour le calcul des AEP
########################################
AEP_WT_pond("ABH")
flextable(AEP_WT_pond("ABH"))
AEP_WT_pond_moy("AEB")


df_moy_pond <- data.frame()
for (name in names) {
  df_moy_pond <- rbind(df_moy_pond, AEP_WT_pond_moy(name))
}

df_pourcent_pond <- data.frame(parc = names)
for (name in names(df_moy_pond)[-1]) {
  for (i in seq_along(names)) {
    df_pourcent_pond[[name]][i] <- round((df_moy_pond[[name]][i] - df_moy_pond[["AEP_scada"]][i]) / df_moy_pond[["AEP_scada"]][i] * 100, 2)
  }
}

df_pourcent_pond <- df_pourcent_pond%>%
  select(parc,AEP_SLT_Cons,AEP_SLT_SLT,AEP_Rea_Cons,AEP_scada)

df_pourcent_pond_long <- df_pourcent_pond %>%
  pivot_longer(cols = -parc,
               names_to = "AEP",
               values_to = "Valeurs")

ggplot(df_pourcent_pond_long%>%filter(AEP!="AEP_scada"), aes(x = parc, y = Valeurs, fill = AEP)) +
  geom_col(position = "dodge") +
  labs(x = "Parc", y = "Valeur (%)", title = "Pourcentage d'écart par rapport à AEP_scada") +
  scale_y_continuous(
    breaks = seq(
      -20,40,
      by = 5
    )
  ) +
  theme_minimal()




