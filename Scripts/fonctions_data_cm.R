###########################################
# Boxplot des différentes variables
##########################################
#' Plot boxplots of variables sharing a common prefix
#' 
#' This function loads a dataframe for a specified wind turbine and plots boxplots for all variables
#' whose names start with the same prefix. This is useful to visually compare similar measurements
#' (e.g., different estimates of power, wind speed, or turbulence intensity).
#' 
#' @param name (character) Name of the wind turbine.
#' @param var (character) Common prefix of the variables you want to compare (e.g., "P", "ws", "ti").
#' 
#' @return A ggplot2 object showing the boxplots of selected variables.
#' 
#' @examples
#' \dontrun{
#' boxplot_WT(name = "EDS", var = "P_rea")
#' }
#' 
#' @export
boxplot_WT <- function(name = name, var = "P") {
  df <- readRDS(glue("./2-Inputs/{name}/df_{name}_cleaned_merge.rds"))%>%
    select(-TimeStamp)
  
  if (! any(startsWith(names(df), var))) {
    stop(glue("❌ `var` doit être le début d'une de ces variables : {toString(names(df))}"))
  }
  
  df_long <- df %>%
    pivot_longer(cols = starts_with(var), names_to = "Variable", values_to = "Valeur")

  titre <- ifelse(var == "P", "puissances", ifelse(var == "ws", "vitesses de vent","intensités de tubulence"))

  ggplot(data = df_long) +
    geom_boxplot(aes(x = Variable, y = Valeur, fill = Variable)) +
    labs( x = "Variables",
      y = ifelse(var == "P", "Puissance (kWh)", ifelse(var=="ws","Vitesse du vent (m/s)","Intensité de turbulence")),
      title = glue("Boxplot des différentes {titre} pour l'éolienne {name}"),
      
    ) +
    theme_light()+
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
}

######################################
# Histogramme des variables
#####################################
#' Plot overlapping histograms (frequency polygons) for selected variables
#'
#' This function reads the cleaned dataset for a given wind turbine and plots density curves
#' (as frequency polygons) for the specified variables, allowing comparison of their distributions.
#'
#' @param name (character) Name of the wind turbine.
#' @param vars (character vector) Names of the variables to include in the histogram comparison.
#'
#' @return A ggplot2 object showing frequency polygons of selected variables.
#'
#' @examples
#' \dontrun{
#' histo_WT(name = "EDS", vars = c("P_scada", "P_SLT_SLT"))
#' }
#'
#' @export
histo_WT <- function(name = name, vars = c("P_scada","P_SLT_SLT")) {
  
  df <- readRDS(glue("./2-Inputs/{name}/df_{name}_cleaned_merge.rds"))%>%
    select(-TimeStamp)
  
  if (!all(vars %in% names(df))){
    stop(glue("vars doit contenir une de ces variables : {toString(names(df))}"))
  }

  # Passage au format long
  df_long <- df %>%
    dplyr::select(all_of(vars))%>%
    tidyr::pivot_longer(cols = everything(), names_to = "Variable", values_to = "Valeur")

  ggplot(df_long) +
    geom_freqpoly(
      aes(x = Valeur, y = after_stat(density), color = Variable),
      bins = 50, linewidth = 1.2, alpha = 0.6
    ) +
    labs(
      title = glue("Densités pour l'éolienne {name}"),
      color = "Variable"
    ) +
    theme_minimal()
}

#######################################################
# Comparaison des distributions des variables pour les différentes années
#################################
#' Plot yearly histograms with mean annotations for a variable
#'
#' This function loads cleaned data for a wind turbine and plots histograms by year
#' for a selected variable. It adds vertical dashed lines and annotations showing the
#' yearly means.
#'
#' @param name (character) Name of the wind turbine.
#' @param var (character) Name of the variable to visualize (default is `"P_scada"`).
#'
#' @return A faceted ggplot2 object showing histograms of the variable for each year.
#'
#' @examples
#' \dontrun{
#' histo_year_WT(name = "EDS", var = "P_scada")
#' }
#'
#' @export
histo_year_WT <- function(name, var = "P_scada") {
  df <- readRDS(glue("./2-Inputs/{name}/df_{name}_cleaned_merge.rds"))
  
  if (! var %in% names(df)) {
    stop(glue("❌ `var` doit être une de ces variables : {toString(setdiff(names(df), 'TimeStamp'))}"))
  }
  
  df_long <- df %>%
    mutate(Year = as.factor(lubridate::year(TimeStamp))) %>%
    dplyr::select(Year, all_of(var))
  
  # Moyennes par année
  moyennes_par_an <- df_long %>%
    group_by(Year) %>%
    summarize(moy = mean(.data[[var]], na.rm = TRUE), .groups = "drop")
  
  # Densité max pour annotation
  dens_max <- df_long %>%
    group_by(Year) %>%
    summarise(max_density = max(density(.data[[var]], na.rm = TRUE)$y, na.rm = TRUE), .groups = "drop")
  
  annotations <- left_join(moyennes_par_an, dens_max, by = "Year") %>%
    mutate(y_text = 0.95 * max_density)
  
  xlim_max <- ifelse(str_starts(var, "ti"), 0.3, 1.1 * max(df[[var]], na.rm = TRUE))
  
  ggplot(df_long, aes(x = .data[[var]], y = after_stat(density), fill = Year)) +
    geom_histogram(color = "black", bins = 30, alpha = 0.7) +
    geom_vline(data = annotations, aes(xintercept = moy), linetype = "dashed") +
    geom_text(
      data = annotations,
      aes(x = moy, y = y_text, label = round(moy, 2)),
      inherit.aes = FALSE, color = "black", size = 3, angle = 0, hjust = -0.5
    ) +
    facet_wrap(~Year, nrow = 1) +
    theme_minimal() +
    xlim(0, xlim_max) +
    labs(
      title = glue("Distributions de {var} pour l'éolienne {name}"),
      x = glue("{var}"),
      y = "Fréquence"
    ) +
    theme(legend.position = "none")
}

##############################
# Variables synthétiques : on ramène ws_SLT vers ws_scada
################################
var_synth <- function(name) {
  df_temp <- import_WT(name) %>%
    dplyr::select(TimeStamp, starts_with(c("w", "P_SLT")))
  idx <- str_starts(colnames(df_temp), "P")
  variables <- colnames(df_temp)[idx]

  for (j in seq_along(variables)) {
    df_temp[[variables[j]]] <- df_temp[[variables[j]]] * mean(df_temp$ws_scada, na.rm = T) / mean(df_temp$ws_SLT, na.rm = T)
  }

  df_temp <- df_temp %>%
    dplyr::select(TimeStamp, starts_with("P"))
  assign(glue("df_{name}_synth"), df_temp, envir = .GlobalEnv)
}


################################################## "
# df des biais normalisés en % par rapport à var_scada
##################################################
#' Compute relative mean errors between SCADA data and modelled variables
#'
#' This function computes the relative mean difference (in %) between a reference SCADA variable
#' (e.g. `"P_scada"` or `"ws_scada"`) and one or more modeled variables across multiple wind turbines (WT).
#'
#' @param var_scada (character) Name of the reference SCADA variable (`"P_scada"` or `"ws_scada"`).
#' @param names (character vector) Names of the wind turbines (WT) to process.
#' @param vars_to_compare (character vector) Variables to compare against `var_scada`.
#' @param type (character) `"normal"` for standard processing, or `"synthetic"` to compare synthetic variables to SCADA.
#'
#' @return A data frame with one row per WT and columns giving the relative mean error (%) for each variable.
#'
#' @examples
#' \dontrun{
#' res_error(var_scada = "P_scada", names = c("EDS", "ABH"), vars_to_compare = c("P_rea", "P_wrf"),metric="nrmse")
#' }
#'
#' @export
res_error <- function(var_scada = "P_scada",names, vars_to_compare, metric = "biais") {
  df <- readRDS(glue("./2-Inputs/ABH/df_ABH_cleaned_merge.RDS"))
  
  if (var_scada == "P_scada") {
    vars_ref <- setdiff(names(df)[startsWith(names(df), "P")], "P_scada")
    if (!all(vars_to_compare %in% vars_ref)) {
      warning(glue("❌ Il faut comparer des puissances. Choisir pour `vars_to_compare` parmi : 
                {toString(vars_ref)}"))
    }
  } else if (var_scada == "ws_scada") {
    vars_ref <- setdiff(names(df)[startsWith(names(df), "ws")], "ws_scada")
    if (!all(vars_to_compare %in% vars_ref)) {
      warning(glue("❌ Il faut comparer des vitesses. Choisir pour `vars_to_compare` parmi : 
                {toString(vars_ref)}"))
    }
  }
    
    df_res <- data.frame(parc = names)
  
  for (i in seq_along(names)) {
    parc <- names[i]
    print(parc)
    
    # Charger les données
    df <- readRDS(glue("./2-Inputs/{parc}/df_{parc}_cleaned_merge.RDS"))
    
    if (var_scada == "ws_scada" && parc == "CDH") {
      df <- df %>% filter(year(TimeStamp) < 2022)
    }
    
    if (var_scada == "ws_scada" 
        && "ws_mat_corrige" %in% vars_to_compare
        && !("ws_mat_corrige" %in% names(df))){
      df <- df %>%
        mutate(ws_mat_corrige = ws_mat)
    }
    
    if (var_scada == "P_scada" 
        && "P_SLT_mat_corr_first" %in% vars_to_compare
        && !("P_SLT_mat_corr_first" %in% names(df))
        ){
      df <- df %>%
        mutate(P_SLT_mat_corr_first = P_SLT_mat_first)
    }
    
    # Comparaison pour chaque variable spécifiée
    for (var_model in vars_to_compare) {
      if (metric == "biais") {
        # Vérifier que les colonnes existent
        if (all(c(var_model, var_scada) %in% colnames(df))) {
          delta <- mean(df[[var_model]] - df[[var_scada]], na.rm = TRUE)
          ratio <- mean(df[[var_scada]], na.rm = TRUE)
          df_res[[var_model]][i] <- delta / ratio * 100
        } else {
          warning(glue("Variable manquante dans {parc}: {var_model} ou {var_scada}"))
          df_res[[var_model]][i] <- NA
        }
      } else if (metric == "nrmse") {
        # Comparaison à la moyenne P_scada
        if (var_model %in% colnames(df)) {
          df_res[[var_model]][i] <- round(sqrt(mean((df[[var_model]]-df[[var_scada]])^2,na.rm=T))/mean(df[[var_scada]],na.rm=T)*100,2)
        } else {
          warning(glue("Variable manquante dans {parc}: {var_model} ou {var_scada}"))
          df_res[[var_model]][i] <- NA
        }
        
      }
      else {
        stop("metric doit être 'biais' ou 'nrmse'")
      }
    }
  }
  
  return(df_res)
}

################################################## "
# barplot des ecart en % par rapport à P_scada
##################################################
#' Plot relative mean errors between SCADA and modeled variables
#'
#' This function visualizes the relative mean errors (in %) between a reference SCADA variable
#' (e.g. `"P_scada"` or `"ws_scada"`) and modeled variables across multiple wind turbines (WT).
#'
#' @param var_scada (character) Name of the reference SCADA variable (`"P_scada"` or `"ws_scada"`).
#' @param names (character vector) Names of the wind turbines (WT) to process.
#' @param vars_to_compare (character vector) Variables to compare against `var_scada`.
#' @param type (character) `"normal"` (default) or `"synthetic"` depending on the dataset to use.
#'
#' @return A ggplot object showing bar plots of the relative mean errors.
#'
#' @examples
#' \dontrun{
#' plot_error(var_scada = "P_scada", names = c("EDS", "ABH"),
#'                 vars_to_compare = c("P_rea", "P_wrf"),metric="biais")
#' }
#'
#' @export
plot_error <- function(var_scada = "P_scada",names, vars_to_compare, metric = "biais") {
  # Appel à la fonction précédente pour construire df_res
  df_res <- res_error(
    var_scada ,names, vars_to_compare, metric = metric
  )
  
  # Format long pour ggplot
  df_long <- df_res %>%
    pivot_longer(cols = -parc, names_to = "variable", values_to = "valeur")
  
  if (metric == "biais"){
    ylabel = "Ecart moyen (%)"
  } else if (metric == "nrmse"){
    ylabel = "NRMSE (%)"
  }
  
  # Plot avec valeurs au-dessus des barres
  ggplot(df_long, aes(x = parc, y = valeur, fill = variable)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9),color="black") +
    geom_text(aes(label = sprintf("%.1f%%", valeur)),
              position = position_dodge(width = 0.9),
              vjust = -0.5,
              size = 2,
              fontface="bold") +
    labs(y = ylabel, x = "Parc", fill = "Variable") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}



#########################################
# Biais annuel ou mensuel des différentes prédictions avec les scada
############################################
#' Visualise le biais moyen relatif par rapport aux données SCADA
#'
#' Cette fonction calcule et affiche l'écart moyen (en pourcentage) entre des variables
#' de puissance ou de vitesse modélisées et la référence SCADA (`P_scada` ou `ws_scada_norm`),
#' soit par mois, soit par année, pour un parc éolien donné.
#'
#' @param name (character) Nom du parc éolien (ex. `"CDH"`).
#' @param var (character vector) Variables à comparer (ex. `"P_SLT_SLT"`, `"P_SLT_CMV"`).
#'               Doivent toutes commencer par `"P_"` ou `"ws"` et être du même type.
#' @param type (character) Type d’agrégation temporelle : `"month"` ou `"year"`.
#' @param annee (numeric) Année à filtrer si `type = "month"` (par défaut `2021`).
#'
#' @return Un graphique ggplot des biais moyens relatifs, avec indication de la
#' proportion d’observations par mois ou par année.
#'
#' @examples
#' \dontrun{
#' biais_to_scada(name = "CDH",
#'                var = c("P_SLT_SLT", "P_SLT_CMV"),
#'                type = "month",
#'                annee = 2021)
#' }
#'
#' @export
biais_to_scada <- function(name = "CDH", var = c("P_SLT_SLT", "P_SLT_CMV"), type = "month", annee = 2021) {
  df <- readRDS(glue("./2-Inputs/{name}/df_{name}_cleaned_merge.RDS"))
  
  # Vérifier que toutes les variables commencent par le même préfixe
  prefixes <- unique(substr(var, 1, 2))
  if (length(prefixes) != 1 || !(prefixes %in% c("P_", "ws"))) {
    stop("❌ Toutes les variables doivent commencer par 'P' ou 'ws' et être homogènes.")
  }
  
  # Déduction de la variable scada de référence
  var_scada <- if (startsWith(var[1], "P")) "P_scada" else "ws_scada_norm"
  
  # Vérification de la présence des variables dans le data.frame
  vars_ref <- setdiff(names(df)[startsWith(names(df), substr(var_scada, 1, 2))], var_scada)
  if (!all(var %in% vars_ref)) {
    stop(glue("❌ Les variables doivent être parmi : {toString(vars_ref)}"))
  }
  
  # Préparation
  df_base <- df %>%
    mutate(
      year = year(TimeStamp),
      month = month(TimeStamp),
      mean_scada = mean(.data[[var_scada]], na.rm = TRUE),
      
    )
  
  if (var_scada == "ws_scada_norm" & name == "CDH"){
    df_base <- df_base %>%
      filter(year <= 2021) %>%
      mutate(mean_scada = mean(.data[[var_scada]], na.rm = TRUE)
             )
  }
  
  # Calculs pour chaque variable
  df_plot <- map_dfr(var, function(v) {
    df_temp <- df_base %>%
      mutate(
        pourcent = (.data[[v]] - .data[[var_scada]]) / mean_scada * 100,
        variable = v
      )
    
    if (type == "month") {
      count_per_month <- df_temp %>%
        filter(year == annee) %>%
        group_by(month) %>%
        summarise(
          count = n(),
          .groups = "drop"
        ) %>%
        mutate(prop = round(count / sum(count) * 100, 1))
      
      # Puis on calcule le biais moyen par variable
      df_temp %>%
        filter(year == annee) %>%
        group_by(month, variable) %>%
        summarise(
          mean_pourcent = mean(pourcent, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        left_join(count_per_month, by = "month")  # On ajoute la proportion
    } else if (type == "year") {
      df_temp %>%
        group_by(year, variable) %>%
        summarise(
          count = n(),
          mean_pourcent = mean(pourcent, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(prop = round(count / sum(count) * 100, 1))
    } else {
      stop("❌ Le type doit être 'month' ou 'year'.")
    }
  })
  
  # Plot
  if (type == "month") {
    # On calcule y_max par mois (ou on peut prendre max global si besoin)
    df_plot <- df_plot %>%
      mutate(y_max = max(mean_pourcent, na.rm = TRUE) * 1.3) %>%
      ungroup()
    
    ggplot(df_plot, aes(x = factor(month, levels = 1:12,
                                   labels = c("Janv", "Févr", "Mars", "Avril", "Mai", "Juin",
                                              "Juil", "Août", "Sept", "Oct", "Nov", "Déc")),
                        y = mean_pourcent, fill = variable)) +
      geom_col(position = "dodge", color = "black") +
      geom_text(aes(label = round(mean_pourcent, 1)), 
                position = position_dodge(width = 0.9), 
                vjust = -0.5, size = 3) +
      geom_text(aes(y = y_max, label = glue("{prop}%")),
                position = position_dodge(width = 0.9),
                size = 3, color = "gray30") +
      labs(
        title = glue("Écart mensuel moyen avec {var_scada} pour {name} en {annee}"),
        x = "Mois", y = "Écart (%)", fill = "Variable",
        subtitle = "Proportion d’observations par mois"
      ) +
      theme_minimal()
  } else {
    df_plot <- df_plot %>%
      mutate(y_max = max(mean_pourcent, na.rm = TRUE) * 1.3) %>%
      ungroup()
    
    ggplot(df_plot, aes(x = factor(year), y = mean_pourcent, fill = variable)) +
      geom_col(position = "dodge", color = "black") +
      geom_text(aes(label = round(mean_pourcent, 1)), 
                position = position_dodge(width = 0.9), 
                vjust = -0.5, size = 3) +
      geom_text(aes(y = y_max, label = glue("{prop}%")),
                position = position_dodge(width = 0.9),
                size = 3, color = "gray30") +
      labs(
        title = glue("Écart annuel moyen avec {var_scada} pour {name}"),
        x = "Année", y = "Écart (%)", fill = "Variable",
        subtitle = "Proportion d’observations par année"
      ) +
      theme_minimal()
  }
}

#Stats usuelles sur df_cleaned_merge
stats_res <- function(var="P_scada",name = name){
  df_cm <- readRDS(glue("./2-Inputs/{name}/df_{name}_cleaned_merge.rds"))
  data.frame( min = min(df_cm[[var]],na.rm=T),
              moy = mean(df_cm[[var]],na.rm=T),
              med = median(df_cm[[var]],na.rm=T),
              max = max(df_cm[[var]],na.rm=T),
              sd = sd(df_cm[[var]],na.rm=T)
  )
}
