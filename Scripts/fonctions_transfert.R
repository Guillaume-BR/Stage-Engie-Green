###############################
# Extraction des données
###############################
#' Extract Objects from an RData File by Name Prefix
#'
#' This function loads an RData file into a temporary environment
#' and extracts all objects whose names start with any of the specified prefixes.
#'
#' @param file Character. The path to the RData file to load.
#' @param startsWithVec Character vector. One or more prefixes used to match object names.
#'                       Objects with names starting with any of these prefixes will be extracted.
#'
#' @return A named list containing all matched objects loaded from the file.
#'         The list names correspond to the original object names.
#'
#' @details
#' The function avoids loading all objects directly into the global environment
#' by using a new environment for loading. It then filters objects based on
#' whether their names start with any of the provided prefixes.
#'
#' @examples
#' \dontrun{
#' objs <- extractRData("data/myfile.RData", c("df_", "model_"))
#' }
#'
#' @export
extractRData <- function(file, startsWithVec) {
  E <- new.env()
  load(file = file, envir = E)

  matchedObjects <- ls(E)[sapply(ls(E), function(obj) any(startsWith(obj, startsWithVec)))]

  # Crée une liste nommée avec les objets extraits
  result_list <- lapply(matchedObjects, function(obj) get(obj, envir = E))
  names(result_list) <- matchedObjects

  return(result_list)
}

#######################################
# Import des modèles et données
########################################

#' Load and Assign Multiple Model and Data Objects from RData Files
#'
#' This function iterates over a vector of names, loads RData files from
#' corresponding subdirectories, extracts specific objects from each file,
#' and assigns them to the global environment with names appended by the folder name.
#'
#' @param names Character vector. Names of folders/data sets to load.
#'              Each folder is expected to contain a file named `data_<name>.Rdata` in `./data/<name>/`.
#'
#' @details
#' For each `name` in `names`, the function constructs the file path
#' `./data/{name}/data_{name}.Rdata` and uses the `extractRData` function
#' to extract objects whose names start with any of the following prefixes:
#' `"fit_P_SLT"`, `"pred_P_SLT"`, `"df_cm"`, `"df_SLT"`, `"fit_P_rea"`, `"fit_P_mat"`, and `"pred_P_mat"`.
#' Each extracted object is then assigned in the global environment with a name
#' formed by concatenating the original object name and the dataset name separated by an underscore.
#'
#' @examples
#' \dontrun{
#' model_and_data(c("CDS", "CDH"))
#' }
#'
#' @export
model_and_data <- function(names = names) {
  for (name in names) {
    file <- glue("./5-Résultats/{name}/data_{name}.Rdata")
    list <- extractRData(file, startsWithVec = c("fit_P", "pred_P", "df_cm", "df_SLT"))
    for (nm in names(list)) {
      assign(glue("{nm}_{name}"), list[[nm]], envir = .GlobalEnv)
    }
  }
}

###########################################################################
# Prédiction entre modèle de l'éolienne 1 et données SLT de l'éolienne 2
# Calcul RMSE et ecart avec P_scada_name1
#############################################################################
#' Transfer Prediction Between Models and Datasets
#'
#' This function performs a transfer prediction by applying a model trained on data from one site (`name1`)
#' to another site's data (`name2`). It supports different data sources and model types.
#' The function calculates the root mean square error (RMSE) and the average percentage deviation
#' of predicted power from SCADA power of the site 2, then appends the results to a global summary table.
#'
#' @param name1 Character. Name of the site where the model was trained.
#' @param name2 Character. Name of the site on which prediction will be made.
#' @param data Character, default `"rea"`. Indicates which data set to use for prediction.
#'             Can be `"rea"` for real data or others used internally.
#' @param type Character, default `"mat"`. Indicates the type of model/data to use,
#'             e.g., `"mat"` or others.
#'
#' @details
#' Depending on the `data` and `type` parameters and the site names, the function selects
#' the appropriate dataframe and model from the global environment.
#' It predicts power values (`P`) on the selected dataframe using the chosen model.
#' It then merges the predicted values with actual SCADA power data from site `name1`
#' to compute performance metrics: RMSE and mean relative deviation from SCADA power.
#' Finally, it updates a global dataframe `res_pred_transfert` with the results,
#' and assigns the predicted values vector to a new global variable with a descriptive name.
#'
#' @return
#' No return value. Side effects include:
#' - Printing the merged dataframe and RMSE.
#' - Updating `res_pred_transfert` in the global environment.
#' - Creating a global variable `pred_<data>_<type>_<name1>_<name2>` holding predictions.
#'
#' @examples
#' \dontrun{
#' pred_transfert("SiteA", "SiteB", data = "rea", type = "mat")
#' }
#' @export

pred_transfert <- function(fit,df) {
  
  #récupérer le nom des objets utilisés
  name_fit <- deparse(substitute(fit))
  name_df <- deparse(substitute(df))
  
  #on récupère les noms des parcs à partir du nom des objets
  modele <- str_extract(name_fit , pattern = "[^_]+$" )
  print(modele)
  donnees <- str_extract(name_df , pattern = "[^_]+$" )
  print(donnees)
  
   # Prédiction
  if ("ws_mat_corrige" %in% names(fit$trainingData) && donnees %in% c("COF1","COF2")){
    df <- df%>%
      rename(ws_mat_corrige = ws_mat)
  }
  
  if ("ws_mat_corrige" %in% names(df) && modele %in% c("COF1","COF2")){
    df <- df%>%
      rename(ws_mat = ws_mat_corrige)
  }
  
  pred <- predict(fit, df)
  df <- cbind(df, P_pred = pred)

    df_merge <- get(glue("df_cm_{donnees}")) %>%
      select(TimeStamp, P_scada) %>%
      inner_join(df, by = "TimeStamp")

  nrmse <- round(sqrt(mean((df_merge$P_pred - df_merge$P_scada)^2, na.rm = TRUE))/mean(df_merge$P_scada,na.rm=T)*100,1)

  ecart_to_scada <- df_merge %>%
    dplyr::mutate(ecart = (P_pred - mean(P_scada, na.rm = TRUE)) / mean(P_scada, na.rm = TRUE) * 100) %>%
    dplyr::summarise(ecart_moy = mean(ecart, na.rm = TRUE)) %>%
    dplyr::pull(round(ecart_moy, 1))
  
  
  
  
  # Ajouter une ligne au tableau global
  new_row <- data.frame(
    modele = modele,
    donnees = donnees,
    type = str_extract(name_fit, "(?<=_).*(?=_)"),
    nrmse = nrmse,
    ecart_to_scada = ecart_to_scada,
    stringsAsFactors = FALSE
  )

  # MAJ du tableau global
  assign("res_pred_transfert",
         rbind(get("res_pred_transfert", envir = .GlobalEnv), new_row),
         envir = .GlobalEnv)
  
  #suffix <- glue("{name_fit}_{name_df}")
  #assign(glue("pred_{suffix}"),
  #  pred,
  #  envir = .GlobalEnv
  #)
}


###################################
# Graphique de comparaison
###################################
#ne fonctionne plus
graphe_comp <- function(name1, name2, type = "CMV") {
  df <- get(glue("df_SLT_{name2}"))
  suffix <- glue("{name1}_{name2}")

  pred_ref <- get(glue("pred_P_{type}"))
  pred_trans <- get(glue("pred_{type}_{suffix}"))

  df1 <- data.frame(ws = df$ws_hub_norm, pred = pred_ref, source = name1)
  df2 <- data.frame(ws = df$ws_hub_norm, pred = pred_trans, source = name2)
  df_all <- rbind(df1, df2)
  df_all$source <- factor(df_all$source, levels = c(name1, name2))

  colors <- setNames(c("orange3", "darkgreen"), c(name1, name2))
  rmse <- round(get(glue("rmse_{type}_{suffix}")), 2)

  p <- ggplot(df_all, aes(x = ws, y = pred, color = source)) +
    geom_point(size = 1, alpha = 0.8) +
    theme_minimal() +
    xlab("Vitesse du vent (m/s)") +
    ylab("Puissance (kWh)") +
    ggtitle(glue("Prédictions SLT de {name1} et {name2} à partir du modèle {type} de {name1}")) +
    geom_label(
      data = data.frame(x = 15, y = 500, label = glue("RMSE = {rmse} kWh")),
      aes(x = x, y = y, label = label),
      inherit.aes = FALSE
    ) +
    scale_color_manual(name = "Courbes", values = colors)

  assign(glue("plot_{type}_{suffix}"), p, envir = .GlobalEnv)
}
