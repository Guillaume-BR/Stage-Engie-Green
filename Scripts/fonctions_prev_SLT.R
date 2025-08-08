
  #######################################
# Renommer première ligne data_cons
###################################### "
#' Rename Columns of a Power Curve Data Frame
#'
#' This function standardizes column names of a manufacturer power curve data frame (`df_cons`).
#' It is useful when the input data has inconsistent or unnamed columns.
#'
#' @param df_cons A data frame containing power curve data. It must have either 3 or 4 columns:
#'   - If 4 columns: renamed to `X`, `V`, `P`, `ct`
#'   - If 3 columns: renamed to `V`, `P`, `ct`
#'
#' @return A data frame with standardized column names:
#'   - `V`: Wind speed
#'   - `P`: Power
#'   - `ct`: Thrust coefficient
#'   - `X` (optional): an extra column, often used as an index or auxiliary variable
#'
#' @examples
#' \dontrun{
#' df <- data.frame(runif(10), seq(1,10), rnorm(10), rnorm(10))
#' colname_cons(df)
#'
#' df2 <- data.frame(seq(1,10), rnorm(10), rnorm(10))
#' colname_cons(df2)
#' }
#'
#' @export
colname_cons <- function(df_cons) {
  if (length(names(df_cons)) == 4) {
    colnames(df_cons) <- c("X", "V", "P", "ct")
  } else if (length(names(df_cons)) == 3) {
    colnames(df_cons) <- c("V", "P", "ct")
  }
  return(df_cons)
}


##################################
# Moyennage par heure si présence de 6 données et filtrage des données
##################################
#' Compute Hourly Averages from Filtered Data
#'
#' This function computes hourly averages from filtered LIDAR or measurement matrix data,
#' retaining only the hours with exactly 6 measurements.
#'
#' @param name A character string indicating the name of the site (e.g., "ABH", "CDH").
#' @param type A character string indicating the data type to use:
#'   - `"lidar"` (default): use filtered LIDAR data.
#'   - `"mat"`: use filtered measurement matrix data.
#'
#' @return A data frame containing hourly averaged values for all numeric variables.
#'   The time column is named `TimeStamp` and corresponds to the start of each hour.
#'
#' @details
#' The function expects the data to be stored in files named:
#' - `"./2-Inputs/{name}/df_lidar_filtered.rds"` for LIDAR
#' - `"./2-Inputs/{name}/df_mat_filtered.rds"` for measurement matrix
#'
#' Only hours with exactly 6 measurements are included in the average computation.
#'
#' @examples
#' \dontrun{
#' df_hourly_lidar <- moy_filt("ABH", type = "lidar")
#' df_hourly_mat <- moy_filt("CDH", type = "mat")
#' }
#'
#' @export
moy_filt <- function(name,type="lidar") {
  if (type == "lidar"){
    df_ref <- readRDS(glue("./2-Inputs/{name}/df_lidar_filtered.rds"))
  } else if (type == "mat"){
    df_ref <- readRDS(glue("./2-Inputs/{name}/df_mat_filtered.rds"))
  }
  
  df_moy <- df_ref %>%
    mutate(HourGroup = floor_date(TimeStamp, "hour")) %>%
    group_by(HourGroup) %>%
    filter(n() == 6) %>%                       # Conserve seulement les heures avec 6 valeurs
    summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)), .groups = "drop") %>%
    rename(TimeStamp = HourGroup)
  
  return(df_moy)
}

############################################
# fonction de la courbe de puissance
############################################
#' Compute Power Curve from Wind Speed Using Spline Interpolation
#'
#' This function computes a power output vector from a vector of wind speeds
#' using a smoothed spline fitted to a given wind turbine power curve (`wtg`).
#'
#' @param ws A numeric vector of wind speeds for which to compute the predicted power.
#' @param wtg A data frame containing the wind turbine's power curve with columns:
#'   - `V`: wind speed values (in m/s)
#'   - `P`: corresponding power output values (in kW or MW)
#'
#' @return A numeric vector of predicted power values corresponding to the input wind speeds.
#'
#' @details
#' - The function removes rows in `wtg` with `NA` values in either `V` or `P`.
#' - It adds three anchor points `(0, 0)` to ensure the power curve starts at 0.
#' - A smoothed spline is fitted to the cleaned and extended data using `smooth.spline`.
#' - Predicted values are returned even if some `ws` values are `NA` (`na.pass`).
#'
#' @examples
#' \dontrun{
#' wtg_data <- data.frame(V = c(3, 5, 8, 12, 15), P = c(0, 200, 800, 1500, 2000))
#' wind_speeds <- seq(0, 20, by = 0.1)
#' predicted_power <- power_curve(wind_speeds, wtg_data)
#' plot(wind_speeds, predicted_power, type = "l")
#' }
#'
#' @export
power_curve <- function(ws, wtg) {
# Suppression des lignes avec NA dans V ou P
wtg <- wtg %>% filter(!is.na(V), !is.na(P))

# Sécurité : vérifier qu'il reste assez de points pour le spline
if (nrow(wtg) < 4) stop("Pas assez de points valides pour ajuster la courbe puissance.")

mod.spline <- stats::smooth.spline(x = c(0, 1, 2, wtg$V), y = c(0, 0, 0, wtg$P))
return(predict(mod.spline, ws, na.action = na.pass)$y)
}

#################################################
# Fonction pour splitter et entrainer, possibilité d'inclure le "shear" dans les prédicteurs
# A enchaîner avec test_and_result
#################################################

#' Train and Save a Machine Learning Model on Wind Farm Data
#'
#' This function trains a machine learning model (default: random forest via `ranger`)
#' on wind turbine data, using a training/test split, and optionally includes shear.
#'
#' @param variable Character. The name of the target variable to predict (e.g., "P_SLT_Cons").
#' @param df_merge Data frame. The merged dataset containing all features and the target variable.
#' @param df_rea Data frame. The reference dataset used to define the set of features (excluding timestamp).
#' @param method Character. The model to use in `caret::train()` (default is `"ranger"`).
#' @param shear Character. Either `"no"` (exclude shear from training) or any other value (include shear). Default is `"no"`.
#'
#' @details
#' - Sets a fixed random seed (1809) for reproducibility.
#' - Drops `temp` and `shear` from features if `shear = "no"`, only `temp` otherwise.
#' - Uses 80% of data for training, 20% for testing (random split).
#' - Applies 5-fold cross-validation repeated 10 times.
#' - Trains the model using the formula: `variable ~ .` (dynamically built).
#' - The model is stored in the global environment under the name:
#'   - `"fit_<variable>"` if `shear = "no"`
#'   - `"fit_<variable>_shear"` otherwise.
#' - Training and testing sets are also saved to the global environment as `train` and `test`.
#'
#' @return None. Saves objects to the global environment.
#'
#' @import caret
#' @importFrom dplyr select
#' @importFrom glue glue
#'
#' @examples
#' \dontrun{
#' train_and_fit("P_SLT_Cons", df_merge, df_rea, method = "ranger", shear = "no")
#' print(fit_P_SLT_Cons)
#' }
#'
#' @export
train_and_fit <- function(variable, df_merge, df_rea,method = "ranger", shear = "no") {
  set.seed(1809) # Fixe la graine pour reproductibilité
  if (shear=="no"){
    df_rea <- df_rea %>%
      dplyr::select(-temp,-shear)
  }
  else {
    df_rea <- df_rea %>%
      dplyr::select(-temp)
  }
  
  # Séparation des données
  index <- sample(seq_len(nrow(df_merge)), size = 0.8 * nrow(df_merge))
  train <- df_merge[index, ] %>%
    dplyr::select(setdiff(names(df_rea), "TimeStamp"), variable)
  test <- df_merge[-index, ] %>%
    dplyr::select(setdiff(names(df_rea), "TimeStamp"), variable)
  
  # fitControl
  fitControl <- trainControl(
    method = "repeatedcv",
    number = 5,
    repeats = 10
  )
  
  # Entraînement du modèle
  fit <- train(
    reformulate(".", response = variable), # Formule dynamique
    data = train,
    method = method,
    trControl = fitControl,
    verbose = FALSE,
    importance = "impurity"
  )
  
  # Sauvegarde du modèle avec dynamite nom dynamique
  assign(ifelse( shear == "no",glue("fit_{variable}"),glue("fit_{variable}_shear")), fit, envir = .GlobalEnv)
  assign("train", train, envir = .GlobalEnv)
  assign("test", test, envir = .GlobalEnv)
}

#####################################################################
# fonction pour tester et prédire les résultat à partir de train_and_fit
###################################################################
#' Evaluate a Trained Model on the Test Set and Save the Results
#'
#' This function evaluates a previously trained machine learning model on the test set and computes performance metrics.
#'
#' @param variable Character. The name of the target variable used during training.
#' @param shear Character. `"no"` if the model without shear was used, or any other value for the model including shear. Default is `"no"`.
#'
#' @details
#' - Retrieves the trained model `fit_<variable>` or `fit_<variable>_shear` from the global environment.
#' - Predicts on the global test set `test`.
#' - Calculates the following performance metrics:
#'   - **Bias**: Mean error (prediction - observed)
#'   - **RMSE**: Root Mean Squared Error
#'   - **NRMSE**: Normalized RMSE in percentage
#'   - **MAE**: Mean Absolute Error
#'   - **NMAE**: Normalized MAE in percentage
#'   - **R²**: Coefficient of determination
#' - Saves:
#'   - Predictions as `pred_<variable>` or `pred_<variable>_shear` in the global environment
#'   - Results as a data frame `resultat_<variable>` or `resultat_<variable>_shear`
#'
#' @return None. Saves prediction and metrics to the global environment.
#'
#' @importFrom glue glue
#'
#' @examples
#' \dontrun{
#' test_and_result("P_SLT_Cons")
#' print(resultat_P_SLT_Cons)
#' }
#'
#' @export

test_and_result <- function(variable,shear="no") {
  if (shear == "no") {
    fit <- get(glue("fit_{variable}"))
  } else {
    fit <- get(glue("fit_{variable}_shear"))
  }
  
  pred <- predict(fit, newdata = test)
  biais <- round(mean(pred - test[[variable]]), 4)
  rmse <- round(sqrt(mean((pred - test[[variable]])^2)), 4)
  mae <- round(mean(abs(pred - test[[variable]])), 4)
  
  mu <- round(mean(test[[variable]]), 4)
  nrmse <- round(rmse / mu * 100, 2)
  nmae <- round(mae / mu * 100, 4)
  R2 <- round(1 - sum((pred - test[[variable]])^2) / sum((test[[variable]] - mu)^2), 4)
  resultat <- data.frame(biais = biais, rmse = rmse, nrmse = nrmse, mae = mae, nmae = nmae, R2 = R2)
  
  assign(ifelse(shear=="no",glue("pred_{variable}"),glue("pred_{variable}_shear")), pred, envir = .GlobalEnv)
  assign(ifelse(shear=="no",glue("resultat_{variable}"),glue("resultat_{variable}_shear")), resultat, envir = .GlobalEnv)
}


###########################################
# Fonction qui retourne à partir d'un nuage de points, un df des données par tranche et une fonction lissée
###########################################
#' Bin and Average a Continuous Variable
#'
#' This function performs binning of a numeric variable (`x`) and computes the mean of another variable (`y`) within each bin.
#'
#' @param x Numeric vector. The variable to be binned (e.g., wind speed).
#' @param y Numeric vector. The values to average within each bin (e.g., power).
#' @param start Numeric. The starting point of the binning range. Default is `0`.
#' @param stop Numeric. The end point of the binning range. Default is `20`.
#' @param pas Numeric. The bin width. Default is `0.5`.
#'
#' @details
#' - Bins `x` into intervals from `start` to `stop` with a step size `pas`.
#' - Computes the mean of `y` in each bin.
#' - Ensures that all bins are represented, even if empty (missing values will be `NA`).
#' - Returns both the binned data and an interpolation function (`approxfun`) based on the bin means.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{`df_bin`}{A data frame with two columns: `V` (bin centers) and `P` (mean values of `y` in each bin).}
#'   \item{`CP_bin_fun`}{An interpolation function that estimates `y` for a given `x` using linear interpolation between bin means.}
#' }
#'
#' @examples
#' \dontrun{
#' result <- binning(x = wind_speed, y = power_output)
#' df_bin <- result$df_bin
#' curve(result$CP_bin_fun, from = 0, to = 20, add = TRUE, col = "blue")
#' }
#'
#' @export

binning <- function(x, y, start = 0, stop = 20, pas = 0.5) {
  breaks <- seq(start, stop, pas)
  
  bins <- cut(x, breaks = breaks, include.lowest = FALSE, right = FALSE)
  
  
  # Tous les niveaux de bins possibles
  all_bins <- levels(cut(breaks[-length(breaks)], breaks = breaks, right = FALSE))
  
  
  binned_data <- aggregate(y, by = list(bins), FUN = mean)
  names(binned_data) <- c("bin", "P")
  
  # Ajouter les bins manquants
  df_bin <- merge(data.frame(bin = all_bins), binned_data, by = "bin", all.x = TRUE)
  
  # Extraire le centre des bins
  binned_x <- as.numeric(sub("\\[(.+),.*", "\\1", df_bin$bin)) + pas / 2
  
  df_bin <- data.frame(V = binned_x, P = df_bin$P)
  df_bin <- df_bin %>%
    arrange(V)
  
  CP_bin_fun <- approxfun(x = df_bin$V, y = df_bin$P)
  
  return(list(df_bin = df_bin, CP_bin_fun = CP_bin_fun))
}


###########################################################
# courbe de binning + constructeur
########################################################
#' Plot Binned Power Curve with Raw Data and Manufacturer Curve
#'
#' This function creates a plot showing the raw wind turbine power data, a binned average power curve,
#' and the manufacturer’s reference power curve.
#'
#' @param df Data frame containing the turbine measurements, including normalized hub wind speed (`ws_hub_norm`),
#'   power (`P`), and turbulence intensity at the hub (`ti_hub`).
#' @param df_cons Data frame with the manufacturer’s power curve data, with columns `V` (wind speed) and `P` (power).
#' @param start Numeric. Starting wind speed for binning (default is 0).
#' @param stop Numeric. Ending wind speed for binning (default is 20).
#' @param pas Numeric. Bin width for the wind speed bins (default is 0.5).
#'
#' @return A ggplot object showing:
#'   - Raw power measurements colored by turbulence intensity.
#'   - Binned average power curve (red line).
#'   - Manufacturer’s power curve (blue line).
#'
#' @details
#' The function bins the data by wind speed using the `binning()` function, calculates average power per bin,
#' and overlays this binned curve and the manufacturer curve on the scatter plot of raw data.
#' Turbulence intensity is visualized via a color gradient on the points.
#'
#' @examples
#' \dontrun{
#' binning_curve(df = df_lidar, df_cons = df_cons)
#' }
#'
#' @export
binning_curve <- function(df = df_lidar, df_cons = df_cons, start = 0, stop = 20, pas = 0.5) {
  x <- df[["ws_hub_norm"]]
  y <- df[["P"]]
  
  # intervalle sans borne inférieure
  df_bin <- binning(x, y, start, stop, pas)[["df_bin"]]
  
  ggplot() +
    geom_point(aes(x = x, y = y, fill = df[["ti_hub"]] * 100), shape = 21) + # Points bruts
    geom_line(data = df_bin, aes(x = V, y = P, color = "Binning"), linewidth = 1.5) + # Binning
    geom_line(data = df_cons, aes(x = V, y = P, color = "Constructeur"), inherit.aes = FALSE, linewidth = 1.5) + # courbe constructeur
    xlab("Wind Speed Hub (m/s)") +
    ylab("Puissance produite (KWh)") +
    scale_colour_manual(name = "Courbes", values = c("Binning" = "red", "Constructeur" = "blue"), guide = "legend") +
    scale_fill_gradient2(name = "TI Hub (%)", low = "white", mid = "green3", high = "black", limits = c(min(df[["ti_hub"]]), 30)) +
    theme_minimal()
}

########################################
# Calcul AEP données CMV + CP constructeur
#########################################

AEP_CMV_fun <- function(name) {
  df_counts <- df_lidar %>%
    mutate(HourGroup = floor_date(TimeStamp, "hour")) %>%
    add_count(HourGroup, name = "count")
  
  # Calcul de la proportion d'année sans compter les intervalles manquants
  annee_complete <- 365 * 24 * 6
  proportion_annee <- nrow(df_lidar) / annee_complete
  
  # Joindre AEB avec df_AEB_moy pour récupérer 'count' sur les données brutes
  AEP_CMV_cons <- round(df_counts %>%
                          mutate(pondere = power_curve(ws = ws_hub_norm, wtg = df_cons) / count) %>%
                          summarise(AEP = sum(pondere) / proportion_annee) %>%
                          pull(AEP) / 1e6, 3)
  
  AEP_CMV <- round(df_counts %>%
                     mutate(pondere = P / count) %>%
                     summarise(AEP = sum(pondere) / proportion_annee) %>%
                     pull(AEP) / 1e6, 3)
  
  assign("AEP_CMV", AEP_CMV, envir = .GlobalEnv)
  assign("AEP_CMV_cons", AEP_CMV_cons, envir = .GlobalEnv)
}

####################################################
# Courbe de puissance SLT + binning + binning scada
####################################################
#' Plot Predicted Power vs Wind Speed with Manufacturer and Binned Curves
#'
#' This function plots predicted power from a SLT dataset against wind speed,
#' alongside the manufacturer’s power curve, and binned average curves from both SLT predictions and SCADA data.
#'
#' @param df_SLT Data frame containing SLT data with normalized hub wind speed column `ws_hub_norm`.
#' @param pred_P Numeric vector of predicted power values corresponding to `df_SLT`.
#' @param df_cons Data frame of the manufacturer’s power curve with columns `V` (wind speed) and `P` (power).
#' @param df_cm Data frame of SCADA data with columns `ws_scada` (wind speed) and `P_scada` (power).
#'
#' @return A ggplot object displaying:
#'   - Predicted power points (gray crosses).
#'   - Manufacturer’s power curve (blue line).
#'   - Binned average power curve of SLT predictions (red line).
#'   - Binned average power curve of SCADA data (green line).
#'
#' @details
#' The function bins predicted and SCADA power data by wind speed using the `binning()` function,
#' then overlays the raw predicted data, the manufacturer curve, and both binned curves.
#' This facilitates comparison between predicted and actual performance and the reference curve.
#'
#' @examples
#' \dontrun{
#' plot_P_SLT(df_SLT, pred_P, df_cons, df_cm)
#' }
#'
#' @export
plot_P_SLT <- function(df_SLT, pred_P, df_cons,df_cm) {
  x <- df_SLT[["ws_hub_norm"]]
  y <- pred_P
  x_scada <- df_cm[["ws_scada"]]
  y_scada <- df_cm[["P_scada"]]
  
  df_bin <- binning(x, y)[["df_bin"]]
  df_bin_scada <- binning(x_scada, y_scada)[["df_bin"]]
  
  ggplot() +
    geom_point(aes(x = x, y = y), color = "gray", size = 1, shape = 4) +
    geom_line(data = df_cons, aes(x = V, y = P, color = "Constructeur"), linewidth = 1) +
    geom_line(data = df_bin, aes(x = V, y = P, color = "Binning SLT"), linewidth = 1) + # Binning
    geom_line(data=df_bin_scada,aes(x = V, y = P, color = "Binning scada"), linewidth = 1) +
    theme_minimal() +
    xlab("Vitesse du vent (m/s)") +
    ylab("Puissance (KW)") +
    scale_colour_manual(name = "Courbes", values = c("Constructeur" = "blue", "Binning SLT" = "red","Binning scada"="chartreuse3"), guide = "legend")
}


###########################
# Power deviation matrix
###########################
#' Power Deviation Matrix Plot by Wind Speed and Turbulence Intensity
#'
#' This function computes and visualizes the percentage deviation of measured power
#' from the expected power (manufacturer's curve) as a matrix across binned wind speed and turbulence intensity.
#' The output is a heatmap with annotated deviation values.
#'
#' @param df Data frame containing wind turbine measurements with columns:
#'   - `ws_hub_norm`: normalized wind speed at the hub (m/s)
#'   - `ti_hub`: turbulence intensity (fractional, e.g., 0.1 for 10%)
#'   - `P`: measured power output (kW or kWh)
#' @param df_cons Data frame of the manufacturer's power curve with columns:
#'   - `V`: wind speed (m/s)
#'   - `P`: expected power at each wind speed
#'
#' @return A ggplot2 heatmap showing power deviation (%) across bins of wind speed (x-axis) and turbulence intensity (y-axis).
#'
#' @details
#' The function bins wind speed into 1 m/s intervals (from 4.5 to ~25.5 m/s) and turbulence intensity into 2% increments (0% to 30%).
#' For each bin, it calculates the mean percentage deviation of measured power from the expected power,
#' then plots this as a colored tile with annotated values.
#' Blue represents negative deviation, red positive deviation, and white zero deviation.
#'
#' @examples
#' \dontrun{
#' PowMatDev(df, df_cons)
#' }
#'
#' @export
PowMatDev <- function(df, df_cons) {
  df_name <- deparse(substitute(df))
  df_cons_name <- deparse(substitute(df_cons))
  # creation d'un df avec les variables nécessaires
  df_PMD <- data.frame(ws = df[["ws_hub_norm"]], ti = df[["ti_hub"]] * 100, P = df[["P"]])
  df_binned <- df_PMD %>%
    mutate(
      ws_bin = cut(ws, breaks = seq(4.5, 25.5, by = 1)), # Tranches de 1 m/s
      ti_bin = cut(ti, breaks = seq(0, 30, by = 2)), # Tranches de turbulence
    ) %>%
    group_by(ws_bin, ti_bin)
  
  # df avec les bornes inf, sup des intervalles de vitesse et la puissance correspondante
  ecart_ws <- df_cons[["V"]][2] - df_cons[["V"]][1]
  idx_premier <- which(df_cons[["V"]] %% 1 == 0)[1]
  df_cons <- df_cons[seq(idx_premier, nrow(df_cons), by = 1 / ecart_ws), ]
  rownames(df_cons) <- 1:nrow(df_cons)
  
  inf <- which(df_cons[["V"]] == 5)
  sup <- which.max(df_cons[["V"]])
  
  P_table <- data.frame(
    ws_min = seq(4.5, (floor(max(df_cons[["V"]])) - 0.5), by = 1), # Intervalles de vitesse (borne inférieure)
    ws_max = seq(5.5, (floor(max(df_cons[["V"]])) + 0.5), by = 1), # Borne supérieure
    p_expected = df_cons[["P"]][inf:sup]
  )
  
  df_binned <- df_binned %>%
    rowwise() %>%
    mutate(p_expected = P_table$p_expected[which(ws >= P_table$ws_min & ws < P_table$ws_max)[1]]) %>%
    ungroup() %>%
    mutate(PMD = (P - p_expected) / p_expected * 100) %>%
    group_by(ws_bin, ti_bin) %>%
    summarise(PMD_mean = mean(PMD, na.rm = T), .groups = "drop") %>%
    filter(!is.na(ws_bin), !is.na(ti_bin)) %>%
    mutate(PMD_mean = ifelse(is.na(PMD_mean), 0, PMD_mean))
  
  ggplot(df_binned, aes(x = ws_bin, y = ti_bin, fill = PMD_mean)) +
    geom_tile(color = "white") + # Contours blancs pour séparer les cases
    geom_text(aes(label = round(PMD_mean, 1)), size = 3, color = "black") + # Texte blanc
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, na.value = "black") +
    labs(
      title = glue("Power Deviation Matrix  en fonction du vent et de la turbulence"),
      x = "Vitesse du vent (m/s)",
      y = "Intensité de turbulence (%)",
      fill = "Déviation (%)"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, color = "black"), # Étiquettes X en blanc
    )
}

##########################################
#Rose des vents
##############################################
plot.windrose <- function(data,
                          spd,
                          dir,
                          spdres = 2,
                          dirres = 30,
                          spdmin = 2,
                          spdmax = 20,
                          spdseq = NULL,
                          palette = "YlGnBu",
                          countmax = NA,
                          debug = 0){
  
  
  # Look to see what data was passed in to the function
  if (is.numeric(spd) & is.numeric(dir)){
    # assume that we've been given vectors of the speed and direction vectors
    data <- data.frame(spd = spd,
                       dir = dir)
    spd = "spd"
    dir = "dir"
  } else if (exists("data")){
    # Assume that we've been given a data frame, and the name of the speed
    # and direction columns. This is the format we want for later use.
  }
  
  # Tidy up input data ----
  n.in <- NROW(data)
  dnu <- (is.na(data[[spd]]) | is.na(data[[dir]]))
  data[[spd]][dnu] <- NA
  data[[dir]][dnu] <- NA
  
  # figure out the wind speed bins ----
  if (missing(spdseq)){
    spdseq <- seq(spdmin,spdmax,spdres)
  } else {
    if (debug >0){
      cat("Using custom speed bins \n")
    }
  }
  # get some information about the number of bins, etc.
  n.spd.seq <- length(spdseq)
  n.colors.in.range <- n.spd.seq - 1
  
  # create the color map
  spd.colors <- colorRampPalette(brewer.pal(min(max(3,
                                                    n.colors.in.range),
                                                min(9,
                                                    n.colors.in.range)),
                                            palette))(n.colors.in.range)
  
  if (max(data[[spd]],na.rm = TRUE) > spdmax){
    spd.breaks <- c(spdseq,
                    max(data[[spd]],na.rm = TRUE))
    spd.labels <- c(paste(c(spdseq[1:n.spd.seq-1]),
                          '-',
                          c(spdseq[2:n.spd.seq])),
                    paste(spdmax,
                          "-",
                          max(data[[spd]],na.rm = TRUE)))
    spd.colors <- c(spd.colors, "grey50")
  } else{
    spd.breaks <- spdseq
    spd.labels <- paste(c(spdseq[1:n.spd.seq-1]),
                        '-',
                        c(spdseq[2:n.spd.seq]))
  }
  data$spd.binned <- cut(x = data[[spd]],
                         breaks = spd.breaks,
                         labels = spd.labels,
                         ordered_result = TRUE)
  # clean up the data
  data. <- na.omit(data)
  
  # figure out the wind direction bins
  dir.breaks <- c(-dirres/2,
                  seq(dirres/2, 360-dirres/2, by = dirres),
                  360+dirres/2)
  dir.labels <- c(paste(360-dirres/2,"-",dirres/2),
                  paste(seq(dirres/2, 360-3*dirres/2, by = dirres),
                        "-",
                        seq(3*dirres/2, 360-dirres/2, by = dirres)),
                  paste(360-dirres/2,"-",dirres/2))
  # assign each wind direction to a bin
  dir.binned <- cut(data[[dir]],
                    breaks = dir.breaks,
                    ordered_result = TRUE)
  levels(dir.binned) <- dir.labels
  data$dir.binned <- dir.binned
  
  # Run debug if required ----
  if (debug>0){
    cat(dir.breaks,"\n")
    cat(dir.labels,"\n")
    cat(levels(dir.binned),"\n")
  }
  
  # deal with change in ordering introduced somewhere around version 2.2
  if(packageVersion("ggplot2") > "2.2"){
    cat("Hadley broke my code\n")
    data$spd.binned = with(data, factor(spd.binned, levels = rev(levels(spd.binned))))
    spd.colors = rev(spd.colors)
  }
  
  # create the plot ----
  p.windrose <- ggplot(data = data,
                       aes(x = dir.binned,
                           fill = spd.binned)) +
    geom_bar() +
    scale_x_discrete(drop = FALSE,
                     labels = waiver()) +
    coord_polar(start = -((dirres/2)/360) * 2*pi) +
    scale_fill_manual(name = "Wind Speed (m/s)",
                      values = spd.colors,
                      drop = FALSE) +
    theme(axis.title.x = element_blank())
  
  # adjust axes if required
  if (!is.na(countmax)){
    p.windrose <- p.windrose +
      ylim(c(0,countmax))
  }
  
  # print the plot
  print(p.windrose)
  
  # return the handle to the wind rose
  return(p.windrose)
}

##############################################"
#CAlcul de la divergence de Kullback-Leibler pour une année donnée
###################################################################"
#' Calculate Kullback-Leibler (KL) divergence for a specific year for a given wind farm
#'
#' This function computes the KL divergence between the power distribution for a given year
#' and the overall distribution af the specified variable
#'
#' @param df (dataframe) Name of the dataframe inside your global environnement. Has to contain `TimeStamp` column
#' @param var (character) Name of the tested variable
#' @param year (numeric) Year of the distrbution
#'
#' @return A numeric value corresponding to an approximation of the Kullback-Leibler value
#'
#' @details
#' - The reference distribution is the global distribution of all values of the selected variable.
#' - KL divergence is calculated using histograms with 50 bins from min to max of variable's values.
#' - A small constant (1e-10) is added to the histogram densities to avoid division by zero.
#'
#' @examples
#' \dontrun{
#' result <- KL_year(df_cm,var = 'P_scada', year=2022)
#' print(result)
#' }
#'
#' @export
KL_year <- function(df,var,year){
  if (!('TimeStamp' %in% names(df))){
    stop(" 'TimeStamp' doit être présent dans votre dataframe df.")
  }
  
  breaks_KL <- seq(min(df[[var]],na.rm=T), max(df[[var]],na.rm=T) , length.out = 50)
  
  x <- df[[var]]
  x <- x[!is.na(x)]  # retirer les NA
  
  px <- hist(x, breaks = breaks_KL, plot = FALSE)
  
  P <- px$density + 1e-10 #pour éviter les erreurs
  
  y <- df %>%
    mutate(annee = year(TimeStamp)) %>%
    filter(annee == year) %>%
    pull(var)
  y <- y[!is.na(y)]
  
  py <- hist(y, breaks = breaks_KL, plot = FALSE)
  Q <- py$density + 1e-10
  
  KL_PQ <- sum(P * log(P / Q)) * diff(breaks_KL)[1]
  
  return(KL_PQ)
}

############################################################
#test de kullback-leibler pour connaître la distrib la plus proche des données complètes
#############################################################
#' Calculate Kullback-Leibler (KL) divergence by year for a given wind farm
#'
#' This function computes the KL divergence between the power distribution for each year
#' and the overall distribution of the specified wind farm.
#'
#' @param name (character) Name of the wind farm to analyze.
#' @param var (character) Name of the tested variable
#' @param ref (character) Type of reference data. Can be 'mat' or 'scada'
#'
#' @return A list containing:
#'   - df_result: a data.frame with years and their corresponding KL divergence values,
#'   - best_year: the year with the minimum KL divergence (closest distribution) using `KL_year()` function,
#'   - best_KL: the minimum KL divergence value.
#'
#' @details
#' - The reference distribution is the global distribution of all values of the selected variable.
#' - KL divergence is calculated using histograms with 50 bins from min to max of variable's values from `KL_year()` function.
#'
#' @examples
#' \dontrun{
#' result <- test_year_KL("FarmA",var = 'P_scada', ref="scada")
#' print(result$best_year)
#' plot(result$df_result$year, result$df_result$KL, type = "b",
#'      xlab = "Year", ylab = "KL Divergence")
#' }
#'
#' @export
test_best_year_KL <- function(name,var,ref) {
  
  if (ref == "scada"){
    df_ref <- readRDS(glue("./2-Inputs/{name}/df_{name}_cleaned_merge.rds"))
  } else if (ref =="mat"){
    df_ref <- readRDS(glue("./2-Inputs/{name}/df_mat_filtered.rds"))
  } else {
    stop(glue("ref doit être 'mat' ou 'scada'"))
  }
  
  if (!(var %in% names(df_cm))){
    stop(glue("var doit faire partie de : {toString(names(df_ref))} "))
  }
  
  min_year <- min(year(df_ref$TimeStamp), na.rm = TRUE)
  max_year <- max(year(df_ref$TimeStamp), na.rm = TRUE)
  annees <- seq(min_year, max_year, by = 1)
  
  df_result_KL <- data.frame(year = annees, KL = NA_real_)
  
  for (i in seq_along(annees)) {
    KL_PQ <- KL_year(df = df_ref, var=var, year=annees[i])
    df_result_KL$KL[i] <- KL_PQ
  }
  
  return(list(df_result = df_result_KL,
              best_year = df_result_KL$year[which.min(df_result_KL$KL)],
              best_KL = min(df_result_KL$KL)
              )
         )
}


#####################################################################################
# Entraînement et prediction de P_SLT sur données scada avec entrainements sur une année donnée
#####################################################################################
#' Fit a Power Prediction Model for a Specific Year Using SCADA Data
#'
#' This function fits a predictive model for power output (P) using SCADA and related data
#' for a given year. The data source depends on whether material (`mat`) or shear corrections (`shear`) are used.
#' The model is trained using the specified method (default is random forest via 'ranger').
#'
#' @param year Integer. The year for which the model should be trained. Must match years present in the data.
#' @param mat Character string ("no" or other). Specifies whether to use material-related dataset (`df_SLT_mat` or `df_SLT_mat_shear`) or standard dataset (`df_SLT` or `df_SLT_shear`).
#' @param shear Character string ("no" or other). Specifies whether to use shear-corrected datasets (`df_SLT_shear` or `df_SLT_mat_shear`) or not (`df_SLT` or `df_SLT_mat`).
#' @param method Character string. The modeling method to use in `caret::train`. Default is `"ranger"` (random forest).
#'
#' @return A model object returned by `caret::train` containing the trained model for power prediction.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Selects the appropriate data frame based on `mat` and `shear` parameters.
#'   \item Joins the selected data with SCADA power data (`P_scada`) by `TimeStamp`.
#'   \item Filters the combined data for the specified year.
#'   \item Trains a predictive model to estimate power (`P`) from all other predictors except `TimeStamp`.
#' }
#' The model can be any method supported by `caret::train`, with `ranger` used by default for fast random forest fitting.
#'
#' @examples
#' \dontrun{
#' fit_2020 <- fit_P_year_scada(year = 2020, mat = "no", shear = "no", method = "ranger")
#' summary(fit_2020)
#' }
#'
#' @import dplyr
#' @import caret
#'
#' @export
fit_P_year_scada <- function(year = NULL, type = "lidar", shear = "no",method="ranger") {
  if (type == "lidar") {
    df_SLT_used <- if (shear == "no") df_SLT else df_SLT_shear
  } else if (type == "mat"){
    df_SLT_used <- if (shear == "no") df_SLT_mat else df_SLT_mat_shear
  } else if (type == "rea"){
    df_SLT_used <- if (shear == "no") df_wrf_norm %>% select(TimeStamp,ws,ti,wa) else df_wrf_norm %>% select(TimeStamp,ws,ti,wa,shear)
  }
  
  df <- df_SLT_used %>%
    dplyr::inner_join(df_cm %>% select(TimeStamp, P_scada), by = "TimeStamp") %>%
    dplyr::rename(P = P_scada) %>%
    dplyr::mutate(Year = year(TimeStamp)) %>%
    dplyr::filter(Year == year) %>%
    dplyr::select(-Year)
  
  set.seed(1809)
  
  fit <- caret::train(P ~ . - TimeStamp,
                      data = df,
                      method = method,
                      trControl = fitControl,
                      verbose = FALSE,
                      importance = "impurity")
  
  return(fit)
}


#' Predict Power Output for a Specific Year Using a Trained SCADA Model
#'
#' This function fits a predictive model for a specified year (using `fit_P_year_scada`) and then
#' uses that model to predict power output on the full dataset corresponding to the selected data and parameters.
#'
#' @param year Integer. The year for which the model should be trained and used for prediction.
#' @param mat Character string ("no" or other). Indicates whether to use the material-related dataset (`df_SLT_mat` or `df_SLT_mat_shear`) or standard dataset (`df_SLT` or `df_SLT_shear`).
#' @param shear Character string ("no" or other). Indicates whether to use shear-corrected datasets (`df_SLT_shear` or `df_SLT_mat_shear`) or not (`df_SLT` or `df_SLT_mat`).
#' @param method Character string. The modeling method to use in `caret::train`. Default is `"ranger"` (random forest).
#'
#' @return A numeric vector of predicted power output values for all rows in the selected dataset.
#'
#' @details
#' The function first trains a power prediction model for the specified year using `fit_P_year_scada`. It then
#' applies the trained model to the entire dataset selected according to the `mat` and `shear` arguments to generate predictions.
#'
#' This allows for year-specific power output predictions to be generated on the full data, useful for further analysis or visualization.
#'
#' @examples
#' \dontrun{
#' pred_2020 <- predict_P_year_scada(year = 2020, mat = "no", shear = "no", method = "ranger")
#' head(pred_2020)
#' }
#'
#' @import caret
#'
#' @export
predict_P_year_scada <- function(year, mat = "no", shear = "no",method="ranger") {
  fit <- fit_P_year_scada(year = year, mat = mat, shear = shear,method=method)
  
  newdata <- if (mat == "no") {
    if (shear == "no") df_SLT else df_SLT_shear
  } else {
    if (shear == "no") df_SLT_mat else df_SLT_mat_shear
  }
  
  pred <- predict(fit, newdata = newdata)
  return(pred)
}


####################################################
#Calcul des biais par tranche : nécessite df_SLT_mat
####################################################
#' Calculate and Plot Bias by Binned Intervals for Wind Data
#'
#' This function computes the bias (difference) between predicted and reference wind variables
#' within specified bins and visualizes the bias distribution as a bar plot.
#'
#' @param name Character. Name of the site or park for which to calculate bias.
#' @param var Character. The variable to analyze; either "ws" for wind speed or "wa" for wind angle.
#' @param ref Character. The reference dataset to compare against; either "scada" or "mat".
#' @param pred Character. The prediction dataset to compare against; either "rea" or "mat".
#' 
#' @return A ggplot object showing bias per binned interval of the chosen variable,
#' along with the proportion (%) of data points in each bin.
#'
#' @details
#' The function requires the dataset `df_SLT_mat` to be present in the environment,
#' which should contain material-related wind data.
#' It bins the reference and predicted variables into intervals (1 m/s for wind speed, 30° for wind angle),
#' computes the average bias (predicted minus reference) within each bin, and
#' calculates the proportion of data points in each bin.
#'
#' The result is plotted as a bar chart with bias values and proportions annotated.
#'
#' @examples
#' \dontrun{
#' biais_tranche(name="PRP", var = "ws", ref = "scada",pred="rea")
#' biais_tranche(name="COF1", var = "wa", ref = "mat",pred="mat")
#' }
#'
#' @import dplyr ggplot2 glue
#'
#' @export
biais_tranche <- function(name, var = "ws", ref = "scada", pred = "mat") {
  if (pred == "mat" & !exists("df_SLT_mat", envir = .GlobalEnv)){
    stop("df_SLT_mat indispensable. Faire mcp sur données de mat avant.")
  }
  
  if (var == "ws") {
    breaks <- seq(0, 29, by = 1)
  } else if (var == "wa") {
    breaks <- seq(0, 360, by = 30)
  } else {
    stop("var doit être 'ws' ou 'wa'")
  }
  
  if (ref == "scada") {
    df_ref <- clean_filt(name=name)
    
    if (pred == "rea"){
      df_pred <- readRDS(glue("./2-Inputs/{name}/df_wrf_filtered.rds"))
    } else if (pred == "mat"){
      df_pred <- df_SLT_mat
    }
    
    var_ref <- names(df_ref)[startsWith(names(df_ref),var)]
    var_pred <- names(df_pred)[startsWith(names(df_pred),var)]
    
    df_biais <- df_ref %>%
      select(TimeStamp,.data[[var_ref]])%>%
      inner_join(df_pred%>%
                   select(TimeStamp,.data[[var_pred]]))%>%
      mutate(
        bin_ref = cut(.data[[var_ref]], breaks = breaks , include.lowest = TRUE, right = FALSE),
        bin_pred = cut(.data[[var_pred]], breaks = breaks , include.lowest = TRUE, right = FALSE)
      ) %>%
      filter(bin_ref == bin_pred) 

    df_biais <- df_biais%>%
      group_by(bin_ref) %>%
      summarise(
        count = n(),
        biais = round(mean(.data[[var_pred]], na.rm = T) - mean(.data[[var_ref]], na.rm = T), 2),
        .groups = "drop"
      ) %>%
      mutate(prop = round(count / sum(count, na.rm = T) * 100, 2))
    
  } else if (ref == "mat"){
    df_ref <- moy_filt(name=name,type="mat")
    
    var_ref <- names(df_ref)[startsWith(names(df_ref),var)]
    
    df_ref <- df_ref%>%
      rename(var_ref_temp = !!sym(var_ref))
  
    if (pred == "rea"){
      df_pred <- readRDS(glue("./2-Inputs/{name}/df_wrf_filtered.rds"))
    } else if (pred =="mat"){
      df_pred <- df_SLT_mat
    }
    
    var_pred <- names(df_pred)[startsWith(names(df_pred),var)]
    
    df_biais <- df_ref %>%
      select(TimeStamp, var_ref_temp) %>%
      inner_join(df_pred %>%
                 select(TimeStamp, .data[[var_pred]]), by = "TimeStamp") %>%
      mutate(
        bin_ref = cut(var_ref_temp, breaks = breaks),
        bin_pred = cut(.data[[var_pred]], breaks = breaks)
      ) %>%
      filter(bin_ref == bin_pred) %>%
      group_by(bin_ref) %>%
      summarise(
        count = n(),
        biais = round(mean(.data[[var_pred]], na.rm = TRUE) - mean(var_ref_temp, na.rm = TRUE), 2),
        .groups = "drop"
      ) %>%
      mutate(prop = round(count / sum(count, na.rm = TRUE) * 100, 2))
  } else {
    stop("ref doit être 'scada' ou 'mat'")
  }
  
  min_biais <- min(df_biais$biais)
  max_biais <- max(df_biais$biais)
  
  if (min_biais == max_biais) {
    min_biais <- min_biais - 1
    max_biais <- max_biais + 1
  }
  
  # Affichage
  ggplot(df_biais, aes(x = bin_ref, y = biais)) +
    geom_col(fill = "skyblue",color="black")   +
    geom_text(aes(label = glue("{biais}")), vjust = -0.5, size = 3) +
    geom_text(aes(label = glue("{prop}%")), size = 3, y = max_biais * 1.3) +
    labs(
      x = glue("Tranches de {var}_{ref}"),
      y = glue("Biais"),
      title = glue("Biais par tranche de {var}_{pred} par rapport à {var}_{ref} pour {name}"),
      subtitle = "Proportion de chaque tranche"
    ) +
    ylim(min(min_biais, 0), max(max_biais, 0) * 1.3) +
    theme_minimal()
}

