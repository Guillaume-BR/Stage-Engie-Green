###############################
#Clean les données scada 10 minutes
#################################"

#' Clean SCADA Data
#'
#' This function loads and cleans a wind turbine SCADA dataset stored in an '.rds' file.
#' It filters the data to include only rows where the turbine status ('wtg_status') is "normal".
#' It also adds a `Year` column based on the 'Date_time' and ensures that the 'P' (power) column
#' is not missing. For certain turbines, the function also formats 'Date_time' explicitly.
#'
#' @param name Name of the wind turbine (used to locate the corresponding '.rds' file in './2-Inputs/{name}/').
#'
#' @return A cleaned 'data.frame' containing only rows with '"normal"' status, a properly formatted
#' 'Date_time' column, and a new 'Year' column. Rows with missing power ('P') values are removed.
#'
#' @import glue
#' @import dplyr
#' @import lubridate
#' @import stringr
#' @export

clean_scada <- function(name = name) {
  df_cleaned <- readRDS(glue::glue("./2-Inputs/{name}/df_{name}_cleaned.rds"))

  if (name %in% c("ABH", "AEB", "COF1", "COF2", "EDS", "PRP")) {
    df_cleaned_filt <- df_cleaned %>%
      filter(wtg_status == "normal") %>%
      mutate(
        Date_time = dplyr::if_else(
          stringr::str_detect(Date_time, "^\\d{4}-\\d{2}-\\d{2}$"),
          paste0(Date_time, " 00:00:00"),
          as.character(Date_time)
        ),
        Date_time = lubridate::ymd_hms(Date_time),
        Year = lubridate::year(Date_time)
      ) %>%
      drop_na(P)
  } else {
    df_cleaned_filt <- df_cleaned %>%
      filter(wtg_status == "normal") %>%
      mutate(
        Year = lubridate::year(Date_time)
      ) %>%
      drop_na(P)
  }

  return(df_cleaned_filt)
}


###################
# Filtrage des données : retourne une liste de df annuels où figure les heures
#ayant au moins "mesures" données par heure
###################

#' Filter SCADA data by year with a minimum number of measurements per hour
#'
#' This function filters SCADA data from a wind turbine by selecting only the measurements
#' where the turbine status is "normal" (`wtg_status == "normal"`), grouped by hour.
#' It keeps only those hourly groups that contain at least a specified number of measurements
#' (`mesures`). The filtered data is returned as a list, with one element per year.
#'
#' @param name Character The name or identifier of the wind turbine (passed to the 'clean_scada' function).
#' @param mesures Integer between 1 and 6. Minimum number of measurements per hour required to keep that hour (default: 1).
#'
#' @return A list of data frames, one per year, each containing only "normal" status data
#' and hourly groups that meet the measurement threshold. The columns match those returned by
#' 'clean_scada', except for the intermediate 'HourGroup' column, which is removed.
#'
#' @details
#' The function uses `clean_scada()` to load and clean raw SCADA data, then filters the
#' data by year and operating status. Measurements are grouped by hour (`HourGroup`), and only
#' hours with at least `mesures` entries are kept.
#'
#' @examples
#' \dontrun{
#' # Filter data for turbine "WTG_01" with at least 4 measurements per hour
#' df_by_year <- filtre_an(name = "WTG_01", mesures = 4)
#'
#' # Access data for the year 2021
#' df_2021 <- df_by_year[["2021"]]
#' }
#'
#' @import dplyr
#' @importFrom lubridate floor_date
#' @export
filtre_an <- function(name = name, mesures = 1) {

  df_cleaned <- clean_scada(name = name)

  debut <- min(df_cleaned$Year, na.rm = TRUE)
  fin <- max(df_cleaned$Year, na.rm = TRUE)

  # Create an empty list to store data frames by year
  df_list <- list()

  for (year in seq(debut, fin)) {
    df_year <- df_cleaned %>%
      dplyr::filter(Year == year, wtg_status == "normal") %>%
      dplyr::mutate(HourGroup = lubridate::floor_date(Date_time, "hour")) %>%
      dplyr::add_count(HourGroup, name = "count") %>%
      dplyr::group_by(HourGroup) %>%
      dplyr:: filter(n() >= mesures) %>%
      dplyr::ungroup() %>%
      dplyr::select(-HourGroup)

    df_list[[as.character(year)]] <- df_year
  }
  return(df_list)
}


##############################################################################
# Calcul des AEP selon le choix du nombre minimum de données par heures conservées en GW
###########################################################################

#' Compute Annual Energy Production (AEP) by Year
#'
#' This function calculates the Annual Energy Production (AEP) for a wind turbine
#' based on SCADA data, filtered by a minimum number of measurements per hour.
#' The AEP is computed per year and adjusted based on data availability (proportion of year covered).
#'
#' @param name Character. Name of the wind turbine (used to locate the SCADA data).
#' @param mesures Integer. Minimum number of measurements required per hour (default = 1).
#' @param debut Integer. Start year for AEP computation (e.g., 2015).
#' @param fin Integer. End year for AEP computation (e.g., 2023).
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{annee}{Year of analysis.}
#'   \item{AEP}{Annual Energy Production (GWh), adjusted based on data coverage.}
#'   \item{prop}{Proportion of the year covered by valid data (0 to 1 scale).}
#' }
#'
#' @details
#' The function internally uses \code{\link{filtre_an}} to clean and filter SCADA data
#' by year and status ("normal"). It adjusts AEP values to account for partial year coverage
#' using the ratio of available seconds over a full year (365 days).
#'
#' Each power value \code{P} is weighted by \code{1/count} where \code{count} is the number
#' of measurements in its hourly group.
#'
#' @import glue
#' @import dplyr
#' @import tidyr
#'
#'
#' @seealso \code{\link{filtre_an}} for SCADA data preprocessing.
#'
#' @examples
#' \dontrun{
#' AEP_func(name = "COF1", mesures = 3, debut = 2016, fin = 2022)
#' }
#'
#' @export

AEP_func <- function(name, mesures = 1, debut = NULL, fin = NULL) {
  if (is.null(debut) || is.null(fin)) {
    stop("Please specify both 'debut' and 'fin' years.")
  }

  # Initialize output data.frame
  df_aep <- data.frame(
    annee = debut:fin,
    AEP = NA_real_,
    prop = NA_real_
  )

  # Get cleaned and filtered data by year
  df_list <- filtre_an(name = name, mesures = mesures)
  annee_complete <- 365 * 24 * 3600  # seconds in a full year

  # Compute AEP year by year
  for (year in debut:fin) {
    df <- df_list[[as.character(year)]]
    if (!is.null(df) && nrow(df) > 0) {
      proportion <- 600 * nrow(df) / annee_complete
      aep_val <- df %>%
        dplyr::mutate(pondere = P / count) %>%
        dplyr::summarise(AEP = round(sum(pondere) / proportion / 1e6, 3)) %>%
        dplyr::pull(AEP)
    } else {
      proportion <- NA
      aep_val <- NA
    }

    df_aep[year - debut + 1, "AEP"] <- aep_val
    df_aep[year - debut + 1, "prop"] <- round(proportion, 2)
  }

  return(df_aep)
}

##########################################
# Variation des AEP selon le nombre de mesures
############################################

#' Visualize AEP Sensitivity to Minimum Hourly Measurements
#'
#' This function evaluates how the Annual Energy Production (AEP) changes depending
#' on the minimum number of valid SCADA measurements required per hour (from 1 to 6).
#' It computes AEP for each year and each threshold, then visualizes and summarizes
#' the variation.
#'
#' @param name Character. Name of the wind turbine (used to locate the SCADA data).
#'
#' @return A data frame with the following columns:
#' \describe{
#'   \item{year}{The year of computation.}
#'   \item{ndata_1 to ndata_6}{AEP values computed using at least 1 to at least 6 measurements/hour respectively.}
#'   \item{min}{Minimum AEP among the 6 thresholds for each year.}
#'   \item{max}{Maximum AEP among the 6 thresholds for each year.}
#'   \item{ecart}{Relative difference (in \%) between min and max AEP per year.}
#' }
#'
#' @importFrom glue glue
#' @importFrom dplyr mutate
#' @importFrom lubridate year
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot aes geom_line geom_text scale_x_continuous ylab ggtitle theme_minimal theme element_text
#'
#'
#' @details
#' For each threshold (from 1 to 6 measurements per hour), the function calls
#' \code{\link{AEP_func}} to compute annual AEP values. It then compares how sensitive
#' AEP estimates are to data completeness assumptions, and generates a time-series plot.
#'
#' A label showing the percentage deviation from max to min AEP is added per year.
#'
#' @seealso \code{\link{AEP_func}} for AEP computation logic,
#' \code{\link{filtre_an}} for SCADA filtering.
#'
#' @examples
#' \dontrun{
#' var_AEP("COF1")
#' }
#'
#' @export

var_AEP <- function(name) {
  df_cleaned <- readRDS(glue::glue("./2-Inputs/{name}/df_{name}_cleaned.rds")) %>%
    dplyr::mutate(Year = lubridate::year(Date_time))

  debut <- min(df_cleaned$Year, na.rm = TRUE)
  fin <- max(df_cleaned$Year, na.rm = TRUE)

  ncols <- 6
  res_mes <- data.frame(year = seq(debut, fin, by = 1), matrix(ncol = ncols))
  colnames(res_mes) <- c("year", glue::glue("ndata_{seq(1, 6)}"))

  # Get AEP values for each number of measurements per hour (1 to 6)
  for (i in 1:6) {
    df_res <- AEP_func(name = name, mesures = i, debut = debut, fin = fin)
    res_mes[[i + 1]] <- df_res$AEP
  }

  # Calculate min, max and variation per year
  res_mes$min <- apply(res_mes[, -1], 1, min, na.rm = TRUE)
  res_mes$max <- apply(res_mes[, -1], 1, max, na.rm = TRUE)
  res_mes$ecart <- round((res_mes$min - res_mes$max) / res_mes$max * 100, 2)

  # Long format for plotting
  long_res <- res_mes %>%
    tidyr::pivot_longer(cols = starts_with("ndata_"), names_to = "ndata", values_to = "AEP")

  # Plotting
  p <- ggplot(data = long_res, ggplot2::aes(x = year, y = AEP, color = ndata)) +
    geom_line(linewidth = 1.2) +
    ggtitle(glue::glue("Variation de l'AEP pour {name} selon le nombre minimum de données par heure")) +
    scale_x_continuous(name = "Année", breaks = seq(debut, fin)) +
    ylab("AEP (GWh)") +
    theme_minimal() +
    theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    geom_text(
      data = res_mes,
      ggplot2::aes(x = year, y = max + 0.2, label = glue::glue("{ecart} %")),
      inherit.aes = FALSE, size = 3
    )

  print(p)

  return(res_mes)
}

###############################################
#Ecart par rapport à une référence de 6 données par heure
#################################################

# Ecart en % par rapport à l'AEP si on considère 6 mesures par heure

 #' Vizualize AEP sensitivity according to a reference
 #'
 #'
 #' This function allows to visualize the deviation for different threshold (from 1 to 5)
 #' to the reference case where AEP is calculate from hours which have 6 measurements.
 #'
 #' @param name Character. Name of the wind turbine
 #'
 #' @return A plot where we can see the deviation of the differents cases
 #'
 #' @details The function call \code{\link{var_AEP}} and calculate the deviation to the reference case.
 #' A time-series plot is generated
 #'
 #' @import dplyr
 #' @import tidyr
 #' @import ggplot2
 #'
 #'  @seealso \code{\link{var_AEP}}, \code{\link{AEP_func}}, \code{\link{filtre_an}}
 #' @examples
#' \dontrun{
#' var_AEP_ref("COF1")
#' }
#'
#' @export

var_AEP_ref <- function(name=name){

  #on récupère le dataframe avec les AEP calculés selon le nombre de données
  #considérées par heure at on calcule les écarts avec le cas de ref (6 mesures/h)
  res_mes <- var_AEP(name=name)%>%
    dplyr::select(1:7) %>%
    dplyr::mutate(
      var_1 = (ndata_1 - ndata_6) / ndata_6 * 100,
      var_2 = (ndata_2 - ndata_6) / ndata_6 * 100,
      var_3 = (ndata_3 - ndata_6) / ndata_6 * 100,
      var_4 = (ndata_4 - ndata_6) / ndata_6 * 100,
      var_5 = (ndata_5 - ndata_6) / ndata_6 * 100,
    ) %>%
    dplyr::select(-starts_with("ndata"))

res_mes_long <- res_mes%>%
  pivot_longer(cols=-year,names_to = "variable",values_to="valeur")

ggplot(res_mes_long,aes(x=year,y=valeur,color=variable))+
  geom_line(linewidth=1
  )+
  labs(x="Année",
       y="Ecart (%)",
       title=glue("Ecart par rapport à l'AEP de référence pour {name}"),
       subtitle = "AEP de référence = calculé avec au minimum 6 mesures par heure"
  )+
  theme_minimal()
}

######################################
# Barplot des AEP par année à partir des données scada
# arguments : nom de l'éolienne et nombre de mesures minimum par heure
######################################
#' Bar Plot of Annual Energy Production (AEP)
#'
#' This function generates a bar plot of the AEP (Annual Energy Production) for a given wind turbine,
#' requiring at least a specified number of SCADA measurements per hour. It also displays the mean AEP and
#' one standard deviation band above and below the mean. The proportion of available data used per year
#' is displayed at the bottom of the bars.
#'
#' @param name Character. Name of the wind turbine (used to locate the SCADA data).
#' @param mesures Integer (1 to 6). Minimum number of measurements per hour required for inclusion.
#'
#' @return A 'ggplot2' object showing annual AEP values, the mean AEP line, ±1 standard deviation bands,
#' and labels for the values and proportions.
#'
#' @details
#' The function extracts the cleaned SCADA dataset ('df_{name}_cleaned.rds') and calculates AEP per year
#' using the specified minimum number of measurements per hour. The function then:
#' - Computes the mean and standard deviation of the AEP values.
#' - Plots annual AEP values as bars.
#' - Adds a solid line for the mean and dashed lines for ±1 SD.
#' - Annotates each bar with the AEP value and proportion of data used.
#'
#' The AEP values are extracted using \code{\link{AEP_func}}, while proportions are based on valid timestamps.
#'
#' @import glue
#' @import dplyr
#' @import lubridate
#' @import ggplot2
#'
#' @seealso \code{\link{AEP_func}}, \code{\link{var_AEP}}
#'
#' @examples
#' \dontrun{
#' AEP_bar("COF1", mesures = 4)
#' }
#'
#' @export

AEP_bar <- function(name, mesures = 6) {

  #on récupère la première et la dernière année des données scada
  df_cleaned <- readRDS(glue("./2-Inputs/{name}/df_{name}_cleaned.rds"))%>%
    mutate(Year = year(Date_time))

  debut <- min(df_cleaned$Year, na.rm = TRUE)
  fin <- max(df_cleaned$Year, na.rm = TRUE)

  prop <- AEP_func(name, mesures,debut=debut, fin=fin)$prop
  resultats <- var_AEP(name)
  aep <- resultats %>%
    select(year, glue("ndata_{mesures}"))
  moy <- round(mean(aep[, 2]), 3)
  sd <- round(sd(aep[, 2]))
  ggplot(data = aep, aes(x = year, y = aep[, 2])) +
    geom_col(fill = "lightblue") +
    geom_abline(slope = 0, intercept = moy, ) +
    geom_abline(slope = 0, intercept = moy - sd, linetype = 2) +
    geom_abline(slope = 0, intercept = moy + sd, linetype = 2) +
    geom_label(aes(label = aep[, 2])) +
    geom_text(aes(label = prop), y = 0.2) +
    scale_x_continuous(
      name = "Année",
      breaks = c(seq(debut, fin, 1))
    ) +
    labs(y="AEP (GWh)",
         title = glue("AEP pour l'éolienne {name} pour au moins {mesures} données par heure - AEP moyen = {moy} GW"),
         subtitle = "Proportion des années présentes en bas des barres")+
    theme_minimal()
}

########################################
#Histogramme par année des données scada selon le nombre de données minimum conservé par heure
#######################################
#' Plot Histograms of Power or Wind Speed for a Given Year
#'
#' This function generates faceted histograms of a selected variable (default is power, `P`)
#' for a given wind turbine and year. It compares distributions across different minimum thresholds
#' of valid measurements per hour (`h >= 1` to `h >= 6`).
#'
#' @param name Character. Name of the wind turbine.
#' @param year Integer. Year to filter the data.
#' @param var Character. Variable to plot (default is `"P"`). Can be 'Rs', 'Ws', 'P', 'Ya', 'Ds' or 'Wa'.
#'
#' @return A `ggplot` object showing 6 histograms (one for each threshold `h >= i` where `i = 1...6`),
#' representing the distribution of the selected variable for the given year and turbine.
#'
#' @details
#' - The function uses `filtre_an()` to retrieve filtered datasets for each threshold `i`.
#' - Each histogram is built using 30 bins and normalized to display densities.
#' - Histograms are faceted by threshold group (`groupe`) with consistent axis ranges.
#'
#' @import ggplot2
#' @import glue
#' @import dplyr
#'
#' @examples
#' \dontrun{
#' histo_year("WT01", 2022, var = "P")
#' histo_year("WT01", 2021, var = "ws")
#' }
#'
#' @seealso \code{\link{filtre_an}}
#'
#' @export

histo_year <- function(name=name, year,var = 'P') {
  p <- ggplot()

  for (i in 1:6) {
    liste <-  filtre_an(name= name,mesures = i)
    df <- as.data.frame(liste[[as.character(year)]])
    print(df)

    if (year <min(df$Year,na.rm=T) | year > max(df$Year,na.rm=T)){
      stop(glue("year doit être compris entre {min(df$Year,na.rm=T) et {max(df$Year,na.rm=T)} inclus."))
    }

    if (!(var %in% c("Rs", "Ws", "P", "Ya", "Ds", "Wa"))){
      stop(glue("var doit être une des variables suivantes : {toString(c('Rs', 'Ws', 'P', 'Ya', 'Ds', 'Wa'))}"))
    }

    df_unit <- data.frame(variable = c("Rs", "Ws", "P", "Ya", "Ds", "Wa"),
                          unite = c("Rotation du rotor (tour/min)","Vitesse du vent (m/s)", "Puissance (kWh)", "Yaw (°)", "Rotation de la génératrice (tour/min)", "Angle du vent (°)"))

    df$groupe <- glue("data/h >= {i}")

    p <- p +
      geom_histogram(data = df, aes(x = .data[[var]], y = after_stat(density), fill = groupe), color="black",alpha = 0.4, bins = 30)
  }

  p +
    facet_wrap(~groupe, nrow = 1, ncol = 6) +
    labs(
      title = glue("Histogrammes de {var}_scada pour l'éolienne {name} pour l'année {year}"),
      subtitle = "selon le nombre de données minimum par heure",
      x = df_unit%>%
        dplyr::filter(variable==var)%>%
        pull(unite),
      y = "Fréquence",
      fill = "Source"
    ) +
    theme_minimal()+
    theme(legend.position = "none")

}

#####################################################################
#AEP en sommant les différentes puissantes. % d'écarts avec l'AEP_scada
####################################################################

#' Comparison of Modeled AEP to SCADA Reference AEP
#'
#' This function computes the Annual Energy Production (AEP) for different modeled power columns
#' (e.g., `P_model`, `P_curve`, etc.) from the merged SCADA dataset `df_{name}_cleaned_merge.rds`.
#' It compares each modeled AEP to the reference AEP from the `P_scada` column and expresses
#' the deviation in percentage.
#'
#' @param name Character. Name of the wind turbine (used to locate the cleaned SCADA dataset).
#'
#' @return A data frame containing :
#' \describe{
#'   \item{year}{The year of computation.}
#'   \item{prop}{The proportion of available hourly data for that year (based on 8760 hours).}
#'   \item{AEP_<var>}{The annual energy production in GWh from each power column (e.g., `P_model`).}
#'   \item{<var>_%}{The percentage deviation of each modeled AEP from the SCADA reference AEP.}
#' }
#'
#' @details
#' The SCADA file used must be located at `./2-Inputs/{name}/df_{name}_cleaned_merge.rds`, and
#' must include at least one reference power column named `P_scada`, along with modeled power
#' columns prefixed with `P_` (e.g., `P_model`, `P_curve`, etc.).
#'
#' All AEP values are expressed in **GWh**, and the deviation is calculated as:
#' \deqn{
#'   100 \times \frac{AEP_{model} - AEP_{scada}}{AEP_{scada}}
#' }
#'
#' @import dplyr
#' @import glue
#' @import stringr
#' @import lubridate
#'
#' @export

diff_AEP_to_scada <- function(name = name) {
  df_name <- readRDS(glue("./2-Inputs/{name}/df_{name}_cleaned_merge.rds"))%>%
    drop_na()

  debut <- min(year(df_name$TimeStamp))
  fin <- max(year(df_name$TimeStamp))

  variables <- names(df_name)[startsWith(names(df_name), "P")]

  df_temp <- data.frame(year = debut:fin)

  for (i in seq(debut:fin)) {
    df <- df_name %>%
      dplyr::filter(year(TimeStamp) == debut + i - 1)

    annee_complete <- 24 * 365

    prop_annee <- round(nrow(df) / annee_complete, 2)
    df_temp$prop[i] <- prop_annee

    for (var in variables) {
      var_temp <- str_sub(var, 3, )
      df_temp[[glue("AEP_{var_temp}")]][i] <- round(sum(df[[var]]) / 1e6, 3)
      df_temp[[glue("{var}_%")]][i] <- round((sum(df[[var]]) - sum(df$P_scada)) / sum(df$P_scada) * 100, 2)
    }
  }

  return(df_temp)
}

#####################################"
#AEP moyen pluriannuel : moyenne pondérée avec par la proportion de chaque année
#####################################

#' Weighted Average AEP Calculation for a Wind Turbine
#'
#' This function computes the weighted average Annual Energy Production (AEP) for all modeled power variables
#' (e.g., `AEP_model`, `AEP_curve`, etc.) contained in the output of `diff_AEP_to_scada()`. The weighting is
#' based on the proportion of available data (`prop`) for each year.
#'
#' @param name Character. Name of the wind turbine (used to locate SCADA data).
#'
#' @return A data frame with two columns:
#' \describe{
#'   \item{variable}{The name of the AEP variable (e.g., `AEP_SLT_SLT`, `AEP_SLT_cons`, etc.).}
#'   \item{moy_ponderee}{The weighted average of that AEP over the available years.}
#' }
#'
#' @details
#' Only numeric AEP variables (excluding year, prop, and any deviation percentage columns containing `%`)
#' are considered. Years with missing or zero `prop` values are excluded from the calculation.
#'
#' The weighted average is computed as:
#' \deqn{
#'   \frac{\sum_{i} AEP_i \times prop_i}{\sum_{i} prop_i}
#' }
#'
#' @import stringr
#'
#' @seealso [diff_AEP_to_scada()]
#'
#' @export
AEP_WT_moy <- function(name) {
  df_temp <- diff_AEP_to_scada(name)

  # Sélection des colonnes à moyenniser (numériques, sans %, ni year/prop)
  vars_to_avg <- names(df_temp)
  vars_to_avg <- vars_to_avg[!(vars_to_avg %in% c("year", "prop"))]
  vars_to_avg <- vars_to_avg[!str_detect(vars_to_avg, "%")]

  # Initialisation du résultat
  df_moy <- data.frame(variable = vars_to_avg, moy_ponderee = NA_real_)

  # Calcul de la moyenne pondérée
  for (i in seq_along(vars_to_avg)) {
    var <- vars_to_avg[i]
    # Index valides (prop non NA/0 et valeur non NA)
    valid_idx <- which(!is.na(df_temp$prop) & df_temp$prop > 0 & !is.na(df_temp[[var]]))

    weights <- df_temp$prop[valid_idx]
    values <- df_temp[[var]][valid_idx]

    df_moy$moy_ponderee[i] <- round(sum(values * weights, na.rm = TRUE) / sum(weights, na.rm = TRUE), 3)
  }

  return(df_moy)
}

###########################################
# Calcul des AEP partir de df_name_cleaned_merge par an en pondérant par la proportion d'année
###########################################
#' Normalized Annual Energy Production (AEP) for a Wind Turbine
#'
#' This function calculates the normalized Annual Energy Production (AEP) for each year and power variable
#' contained in the merged SCADA dataset (`df_{name}_cleaned_merge.rds`). The normalization accounts for
#' the proportion of available data in the year (`prop`), so that AEP values are comparable across years with different data completeness.
#'
#' @param name Character. Name of the wind turbine (used to identify the dataset).
#'
#' @return A data frame with one row per year and columns:
#' \describe{
#'   \item{year}{The year of analysis.}
#'   \item{prop}{The proportion of data available for that year (between 0 and 1).}
#'   \item{AEP_xx}{Normalized AEP values (in GWh) for each variable starting with "P", e.g., `AEP_model`, `AEP_curve`, etc.}
#' }
#'
#' @details
#' The normalization is done by dividing the total energy by the proportion of valid data for that year, as follows:
#'
#' \deqn{
#'   \text{AEP\_norm} = \frac{\sum_t P_t}{\text{prop\_year}}
#' }
#'
#' This allows comparing years even when the number of valid data points is different.
#'
#' @import glue
#' @import dplyr
#' @import lubridate
#' @import stringr
#'
#' @seealso [diff_AEP_to_scada()], [AEP_WT_moy()]
#'
#' @export
AEP_WT_norm <- function(name = name) {
  df_name <- readRDS(glue("./2-Inputs/{name}/df_{name}_cleaned_merge.rds"))%>%
    drop_na()

  debut <- min(year(df_name$TimeStamp))
  fin <- max(year(df_name$TimeStamp))

  variables <- names(df_name)[str_detect(names(df_name), "P")]

  df_temp <- data.frame(year = debut:fin)

  for (i in seq(debut:fin)) {
    df <- df_name %>%
      dplyr::filter(year(TimeStamp) == debut + i - 1)

    annee_complete <- 24 * 365

    prop_annee <- round(nrow(df) / annee_complete, 2)
    df_temp$prop[i] <- prop_annee
    for (var in variables) {
      var_temp <- str_sub(var, 3, ) # pour avoir les noms adéquats après
      df_temp[[glue("AEP_{var_temp}")]][i] <- round(sum(df[[var]]) / prop_annee / 1e6, 3)
    }
  }
  return(df_temp)
}

#################################################################################
# fonction qui donne la distribution des différentes puissances par binning de 100 kW
###############################################################################

#' Frequency Distribution of Power Values by Bins
#'
#' This function calculates the frequency distribution of power values from SCADA data, binned into 100 kW intervals.
#' It applies to all columns starting with `"P_SLT"` and the `"P_scada"` column from the merged cleaned dataset.
#'
#' @param name Character. Name of the wind turbine (used to locate the dataset file).
#'
#' @return A data frame with the following structure:
#' \describe{
#'   \item{bin}{Power bin labels, e.g., `[0-100)`, `[100-200)`, etc.}
#'   \item{P_xxx_freq}{Relative frequency (between 0 and 1) of values in each bin for each power variable.}
#' }
#'
#' @details
#' - The bins are of fixed width (100 kW).
#' - Frequencies are computed as proportions of total counts within each variable.
#' - Bins with no values receive a frequency of 0.
#'
#' @import dplyr
#' @import glue
#' @import stringr
#' @import stats
#'
#' @examples
#' \dontrun{
#' bin_freq("WT01")
#' head(df_freq_bin_WT01)
#' }
#'
#' @export
bin_freq <- function(name = name) {
  df_non_filt <- readRDS(glue("./2-Inputs/{name}/df_{name}_cleaned_merge.rds"))%>%
    select(starts_with("P_SLT"),"P_scada")

  # Définir les bornes des bins
  max_val <- max(df_non_filt, na.rm = TRUE)
  breaks <- seq(0, (floor(max_val / 100) + 5) * 100, by = 100)
  labels <- glue("[{head(breaks,-1)}-{tail(breaks, -1)})")


  # Créer un tableau des bins vide avec les bons noms
  df_freq <- data.frame(bin = factor(labels, levels = labels))

  # Boucle sur les variables
  variables <- names(df_non_filt)

  for (var in variables) {
    temp <- df_non_filt %>%
      mutate(bin = cut(.data[[var]], breaks = breaks, labels = labels, include.lowest = T, right = FALSE)) %>%
      count(bin, name = "count") %>%
      mutate(freq = count / sum(count))

    # Jointure avec le tableau principal
    df_freq <- df_freq %>%
      left_join(
        temp %>%
          dplyr::select(bin, freq) %>%
          dplyr::rename(!!glue("{var}_freq") := freq),
        by = "bin"
      )
  }

  df_freq[is.na(df_freq)] <- 0

  return(df_freq)
}

#################################################################################
# fonction qui va donner l'AEP pour chaque année où chaque puissance en pondérant
# par la distribution
###############################################################################


AEP_WT_pond <- function(name = name) {
  df_name <- readRDS(glue("./2-Inputs/{name}/df_{name}_cleaned_merge.rds"))

  debut <- min(year(df_name$TimeStamp))
  fin <- max(year(df_name$TimeStamp))

  # on récupère les P_LT non mergé et les distributions
  df_non_filt <- df_name%>%
    select(starts_with("P_SLT"),"P_scada")

  variables <- c(names(df_non_filt))
  df_freq <- bin_freq(name)

  # Définir les bornes des bins
  max_val <- max(df_non_filt, na.rm = TRUE)
  breaks <- seq(0, (floor(max_val / 100) + 5) * 100, by = 100)
  labels <- glue("[{head(breaks,-1)}-{tail(breaks, -1)})")

  df_AEP <- data.frame(year = debut:fin)

  for (i in seq(debut:fin)) {
    # on filtre par année
    df <- df_name %>%
      filter(year(TimeStamp) == debut + i - 1)

    # on détermine la proportion d'année pour chaque année
    annee_complete <- 24 * 365
    prop_annee <- nrow(df) / annee_complete

    # on rajoute les tranches d'appartenance pour chaque puissance
    for (var in variables) {
      bin_col <- glue("{var}_bin")
      df <- df %>%
        mutate(!!bin_col := cut(.data[[var]],
                                breaks = breaks,
                                labels = labels,
                                include.lowest = TRUE,
                                right = FALSE
        ))
    }

    for (j in seq_along(variables)) {
      var <- variables[j]
      var_temp <- str_sub(var, 3, ) # pour avoir les noms adéquats après

      df_temp <- df %>%
        dplyr::select(var, glue("{var}_bin")) %>%
        group_by(bin = .data[[glue("{var}_bin")]]) %>%
        summarise(
          energie_bin = sum(.data[[var]], na.rm = TRUE),
          count_bin = n(), # on compte le nombre de puissance par bin
          .groups = "drop"
        ) %>%
        left_join(df_freq, by = setNames("bin", "bin")) %>%
        mutate(
          freq = .data[[glue("{var}_freq")]],
          energie_ponderee = (energie_bin * freq) / count_bin
        )
      df_AEP[[glue("AEP_{var_temp}")]][i] <- round(sum(df_temp$energie_ponderee, na.rm = T) * nrow(df) / prop_annee / 1e6, 3)
    }
  }
  return(df_AEP)
}

#######################################################################
# fonction donnant l'AEP moyen pondéré par la distribution pour chacune des puissances
########################################################################
#' Annual Energy Production (AEP) Weighted by Bin Distributions (Yearly)
#'
#' This function computes the annual energy production (AEP) for a given wind turbine,
#' weighted by the empirical frequency distribution of power bins (100 kW bins).
#'
#' It loops through each year of SCADA data, computes energy per power bin, and
#' reweights these based on long-term frequency distributions obtained from `bin_freq()`.
#'
#' @param name Character. Name of the wind turbine.
#'
#' @return A data frame with yearly AEP values for each power variable (e.g. `P_SLT_x` or `P_scada`),
#' weighted by their respective power bin distributions. One row per year.
#'
#' @details
#' - Power values are binned into fixed 100 kW intervals.
#' - For each power variable, energy per bin is scaled by the long-term frequency of that bin.
#' - Results are normalized by the proportion of the year with available data.
#'
#' @seealso \code{\link{AEP_WT_pond_moy}}, \code{\link{bin_freq}}
#'
#' @import dplyr
#' @import lubridate
#' @import glue
#' @import stringr
#'
#' @examples
#' \dontrun{
#' AEP_WT_pond("PRP")
#' }
#'
#' @export

AEP_WT_pond_moy <- function(name = name) {
  df <- readRDS(glue("./2-Inputs/{name}/df_{name}_cleaned_merge.rds"))

  debut <- min(year(df$TimeStamp))
  fin <- max(year(df$TimeStamp))

  # on récupère les P_LT non mergé et les distributions
  df_freq <- bin_freq(name)
  df_non_filt <- df%>%
    dplyr::select(starts_with("P_SLT"),"P_scada")
  variables <- c(names(df_non_filt))

  # Définir les bornes des bins
  max_val <- max(df_non_filt, na.rm = TRUE)
  breaks <- seq(0, (floor(max_val / 100) + 5) * 100, by = 100)
  labels <- glue("[{head(breaks,-1)}-{tail(breaks, -1)})")

  df_AEP <- data.frame(Eolienne = glue("{name}"))

  # on détermine la proportion d'année pour chaque année
  annee_complete <- 24 * 365
  prop_annee <- nrow(df) / annee_complete

  # on rajoute les tranches d'appartenance pour chaque puissance
  for (var in variables) {
    bin_col <- glue("{var}_bin")
    df <- df %>%
      mutate(!!bin_col := cut(.data[[var]],
                              breaks = breaks,
                              labels = labels,
                              include.lowest = TRUE,
                              right = FALSE
      ))
  }
  #assign(glue("df_{name}_cm_bin"), df, envir = .GlobalEnv)

  for (j in seq_along(variables)) {
    var <- variables[j]
    var_temp <- str_sub(var, 3, ) # pour avoir les noms adéquats après
    df_temp2 <- df %>%
      dplyr::select(var, glue("{var}_bin")) %>%
      dplyr::group_by(bin = .data[[glue("{var}_bin")]]) %>%
      dplyr::summarise(
        energie_bin = sum(.data[[var]], na.rm = TRUE),
        count_bin = n(), # <- ici tu comptes le nombre de points par bin
        .groups = "drop"
      ) %>%
      dplyr::left_join(df_freq, by = setNames("bin", "bin")) %>%
      dplyr::mutate(
        freq = .data[[glue("{var}_freq")]],
        energie_ponderee = (energie_bin * freq) / count_bin
      )

    df_AEP[[glue("AEP_{var_temp}")]][1] <- round(sum(df_temp2$energie_ponderee, na.rm = T) * nrow(df) / prop_annee / 1e6, 3)
  }
  return(df_AEP)
}








