
#############################
#Figure M1 :
#############################
#' Generate Normalized Wind Power Adjustment and Reference Turbulence Simulation
#'
#' This function processes a dataframe of normalized wind turbine data to compute
#' a turbulence-corrected power curve reference. It uses helper functions `figure_M2` and `figure_M4`
#' to generate intermediate data and a power adjustment function.
#'
#' @param df_norm Data frame. Must contain the columns:
#'   - `ws_hub_norm`: normalized hub wind speed,
#'   - `ti_hub`: turbulence intensity at the hub,
#'   - `P`: power output.
#'
#' @details
#' The function first checks if the required columns exist in `df_norm`.
#' Then it retrieves a zero-turbulence power adjustment function from `figure_M4`
#' and binned wind speed data from `figure_M2`.
#'
#' It calculates the reference turbulence intensity as the mean of `ti_hub`.
#' Then, for each row, it simulates wind speed samples using a normal distribution centered
#' on the normalized wind speed with standard deviation proportional to turbulence.
#' These samples are fed through the zero-turbulence power function, and the mean predicted power
#' under reference turbulence is calculated.
#'
#' Finally, it computes a corrected power variable `P_Iref`, bins it by wind speed,
#' and creates an interpolation function over the binned power.
#'
#' The function returns a list containing:
#' - `df_bin`: data frame with binned wind speed and corrected power,
#' - `P_Iref_fun`: interpolation function of corrected power vs wind speed,
#' - `I_ref`: the reference turbulence intensity.
#'
#' @return
#' A list with elements:
#' - `df_bin`: Data frame with wind speed bins and corrected power,
#' - `P_Iref_fun`: function interpolating power by wind speed,
#' - `I_ref`: numeric reference turbulence intensity value.
#'
#' @examples
#' \dontrun{
#' result <- figure_M1(df_norm)
#' plot(result$df_bin$ws, result$df_bin$P_Iref, type = "b")
#' }
#'
#' @export

figure_M1 <- function(df_norm=df_norm){
  if (!all(c("ws_hub_norm", "ti_hub", "P") %in% names(df_norm))) {
    stop("❌ df_norm doit contenir les variables : ws_hub_norm (vitesse de vent normalisée), ti_hub (intensité de turbulence) et P (puissance).")
  }

  P0_ajust_fun <- figure_M4(df_norm)[['P0_ajust_fun']]
  df_bin <- figure_M2(df_norm)

  IT_ref <- mean(df_norm$ti_hub,na.rm=T)
  print(IT_ref)

  df_norm$I_ref <- IT_ref

  df_norm$P_sim_Iref <- mapply(function(v, t) {
    f_i <- rnorm(10000, v, t * v)
    mean(P0_ajust_fun(f_i))
  }, df_norm$ws_hub_norm, df_norm$I_ref)

  P_Iref <- df_norm$P*1000 - df_norm$P_sim + df_norm$P_sim_Iref

  breaks <- seq(0, 25, 0.5)
  bin <- cut(df_norm$ws_hub_norm, breaks = breaks, include.lowest = FALSE, right = FALSE)
  binned_P_Iref <- aggregate(P_Iref, by = list(bin), FUN = mean,na.rm=TRUE)

  df_bin$P_Iref <- binned_P_Iref[,2]
  P_Iref_fun <- approxfun(x=df_bin$ws,df_bin$P_Iref)

  return(list(df_bin=df_bin,P_Iref_fun=P_Iref_fun,I_ref = IT_ref))
}


#' Plot Normalized Power Curve with Reference Turbulence and Manufacturer Curve
#'
#' This function visualizes the wind turbine power curve corrected for a reference turbulence intensity,
#' alongside the manufacturer's power curve for comparison.
#'
#' @param df_norm Data frame containing normalized wind turbine data.
#'   Must include columns:
#'   - `ws_hub_norm`: normalized hub wind speed,
#'   - `ti_hub`: turbulence intensity at the hub,
#'   - `P`: power output.
#' @param df_cons Data frame containing the manufacturer’s power curve data,
#'   with columns `V` (wind speed) and `P` (power).
#'
#' @details
#' The function checks that `df_norm` contains the required variables.
#' It calls `figure_M1` to compute the power curve corrected to the reference turbulence intensity.
#' Then it plots:
#' - The corrected power curve (`P_Iref`) versus wind speed,
#' - The manufacturer’s power curve for comparison.
#'
#' The plot uses different colors and transparency for clarity.
#'
#' @return
#' A ggplot object displaying the curves.
#'
#' @examples
#' \dontrun{
#' plot_M1(df_norm, df_cons)
#' }
#'
#' #' @import ggplot2
#'
#' @export
plot_M1 <- function(df_norm=df_norm,df_cons=df_cons){
  if (!all(c("ws_hub_norm", "ti_hub", "P") %in% names(df_norm))) {
    stop("❌ df_norm doit contenir les variables : ws_hub_norm (vitesse de vent normalisée), ti_hub (intensité de turbulence) et P (puissance).")
  }
  list_M1 <- figure_M1(df_norm)
  df_bin <- list_M1[['df_bin']]
  I_ref_label <- glue("PC I ref = {list_M1[['I_ref']]}")
  ggplot()+
    geom_line(data=df_bin,aes(x=ws,y=P_Iref/1000, color=I_ref_label),linewidth=1,alpha=0.5)+
    geom_line(data=df_cons,aes(x=V,y=P,color="PC Constructeur"),linewidth=1,alpha=0.5)+
    labs(x="Wind Speed (m/s)",
         y="Puissance (kW)",
         title="Courbes de puissance constructeur et pour l'intensité de turbulence de référence",
         subtitle = "Intensité de turbulence de référence = moyenne de l'intensité de turbulence")
}


################
###Figure M2
################
#' Calculate Binned Wind Data and Power Coefficient
#'
#' This function bins the normalized wind speed (`ws_hub_norm`) into intervals,
#' then computes the average wind speed, turbulence intensity, power (in Watts),
#' and the power coefficient (Cp) per bin.
#'
#' @param df_norm Data frame containing normalized wind turbine data.
#'   Must include columns:
#'   - `ws_hub_norm`: normalized hub wind speed,
#'   - `ti_hub`: turbulence intensity at the hub,
#'   - `P`: power output (assumed in kW).
#'
#' @details
#' The function bins `ws_hub_norm` into 0.5 m/s intervals from 0 to 25 m/s.
#' It computes the mean values of wind speed, turbulence intensity, and power in each bin.
#' Power is converted from kW to Watts by multiplying by 1000.
#' The power coefficient Cp is calculated using the formula:
#' \[
#' Cp = \frac{2P}{\rho \cdot A \cdot ws^3}
#' \]
#' where `rho` is air density and `A` is rotor swept area.
#'
#' **Note:** `rho` and `A` should be defined and accessible in the function’s environment.
#'
#' @return A tibble with columns:
#'   - `bin`: wind speed bin factor,
#'   - `ws`: mean wind speed (m/s) per bin,
#'   - `ti`: mean turbulence intensity per bin,
#'   - `P`: mean power (W) per bin,
#'   - `cp`: power coefficient per bin.
#'
#' @examples
#' \dontrun{
#' df_binned <- figure_M2(df_norm)
#' }
#'
#' @import dplyr
#'
#' @export
figure_M2 <- function(df_norm=df_norm){
  if (!all(c("ws_hub_norm", "ti_hub", "P") %in% names(df_norm))) {
    stop("❌ df_norm doit contenir les variables : ws_hub_norm (vitesse de vent normalisée), ti_hub (intensité de turbulence) et P (puissance).")
  }
  breaks <- seq(0, 25, 0.5)

  rho <- as.numeric(rho[1])
  A <- as.numeric(A[1])

  df_bin <- df_norm %>%
    dplyr::mutate(
      bin = cut(ws_hub_norm, breaks = breaks, include.lowest = FALSE, right = FALSE)
    ) %>%
    dplyr::group_by(bin) %>%
    dplyr::summarise(
      ws = mean(ws_hub_norm, na.rm = TRUE),
      ti = mean(ti_hub, na.rm = TRUE),
      P = mean(P, na.rm = TRUE) * 1000,  # W
      .groups = "drop"
    ) %>%
    mutate( cp = 2 * P / (rho * ws^3 * A))

  return(df_bin)
}


######################################
#Figure M3
######################################

#' Initialize Zero-Turbulence Power Curve Parameters
#'
#' This function computes initial zero-turbulence power curve parameters based on
#' binned normalized wind data. It estimates key turbine characteristics such as
#' rated power, maximum power coefficient, cut-in and rated wind speeds, and constructs
#' a theoretical zero-turbulence power curve.
#'
#' @param df_norm A data frame containing normalized wind turbine data.
#'   Required columns are:
#'   - `ws_hub_norm`: normalized wind speed at the hub,
#'   - `ti_hub`: turbulence intensity at the hub,
#'   - `P`: power output (in kW).
#'
#' @details
#' The function calls `figure_M2` to bin the data and compute average power and Cp.
#' Then it calculates:
#' - `P_rated`: the maximum power observed,
#' - `cp_max`: the maximum power coefficient,
#' - `v_cut_in`: the cut-in wind speed (lowest wind speed with power above 0.1% rated),
#' - `v_rated`: the rated wind speed, derived from the power equation,
#' - `v_cut_out`: set to 100 m/s as a placeholder.
#'
#' Using these parameters, it builds an initial zero-turbulence power curve as a
#' data frame with power values computed via the standard cubic law between cut-in
#' and rated speeds, zero below cut-in and above cut-out, and flat at rated power
#' beyond rated speed.
#'
#' It returns both the data frame of the power curve and an interpolation function.
#'
#' @return A list containing:
#' - `P0_init`: data frame with wind speeds `v`, power `P` (W), and power coefficient `cp`,
#' - `P0_init_fun`: interpolation function for the power curve,
#' - `df_bin`: the binned data from `figure_M2`.
#'
#' @examples
#' \dontrun{
#' result <- figure_M3(df_norm)
#' plot(result$P0_init$v, result$P0_init$P, type = "l")
#' }
#'
#' @import dplyr
#'
#' @export
figure_M3 <- function(df_norm=df_norm){
  if (!all(c("ws_hub_norm", "ti_hub", "P") %in% names(df_norm))) {
    stop("❌ df_norm doit contenir les variables : ws_hub_norm (vitesse de vent normalisée), ti_hub (intensité de turbulence) et P (puissance).")
  }
  df_bin <- figure_M2(df_norm)

  rho <- as.numeric(rho[1])
  A <- as.numeric(A[1])

  #Initial zero-turb PC parameters
  P_rated <- max(df_bin$P, na.rm = TRUE)
  cp_max <- max(df_bin$cp, na.rm = TRUE)
  v_cut_in <- min(df_bin$ws[df_bin$P > 0.001 * P_rated], na.rm = TRUE)
  v_rated <- as.numeric((2 * P_rated / (rho * cp_max * A))^(1 / 3))
  v_cut_out <- 100

  P0_init <- data.frame ( v = seq ( 0, 100, 0.1)) %>%
    dplyr::mutate (
      P = case_when (
        v < v_cut_in | v >= v_cut_out ~ 0,
        between ( v, v_cut_in , v_rated) ~ (0.5 * rho * cp_max * A * v^3),
        between ( v, v_rated, v_cut_out) ~ P_rated
      ),
      cp = (2 * P) / ( rho * v ^ 3 * A),
    )

  #approximation linéaire de P0_init
  P0_init_fun <- approxfun (x = P0_init$v, y = P0_init$P)

  return(list(P0_init=P0_init, P0_init_fun = P0_init_fun,df_bin=df_bin))
}


#' Plot Measured and Initial Zero-Turbulence Power Curves and Cp
#'
#' This function plots:
#' - Measured power (P) vs. wind speed,
#' - Initial zero-turbulence power curve (P0_init),
#' - Measured power coefficient (cp),
#' - Initial zero-turbulence power coefficient curve (cp0_init),
#' on a dual y-axis plot.
#'
#' @param df_norm Data frame containing normalized turbine data.
#'   Must include columns: ws_hub_norm (normalized wind speed),
#'   ti_hub (turbulence intensity), and P (power in kW).
#'
#' @return A ggplot object with the plotted curves.
#'
#' @import ggplot2
#'
#' @export
plot_M3 <- function(df_norm=df_norm){
  if (!all(c("ws_hub_norm", "ti_hub", "P") %in% names(df_norm))) {
    stop("❌ df_norm doit contenir les variables : ws_hub_norm (vitesse de vent normalisée), ti_hub (intensité de turbulence) et P (puissance).")
  }
  #récupération des objets
  res_M3 <- figure_M3(df_norm)
  df_bin <- res_M3[['df_bin']]
  P0_init <- res_M3[['P0_init']]

  #graphique
  ggplot() +
    geom_point(data = df_bin, aes(x = ws, y = P/1000, color = "P mesurée"), shape = 4) +
    geom_line(data = df_bin, aes(x = ws, y = P/1000, color = "P mesurée"), linewidth = 1) +
    geom_line(data = P0_init, aes(x = v, y = P/1000, color = "P0_init"), linewidth = 1) +
    geom_point(data = df_bin, aes(x = ws, y = cp * 1.1 * max(df_bin$P) /1000, color = "Cp mesuré"), shape = 4) +
    geom_line(data = df_bin, aes(x = ws, y = cp * 1.1 * max(df_bin$P) /1000, color = "Cp mesuré"), linewidth = 1) +
    geom_line(data = P0_init, aes(x = v, y = cp * 1.1 * max(df_bin$P) /1000, color = "Cp0_init"), linewidth = 1) +
    scale_y_continuous(
      name = "Puissance (kWh)",
      sec.axis = sec_axis(~ . / (1.1 * max(df_bin$P) /1000), name = "Power coefficient cp"),
      limits = c(0, 1.1 * max(df_bin$P/1000))  # Appliquer directement ici
    ) +
    scale_x_continuous(limits = c(0, 30), name = "Wind Speed (m/s)") +
    theme_minimal()
}


############################
#Figure M4 : courbe de puissance théorique avec zéro-turbulence

##########################
#initialisation

#' Estimate Adjusted Zero-Turbulence Power Curve with Iterative Parameter Tuning
#'
#' This function computes an adjusted zero-turbulence power curve (IT=0) by iteratively
#' adjusting the rated power and power coefficient to match the measured data.
#' It simulates power curves considering turbulence intensity distribution and updates parameters
#' until convergence criteria are met or a maximum number of iterations is reached.
#'
#' @param df_norm Data frame containing normalized turbine data.
#'   Must include columns:
#'   - ws_hub_norm: normalized wind speed at hub height,
#'   - ti_hub: turbulence intensity at hub height,
#'   - P: power output (in kW).
#'
#' @return A list containing:
#'   - P0_ajust_fun: the adjusted zero-turbulence power curve function,
#'   - df_bin: binned data frame with measured and simulated power and Cp,
#'   - v_cut_in_ajust: adjusted cut-in wind speed,
#'   - cp_th: adjusted theoretical power coefficient,
#'   - P_rated_th: adjusted rated power,
#'   - v_rated_th: adjusted rated wind speed.
#'
#'@export

figure_M4 <- function(df_norm=df_norm){
  if (!all(c("ws_hub_norm", "ti_hub", "P") %in% names(df_norm))) {
    stop("❌ df_norm doit contenir les variables : ws_hub_norm (vitesse de vent normalisée), ti_hub (intensité de turbulence) et P (puissance).")
  }

  df_bin <- figure_M2(df_norm)

  rho <- as.numeric(rho[1])
  A <- as.numeric(A[1])

  #valeurs d'initialisation
  P_rated <- max(df_bin$P, na.rm = TRUE)
  P_rated_th <- P_rated
  cp_th = max(df_bin$cp,na.rm=T)
  cp_max <- max(df_bin$cp, na.rm = TRUE)
  v_cut_in <- min(df_bin$ws[df_bin$P > 0.001 * P_rated], na.rm = TRUE)
  v_rated <- as.numeric((2 * P_rated / (rho * cp_max * A))^(1 / 3))
  v_cut_out <- 100


  P0_init_fun <- figure_M3(df_norm)[['P0_init_fun']]

  #Simulated power for all bin-averaged data:
  df_bin$P_sim <- 0
  df_bin$P_sim <- mapply(function(v, t) {
    f_i <- rnorm(10000, v, t * v)
    mean(P0_init_fun(f_i))
  }, df_bin$ws, df_bin$ti)

  df_bin$P_sim[is.na(df_bin$P_sim)] <- 0

  df_bin$cp_sim <- 2*df_bin$P_sim / (rho*df_bin$ws^3*A)

  v_cut_in_sim <- min(df_bin$ws[df_bin$P_sim > 0.001 * P_rated], na.rm = TRUE)


  nb_boucle <- 0

  test <- (abs(max(df_bin$P_sim) - P_rated) < 0.001 * P_rated &&
             abs(v_cut_in_sim - v_cut_in) < 0.5 &&
             abs(max(df_bin$cp_sim) - max(df_bin$cp)) < 0.01
  )
  #choix arbitraire du nombre de tour ici, l'algo ne convergeant pas sinon
  while (test == FALSE && nb_boucle < 10){
    nb_boucle <- nb_boucle + 1

    #MAJ des paramètres
    P_rated_th <- P_rated_th - max(df_bin$P_sim) + P_rated
    print(glue("P_rated_th={P_rated_th}"))

    cp_th <- cp_th - max(df_bin$cp_sim) + max(df_bin$cp)
    print(glue("cp_th={cp_th}"))

    #MAJ de la v_rated
    v_rated_th = ((2*P_rated_th) / (rho * cp_th * A))^(1/3)
    print(glue("v_rated_th={v_rated_th}"))

    #MAJ courbe ajuste IT=0
    P0_ajust <- data.frame ( v = seq ( 0, 100, 0.1)) %>%
      mutate (
        P = case_when (
          v < v_cut_in_sim | v > v_cut_out ~ 0,
          between ( v, v_cut_in_sim , v_rated_th) ~ (0.5 * 1.225 * cp_th * A * v^3),
          between ( v, v_rated_th, v_cut_out) ~ P_rated_th
        )
      )

    P0_ajust_fun <- approxfun (x = P0_ajust$v, y = P0_ajust$P)

    df_bin$P_sim <- mapply(function(v, t) {
      f_i <- rnorm(10000, v, t * v)
      mean(P0_ajust_fun(f_i))
    }, df_bin$ws, df_bin$ti)

    df_bin$P_sim[is.na(df_bin$P_sim)] <- 0

    #MAJ des valeurs simulées
    df_bin$cp_sim <- 2*df_bin$P_sim / (rho*df_bin$ws^3*A)
    v_cut_in_sim <- min(df_bin$ws[df_bin$P_sim > 0.001 * P_rated_th], na.rm = TRUE)
  }
  print(nb_boucle)
  return(list(P0_ajust_fun = P0_ajust_fun,
              df_bin = df_bin,
              v_cut_in_ajust = v_cut_in_sim,
              cp_th = cp_th,
              P_rated_th = P_rated_th,
              v_rated_th = v_rated_th))
}

####################################
#graphe entre initiale et ajustée
####################################

#' Plot Adjusted Zero-Turbulence Power Curve and Power Coefficient
#'
#' This function plots the initial and adjusted zero-turbulence power curves (PC)
#' and their corresponding power coefficients (Cp) based on normalized wind speed,
#' turbulence intensity, and power data. It uses helper functions \code{figure_M3}
#' and \code{figure_M4} to compute the power curves and adjustment parameters.
#'
#' @param df_norm A data frame containing at least the following columns:
#' \code{ws_hub_norm} (normalized wind speed), \code{ti_hub} (turbulence intensity),
#' and \code{P} (power).
#'
#' @return A \code{ggplot} object showing:
#' \itemize{
#'   \item Initial zero-turbulence power curve,
#'   \item Adjusted zero-turbulence power curve,
#'   \item Initial power coefficient curve,
#'   \item Adjusted power coefficient curve.
#' }
#' The x-axis represents wind speed (m/s), and the primary y-axis power (kW).
#' The secondary y-axis shows the power coefficient (Cp).
#'
#' @examples
#' \dontrun{
#' plot_M4(df_norm)
#' }
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom stats approxfun
#' @export

plot_M4 <- function(df_norm = df_norm){
  if (!all(c("ws_hub_norm", "ti_hub", "P") %in% names(df_norm))) {
    stop("❌ df_norm doit contenir les variables : ws_hub_norm (vitesse de vent normalisée), ti_hub (intensité de turbulence) et P (puissance).")
  }
  #Récupération des résultats
  res_M3 <- figure_M3(df_norm)
  P0_init <- res_M3[['P0_init']]
  df_bin <- res_M3[['df_bin']]
  res_M4 <- figure_M4(df_norm)
  P0_ajust_fun <- res_M4[['P0_ajust_fun']]
  v_cut_in_ajust <- res_M4[['v_cut_in_ajust']]
  cp_th <- res_M4[['cp_th']]
  P_rated_th <- res_M4[['P_rated_th']]
  v_rated_th = res_M4[['v_rated_th']]
  v_cut_out = 100

  df <- data.frame(v = seq ( 0, 100, 0.1)) %>%
    dplyr::mutate(P0_ajust = sapply(v,P0_ajust_fun),
           cp_ajust = case_when (
          v < v_cut_in_ajust | v > v_cut_out ~ 0,
          between ( v, v_cut_in_ajust , v_rated_th) ~ cp_th,
          between ( v, v_rated_th, v_cut_out) ~ (2*P_rated_th)/(rho*A*v^3)
          )
    )

  ggplot()+
    geom_line(data=P0_init,aes(x=v,y=P/1000,color='PC0 initiale'),linewidth=1) +
    geom_line(data=df,aes(v,P0_ajust/1000,color='PC0 ajustée'),linewidth=1) +
    geom_line(data=P0_init,aes(x=v,cp * 1.1 * max(P) / 1000,color='cp initial'),linewidth=1) +
    geom_line(data=df,aes(x=v,y=cp_ajust * 1.1 * max(P0_init$P) / 1000,color='cp ajusté'),linewidth=1) +
    labs(x="Vitesse du vent (m/s)",
         y="Puissance (kW)")+
    scale_y_continuous(
      name = "Puissance (kWh)",
      sec.axis = sec_axis(~ . / (1.1 * max(P0_init$P) /1000), name = "Power coefficient cp"),
      limits = c(0, 1.1 * max(df_bin$P/1000)))+  # Appliquer directement ici
    theme_minimal()+
    xlim(0,20)
}



################################################
#Figure M6
##############################################
#' Compute Adjusted Zero-Turbulence Power Output and Binned Averages
#'
#' This function calculates an adjusted zero-turbulence power output (P0) for each observation
#' in the input data frame by correcting the measured power with a simulated power curve adjusted
#' for turbulence intensity effects. It uses the power curve adjustment function returned by \code{figure_M4}.
#'
#' @param df_norm A data frame containing at least the following columns:
#' \code{ws_hub_norm} (normalized wind speed), \code{ti_hub} (turbulence intensity),
#' and \code{P} (power in kW).
#'
#' @return A data frame with binned statistics including:
#' \itemize{
#'   \item \code{ws} - mean wind speed per bin,
#'   \item \code{ti} - mean turbulence intensity per bin,
#'   \item \code{P} - mean measured power (in Watts) per bin,
#'   \item \code{cp} - power coefficient computed per bin,
#'   \item \code{P0} - mean adjusted zero-turbulence power output per bin.
#' }
#'
#' @details
#' The adjusted zero-turbulence power output \code{P0} is computed by removing the turbulence
#' effect from the measured power and adding the simulated power from the adjusted power curve
#' at the observed wind speeds.
#'
#' The data is binned in wind speed intervals of 0.5 m/s from 0 to 25 m/s for aggregation.
#'
#' @examples
#' \dontrun{
#' df_bin <- figure_M6(df_norm)
#' }
#'
#' @import dplyr
#' @export
figure_M6 <- function(df_norm=df_norm){
  if (!all(c("ws_hub_norm", "ti_hub", "P") %in% names(df_norm))) {
    stop("❌ df_norm doit contenir les variables : ws_hub_norm (vitesse de vent normalisée), ti_hub (intensité de turbulence) et P (puissance).")
  }
  P0_ajust_fun <- figure_M4(df_norm)[['P0_ajust_fun']]

  df_norm$P_sim <- 0
  df_norm$P_sim <- mapply(function(v, t) {
    f_i <- rnorm(10000, v, t * v)
    mean(P0_ajust_fun(f_i))
  }, df_norm$ws_hub_norm, df_norm$ti_hub)


  df_norm$P0 <- df_norm$P*1000 - df_norm$P_sim + P0_ajust_fun(df_norm$ws_hub_norm)

  #on crée df_bin
  df_bin <- figure_M2(df_norm)

  breaks <- seq(0, 25, 0.5)
  df_P0_bin <- df_norm %>%
    dplyr::mutate(bin = cut(ws_hub_norm, breaks = breaks, include.lowest = FALSE, right = FALSE)) %>%
    dplyr::group_by(bin) %>%
    dplyr::summarise(P0 = mean(P0, na.rm = TRUE), .groups = "drop")

  df_bin <- df_bin %>%
    dplyr::left_join(df_P0_bin, by = "bin")

  return(df_bin)
}

#' Plot Adjusted Zero-Turbulence Power Curve
#'
#' This function plots the adjusted zero-turbulence power curve (P0)
#' binned by wind speed, computed by the \code{figure_M6} function.
#'
#' @param df_norm A data frame containing at least the following columns:
#' \code{ws_hub_norm} (normalized wind speed), \code{ti_hub} (turbulence intensity),
#' and \code{P} (power in kW).
#'
#' @return A \code{ggplot2} object displaying the adjusted zero-turbulence power curve.
#'
#' @examples
#' \dontrun{
#' plot_M6(df_norm)
#' }
#'
#' @import ggplot2
#' @export
plot_M6 <- function(df_norm){
  if (!all(c("ws_hub_norm", "ti_hub", "P") %in% names(df_norm))) {
    stop("❌ df_norm doit contenir les variables : ws_hub_norm (vitesse de vent normalisée), ti_hub (intensité de turbulence) et P (puissance).")
  }
  df_bin <- figure_M6(df_norm)
  ggplot()+
    geom_point(data=df_bin,aes(x=ws,y=P0/1000,color="P0"),shape=4)+
    geom_line(data=df_bin,aes(x=ws,y=P0/1000,color="P0"),linewidth=1)+
    ggtitle('Courbe de puissance sans turbulence P0')
}


##################################
#Comparaison modèle 0 turbu et bin
####################################

#' Calculate Theoretical Power Curve (P0_th) for a Wind Turbine
#'
#' Computes the theoretical power output of a wind turbine based on the given parameters:
#' power coefficient at rated speed, rated wind speed, rated power, cut-in and cut-out speeds,
#' air density, rotor swept area, and wind speed vector.
#'
#' @param cp_rated Numeric. Power coefficient at rated wind speed (dimensionless, typically ≤ 0.5).
#' @param v_rated Numeric. Rated wind speed (m/s) where turbine reaches rated power.
#' @param P_rated_th Numeric. Rated power of the turbine (Watts).
#'
#' @return A data.frame with columns:
#' \describe{
#'   \item{v}{Wind speed values (m/s).}
#'   \item{P}{Theoretical power output at each wind speed (Watts).}
#' }
#'
#' @examples
#' \dontrun{
#' ws <- seq(0, 30, by = 0.1)
#' P_curve <- P0_th(cp_rated = 0.45, v_rated = 12, P_rated_th = 3000000)
#' }
#'
#' @export
P0_th <- function(cp_rated, v_rated, P_rated_th) {
  df_P0 <- data.frame(v = df_norm$ws_hub_norm) %>%
    mutate(
      P = case_when(
        v < v_cut_in | v >= v_cut_out ~ 0,
        v >= v_cut_in & v < v_rated ~ (0.5 * rho * cp_rated * A * v^3 ),
        v >= v_rated & v < v_cut_out ~ P_rated_th
      )
    )
  return(df_P0)
}


#calcul du rmse entre P_CMV et P0 selon la valeur de cp_rated
#' Calculate RMSE Between Theoretical and Measured Power Curves for Multiple Parameters
#'
#' This function evaluates the Root Mean Squared Error (RMSE) between observed power data and
#' theoretical power curves generated for different combinations of power coefficient (`cp_rated`)
#' and rated wind speed (`v_rated`). It returns the RMSE table, the optimal parameters minimizing
#' RMSE, and the corresponding theoretical power curve.
#'
#' @param cp_rated Numeric vector. Vector of candidate power coefficients at rated wind speed.
#' @param v_rated Numeric vector. Vector of candidate rated wind speeds (m/s). (Note: computed inside the function based on `cp_rated` and other constants).
#' @param P_rated_th Numeric. Rated power of the turbine (Watts).
#'
#' @return A list containing:
#' \describe{
#'   \item{rmse}{Data frame with rows: `cp_rated`, `v_rated`, and computed `rmse` values for each parameter combination.}
#'   \item{cp_opt}{Optimal power coefficient minimizing RMSE.}
#'   \item{v_rated_opt}{Optimal rated wind speed corresponding to `cp_opt`.}
#'   \item{df_P0}{Data frame of the theoretical power curve computed with optimal parameters.}
#' }
#'
#' @details
#' The function uses the helper function `P0_th()` to compute theoretical power curves.
#' The RMSE is computed against a global data frame `df_norm` assumed to be in the environment,
#' which must contain measured power values in the column `P`.
#'
#' @examples
#' \dontrun{
#' cp_candidates <- seq(0.3, 0.5, length.out = 10)
#' results <- result_rmse(cp_rated = cp_candidates, v_rated = rep(NA, 10), P_rated_th = 3000000)
#' print(results$cp_opt)
#' plot(results$df_P0$v, results$df_P0$P / 1000, type = "l",
#'      xlab = "Wind speed (m/s)", ylab = "Power (kW)", main = "Optimal Theoretical Power Curve")
#' }
#'
#' @export
result_rmse <- function(cp_rated = cp_rated, v_rated = v_rated, P_rated_th = P_rated_th) {

  # Calcul des courbes théoriques pour chaque combinaison cp/v_rated
  resultat_list <- mapply(
    P0_th,
    cp_rated,
    v_rated,
    MoreArgs = list(P_rated_th = P_rated_th),
    SIMPLIFY = FALSE
  )

  # Initialisation du tableau de résultats
  rmse <- data.frame(matrix(nrow = 3, ncol = length(resultat_list)))
  colnames(rmse) <- glue::glue("coeff={cp_rated}")
  rownames(rmse) <- c("cp_rated", "v_rated", "rmse")

  # Calcul du RMSE pour chaque combinaison
  for (i in seq_along(resultat_list)) {
    rmse[1, i] <- cp_rated[i]
    rmse[2, i] <- ((2 * P_rated_th) / (rho * A * cp_rated[i]))^(1/3)
    rmse[3, i] <- sqrt(mean((df_norm$P - resultat_list[[i]]$P/1000)^2, na.rm = TRUE))
  }

  # Recherche du minimum
  best_index <- which.min(rmse[3, ])
  cp_opt <- rmse[1, best_index]
  v_rated_opt <- rmse[2, best_index]

  # Recalcul de la courbe P0 optimale
  df_P0 <- P0_th(cp_rated = cp_opt, v_rated = v_rated_opt, P_rated_th = P_rated_th)

  return(list(
    rmse = rmse,
    cp_opt = cp_opt,
    v_rated_opt = v_rated_opt,
    df_P0 = df_P0
  ))
}



#tracé de la figure cp_opt et v_opt
#' Plot Optimal Power Curve with Binned and Manufacturer Data
#'
#' This function computes the optimal theoretical power curve by minimizing the RMSE
#' between observed and modeled data, then plots it alongside binned observed power
#' data and manufacturer power curve data.
#'
#' @param cp_rated Numeric vector. Candidate power coefficients at rated wind speed.
#' @param v_rated Numeric vector. Candidate rated wind speeds (m/s).
#' @param P_rated_th Numeric. Rated power of the turbine (Watts).
#'
#' @return A ggplot object showing:
#' \itemize{
#'   \item The binned observed power curve (`CP binning`)
#'   \item The manufacturer’s power curve (`CP Constructeur`)
#'   \item The optimal theoretical zero-turbulence power curve (`CP TI=0 théorique`)
#' }
#'
#' @details
#' This function assumes the existence of the global data frames:
#' - `df_norm` containing observed wind speed and power data
#' - `df_cons` containing the manufacturer power curve data
#'
#' The `binning()` function is used to aggregate observed data into wind speed bins.
#'
#' @examples
#' \dontrun{
#' cp_candidates <- seq(0.3, 0.5, length.out = 10)
#' v_candidates <- rep(NA, 10) # or estimated values
#' rated_power <- 3000000
#' plot_opt(cp_candidates, v_candidates, rated_power)
#' }
#'
#' @export
plot_opt <- function(cp_rated = cp_rated, v_rated = v_rated,P_rated_th=P_rated_th) {
  res <- result_rmse(cp_rated = cp_rated, v_rated = v_rated,P_rated_th=P_rated_th)
  df_P0 <- res[['df_P0']]
  df_bin <- binning(df_norm$ws_hub_norm, df_norm$P, start = 0, stop = 20, pas = 0.5)[['df_bin']]

  ggplot() +
    geom_line(data = df_bin, aes(x = V, y = P, color = "CP binning"),linewidth=1)+
    geom_line(data = df_cons, aes(x = V, y = P, color = "CP Constructeur"),linewidth=1) +
     geom_line(data = df_P0, aes(x = v, y = P/1000, color = "CP TI=0 théorique"),linewidth=1) +
    labs(title = "",
         x = "Vitesse du vent (m/s)",
         y = "Puissance (kW)",
         color = "") +
    theme_minimal()
}



