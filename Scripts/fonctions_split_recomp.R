#########################################################################################
#Fonction pour spliter un fichier en plusieurs fichiers annuels que l'on peut ensuite cleaner
#Choix de la turbine possible
#Pour CDH et CDS c'est déjà nettoyé
#########################################################################################
split_df <- function(name, code = NA) {
  if (! name %in% c("CDH","CDS")){
  #on détecte les fichiers csv commençant par WIF
  file_wif <- list.files(
    path = file.path(path_input, name),
    pattern = "^WIF.*\\.csv$",
    full.names = TRUE
  )

  df <- read.csv(file_wif[1])%>%
    mutate(year=year(Date_time))%>%
    filter(wtg_status=normal)
  } else {
    df <- readRDS(glue("./2-Inputs/{name}/data_long_WS_cleaned_completed_{name}.rds"))%>%
      mutate(year=year(Date_time))%>%
      filter(wtg_status=normal)
  }

  #première et dernière année
  debut <- min(df$year,na.rm=T)
  fin <- max(df$year,na.rm=T)

  # si on veut une éolienne en particulier
  if (!is.na(code)) {
    df <- df %>%
      filter(WIT_ENT_CODE == code)
    #saveRDS(df, glue("./2-Inputs/{name}/df_SCADA_{name}_{code}.rds"))
  }

  for (year in debut:fin) {
    df_year <- df %>%
      filter(year=year)%>%
      select(-year)

    saveRDS(df_year, glue("./2-Inputs/{name}/df_{name}_{year}.rds"))
  }
}

#######################################
# On rbind les df_name_year_cleaned pour en obtenir un seul
#########################################
recomp_clean <- function(name) {
  if (! name %in% c("CDH","CDS")){
    #on détecte les fichiers csv commençant par WIF
    file_wif <- list.files(
      path = file.path(path_input, name),
      pattern = "^WIF.*\\.csv$",
      full.names = TRUE
    )

    df <- read.csv(file_wif[1])%>%
      mutate(year=year(Date_time))
  } else {
    warning(glue("prendre directement le fichier data_long_WS_cleaned_completed_{name}.rds et filtré avec wtg_status=normal"))
  }

  #première et dernière année
  debut <- min(df$year,na.rm=T)
  fin <- max(df$year,na.rm=T)

  df <- data.frame()
  for (year in debut:fin) {
    df_load <- readRDS(glue("./2-Inputs/{name}/df_{name}_{year}_cleaned.rds"))
    df <- rbind(df, df_load)
  }

  df <- df %>%
    distinct(Date_time, .keep_all = TRUE)

  saveRDS(df, glue("./2-Inputs/{name}/df_{name}_cleaned.rds"))
}

##########################################
# Data Frame filtré à 6 données par heures et moyenné
# utilisé dans la fonction merge_save
#########################################
clean_filt <- function(name = name) {
  df_cleaned <- readRDS(glue("./2-Inputs/{name}/df_{name}_cleaned.rds"))

  if (name %in% c("ABH", "AEB", "COF1", "COF2", "EDS", "PRP")) {
    df_cleaned_filt <- df_cleaned %>%
      filter(wtg_status == "normal") %>%
      mutate(
        Date_time = if_else(
          str_detect(Date_time, "^\\d{4}-\\d{2}-\\d{2}$"),
          paste0(Date_time, " 00:00:00"),
          as.character(Date_time)
        ),
        Date_time = ymd_hms(Date_time),
        Year = year(Date_time),
        HourGroup = floor_date(Date_time, "hour")
      ) %>%
      group_by(HourGroup) %>%
      filter(n() == 6) %>%
      summarise(across(where(is.numeric), \(x) round(mean(x, na.rm = TRUE), 2)), .groups = "drop")
  } else {
    df_cleaned_filt <- df_cleaned %>%
      filter(wtg_status == "normal") %>%
      mutate(
        Year = year(Date_time),
        HourGroup = floor_date(Date_time, "hour")
      ) %>%
      group_by(HourGroup) %>%
      filter(n() == 6) %>%
      summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)), .groups = "drop")
  }

  df_cleaned_filt <- df_cleaned_filt %>%
    dplyr::select(HourGroup, Ws,Wa, P)
  colnames(df_cleaned_filt) <- c("TimeStamp", "ws_scada", "wa_scada", "P_scada")

  return(df_cleaned_filt)
}

#####################################################
# pourcentage de données entre l'origine et le clean sur les données scada
# retourne un df avec ndata, ndata_clean,
######################################################
diff_data <- function(name) {
  if (! name %in% c("CDH","CDS")){
    #on détecte les fichiers csv commençant par WIF
    file_wif <- list.files(
      path = file.path(path_input, name),
      pattern = "^WIF.*\\.csv$",
      full.names = TRUE
    )

    df <- read.csv(file_wif[1])%>%
      mutate(year=year(Date_time))%>%
      filter(wtg_status=normal)
  } else {
    df <- readRDS(glue("./2-Inputs/{name}/data_long_WS_cleaned_completed_{name}.rds"))%>%
      mutate(year=year(Date_time))%>%
      filter(wtg_status=normal)
  }

  #première et dernière année
  debut <- min(df$year,na.rm=T)
  fin <- max(df$year,na.rm=T)

  pourcent <- data.frame(year = seq(debut, fin), n_data = NA, n_data_clean = NA, proportion = NA)

  for (year in debut:fin) {
    path_df <- glue("./2-Inputs/{name}/df_{name}_{year}.rds")
    path_df_clean <- glue("./2-Inputs/{name}/df_{name}_{year}_cleaned.rds")
    df <- readRDS(path_df)
    df_clean <- readRDS(path_df_clean)

    n_normal <- sum(df_clean$wtg_status == "normal", na.rm = TRUE)

    pourcent[year - debut + 1, 2] <- nrow(df)
    pourcent[year - debut + 1, 3] <- n_normal
    pourcent[year - debut + 1, 4] <- round(n_normal / nrow(df) * 100, 2)
  }
  assign(glue("diff_data_scada_{name}"), pourcent, envir = .GlobalEnv)
}


######################################
# Normalisation Ws_scada à partir de df_{name}_cleaned_merge
######################################
ws_norm_scada <- function(name) {
  # Environnement temporaire
  env_temp <- new.env()

  # Chargement du Rdata
  load(glue("{path_result}/{name}/data_{name}.Rdata"), envir = env_temp)

  df_scada <- readRDS(glue("./2-Inputs/{name}/df_{name}_cleaned_merge.RDS"))
  df_wrf <- env_temp$df_wrf

  if ("ws_scada_norm" %in% names(df_scada)){
    stop(glue("ws_scada_norm existe déjà"))
  }

  df_scada <- df_scada %>%
    inner_join(df_wrf %>% select(TimeStamp, temp), by = "TimeStamp") %>%
    mutate(ws_scada_norm = ws_scada * (288 / (temp + 273))^(1 / 3)) %>%
    select(-temp)

  saveRDS(df_scada, glue("./2-Inputs/{name}/df_{name}_cleaned_merge.RDS"))
}


###################################################################"
#Merge df_scada, df_SLT et df_rea : sortie : df_{name}_cleaned_merge
####################################################################
merge_save <- function(name,overwrite=FALSE){
  load(glue('{path_result}/{name}/data_{name}.Rdata'))

  #test d'existence du fichier cleaned_merge et arrêt si nécessaire
  rds_path <- glue("./2-Inputs/{name}/df_{name}_cleaned_merge.rds")
  if (file.exists(rds_path) && !overwrite) {
    stop(glue("❌ Le fichier {basename(rds_path)} existe déjà. Utilisez overwrite = TRUE pour forcer l'écrasement."))
  }

  df_rea_norm <- df_rea %>%
    rename(ws_rea_norm=ws_hub_norm,ti_rea=ti_hub)

  df_complete <- df_SLT %>%
    mutate(TimeStamp = force_tz(TimeStamp, tzone = "UTC"),
           P_SLT_Cons = P_SLT_cons,
           P_SLT_TI = P_SLT_TI/1000,
           P_SLT_CMV = pred_P_CMV,
           P_SLT_SLT = pred_P_SLT,
           P_Rea_Cons = P_Rea_Cons,
           P_Rea_TI = P_Rea_TI/1000,
           P_Rea_CMV = pred_P_CMV_deg) %>%
    dplyr::select(-v_shear,-v_veer)

  load(glue('{path_result}/{name}/data_{name}2.Rdata'))

  df_complete <- df_complete%>%
    mutate(P_SLT_Cons2 = P_SLT_cons2,
           P_SLT_TI2 = P_SLT_TI2/1000)

  colnames(df_complete) <- c("TimeStamp" , "ws_SLT", "ti_SLT" ,  "P_SLT_Cons" , "P_SLT_TI" ,
                             "P_SLT_CMV" , "P_SLT_SLT", "P_Rea_Cons" , "P_Rea_TI"  ,  "P_Rea_CMV",
                             "P_SLT_Cons2", "P_SLT_TI2")

  df_temp <- clean_filt(name)

  df_cm <- df_temp %>%
    inner_join(df_complete, by = c("TimeStamp")) %>%
    inner_join(df_rea_norm,by = c("TimeStamp")) %>%
    mutate(across(2:ncol(.),~ round (.x,3)))

  saveRDS(df_cm,glue("./2-Inputs/{name}/df_{name}_cleaned_merge.rds"))
}
