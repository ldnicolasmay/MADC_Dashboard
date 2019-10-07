#!/usr/bin/env Rscript

# Build data for conditions (condx) by dx plots

`%>%` <- magrittr::`%>%`

deployed <- TRUE
# deployed <- FALSE

if (deployed) {
  path_to_app <- # Michigan Medicine R Shiny server
    "~/ShinyApps/MADCDashboard/" 
} else {
  path_to_app <- # local
    "~/Box Sync/Documents/MADC_Dashboard/MADCDashboard/"
}

source(paste0(path_to_app, "helper_fxns_summ_tbls.R"), local = TRUE)

# **** ----
# GET DATA ----

# _ Load `df_ms_xfrm` as `data` ----
data <- readRDS(paste0(path_to_app, "rds/df_ms_xfrm.Rds"))

# _ Define `condx_vctr` ----
condx_vctr <- condx_vctr <- c(
  'cancer'     # Condx -- Cancer
  , 'diabet'   # Condx -- Diabetes
  , 'myoinf'   # Condx -- Myocardial infarction
  , 'conghrt'  # Condx -- Congestive heart failure
  , 'hypert'   # Condx -- Hypertension
  , 'hypchol'  # Condx -- Hypercholesterolemia
  , 'arth'     # Condx -- Arthritis
  , 'sleepap'  # Condx -- Sleep apnea
  , 'remdis'   # Condx -- REM sleep behavior disorder
  , 'hyposom'  # Condx -- Hyposomnia / insomnia
)
# _ Define `dx_levels` ----
dx_levels <- c("MCI", "NL", "LBD", "AD", "FTD", "Impaired, not MCI",
               "Pending consensus", "Withdrew", "Other")

# **** ----
# FILTER / MUTATE / CLEAN DATA ----

# _ Select only relevant fields ----
data <- data[, c("subject_id", "uds_dx", condx_vctr)]

# _ Coerce condx fields to integer
condx_col_first <- which(names(data) == "cancer")
condx_col_last  <- which(names(data) == "hyposom")
# condx_col_first <- which(names(data) == "diabet")
# condx_col_last  <- which(names(data) == "hypchol")
data[, condx_col_first:condx_col_last] <- 
  purrr::map_df(data[, condx_col_first:condx_col_last], as.integer)
# purrr::map_chr(data, class)

# _ Add `nullcond` field ----
data <- data %>% 
  dplyr::mutate(nullcond = NA_integer_) %>% 
  dplyr::select(subject_id, uds_dx, nullcond, dplyr::everything())

condx_col_first <- which(names(data) == "cancer")
condx_col_last  <- which(names(data) == "hyposom")
# condx_col_first <- which(names(data) == "diabet")
# condx_col_last  <- which(names(data) == "hypchol")
for (i in 1:nrow(data)) {
  if (!any(as.logical(data[i, condx_col_first:condx_col_last]))) {
    data[i, "nullcond"] <- 1L
  } else {
    data[i, "nullcond"] <- 0L
  }
}
# purrr::map_chr(data, class)

# ******** ----
# Binary hash approach ----

# data$condx_combn <- paste0(as.integer(data[, condx_vctr]), collapse = "")

data$condx_combn <- NA_character_
for (i in 1:nrow(data)) {
  data[i, "condx_combn"] <-
    paste0(
      as.character(as.integer(unlist(data[i, condx_vctr]))), 
      collapse = "")
}

data$condx_combn_name <- NA_character_
for (i in 1:nrow(data)) {
  data[i, "condx_combn_name"] <-
    paste0(
      condx_vctr[
        as.logical(as.integer(unlist(strsplit(x = data[[i, "condx_combn"]], 
                                              split = ""))))
        ],
      collapse = "_"
    )
}

# **** ----
# WRITE TO RDS ----

saveRDS(data, paste0(path_to_app, "rds/data_condx.Rds"))




