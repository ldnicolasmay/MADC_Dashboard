#!/usr/bin/env Rscript

# Build list of report summary tables

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

data <- readRDS(paste0(path_to_app, "rds/df_ms_xfrm.Rds"))
source(paste0(path_to_app, "helper_fxns_summ_tbls.R"))


# build_lst_summ_tbls <- function(data) {

## Create lists for bundling *_cts dfs and *_tbl dfs
lst_summ_cts <- list()
lst_summ_tbls <- list()

# # # # # 
# Build lst_summ_cts ----

# Total counts
lst_summ_cts$total_cts <- 
  single_grp_table(data, 
                   group_var = quo(uds_dx))

# Visit counts
lst_summ_cts$visit_cts <-
  double_grp_table(data,
                   group_var_1 = quo(uds_dx),
                   group_var_2 = quo(visit_cumsum))

# UDS Version counts 
lst_summ_cts$uds_vers_cts <-
  double_grp_table(data,
                   group_var_1 = quo(uds_dx),
                   group_var_2 = quo(uds_version))

# Demographic: Sex counts
lst_summ_cts$sex_cts <-
  double_grp_table(data,
                   group_var_1 = quo(uds_dx),
                   group_var_2 = quo(sex_value))

# Demographic: Race counts
lst_summ_cts$race_cts <-
  double_grp_table(data,
                   group_var_1 = quo(uds_dx),
                   group_var_2 = quo(race_value))

# Demographic: Sex + Race counts
lst_summ_cts$sex_race_cts <-
  triple_grp_table(data,
                   group_var_1 = quo(uds_dx),
                   group_var_2 = quo(sex_value),
                   group_var_3 = quo(race_value))

# Research: Autopsy Consent Yes counts
lst_summ_cts$autopsy_yes_cts <-
  single_grp_filter_table(data,
                          group_var = quo(uds_dx),
                          filter_var = quo(consent_to_autopsy),
                          filter_var_string = "Yes")

# Research: Autopsy Consent Considering counts
lst_summ_cts$autopsy_consid_cts <-
  single_grp_filter_table(data,
                          group_var = quo(uds_dx),
                          filter_var = quo(consent_to_autopsy),
                          filter_var_string = "Considering")

# Research: MRI Yes counts
lst_summ_cts$mri_yes_cts <-
  single_grp_filter_table(data,
                          group_var = quo(uds_dx),
                          filter_var = quo(mri_completed),
                          filter_var_string = "1. Yes")

# Research: Blood Drawn Yes counts
lst_summ_cts$blood_yes_cts <-
  single_grp_filter_table(data,
                          group_var = quo(uds_dx),
                          filter_var = quo(blood_drawn),
                          filter_var_string = "1. Yes")

# UDS Version + Research: Autopsy Yes counts
lst_summ_cts$uds_autopsy_yes_cts <-
  double_grp_filter_table(data,
                          group_var_1 = quo(uds_dx),
                          group_var_2 = quo(uds_version),
                          filter_var = quo(consent_to_autopsy),
                          filter_var_string = "Yes")

# UDS Version + Research: Autopsy Considering counts
lst_summ_cts$uds_autopsy_consid_cts <-
  double_grp_filter_table(data,
                          group_var_1 = quo(uds_dx),
                          group_var_2 = quo(uds_version),
                          filter_var = quo(consent_to_autopsy),
                          filter_var_string = "Considering")

# UDS Version + Research: MRI Yes counts
lst_summ_cts$uds_mri_yes_cts <-
  double_grp_filter_table(data,
                          group_var_1 = quo(uds_dx),
                          group_var_2 = quo(uds_version),
                          filter_var = quo(mri_completed),
                          filter_var_string = "1. Yes")

# UDS Version + Research: Blood Drawn Yes counts
lst_summ_cts$uds_blood_yes_cts <-
  double_grp_filter_table(data,
                          group_var_1 = quo(uds_dx),
                          group_var_2 = quo(uds_version),
                          filter_var = quo(blood_drawn),
                          filter_var_string = "1. Yes")
if (!("UDS 2/3" %in% names(lst_summ_cts$uds_blood_yes_cts))) {
  lst_summ_cts$uds_blood_yes_cts$`UDS 2/3` <- NA_integer_ 
} else if (!("UDS 3" %in% names(lst_summ_cts$uds_blood_yes_cts))) {
  lst_summ_cts$uds_blood_yes_cts$`UDS 3` <- NA_integer_
}

# Race + Research: Autopsy Yes counts
lst_summ_cts$race_autopsy_yes_cts <-
  double_grp_filter_table(data,
                          group_var_1 = quo(uds_dx),
                          group_var_2 = quo(race_value),
                          filter_var = quo(consent_to_autopsy),
                          filter_var_string = "Yes")

# Race + Research: Autopsy Considering counts
lst_summ_cts$race_autopsy_consid_cts <-
  double_grp_filter_table(data,
                          group_var_1 = quo(uds_dx),
                          group_var_2 = quo(race_value),
                          filter_var = quo(consent_to_autopsy),
                          filter_var_string = "Considering")

# Race + Research: MRI Yes counts
lst_summ_cts$race_mri_yes_cts <-
  double_grp_filter_table(data,
                          group_var_1 = quo(uds_dx),
                          group_var_2 = quo(race_value),
                          filter_var = quo(mri_completed),
                          filter_var_string = "1. Yes")

# Race + Research: Blood Drawn Yes counts
lst_summ_cts$race_blood_yes_cts <-
  double_grp_filter_table(data,
                          group_var_1 = quo(uds_dx),
                          group_var_2 = quo(race_value),
                          filter_var = quo(blood_drawn),
                          filter_var_string = "1. Yes")

# Sex + Research: Autopsy Yes counts
lst_summ_cts$sex_autopsy_yes_cts <-
  double_grp_filter_table(data,
                          group_var_1 = quo(uds_dx),
                          group_var_2 = quo(sex_value),
                          filter_var = quo(consent_to_autopsy),
                          filter_var_string = "Yes")

# Sex + Research: Autopsy Considering counts
lst_summ_cts$sex_autopsy_consid_cts <-
  double_grp_filter_table(data,
                          group_var_1 = quo(uds_dx),
                          group_var_2 = quo(sex_value),
                          filter_var = quo(consent_to_autopsy),
                          filter_var_string = "Considering")

# Sex + Research: MRI Yes counts
lst_summ_cts$sex_mri_yes_cts <-
  double_grp_filter_table(data,
                          group_var_1 = quo(uds_dx),
                          group_var_2 = quo(sex_value),
                          filter_var = quo(mri_completed),
                          filter_var_string = "1. Yes")

# Sex + Research: Blood Drawn Yes counts
lst_summ_cts$sex_blood_yes_cts <-
  double_grp_filter_table(data,
                          group_var_1 = quo(uds_dx),
                          group_var_2 = quo(sex_value),
                          filter_var = quo(blood_drawn),
                          filter_var_string = "1. Yes")


# # # # # 
# Build lst_summ_tbls ----

# Visit count table
lst_summ_tbls$visit_tbl <-
  bind_cols(lst_summ_cts$total_cts,
            lst_summ_cts$visit_cts[, -1]) %>%
  arrange(tolower(uds_dx))

# UDS Version table
lst_summ_tbls$uds_vers_tbl <- 
  bind_cols(lst_summ_cts$total_cts, 
            lst_summ_cts$uds_vers_cts[, -1]) %>% 
  arrange(tolower(uds_dx))

# Sex table
lst_summ_tbls$sex_tbl <- 
  bind_cols(lst_summ_cts$total_cts, 
            lst_summ_cts$sex_cts[, -1]) %>% 
  arrange(tolower(uds_dx))

# Race table
lst_summ_tbls$race_tbl <- 
  bind_cols(lst_summ_cts$total_cts, 
            lst_summ_cts$race_cts[, -1]) %>% 
  arrange(tolower(uds_dx))

# Sex + Race table
lst_summ_tbls$sex_race_tbl <-
  bind_cols(lst_summ_cts$total_cts, 
            lst_summ_cts$sex_race_cts[, -1]) %>% 
  arrange(tolower(uds_dx))

# Research table
lst_summ_tbls$rsrch_tbl <- 
  bind_cols(lst_summ_cts$total_cts, 
            lst_summ_cts$autopsy_yes_cts[, -1], 
            lst_summ_cts$autopsy_consid_cts[, -1], 
            lst_summ_cts$mri_yes_cts[, -1], 
            lst_summ_cts$blood_yes_cts[, -1]) %>% 
  arrange(tolower(uds_dx))

# UDS Version + Research table
lst_summ_tbls$uds_rsrch_tbl <-
  bind_cols(lst_summ_cts$total_cts, 
            lst_summ_cts$uds_autopsy_yes_cts[, -1], 
            lst_summ_cts$uds_autopsy_consid_cts[, -1],
            lst_summ_cts$uds_mri_yes_cts[, -1], 
            lst_summ_cts$uds_blood_yes_cts[, -1]) %>% 
  arrange(tolower(uds_dx))

# # Race + Research table
# lst_summ_tbls$race_rsrch_tbl <-
#   bind_cols(lst_summ_cts$total_cts,
#             lst_summ_cts$race_autopsy_yes_cts[, -1],
#             lst_summ_cts$race_autopsy_consid_cts[, -1],
#             lst_summ_cts$race_mri_yes_cts[, -1],
#             lst_summ_cts$race_blood_yes_cts[, -1]) %>% 
#   arrange(tolower(uds_dx))

# Sex + MRI Yes table
lst_summ_tbls$sex_mri_yes_tbl <-
  bind_cols(lst_summ_cts$total_cts,
            lst_summ_cts$sex_mri_yes_cts[, -1]) %>% 
  arrange(tolower(uds_dx))

# Race + MRI Yes table
lst_summ_tbls$race_mri_yes_tbl <-
  bind_cols(lst_summ_cts$total_cts,
            lst_summ_cts$race_mri_yes_cts[, -1]) %>% 
  arrange(tolower(uds_dx))

## Add a "totals row" to each table in `lst_summ_tbls`
lst_summ_tbls <- lapply(lst_summ_tbls, function(tbl) {
  add_totals_row(tbl)
})

## Add a "proportions row" to each table in `lst_summ_tbls`
lst_summ_tbls <- lapply(lst_summ_tbls, function(tbl) {
  add_proportions_row(tbl)
})

## Ensure that appropriate cols in each table of `lst_summ_tbls` are numeric
lst_summ_tbls <- lapply(lst_summ_tbls, function(tbl) { 
  tbl[, -1] <- purrr::map(tbl[, -1], as.numeric) 
  return(tbl) 
})

# # # # # 
## Rename some headers (generalize later) ----

lst_summ_tbls$rsrch_tbl <- lst_summ_tbls$rsrch_tbl %>% 
  rename(`Autopsy Yes` = Total1, `Autopsy Considering` = Total2,
         `MRI Yes` = Total3, `Blood Drawn Yes` = Total4)
lst_summ_tbls$uds_rsrch_tbl <- lst_summ_tbls$uds_rsrch_tbl %>% 
  rename(`UDS 2/3 Autopsy Yes` = `UDS 2/3`, `UDS 3 Autopsy Yes` = `UDS 3`,
         `UDS 2/3 Autopsy Consider` = `UDS 2/31`, `UDS 3 Autopsy Consider` = `UDS 31`,
         `UDS 2/3 MRI Yes` = `UDS 2/32`, `UDS 3 MRI Yes` = `UDS 32`,
         `UDS 2/3 Blood Yes` = `UDS 2/33`, `UDS 3 Blood Yes` = `UDS 33`)

# # # # # 
## Return `lst_summ_tbls` ---

# return(lst_summ_tbls)
# }

saveRDS(lst_summ_tbls, paste0(path_to_app, "rds/lst_summ_tbls.Rds"))




