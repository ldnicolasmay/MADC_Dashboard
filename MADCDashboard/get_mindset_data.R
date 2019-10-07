#!/usr/bin/env Rscript


# # # # #
# USEFUL VARS ----

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
source(paste0(path_to_app, "config.R"), local = TRUE)


# # # # # 
# GET DATA ----

# _ MiNDSet data ----
# _ _ Demox data ----
# _ _ Research data ----
# _ _ Timeline data ----
fields_ms <- 
  c(
    'subject_id',         # partic. ID
    'exam_date',          # visit date
    'race_value',         # demox
    'sex_value',          # demox
    'county',             # demox
    'zip_code',           # demox
    'comp_withd',         # research
    'blood_drawn',        # research
    'consent_to_autopsy', # research
    'mri_completed',      # research
    'sample_given',       # research
    'scored',             # timeline
    'dbl_scored',         # timeline
    'consensus_date',     # timeline
    'second_consensus',   # timeline
    'fb_date'             # timeline
  ) %>% paste(collapse = ',')
json_ms <- RCurl::postForm(
  uri=API_URL,
  token=API_TOKEN_MINDSET,
  content='record',
  format='json',
  type='flat',
  fields=fields_ms,
  rawOrLabel='label',
  rawOrLabelHeaders='raw',
  exportCheckboxLabel='false',
  exportSurveyFields='false',
  exportDataAccessGroups='false',
  returnFormat='json',
  filterLogic='([exam_date] >= "2017-03-28")',
  # .opts = list(ssl.verifypeer = TRUE, verbose = TRUE) # can't use now*
  .opts = list(ssl.verifypeer = FALSE, verbose = TRUE)
)
df_ms <- jsonlite::fromJSON(json_ms) %>% 
  dplyr::na_if("")
## Filter rows
df_ms_xfrm <- df_ms %>% 
  dplyr::filter(stringr::str_sub(subject_id, 1, 2) == "UM")

# _ UDS 2.0 data ----
# _ _ ID data only ----
fields_uds2 <- c('subject_id') %>% paste(collapse = ",") # partic. ID
json_uds2 <-
  RCurl::postForm(
    uri=API_URL,
    token=API_TOKEN_UDS2,
    content='record',
    format='json',
    type='flat',
    fields=fields_uds2,
    'events[1]'='visit_01_arm_1',
    rawOrLabel='raw',
    rawOrLabelHeaders='raw',
    exportCheckboxLabel='false',
    exportSurveyFields='false',
    exportDataAccessGroups='false',
    returnFormat='json',
    filterLogic='([madc_id] >= "2013")',
    # .opts = list(ssl.verifypeer = TRUE, verbose = TRUE) # see note below*
    .opts = list(ssl.verifypeer = FALSE, verbose = TRUE)
  )
df_uds2 <- jsonlite::fromJSON(json_uds2) %>% 
  dplyr::na_if("") %>%
  dplyr::select(subject_id) # only keep `subject_id` field


# _ UDS 3.0 data ----
# _ _ Dx data (clin. pheno. + etiology) ----
# _ _ Condx data ----
condx_vctr <- 
  c(
    # D2 - Initial visits
    'cancer',       # Condx -- Cancer
    'diabet',       # Condx -- Diabetes
    'myoinf',       # Condx -- Myocardial infarction
    'conghrt',      # Condx -- Congestive heart failure
    'hypert',       # Condx -- Hypertension
    'hypchol',      # Condx -- Hypercholesterolemia
    'arth',         # Condx -- Arthritis
    'sleepap',      # Condx -- Sleep apnea
    'remdis',       # Condx -- REM sleep behavior disorder
    'hyposom',      # Condx -- Hyposomnia / insomnia
    # D2 - Follow-up visits
    'fu_cancer',    # Condx -- Cancer
    'fu_diabet',    # Condx -- Diabetes
    'fu_myoinf',    # Condx -- Myocardial infarction
    'fu_conghrt',   # Condx -- Congestive heart failure
    'fu_hypert',    # Condx -- Hypertension
    'fu_hypchol',   # Condx -- Hypercholesterolemia
    'fu_arth',      # Condx -- Arthritis
    'fu_sleepap',   # Condx -- Sleep apnea
    'fu_remdis',    # Condx -- REM sleep behavior disorder
    'fu_hyposom'    # Condx -- Hyposomnia / insomnia
  )
fields_uds3 <- 
  c(
    'ptid',         # partic. ID
    'form_date',    # visit date
    # D1 - Initial visits
    'normcog',      # Dx -- NL
    'demented',     # --
    'mciamem',      # Dx -- aMCI
    'mciaplus',     # Dx -- aMCI
    'mcinon1',      # Dx -- naMCI
    'mcinon2',      # Dx -- naMCI
    'impnomci',     # Dx -- Cognitively impaired
    'alzdis',       # Dx -- AD
    'alzdisif',     # Dx -- AD
    'lbdis',        # Dx -- LBD
    'lbdif',        # Dx -- LBD
    'psp',          # Dx -- FTD
    'pspif',        # Dx -- FTD
    'cort',         # Dx -- FTD
    'cortif',       # Dx -- FTD
    'ftldmo',       # Dx -- FTD
    'ftldmoif',     # Dx -- FTD
    'ftldnos',      # Dx -- FTD
    'ftldnoif',     # Dx -- FTD
    # D1 - Follow-up visits
    'fu_normcog',   # Dx -- NL
    'fu_demented',  # --
    'fu_mciamem',   # Dx -- aMCI
    'fu_mciaplus',  # Dx -- aMCI
    'fu_mcinon1',   # Dx -- naMCI
    'fu_mcinon2',   # Dx -- naMCI
    'fu_impnomci',  # Dx -- Cognitively impaired
    'fu_alzdis',    # Dx -- AD
    'fu_alzdisif',  # Dx -- AD
    'fu_lbdis',     # Dx -- LBD
    'fu_lbdif',     # Dx -- LBD
    'fu_psp',       # Dx -- FTD
    'fu_pspif',     # Dx -- FTD
    'fu_cort',      # Dx -- FTD
    'fu_cortif',    # Dx -- FTD
    'fu_ftldmo',    # Dx -- FTD
    'fu_ftldmoif',  # Dx -- FTD
    'fu_ftldnos',   # Dx -- FTD
    'fu_ftldnoif',  # Dx -- FTD
    condx_vctr      # Condx (10 condx.s, initial & follow-up)
  ) %>% paste(collapse = ',')
json_uds3 <- RCurl::postForm(
  uri=API_URL,
  token=API_TOKEN_UDS3,
  content='record',
  format='json',
  type='flat',
  fields=fields_uds3,
  rawOrLabel='raw',
  rawOrLabelHeaders='raw',
  exportCheckboxLabel='false',
  exportSurveyFields='false',
  exportDataAccessGroups='false',
  returnFormat='json',
  # .opts = list(ssl.verifypeer = TRUE, verbose = TRUE) # see note below*
  .opts = list(ssl.verifypeer = FALSE, verbose = TRUE)
)
df_uds3 <- jsonlite::fromJSON(json_uds3) %>% 
  dplyr::na_if("")
df_uds3 <- df_uds3 %>% 
  dplyr::filter(!is.na(form_date))
# _ _ Simplify UDS3 dx ----
df_uds3 <- df_uds3 %>% 
  dplyr::mutate(uds_dx = dplyr::case_when(
    # Initial visits
    normcog == 1                                  ~ "NL",
    demented != 1 & mciamem == 1                  ~ "aMCI",
    demented != 1 & mciaplus == 1                 ~ "aMCI",
    demented != 1 & mcinon1 == 1                  ~ "naMCI",
    demented != 1 & mcinon2 == 1                  ~ "naMCI",
    demented != 1 & impnomci == 1                 ~ "Impaired, not MCI",
    demented == 1 & alzdis == 1   & alzdisif == 1 ~ "AD",
    demented == 1 & lbdis == 1    & lbdif == 1    ~ "LBD",
    demented == 1 & psp == 1      & pspif == 1    ~ "FTD",
    demented == 1 & cort == 1     & cortif == 1   ~ "FTD",
    demented == 1 & ftldmo == 1   & ftldmoif == 1 ~ "FTD",
    demented == 1 & ftldnos == 1  & ftldnoif == 1 ~ "FTD",
    # Follow-up visits
    fu_normcog == 1                               ~ "NL",
    fu_demented != 1 & fu_mciamem == 1            ~ "aMCI",
    fu_demented != 1 & fu_mciaplus == 1           ~ "aMCI",
    fu_demented != 1 & fu_mcinon1 == 1            ~ "naMCI",
    fu_demented != 1 & fu_mcinon2 == 1            ~ "naMCI",
    fu_demented != 1 & fu_impnomci == 1           ~ "Impaired, not MCI",
    fu_demented == 1 & fu_alzdis == 1   & fu_alzdisif == 1 ~ "AD",
    fu_demented == 1 & fu_lbdis == 1    & fu_lbdif == 1    ~ "LBD",
    fu_demented == 1 & fu_psp == 1      & fu_pspif == 1    ~ "FTD",
    fu_demented == 1 & fu_cort == 1     & fu_cortif == 1   ~ "FTD",
    fu_demented == 1 & fu_ftldmo == 1   & fu_ftldmoif == 1 ~ "FTD",
    fu_demented == 1 & fu_ftldnos == 1  & fu_ftldnoif == 1 ~ "FTD",
    TRUE ~ "Other"
  ))

# Bind UDS3 dx and condx data to MiNDSet data
ms_dx <- df_ms_xfrm %>% 
  dplyr::select(subject_id, exam_date)
uds3_dx <- df_uds3 %>% 
  dplyr::select(ptid, form_date, uds_dx, 
                condx_vctr) %>% 
  dplyr::rename(subject_id = ptid,
                exam_date = form_date)
ms_uds3_dx <- dplyr::left_join(ms_dx, 
                               uds3_dx, 
                               by = c("subject_id", "exam_date"))
df_ms_xfrm$uds_dx <- ms_uds3_dx$uds_dx
df_ms_xfrm[, condx_vctr] <-
  ms_uds3_dx[, condx_vctr]
df_ms_xfrm <- df_ms_xfrm %>% 
  dplyr::mutate(
    cancer = dplyr::case_when( # Cancer
      cancer == "1" | cancer == "2" |
        fu_cancer == "1" | fu_cancer == "2" ~ "1",
      cancer == "0" | fu_cancer == "0" ~ "0",
      is.na(cancer) | is.na(fu_cancer) ~ "0",
      TRUE ~ "0"
    ),
    diabet = dplyr::case_when( # Diabetes
      diabet == "1" | diabet == "2" |
        fu_diabet == "1" | fu_diabet == "2" ~ "1",
      diabet == "0" | fu_diabet == "0" ~ "0",
      is.na(diabet) | is.na(fu_diabet) ~ "0",
      TRUE ~ "0"
    ),
    myoinf = dplyr::case_when( # Myocardial infarction
      myoinf == "1" | myoinf == "2" |
        fu_myoinf == "1" | fu_myoinf == "2" ~ "1",
      myoinf == "0" | fu_myoinf == "0" ~ "0",
      is.na(myoinf) | is.na(fu_myoinf) ~ "0",
      TRUE ~ "0"
    ),
    conghrt = dplyr::case_when( # Congestive heart failure
      conghrt == "1" | conghrt == "2" |
        fu_conghrt == "1" | fu_conghrt == "2" ~ "1",
      conghrt == "0" | fu_conghrt == "0" ~ "0",
      is.na(conghrt) | is.na(fu_conghrt) ~ "0",
      TRUE ~ "0"
    ),
    hypert = dplyr::case_when( # Hypertension
      hypert == "1" | fu_hypert == "1" ~ "1",
      hypert == "0" | fu_hypert == "0" ~ "0",
      is.na(hypert) | is.na(fu_hypert) ~ "0",
      TRUE ~ "0"
    ),
    hypchol = dplyr::case_when( # Hypercholesterolemia
      hypchol == "1" | hypchol == "2" |
        fu_hypchol == "1" | fu_hypchol == "2" ~ "1",
      hypchol == "0" | fu_hypchol == "0" ~ "0",
      is.na(hypchol) | is.na(fu_hypchol) ~ "0",
      TRUE ~ "0"
    ),
    arth = dplyr::case_when( # Arthritis
      arth == "1" | arth == "2" |
        fu_arth == "1" | fu_arth == "2" ~ "1",
      arth == "0" | fu_arth == "0" ~ "0",
      is.na(arth) | is.na(fu_arth) ~ "0",
      TRUE ~ "0"
    ),
    sleepap = dplyr::case_when( # Sleep apnea
      sleepap == "1" | fu_sleepap == "1" ~ "1",
      sleepap == "0" | fu_sleepap == "0" ~ "0",
      is.na(sleepap) | is.na(fu_sleepap) ~ "0",
      TRUE ~ "0"
    ),
    remdis = dplyr::case_when( # REM sleep behavior disorder
      remdis == "1" | remdis == "2" |
        fu_remdis == "1" | fu_remdis == "2" ~ "1",
      remdis == "0" | fu_remdis == "0" ~ "0",
      is.na(remdis) | is.na(fu_remdis) ~ "0",
      TRUE ~ "0"
    ),
    hyposom = dplyr::case_when( # Hyposomnia / insomnia
      hyposom == "1" | hyposom == "2" |
        fu_hyposom == "1" | fu_hyposom == "2" ~ "1",
      hyposom == "0" | fu_hyposom == "0" ~ "0",
      is.na(hyposom) | is.na(fu_hyposom) ~ "0",
      TRUE ~ "0"
    )
  )
df_ms_xfrm <- df_ms_xfrm[, !(names(df_ms_xfrm) %in% condx_vctr[11:20])]
# readr::write_csv(ms_uds3_dx, "ms_uds3_dx.csv", na = "")

# # # # #  
## Define factor levels ----
##   1. redcap_event_name_levels
##   2. dx_levels
##   3. race_levels
##   4. sex_levels

## 1. redcap_event_name_levels
redcap_event_name_levels <- c("Baseline",
                              paste0("Visit 0", 1:9),
                              paste0("Visit ", 10:15))

## 2. dx_levels
dx_levels <- c("MCI", "NL", "LBD", "AD", "FTD", "Impaired, not MCI",
               "Pending consensus", "Withdrew", "Other",
               # target diagnoses
               "MCI target", "NL target", "LBD target",
               "AD target", "FTD target")

## 3. race_levels
race_levels <- c("Asian", "Black", "Other", "White",
                 "American Indian or Alaska Native",
                 "Native Hawaiian or Other Pacific Islander")

## 4. sex_levels
sex_levels <- c("Female", "Male")


# # # # # 
## Mutate fields (if nec.); Coerce fields to appropriate classes ----

## Coerce `redcap_event_name` to factor class
df_ms_xfrm <- df_ms_xfrm %>%
  dplyr::mutate(
    redcap_event_name = readr::parse_factor(redcap_event_name,
                                            levels = redcap_event_name_levels))

## Coerce `exam_date` to Date class
## Coerce `scored` to Date class
## Coerce `dbl_scored` to Date class
## Coerce `consensus_date` to Date class
## Coerce `second_consensus` to Date class
## Coerce `fb_date` to Date class
df_ms_xfrm <- df_ms_xfrm %>% 
  dplyr::mutate(exam_date        = lubridate::ymd(exam_date),
                scored           = lubridate::ymd(scored),
                dbl_scored       = lubridate::ymd(dbl_scored),
                consensus_date   = lubridate::ymd(consensus_date),
                second_consensus = lubridate::ymd(second_consensus),
                fb_date          = lubridate::ymd(fb_date))

## Mutate `uds_dx` and coerce to factor class
df_ms_xfrm <- df_ms_xfrm %>%
  dplyr::mutate(uds_dx = dplyr::case_when(
    uds_dx == "aMCI"                  ~ "MCI",
    uds_dx == "naMCI"                 ~ "MCI",
    is.na(uds_dx) & comp_withd == "Y" ~ "Withdrew",
    # uds_dx == ""  & comp_withd == "Y" ~ "Withdrew",
    is.na(uds_dx) & is.na(comp_withd) ~ "Pending consensus",
    # uds_dx == ""  & comp_withd == ""  ~ "Pending consensus",
    # is.na(uds_dx) & comp_withd == ""  ~ "Pending consensus",
    # uds_dx == ""  & is.na(comp_withd) ~ "Pending consensus",
    # is.na(uds_dx) | uds_dx == ""      ~ "Other",
    is.na(uds_dx)                     ~ "Other",
    TRUE ~ uds_dx
  )) %>%
  dplyr::mutate(
    uds_dx = readr::parse_factor(uds_dx, levels = dx_levels))

## Mutate `race_value` and coerce to factor class
df_ms_xfrm <- df_ms_xfrm %>%
  dplyr::mutate(race_value = dplyr::case_when(
    race_value == "Asian"    ~ "Asian",
    race_value == "Black"    ~ "Black",
    race_value == "Other"    ~ "Other",
    race_value == "White"    ~ "White",
    race_value == "Hispanic" ~ "Other",
    # race_value == ""         ~ "Other",
    is.na(race_value)        ~ "Other"
  )) %>%
  dplyr::mutate(
    race_value = readr::parse_factor(race_value, levels = race_levels))

## Coerce `sex_value` to factor class
df_ms_xfrm <- df_ms_xfrm %>%
  dplyr::mutate(sex_value = forcats::as_factor(sex_value, levels = sex_levels))

## Coerce `comp_withd` to factor class
df_ms_xfrm <- df_ms_xfrm %>%
  dplyr::mutate(comp_withd = forcats::as_factor(comp_withd))

## Coerce `blood_drawn` to factor class
df_ms_xfrm <- df_ms_xfrm %>%
  dplyr::mutate(blood_drawn = forcats::as_factor(blood_drawn))

## Coerce `consent_to_autopsy` to factor class
df_ms_xfrm <- df_ms_xfrm %>%
  dplyr::mutate(consent_to_autopsy = forcats::as_factor(consent_to_autopsy))

## Coerce `mri_completed` to factor class
df_ms_xfrm <- df_ms_xfrm %>%
  dplyr::mutate(mri_completed = forcats::as_factor(mri_completed))

## Leave `county` field as character class

## Mutate `zip_code` to only include first 5 digits
df_ms_xfrm <- df_ms_xfrm %>%
  dplyr::mutate(zip_code = stringr::str_sub(zip_code, 1, 5))

## Derive (mutate) date durations
df_ms_xfrm <- df_ms_xfrm %>% 
  dplyr::mutate(exam_scored_dur = 
                  lubridate::interval(exam_date, scored) /
                  lubridate::ddays(1),
                exam_dbl_scored_dur = 
                  lubridate::interval(exam_date, dbl_scored) /
                  lubridate::ddays(1),
                exam_consensus_dur = 
                  lubridate::interval(exam_date, consensus_date) /
                  lubridate::ddays(1),
                final_consensus_fb_dur = 
                  lubridate::interval(second_consensus, fb_date) /
                  lubridate::ddays(1)) %>%
  dplyr::mutate(exam_scored_dur = 
                  dplyr::case_when(
                    exam_scored_dur < 0 ~ NA_real_,
                    TRUE ~ exam_scored_dur),
                exam_dbl_scored_dur =
                  dplyr::case_when(
                    exam_dbl_scored_dur < 0 ~ NA_real_,
                    TRUE ~ exam_dbl_scored_dur),
                exam_consensus_dur =
                  dplyr::case_when(
                    exam_consensus_dur < 0 ~ NA_real_,
                    TRUE ~ exam_consensus_dur),
                final_consensus_fb_dur =
                  dplyr::case_when(
                    final_consensus_fb_dur < 0 ~ NA_real_,
                    TRUE ~ final_consensus_fb_dur))

## Arrange and clean `df_ms_xfrm` ----

# Calculate cumulative sum of visits
df_ms_xfrm <- df_ms_xfrm %>%
  dplyr::arrange(subject_id, exam_date) %>%
  dplyr::mutate(visit_unit = 1L) %>%
  dplyr::group_by(subject_id) %>%
  dplyr::mutate(visit_cumsum = cumsum(visit_unit)) %>%
  dplyr::select(-visit_unit) %>% 
  dplyr::ungroup() # ungroup grouped tibble

## Arrange data frame by subject_id (1st), then exam_date (desc, 2nd)
df_ms_xfrm <- df_ms_xfrm %>%
  dplyr::arrange(subject_id, dplyr::desc(exam_date))

## Clean out duplicate visits ... keeping most recent visit
duplicated_subject_ids <- duplicated(df_ms_xfrm$subject_id)
df_ms_xfrm <- df_ms_xfrm %>%
  dplyr::filter(!duplicated_subject_ids)

# # # # # 
## Process `df_uds2` ----
## ... add a simple `uds_version` character column
df_uds2 <- df_uds2 %>% 
  dplyr::mutate(uds_version = "UDS 2/3")

# # # # # 
## Left join `df_ms_xfrm` and `df_uds2` ----
df_ms_xfrm <- df_ms_xfrm %>% 
  dplyr::left_join(df_uds2, by = "subject_id") %>% 
  dplyr::mutate(uds_version = 
                  ifelse(is.na(uds_version), "UDS 3", uds_version))

# # # # # 
## Save xformed MiNDSet data as RDS ----

# saveRDS(df_ms_xfrm_lng, paste0(path_to_app, "rds/df_ms_xfrm_lng.Rds"))
saveRDS(df_ms_xfrm, paste0(path_to_app, "rds/df_ms_xfrm.Rds"))

# * The REDCap server for some reason isn't able to verify the Shiny server
#   SSL certificate. Andrew Carroll's (MICHR) recommendation is to bypass
#   the SSL certificate verfication... "for the time being". (April 2018)





