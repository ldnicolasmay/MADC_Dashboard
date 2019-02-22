#!/usr/bin/env Rscript


# HANDLE SCRIPT EXEC ARGS ----
# Simplifies local-testing v. server-deployed execution of this script
args <- commandArgs(trailingOnly = TRUE)

DEPLOYED <- logical(length = 1L)

if (length(args) != 1) {
  stop(paste0("\nSupply an argument of either `local` or `server`\n",
              "after executing `get_mindset_data.R`.\n", call.=TRUE))
} else if (args[1] == "local") {
  DEPLOYED <- FALSE
} else if (args[1] == "server") {
  DEPLOYED <- TRUE
}


# USEFUL LIBRARIES ----
library(dplyr)
library(stringr)

# USEFUL GLOBALS & FUNCTIONS ----
if (DEPLOYED) {
  path_to_app <- # Michigan Medicine R Shiny server
    "~/ShinyApps/MADCDashboard/" 
} else {
  path_to_app <- # local
    "~/Box Sync/Documents/MADC_Dashboard/MADCDashboard_rf/"
}
source(paste0(path_to_app, "config_new.R"), local = TRUE)
source(paste0(path_to_app, "helpers.R"), local = TRUE)
# source(paste0(path_to_app, "helper_fxns_get_data.R"), local = TRUE)



# GET DATA ----

# _ Define Data Fields / Forms ----

# __ UDS 3 ----

# Header data
fields_u3_hd_raw <-
  c(
    'ptid'           # partic. ID
    ,'form_date'     # visit date
  )
# Form A1 Demographics
fields_u3_a1_raw <-
  c(
    "sex"            # sex
    # , "race"         # race not available for old cohort; no A1 fields!
  ) %>% c(., paste0("fu_", .), paste0("tele_", .))
# Form D1 Diagnosis data (IVP, FVP, TVP)
fields_u3_d1_raw <-
  c(
    'normcog'        # Dx -- NL
    , 'mciamem'      # Dx -- aMCI
    , 'mciaplus'     # Dx -- aMCI
    , 'mcinon1'      # Dx -- naMCI
    , 'mcinon2'      # Dx -- naMCI
    , 'impnomci'     # Dx -- Cognitively impaired
    , 'alzdis'       # Dx -- AD
    , 'alzdisif'     # Dx -- AD
    , 'lbdis'        # Dx -- LBD
    , 'lbdif'        # Dx -- LBD
    , 'psp'          # Dx -- FTD
    , 'pspif'        # Dx -- FTD
    , 'cort'         # Dx -- FTD
    , 'cortif'       # Dx -- FTD
    , 'ftldmo'       # Dx -- FTD
    , 'ftldmoif'     # Dx -- FTD
    , 'ftldnos'      # Dx -- FTD
    , 'ftldnoif'     # Dx -- FTD
  ) %>% c(., paste0("fu_", .), paste0("tele_", .))
# Form D2 Comorbid condition data (IVP, FVP, TVP)
fields_u3_d2_raw <-
  c(
    'cancer'         # Condx -- Cancer
    , 'diabet'       # Condx -- Diabetes
    , 'myoinf'       # Condx -- Myocardial infarction
    , 'conghrt'      # Condx -- Congestive heart failure
    , 'hypert'       # Condx -- Hypertension
    , 'hypchol'      # Condx -- Hypercholesterolemia
    , 'arth'         # Condx -- Arthritis
    , 'sleepap'      # Condx -- Sleep apnea
    , 'remdis'       # Condx -- REM sleep behavior disorder
    , 'hyposom'      # Condx -- Hyposomnia / insomnia
  ) %>% c(., paste0("fu_", .), paste0("tele_", .))
# Combine and collapse `fields_u3_*_raw` vectors
fields_u3 <- c(fields_u3_hd_raw
               , fields_u3_a1_raw
               , fields_u3_d1_raw
               , fields_u3_d2_raw) %>% paste(collapse = ",")
rm(fields_u3_hd_raw)
rm(fields_u3_a1_raw)
rm(fields_u3_d1_raw)
rm(fields_u3_d2_raw)

# __ MiNDSet Registry ----

# Header data
fields_ms_head_raw <- 
  c(
    'subject_id'           # partic. ID
    , 'exam_date'          # visit date
  )
# Demographic data
fields_ms_dem_raw <-
  c(
    "race_value"           # demo
    , 'county'             # demo
    , 'zip_code'           # demo
  )
# Research data
fields_ms_res_raw <-
  c(
    'comp_withd'           # research
    , 'blood_drawn'        # research
    , 'consent_to_autopsy' # research
    , 'mri_completed'      # research
    , 'sample_given'       # research
  )
# Timeline data
fields_ms_time_raw <-
  c(
    'scored'               # timeline
    , 'dbl_scored'         # timeline
    , 'consensus_date'     # timeline
    , 'second_consensus'   # timeline
    , 'fb_date'            # timeline
  )
# Combine and collapse `fields_ms_*_raw` vectors
fields_ms <- c(fields_ms_head_raw,
               fields_ms_dem_raw,
               fields_ms_res_raw,
               fields_ms_time_raw) %>% paste(collapse = ",")
rm(fields_ms_head_raw)
rm(fields_ms_dem_raw)
rm(fields_ms_res_raw)
rm(fields_ms_time_raw)

# __ UDS 2

# _ Retrieve Data via REDCap API ----

# __ UDS 3

# Retrieve JSON object
json_u3 <- 
  RCurl::postForm(
    uri     = REDCAP_API_URI,
    token   = REDCAP_API_TOKEN_UDS3n,
    content = 'record',
    format  = 'json',
    type    = 'flat',
    fields  = fields_u3,
    rawOrLabel             = 'raw',
    rawOrLabelHeaders      = 'raw',
    exportCheckboxLabel    = 'false',
    exportSurveyFields     = 'false',
    exportDataAccessGroups = 'false',
    returnFormat           = 'json',
    # .opts = list(ssl.verifypeer = TRUE, verbose = TRUE) # see note below*
    .opts = list(ssl.verifypeer = FALSE, verbose = TRUE)
  ) %>% 
  str_replace_all(pattern = "\r\n", replacement = " ")
# Convert JSON to tibble; convert "" values to NA
df_u3 <- jsonlite::fromJSON(json_u3) %>% as_tibble() %>%  na_if("")

# __ MiNDSet Registry

# Retrieve JSON object
json_ms <- 
  RCurl::postForm(
    uri     = REDCAP_API_URI,
    token   = REDCAP_API_TOKEN_MINDSET,
    content = 'record',
    format  = 'json',
    type    = 'flat',
    fields  = fields_ms,
    rawOrLabel             = 'label',
    rawOrLabelHeaders      = 'raw',
    exportCheckboxLabel    = 'false',
    exportSurveyFields     = 'false',
    exportDataAccessGroups = 'false',
    returnFormat           = 'json',
    filterLogic            = '([exam_date] >= "2017-03-28")',
    # .opts = list(ssl.verifypeer = TRUE, verbose = TRUE) # see note below*
    .opts = list(ssl.verifypeer = FALSE, verbose = TRUE)
  ) %>% 
  str_replace_all(pattern = "\r\n", replacement = " ")
# Convert JSON to tibble; convert "" values to NA
df_ms <- jsonlite::fromJSON(json_ms) %>% as_tibble() %>% na_if("")


# PROCESS DATA ----

# _ Clean Data ----

# __ UDS 3 ----

df_u3 <- df_u3 %>% 
  # deselect useless field(s)
  select(-redcap_event_name) %>%
  # remove records without visit dates
  filter(!is.na(form_date)) %>% 
  # remove double data entry (DDE) records
  filter(str_detect(ptid, pattern = "^UM\\d{8}$")) %>% 
  # keep only distinct / non-duplicate rows
  distinct(ptid, form_date, .keep_all = TRUE)

# __ MiNDSet Registry ----

df_ms <- df_ms %>% 
  # deselect useless field(s)
  select(-redcap_event_name) %>% 
  # remove records without visit dates
  filter(!is.na(exam_date)) %>% 
  # remove non UM ID records
  filter(str_detect(subject_id, pattern = "^UM\\d{8}$")) %>% 
  # rename `race_value` field to `race`
  rename(race = race_value) %>% 
  # keep only distinct / non-duplicate rows
  distinct(subject_id, exam_date, .keep_all = TRUE)

# _ Mutate Data ----

# __ UDS 3 ----

df_u3 <- df_u3 %>% 
  # coalesce IVP / FVP / TVP columns
  coalesce_ift_cols() %>% 
  # convert `sex` and `race` fields from int to string
  mutate(
    sex = case_when(
      sex == 1 ~ "Male",
      sex == 2 ~ "Female",
      TRUE ~ NA_character_
    )
  ) %>% 
  # simplify UDS 3 dx
  mutate(uds_dx = dplyr::case_when(
    # Initial visits
    normcog == 1                    ~ "NL",
    mciamem == 1  | mciaplus == 1 |
      mcinon1 == 1  | mcinon2 == 1  ~ "MCI",
    impnomci == 1                   ~ "Impaired, not MCI",
    alzdis == 1   & alzdisif == 1   ~ "AD",
    lbdis == 1    & lbdif == 1      ~ "LBD",
    psp == 1      & pspif == 1      ~ "FTD",
    cort == 1     & cortif == 1     ~ "FTD",
    ftldmo == 1   & ftldmoif == 1   ~ "FTD",
    ftldnos == 1  & ftldnoif == 1   ~ "FTD",
    TRUE                            ~ "Other"
  )) %>%
  calculate_visit_num(ptid, form_date) %>% 
  select(
    -normcog,                                # NL
    -mciamem, -mciaplus, -mcinon1, -mcinon2, # MCI
    -impnomci,                               # Impaired, not MCI
    -alzdis, -alzdisif,                      # AD
    -lbdis, -lbdif,                          # LBD
    -psp, -pspif, -cort, -cortif,            # FTD
    -ftldmo, -ftldmoif, -ftldnos, -ftldnoif  # FTD
  ) %>% 
  readr::type_convert(
    col_types = readr::cols(.default  = readr::col_integer(),
                            ptid      = readr::col_character(),
                            form_date = readr::col_date(),
                            uds_dx    = readr::col_character(),
                            sex       = readr::col_character(),
                            race      = readr::col_character())) %>% 
  mutate_at(vars(cancer:hyposom),
            function(x) { 
              case_when(
                is.na(x) | x == 0L ~ 0L,
                x > 0L ~ 1L,
                TRUE ~ NA_integer_)})

# __ MiNDSet Registry ----

df_ms <- df_ms %>% 
  readr::type_convert(
    col_types = readr::cols(.default = readr::col_character(),
                            exam_date = readr::col_date(),
                            scored = readr::col_date(),
                            dbl_scored = readr::col_date(),
                            consensus_date = readr::col_date(),
                            second_consensus = readr::col_date(),
                            fb_date = readr::col_date())
  ) %>% 
  mutate(zip_code = str_sub(zip_code, 1, 5),
         blood_drawn = str_replace(blood_drawn, "\\d{1,}\\. ", ""),
         sample_given = str_replace(sample_given, "\\d{1,} ", ""),
         comp_withd = case_when(
           comp_withd == "Y" ~ "Yes",
           comp_withd == "N" ~ "No",
           TRUE ~ NA_character_
         ),
         mri_completed = str_replace(mri_completed, "\\d{1,}\\. ", ""))



# _ Join Data ----
df_u3_ms <- left_join(x = df_u3, 
                      y = df_ms, 
                      by = c("ptid" = "subject_id", 
                             "form_date" = "exam_date"))

# _ Hash `ptid` field ----
df_u3_ms <- df_u3_ms %>% 
  mutate(ptid = openssl::sha256(ptid))


# _ Write Data ----
saveRDS(df_u3_ms, paste0(path_to_app, "rds/df_u3_ms.Rds"))


###@    #==--  :  --==#    @##==---==##@##==---==##@    #==--  :  --==#    @###
#==##@    #==-- --==#    @##==---==##@   @##==---==##@    #==-- --==#    @##==#
#--==##@    #==-==#    @##==---==##@   #   @##==---==##@    #==-==#    @##==--#
#=---==##@    #=#    @##==---==##@    #=#    @##==---==##@    #=#    @##==---=#
##==---==##@   #   @##==---==##@    #==-==#    @##==---==##@   #   @##==---==##
#@##==---==##@   @##==---==##@    #==-- --==#    @##==---==##@   @##==---==##@#
#  @##==---==##@##==---==##@    EXTRA  :  SPACE    @##==---==##@##==---==##@  #
#@##==---==##@   @##==---==##@    #==-- --==#    @##==---==##@   @##==---==##@#
##==---==##@   #   @##==---==##@    #==-==#    @##==---==##@   #   @##==---==##
#=---==##@    #=#    @##==---==##@    #=#    @##==---==##@    #=#    @##==---=#
#--==##@    #==-==#    @##==---==##@   #   @##==---==##@    #==-==#    @##==--#
#==##@    #==-- --==#    @##==---==##@   @##==---==##@    #==-- --==#    @##==#
###@    #==--  :  --==#    @##==---==##@##==---==##@    #==--  :  --==#    @###
#==##@    #==-- --==#    @##==---==##@   @##==---==##@    #==-- --==#    @##==#
#--==##@    #==-==#    @##==---==##@   #   @##==---==##@    #==-==#    @##==--#
#=---==##@    #=#    @##==---==##@    #=#    @##==---==##@    #=#    @##==---=#
##==---==##@   #   @##==---==##@    #==-==#    @##==---==##@   #   @##==---==##
#@##==---==##@   @##==---==##@    #==-- --==#    @##==---==##@   @##==---==##@#
#  @##==---==##@##==---==##@    EXTRA  :  SPACE    @##==---==##@##==---==##@  #
#@##==---==##@   @##==---==##@    #==-- --==#    @##==---==##@   @##==---==##@#
##==---==##@   #   @##==---==##@    #==-==#    @##==---==##@   #   @##==---==##
#=---==##@    #=#    @##==---==##@    #=#    @##==---==##@    #=#    @##==---=#
#--==##@    #==-==#    @##==---==##@   #   @##==---==##@    #==-==#    @##==--#
#==##@    #==-- --==#    @##==---==##@   @##==---==##@    #==-- --==#    @##==#
###@    #==--  :  --==#    @##==---==##@##==---==##@    #==--  :  --==#    @###
