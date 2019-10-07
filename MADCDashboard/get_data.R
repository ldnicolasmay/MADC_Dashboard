#!/usr/bin/env Rscript

# get_data.R


# USEFUL LIBRARIES ----
suppressMessages( library(dplyr)   )
suppressMessages( library(tidyr)   )
suppressMessages( library(readr)   )
suppressMessages( library(crayon)  )
suppressMessages( library(stringr) )



# HANDLE SCRIPT EXEC ARGS ----

# Simplifies local-testing v. server-deployed execution of this script
args <- commandArgs(trailingOnly = TRUE)
args_vct <- sub("--.*=(.*)", "\\1", args)
names(args_vct) <- sub("--(.*)=.*", "\\1", args)

tryCatch(
  ENV <- args_vct[["env"]],
  error = function(e) {
    # message()
    stop(red(paste0("`--env` arugment not set: ",
                    "Use either `--env=local` or `--env=server`")))
  }
)

tryCatch(
  DEV <- args_vct[["dev"]],
  error = function(e) {
    # message()
    stop(red(paste0("`--dev` arugment not set: ",
                    "Use either `--dev=local` or `--dev=docker`")))
  }
)



# USEFUL GLOBALS & FUNCTIONS ----

if (ENV == "server") { # ignore `DEV` constant
  # Michigan Medicine R Shiny server
  path_to_app <- "~/ShinyApps/MADCDashboard/"
} else if (ENV == "local" && DEV == "local") {
  # Local development w/o Docker container
  warning(
    red(paste("Running this script in a local environment without Docker is",
              bold(underline("NOT")), "recommended!"))
  )
  path_to_app <- "~/Box/Documents/MADC_Dashboard/MADCDashboard/"
} else if (ENV == "local" && DEV == "docker") {
  # Local development on Docker container
  path_to_app <- "/Box/Documents/MADC_Dashboard/MADCDashboard/"
}
source(paste0(path_to_app, "config.R"), local = TRUE)
source(paste0(path_to_app, "helpers.R"), local = TRUE)



# GET DATA ----
if (ENV == "local") { cat(cyan("Retrieving data via REDCap API\n")) }

# _ Define Data Fields / Forms ----
if (ENV == "local") { cat(cyan("  - Defining data fields and forms\n")) }

# _ _ UDS 3 ----
if (ENV == "local") { cat(cyan("    * UM-MAP\n")) }

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
    , "hispanic"
  ) %>% c(., paste0("fu_", .), paste0("tele_", .))
# Form D1 (IVP, FVP, TVP)
fields_u3_d1_raw <-
  c(
    # Diagnosis
    "normcog"    # NL
    , "demented" # Demented
    , "amndem"   # Amnestic multidomain dementia syndrome
    , "pca"      # Posterior cortical atrophy syndrome
    , "ppasyn"   # Primary progressive aphasia (PPA) syndrome
    , "ftdsyn"   # Behavioral variant FTD (bvFTD) syndrome
    , "lbdsyn"   # Lewy body dementia syndrome
    , "namndem"  # Non-amnestic multidomain dementia syndrome
    , "mciamem"  # Amnestic MCI, single domain (aMCI SD) 
    , "mciaplus" # Amnestic MCI, multiple domains (aMCI MD)
    , "mcinon1"  # Non-amnestic MCI, single domain (naMCI SD)
    , "mcinon2"  # Non-amnestic MCI, multiple domains (naMCI MD)
    , "impnomci" # Cognitively impaired, not MCI
    # Etiology
    , "alzdis"   # Alzheimer's disease
    , "alzdisif" 
    , "lbdis"    # Lewy body disease
    , "lbdif" 
    , "msa"      # Multiple system atrophy
    , "msaif"
    , "psp"      # Progressive supranuclear palsy (PSP)
    , "pspif"
    , "cort"     # Corticobasal degeneration (CBD)
    , "cortif"
    , "ftldmo"   # FTLD with motor neuron disease
    , "ftldmoif"
    , "ftldnos"  # FTLD NOS
    , "ftldnoif"
    , "cvd"      # Vascular Brain injury (clinical or imaging evidence)
    , "cvdif" 
    , "esstrem"  # Essential tremor
    , "esstreif"
    , "downs"    # Down syndrome
    , "downsif"
    , "hunt"     # Huntington's disease
    , "huntif"
    , "prion"    # Prion disease (CJD, other)
    , "prionif"
    , "brninj"   # Traumatic brain injury
    , "brninjif"
    , "hyceph"   # Normal-pressure hydrocephalus
    , "hycephif"
    , "epilep"   # Epilepsy
    , "epilepif"
    , "neop"     # CNS neoplasm
    , "neopif" 
    , "hiv"      # Human immunodeficiency virus (HIV)
    , "hivif"
    # Condition
    , "dep"      # Active depression
    , "depif" 
    , "bipoldx"  # Bipolar disorder
    , "bipoldif"
    , "schizop"  # Schizophrenia or other psychosis
    , "schizoif"
    , "anxiet"   # Anxiety disorder
    , "anxietif"
    , "delir"    # Delirium
    , "delirif"
    , "ptsddx"   # Post-traumatic stress disorder (PTSD)
    , "ptsddxif"
    , "othpsy"   # Other psychiatric disease
    , "othpsyif"
    , "alcdem"   # Cognitive impairment due to alcohol abuse
    , "alcdemif"
    , "impsub"   # Cognitive impairment due to other substance abuse
    , "impsubif"
    , "dysill"   # Cognitive impairment due to systemic disease/medical illness
    , "dysillif"
    , "meds"     # Cognitive impairment due to medications
    , "medsif"
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
# Form M1 Milestone data
fields_u3_m1_raw <-
  c(
    "note_mlstn_type"
    # , "protocol"
    , "deceased"
    , "discont"
  )
# Combine and collapse `fields_u3_*_raw` vectors
fields_u3 <- c(fields_u3_hd_raw
               , fields_u3_a1_raw
               , fields_u3_d1_raw
               , fields_u3_d2_raw
               , fields_u3_m1_raw) %>% paste(collapse = ",")
rm(fields_u3_hd_raw)
rm(fields_u3_a1_raw)
rm(fields_u3_d1_raw)
rm(fields_u3_d2_raw)
rm(fields_u3_m1_raw)

# _ _ MiNDSet Registry ----
if (ENV == "local") { cat(cyan("    * MiNDSet Registry\n")) }

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
    # 'blood_drawn'          # research
    'consent_to_autopsy' # research
    , 'mri_completed'      # research
    # , 'sample_given'       # research
    # , 'comp_withd'         # research
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

fields_ms_blood_raw <- 
  c(
    "subject_id"
    , "blood_drawn"
  )
fields_ms_blood <- fields_ms_blood_raw %>% paste(collapse = ",")

fields_ms_saliva_raw <-
  c(
    "subject_id"
    , "sample_given"
  )
fields_ms_saliva <- fields_ms_saliva_raw %>% paste(collapse = ",")


# _ Retrieve JSON ----
if (ENV == "local") { cat(cyan("  - Retrieving JSON\n")) }

# _ _ UDS 3 ----
if (ENV == "local") cat(cyan("    * UM-MAP\n"))

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
    # .opts = list(ssl.verifypeer = TRUE, verbose = FALSE) # see note below*
    .opts = list(ssl.verifypeer = FALSE, verbose = FALSE)
  ) %>% 
  str_replace_all(pattern = "\r\n", replacement = " ")
# Convert JSON to tibble; convert "" values to NA
df_u3 <- jsonlite::fromJSON(json_u3) %>% as_tibble() %>%  na_if("")

# _ _ MiNDSet Registry ----
if (ENV == "local") cat(cyan("    * MiNDSet Registry\n"))

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
    # .opts = list(ssl.verifypeer = TRUE, verbose = FALSE) # see note below*
    .opts = list(ssl.verifypeer = FALSE, verbose = FALSE)
  ) %>% 
  str_replace_all(pattern = "\r\n", replacement = " ")
# Convert JSON to tibble; convert "" values to NA
df_ms <- jsonlite::fromJSON(json_ms) %>% as_tibble() %>% na_if("")

json_ms_blood <-
  RCurl::postForm(
    uri     = REDCAP_API_URI,
    token   = REDCAP_API_TOKEN_MINDSET,
    content = 'record',
    format  = 'json',
    type    = 'flat',
    fields  = fields_ms_blood,
    rawOrLabel             = 'label',
    rawOrLabelHeaders      = 'raw',
    exportCheckboxLabel    = 'false',
    exportSurveyFields     = 'false',
    exportDataAccessGroups = 'false',
    returnFormat           = 'json',
    # filterLogic            = '([exam_date] >= "2017-03-28")',
    # .opts = list(ssl.verifypeer = TRUE, verbose = FALSE) # see note below*
    .opts = list(ssl.verifypeer = FALSE, verbose = FALSE)
  ) %>% 
  str_replace_all(pattern = "\r\n", replacement = " ")
# Convert JSON to tibble; convert "" values to NA
df_ms_blood <- jsonlite::fromJSON(json_ms_blood) %>% as_tibble() %>% na_if("")

json_ms_saliva <-
  RCurl::postForm(
    uri     = REDCAP_API_URI,
    token   = REDCAP_API_TOKEN_MINDSET,
    content = 'record',
    format  = 'json',
    type    = 'flat',
    fields  = fields_ms_saliva,
    rawOrLabel             = 'label',
    rawOrLabelHeaders      = 'raw',
    exportCheckboxLabel    = 'false',
    exportSurveyFields     = 'false',
    exportDataAccessGroups = 'false',
    returnFormat           = 'json',
    # filterLogic            = '([exam_date] >= "2017-03-28")',
    # .opts = list(ssl.verifypeer = TRUE, verbose = FALSE) # see note below*
    .opts = list(ssl.verifypeer = FALSE, verbose = FALSE)
  ) %>% 
  str_replace_all(pattern = "\r\n", replacement = " ")
# Convert JSON to tibble; convert "" values to NA
df_ms_saliva <- jsonlite::fromJSON(json_ms_saliva) %>% as_tibble() %>% na_if("")


# PROCESS DATA ----
if (ENV == "local") cat(cyan("Processing data\n"))


# _ Clean Data ----
if (ENV == "local") cat(cyan("  - Cleaning data\n"))

# _ _ UDS 3 ----
if (ENV == "local") cat(cyan("    * UM-MAP\n"))

# Split off useful milestone data
df_u3_mlstn <- 
  df_u3 %>% 
  select(ptid, note_mlstn_type, deceased, discont) %>% 
  filter(str_detect(ptid, "^UM\\d{8}$")) %>% 
  filter(!is.na(note_mlstn_type)) %>% 
  filter(note_mlstn_type == 0)

# Rejoin useful milestone data to `df_u3`
df_u3 <-
  df_u3 %>% 
  select(-note_mlstn_type, -deceased, -discont) %>% 
  left_join(df_u3_mlstn, by = "ptid") %>% 
  select(
    ptid
    , form_date
    , redcap_event_name
    , note_mlstn_type
    , deceased
    , discont
    , everything()
  )

# General clean
df_u3 <- df_u3 %>% 
  # deselect useless field(s)
  select(-redcap_event_name) %>%
  # remove records without visit dates
  filter(!is.na(form_date)) %>% 
  # remove double data entry (DDE) records
  filter(str_detect(ptid, pattern = "^UM\\d{8}$")) %>% 
  # keep only distinct / non-duplicate rows
  distinct(ptid, form_date, .keep_all = TRUE) %>% 
  type_convert(
    col_types = cols(.default = col_integer(),
                     ptid     = col_character(),
                     redcap_event_name = col_character(),
                     form_date = col_date())
  )

# _ _ MiNDSet Registry ----
if (ENV == "local") cat(cyan("    * MiNDSet Registry\n"))

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

df_ms_blood <- df_ms_blood %>% 
  # deselect useless field(s)
  select(-redcap_event_name) %>% 
  # clean up messy labelled data values
  mutate(blood_drawn = str_extract(blood_drawn, "Yes")) %>% 
  # remove records without `blood_drawn` == "Yes"
  filter(blood_drawn == "Yes") %>% 
  # remove non UM ID records
  filter(str_detect(subject_id, pattern = "^UM\\d{8}$")) %>% 
  # keep only distinct / non-duplicate rows
  distinct(subject_id, blood_drawn, .keep_all = TRUE)

df_ms_saliva <- df_ms_saliva %>% 
  # deselect useless field(s)
  select(-redcap_event_name) %>% 
  # clean up messy labelled data values
  mutate(sample_given = str_extract(sample_given, "Yes")) %>% 
  # remove records without `blood_drawn` == "Yes"
  filter(sample_given == "Yes") %>% 
  # remove non UM ID records
  filter(str_detect(subject_id, pattern = "^UM\\d{8}$")) %>% 
  # keep only distinct / non-duplicate rows
  distinct(subject_id, sample_given, .keep_all = TRUE)


# _ Mutate Data ----

if (ENV == "local") cat(cyan("  - Applying mutates\n"))

# _ _ UDS 3 ----

if (ENV == "local") cat(cyan("    * UM-MAP\n"))

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
  # Propagate values for `sex`
  propagate_value(ptid, form_date, sex) %>%
  # Propagate values for `hispanic`
  propagate_value(ptid, form_date, hispanic) %>% 
  mutate(hispanic = case_when(
    hispanic == 1 ~ "Hispanic",
    TRUE ~ NA_character_
  )) %>% 
  # create MADC diagnosis field
  rowwise() %>% 
  mutate(madc_dx = case_when(
    sum(amndem, pca, ppasyn, ftdsyn, lbdsyn, namndem, na.rm = TRUE) > 1 ~
      "Mixed dementia",
    normcog == 1 ~ "NL",
    normcog == 0 & demented == 0 & 
      (mciamem == 1 | mciaplus == 1 | mcinon1 == 1 | mcinon2 == 1) ~ "MCI",
    normcog == 0 & demented == 0 &
      impnomci == 1 ~ "Impaired not MCI",
    normcog == 0 & demented == 1 & (amndem == 1 | namndem == 1) &
      alzdis == 1 & alzdisif == 1 ~ "AD",
    normcog == 0 & demented == 1 & lbdsyn == 1 ~ "LBD",
    normcog == 0 & demented == 1 & 
      (ftdsyn == 1 | 
         (psp == 1 & pspif == 1) |
         (cort == 1 & cortif == 1) |
         (ftldmo == 1 & ftldmoif == 1) | 
         (ftldnos == 1 & ftldnoif == 1)) ~ "FTD",
    normcog == 0 & demented == 1 &
      cvd == 1 & cvdif == 1 ~ "Vascular dementia",
    normcog == 0 & demented == 1 &
      ppasyn == 1 &
      (is.na(psp) |
         is.na(cort) |
         is.na(ftldmo) |
         is.na(ftldnos)) ~ "PPA",
    demented == 1 ~ "Other dementia",
    TRUE ~ NA_character_
  )) %>% 
  ungroup() %>% 
  # simplify UDS diagnosis fields
  mutate(uds_dx_der = case_when(
    normcog  == 1 ~ "Normal",
    demented == 1 & amndem == 1 ~ 
      "Amnestic multidomain dementia syndrome",
    # "Dementia",
    demented == 1 & pca == 1 ~
      "Posterior cortical atrophy syndrome",
    # "Dementia",
    demented == 1 & ppasyn == 1 ~
      "Primary progressive aphasia syndrome",
    # "Dementia",
    demented == 1 & ftdsyn == 1 ~
      "Behavioral variant FTD syndrome",
    # "Dementia",
    demented == 1 & lbdsyn == 1 ~
      "Lewy body dementia syndrome",
    # "LBD",
    demented == 1 & namndem == 1 ~
      "Non-amnestic multidomain dementia syndrome",
    # "Dementia",
    demented == 0 & mciamem  == 1 ~ "MCI",
    demented == 0 & mciaplus == 1 ~ "MCI",
    demented == 0 & mcinon1  == 1 ~ "MCI",
    demented == 0 & mcinon2  == 1 ~ "MCI",
    demented == 0 & impnomci == 1 ~ "Impaired not MCI",
    TRUE ~ NA_character_
  )) %>% 
  # simplify UDS etiology fields
  mutate(uds_prim_etio = case_when(
    alzdis   == 1 & alzdisif == 1 ~ "AD",
    lbdis    == 1 & lbdif    == 1 ~ "LBD",
    msa      == 1 & msaif    == 1 ~ "MSA",
    psp      == 1 & pspif    == 1 ~ "PSP",
    cort     == 1 & cortif   == 1 ~ "CBD",
    ftldmo   == 1 & ftldmoif == 1 ~ "FTLD with motor neuron disease",
    ftldnos  == 1 & ftldnoif == 1 ~ "FTLD NOS",
    cvd      == 1 & cvdif    == 1 ~ "Vascular brain injury",
    esstrem  == 1 & esstreif == 1 ~ "Essential tremor",
    downs    == 1 & downsif  == 1 ~ "Down syndrome",
    hunt     == 1 & huntif   == 1 ~ "Huntington's disease",
    prion    == 1 & prionif  == 1 ~ "Prion disease",
    brninj   == 1 & brninjif == 1 ~ "Traumatic injury",
    hyceph   == 1 & hycephif == 1 ~ "Normal-pressure hydrocephalus",
    epilep   == 1 & epilepif == 1 ~ "Epilepsy",
    neop     == 1 & neopif   == 1 ~ "CNS neoplasm",
    hiv      == 1 & hivif    == 1 ~ "HIV",
    TRUE ~ NA_character_
  )) %>% 
  # simplify UDS conidtion fields
  mutate(uds_condition = case_when(
    dep      == 1 & depif    == 1 ~ "Active depression",
    bipoldx  == 1 & bipoldif == 1 ~ "Bipoloar disorder",
    schizop  == 1 & schizoif == 1 ~ "Schizophrenia",
    anxiet   == 1 & anxietif == 1 ~ "Anxiety disorder",
    delir    == 1 & delirif  == 1 ~ "Delirium",
    ptsddx   == 1 & ptsddxif == 1 ~ "PTSD",
    othpsy   == 1 & othpsyif == 1 ~ "Other psychiatric disease",
    alcdem   == 1 & alcdemif == 1 ~ 
      "Cognitive impairment due to alcohol abuse",
    impsub   == 1 & impsubif == 1 ~ 
      "Cognitive impairment due to other substance abuse",
    dysill   == 1 & dysillif == 1 ~
      "Cognitive impairment due to systemic disease/medical illness",
    meds     == 1 & medsif   == 1 ~
      "Cognitive impairment due to medications",
    TRUE ~ NA_character_
  )) %>%
  # calculate visit numbers
  calculate_visit_num(ptid, form_date) %>% 
  # deslect now-unnecessary fields
  select(
    -normcog,
    -demented,
    -amndem, 
    -pca, 
    -ppasyn, 
    -ftdsyn, 
    -lbdsyn, 
    -namndem,
    -mciamem,  -mciaplus, 
    -mcinon1,  -mcinon2,
    -impnomci,
    -alzdis,   -alzdisif,
    -lbdis,    -lbdif,
    -msa,      -msaif,
    -psp,      -pspif, 
    -cort,     -cortif,
    -ftldmo,   -ftldmoif, 
    -ftldnos,  -ftldnoif,
    -cvd,      -cvdif,
    -esstrem,  -esstreif,
    -downs,    -downsif,
    -hunt,     -huntif,
    -prion,    -prionif,
    -brninj,   -brninjif,
    -hyceph,   -hycephif,
    -epilep,   -epilepif,
    -neop,     -neopif,
    -hiv,      -hivif,
    -dep,      -depif,
    -bipoldx,  -bipoldif,
    -schizop,  -schizoif,
    -anxiet,   -anxietif,
    -delir,    -delirif,
    -ptsddx,   -ptsddxif,
    -othpsy,   -othpsyif,
    -alcdem,   -alcdemif,
    -impsub,   -impsubif,
    -dysill,   -dysillif,
    -meds,     -medsif
  ) %>%
  # coerce fields to particular data types
  type_convert(
    col_types = cols(.default        = col_integer(),
                     ptid            = col_character(),
                     form_date       = col_date(),
                     madc_dx         = col_character(),
                     uds_dx_der      = col_character(),
                     uds_prim_etio   = col_character(),
                     uds_condition   = col_character(),
                     sex             = col_character(),
                     race            = col_character(),
                     hispanic        = col_character(),
                     note_mlstn_type = col_character())) %>% 
  # ensure 0 or 1 integers fills conditions fields
  mutate_at(vars(cancer:hyposom),
            function(x) {
              case_when(
                is.na(x) | x == 0L ~ 0L,
                x > 0L ~ 1L,
                TRUE ~ NA_integer_)}) %>%
  # create milestone field for deceased, dropped, or telephone FU records
  mutate(milestone = case_when(
    note_mlstn_type == 0L & deceased == 1L ~ "Deceased",
    note_mlstn_type == 0L & discont  == 1L ~ "Dropped",
    # note_mlstn_type == 1L & protocol == 1L ~ "Telephone follow-up",
    TRUE ~ NA_character_
  )) %>%
  # # drop `note_mlstn_type`, `deceased`, `discont`, `protocol`
  # select(-note_mlstn_type, -deceased, -discont, -protocol) 
  # drop `note_mlstn_type`, `deceased`, `discont`
  select(-note_mlstn_type, -deceased, -discont) 

# Add concatenated comorbid conditions fields
df_u3 <- df_u3 %>% 
  # place field name in each comorbid cond field where there's a 1, else NA
  mutate_at(vars(cancer:hyposom),
            function(x) {
              x_name <- as_string(ensym(x))
              case_when(
                x == 1L ~ x_name,
                TRUE ~ NA_character_)}) %>% 
  # unite the cormorbid conditions as a long string, separated by "_"
  unite(col = "condx_combn_name", cancer:hyposom, sep = "_") %>% 
  # Convert instances of "NA_", "_NA", "NA", or "__" to "" (empty string)
  mutate(condx_combn_name = 
           str_replace_all(string = condx_combn_name, 
                           pattern = regex("_NA|NA_|NA|__", ignore_case = FALSE),
                           replacement = ""))

# _ _ MiNDSet Registry ----
if (ENV == "local") cat(cyan("    * MiNDSet Registry\n"))

df_ms <- df_ms %>% 
  # coerce fields to particular data types
  type_convert(
    col_types = cols(.default         = col_character(),
                     exam_date        = col_date(),
                     scored           = col_date(),
                     dbl_scored       = col_date(),
                     consensus_date   = col_date(),
                     second_consensus = col_date(),
                     fb_date          = col_date())
  ) %>% 
  # Propagate values for `race`
  propagate_value(subject_id, exam_date, race) %>% #
  # clean up messy labelled fields
  mutate(zip_code = str_sub(zip_code, 1, 5),
         mri_completed = str_replace(mri_completed, "\\d{1,}\\. ", "")) %>% 
  # add duration columns
  mutate(dur_exam_scrd =        # days duration: exam -> scored
           lubridate::interval(exam_date, scored) /
           lubridate::ddays(1),
         dur_exam_dblscrd =     # days duration: exam -> double scored
           lubridate::interval(exam_date, dbl_scored) /
           lubridate::ddays(1),
         dur_exam_cons =        # days duration: exam -> first consensus
           lubridate::interval(exam_date, consensus_date) /
           lubridate::ddays(1),
         dur_fincons_fdbk =     # days duration: final consensus -> feedback
           lubridate::interval(second_consensus, fb_date) /
           lubridate::ddays(1))

# Fix `blood_drawn` and `sample_given` data sparsity via joins
df_ms <- 
  left_join(x = df_ms,
            y = df_ms_blood,
            by = "subject_id") %>% 
  left_join(x = .,
            y = df_ms_saliva,
            by = "subject_id")


# _ Join Data ----
if (ENV == "local") cat(cyan("  - Joining UM-MAP and MiNDSet Registry\n"))

df_u3_ms <- left_join(x = df_u3, 
                      y = df_ms, 
                      by = c("ptid" = "subject_id", 
                             "form_date" = "exam_date"))


# _ Hash `ptid` field ----
if (ENV == "local") cat(cyan("  - Hashing IDs\n"))

df_u3_ms <- df_u3_ms %>% 
  mutate(ptid = openssl::sha256(ptid))


# _ Rename select fields ----
if (ENV == "local") cat(cyan("  - Renaming select fields\n"))

df_u3_ms <- df_u3_ms %>% 
  rename(
    `Visit Date`      = form_date
    , `Visit Num`     = visit_num
    , `Sex`           = sex
    , `MADC Dx`       = madc_dx
    , `UDS Dx`        = uds_dx_der
    , `UDS Primary Etiology` = uds_prim_etio
    , `UDS Condition` = uds_condition
    , `Milestoned`    = milestone
    , `County`        = county
    , `ZIP Code`      = zip_code
    , `Race`          = race
    , `Hispanic`      = hispanic
    , `Blood Drawn`   = blood_drawn
    , `Saliva Given`  = sample_given
    , `MRI Completed` = mri_completed
    , `Autopsy Consent` = consent_to_autopsy
  )


# WRITE DATA ----
if (ENV == "local") cat(cyan("Writing data to Rds\n"))

saveRDS(df_u3_ms, paste0(path_to_app, "rds/df_u3_ms.Rds"))

invisible( gc() )


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
