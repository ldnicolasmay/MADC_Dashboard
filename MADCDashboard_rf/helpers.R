# helpers.R

library(dplyr)
library(stringr)
library(lubridate)
library(rlang)

# Useful helper functions used throughout MADC database specialist work

# _ REDCap API helper function
rc_api_get <- function(uri     = REDCAP_API_URI,
                       token,
                       fields  = "",
                       forms   = "",
                       records = NULL,
                       raw     = TRUE) {
  RCurl::postForm(
    uri                    = uri,
    token                  = token,
    content                = "record",
    format                 = "json",
    type                   = "flat",
    fields                 = fields,
    forms                  = forms,
    records                = records,
    rawOrLabel             = ifelse(raw, "raw", "label"),
    rawOrLabelHeaders      = ifelse(raw, "raw", "label"),
    exportCheckboxLabel    = "false",
    exportSurveyFields     = "false",
    exportDataAccessGroups = "false",
    returnFormat           = "json"
  ) %>% 
    str_replace_all(pattern = "\r\n", replacement = " ")
}

# Helper function
#' ## Collapse IVP / FVP / TVP fields based on IVP base name
#' ## Uses rlang non-standard evalution
#' ### 
#' #' Example use in loop:
#' #' # Copy the data frame with collapsible fields
#' #' df_copy <- df
#' #' 
#' #' # Define which fields have "fu_" and "tele_" counterparts
#' #' collapsible_fields <- c("sex", "maristat")
#' #' 
#' #' # Loop over each collapsible field, and do the collapsing
#' #' # NOTICE USE OF QUASIQUOTATION !! IN 2nd FUNCTION ARGUMENT 
#' #' for (field in collapsible_fields) {
#' #'   df_copy <- collapse_ift_cols(df_copy, !!field) # <= NOTICE !!
#' #' }
#' #'
#' ###
#' collapse_ift_cols <- function(df, col_i) {
#'   col_i_enquo <- enquo(col_i) # col : expr => quosure
#'   
#'   col_i_quoname <- quo_name(col_i_enquo)          # col_i : quosure => string
#'   col_f_quoname <- paste0("fu_", col_i_quoname)   # col_f : string => string
#'   col_t_quoname <- paste0("tele_", col_i_quoname) # col_t : string => string
#'   
#'   col_f_enquo <- enquo(col_f_quoname) # col_f : string => quosure
#'   col_t_enquo <- enquo(col_t_quoname) # col_t : string => quosure
#'   
#'   # IVP, FVP (fu_), and TVP (tele_) columns are in df
#'   if (!is.null(df[[col_i_quoname]]) &
#'       !is.null(df[[col_f_quoname]]) &
#'       !is.null(df[[col_t_quoname]])) {
#'     df %>%
#'       mutate(!!col_i_enquo := coalesce(df[[col_i_quoname]],
#'                                        df[[col_f_quoname]],
#'                                        df[[col_t_quoname]])) %>%
#'       select(-!!col_f_enquo, -!!col_t_enquo)
#'   } 
#'   # IVP and FVP (fu_) columns are in df
#'   else if (!is.null(df[[col_i_quoname]]) &
#'            !is.null(df[[col_f_quoname]]) &
#'            is.null(df[[col_t_quoname]])) {
#'     df %>%
#'       mutate(!!col_i_enquo := coalesce(df[[col_i_quoname]],
#'                                        df[[col_f_quoname]])) %>%
#'       select(-!!col_f_enquo)
#'   } 
#'   # IVP and TVP (tele_) columns are in df
#'   else if (!is.null(df[[col_i_quoname]]) &
#'            is.null(df[[col_f_quoname]]) &
#'            !is.null(df[[col_t_quoname]])) {
#'     df %>%
#'       mutate(!!col_i_enquo := coalesce(df[[col_i_quoname]],
#'                                        df[[col_t_quoname]])) %>%
#'       select(-!!col_t_enquo)
#'   }
#' }
coalesce_ift_cols <- function(df) {
  
  # Get collapsible fields and the correpsonding
  # follow-up visit `fu_` and telephone visit `tele_` fields
  i_cols <- get_ift_dups(names(df)) # collapsible_fields
  f_cols <- paste0("fu_", i_cols)
  t_cols <- paste0("tele_", i_cols)
  
  # Iterate over collapsible fields `i_cols`,
  # coalescing `fu_` and/or `tele_` fields into `i_cols`
  for (i in seq_along(i_cols)) {
    # print(paste(i, i_cols[i], f_cols[i], t_cols[i]))
    # IVP, FVP (fu_), and TVP (tele_) columns are in df
    if (!is.null(df[[i_cols[i]]]) &
        !is.null(df[[f_cols[i]]]) &
        !is.null(df[[t_cols[i]]])) {
      df = df %>%
        mutate(!!i_cols[i] := coalesce(df[[i_cols[i]]],
                                       df[[t_cols[i]]],
                                       df[[f_cols[i]]])) %>%
        select(-!!f_cols[[i]] , -!!t_cols[[i]])
    } 
    # IVP and FVP (fu_) columns are in df
    else if (!is.null(df[[i_cols[i]]]) &
             !is.null(df[[f_cols[i]]]) &
             is.null(df[[t_cols[i]]])) {
      df = df %>%
        mutate(!!i_cols[i] := coalesce(df[[i_cols[i]]],
                                       df[[f_cols[i]]])) %>%
        select(-!!f_cols[i])
    }
    # IVP and TVP (tele_) columns are in df
    else if (!is.null(df[[i_cols[i]]]) &
             is.null(df[[f_cols[i]]]) &
             !is.null(df[[t_cols[i]]])) {
      df = df %>%
        mutate(!!i_cols[i] := coalesce(df[[i_cols[i]]],
                                       df[[t_cols[i]]])) %>%
        select(-!!t_cols[i])
    }
  }
  
  # Return df
  df
} 


# Helper function
# Get UDS 3 field names that already exist in IVP form,
# but also exist in FVP and/or TVP forms (prefixed with "fu_" and/or "tele_").
# input:  c("dob", "sex", "fu_sex", "maristat", "fu_maristat", "tele_maristat")
# output: c("sex", "maristat")
get_ift_dups <- function(x) {
  unique(
    str_replace_all(
      string = x[str_detect(string = x, pattern = "fu_|tele_")],
      pattern = "fu_|tele_",
      replacement = ""
    )
  )  
}


# Helper function
# Calculate age from DOB to Visit Date
# Example function call:
#   calculate_age(df_u3, birth_date, form_date)
calculate_age <- function(df, birth_date, age_date) {
  enquo_birth_date = enquo(birth_date)
  enquo_age_date = enquo(age_date)
  df %>% 
    mutate(
      age_years = as.period(interval(!! enquo_birth_date, !! enquo_age_date),
                            unit = "years")$year,
      age_exact = time_length(
        interval(start = !! enquo_birth_date,
                 end = !! enquo_age_date),
        unit = "year"
      ),
      age_units = as.period(interval(start = !! enquo_birth_date, 
                                     end = !! enquo_age_date), 
                            unit = "years"))
}

## Helper function
# Propograte a value across rows where value is missing for a given
# participant ID
propagate_value <- function(df, id_field, date_field, value_field) {
  
  if (nrow(df) < 2) {
    stop("passed dataframe needs at least 2 rows")
  }
  
  enquo_id_field <- enquo(id_field)
  enquo_date_field <- enquo(date_field)
  # enquo_value_field <- enquo(value_field)
  
  quo_name_enquo_id_field <- quo_name(enquo_id_field)
  quo_name_enquo_value_field <- quo_name(enquo(value_field))
  
  df = df %>% 
    arrange(!! enquo_id_field, !! enquo_date_field)
  
  for (i in 2:nrow(df)) {
    prev_id <- df[[i-1, quo_name_enquo_id_field]]
    curr_id <- df[[i, quo_name_enquo_id_field]]
    prev_value <- df[[i-1, quo_name_enquo_value_field]]
    curr_value <- df[[i, quo_name_enquo_value_field]]
    
    if (prev_id == curr_id && !is.na(prev_value) && is.na(curr_value)) {
      df[[i, quo_name_enquo_value_field]] <- 
        df[[i-1, quo_name_enquo_value_field]]
    }
  }
  
  df = df %>% 
    arrange(!! enquo_id_field, desc(!! enquo_date_field))
  
  for (i in 2:nrow(df)) {
    prev_id <- df[[i-1, quo_name_enquo_id_field]]
    curr_id <- df[[i, quo_name_enquo_id_field]]
    prev_value <- df[[i-1, quo_name_enquo_value_field]]
    curr_value <- df[[i, quo_name_enquo_value_field]]
    
    if (prev_id == curr_id && !is.na(prev_value) && is.na(curr_value)) {
      df[[i, quo_name_enquo_value_field]] <- 
        df[[i-1, quo_name_enquo_value_field]]
    }
  }
  
  df = df %>% 
    arrange(!! enquo_id_field, !! enquo_date_field)
  
  df
}
# test_df <- tibble(
#   id = c("A001", "A001", "A001",
#          "A002", "A002", "A002",
#          "A003", "A003", "A003"),
#   date = as.Date(c("2019-01-01", "2020-01-01", "2021-01-01",
#                    "2019-02-01", "2020-02-01", "2021-02-01",
#                    "2019-03-01", "2020-03-01", "2021-03-01")),
#   value = c(NA_integer_, 1L, NA_integer_, 
#             NA_integer_, 2L, NA_integer_,
#             NA_integer_, 3L, NA_integer_)
# )
# test_df
# propogate_value(test_df, id, date, value)

## Helper function
# Add a `visit_num` field after `id_field` and `date_field`
calculate_visit_num <- function(df, id_field, date_field) {
  enquo_id_field <- enquo(id_field)
  enquo_date_field <- enquo(date_field)
  
  df %>% 
    arrange(!! enquo_id_field, !! enquo_date_field) %>% 
    mutate(visit_unit = 1L) %>% 
    group_by(!! enquo_id_field) %>% 
    mutate(visit_num = case_when(
      !is.na(!! enquo_date_field) ~ as.integer(cumsum(visit_unit)),
      TRUE ~ NA_integer_
    )) %>%
    select(-visit_unit) %>% 
    ungroup() %>% 
    select(!! enquo_id_field, !! enquo_date_field, visit_num, everything())
}
# test_df <- tibble(
#   id = c("A001", "A001", "A001",
#          "A002", "A002", "A002",
#          "A003", "A003", "A003"),
#   date = as.Date(c("2019-01-01", "2020-01-01", "2021-01-01",
#                    "2019-02-01", NA_character_, "2021-02-01",
#                    "2019-03-01", "2020-03-01", "2021-03-01")),
#   value = c(NA_integer_, 1L, NA_integer_,
#             NA_integer_, 2L, NA_integer_,
#             NA_integer_, 3L, NA_integer_)
# )
# test_df
# calculate_visit_num(test_df, id, date)


## Helper function
# n is the visit number
# Use n = -Inf for the earliest visit
# Use n = Inf for the latest visit
get_visit_n <- function(df, id_field, date_field, n) {
  # Capture what user passed as `id_field` and `date_field`
  enquo_id_field <- enquo(id_field)
  enquo_date_field <- enquo(date_field)
  
  # Handle n:
  #   if n is finite, capture the number passed by user
  #   if n is negative infinity, set expression to get earliest visit
  #   if n is positive infinity, set expression to get latest visit
  if (is.finite(n)) { vis_cnt_fltr <- enquo(n) }
  else if (n < 0) { vis_cnt_fltr <- expr(min(visit_count)) }
  else { vis_cnt_fltr <- expr(max(visit_count)) }
  
  df %>% 
    filter(!is.na(!!enquo_id_field)) %>% 
    filter(!is.na(!!enquo_date_field)) %>%
    arrange(!!enquo_id_field, !!enquo_date_field) %>% 
    mutate(visit_unit = 1L) %>% 
    group_by(!!enquo_id_field) %>% 
    mutate(visit_count = cumsum(visit_unit)) %>%
    filter(visit_count == !!vis_cnt_fltr) %>% 
    ungroup() %>% 
    select(-visit_unit, -visit_count)
}

