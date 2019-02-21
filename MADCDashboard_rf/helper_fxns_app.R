# helper_fxns_get_data.R

# Helper functions for building UDS report summary table

# Load Libraries ----

library(dplyr)
library(rlang)
library(purrr)


# Helper Functions ----

# 
# Accepts a dataframe and an arbritrary number of filtering expressions and 
# grouping variables, then applies the filters and tallies the number of cases
# in the dataframe by the grouping variables provided.
# Ex:
# filter_group_tally(my_dataframe, sex, race, uds_dx == "AD")
filter_group_tally <- function(df, ...) {
  args <- enexprs(...)
  group_args <- args[map_lgl(args, is_symbol)]
  filter_args <- args[map_lgl(args, is_call)]
  
  df %>% 
    filter(!!!filter_args) %>%  # filtering faster on ungrouped data
    group_by(!!!group_args) %>% 
    tally()
}


list_filter_group_tally <- function(df, groups) {
  groups <- rlang::enquos(groups)
  df %>% 
    dplyr::group_by(!!!groups) %>% 
    dplyr::tally()
}
list_filter_group_tally(df, groups = list("sex_value", "race_value"))


df <- tibble::tibble(
  a = 1:5,
  b = c("a", "a", "b", "a", "b"),
  c = c("c", "d", "c", "c", "d")
)
list("sex_value", "race_value")





