# helper_fxns_get_data.R

# Helper functions for building UDS report summary table

# Load Libraries ----

library(dplyr)
library(rlang)
library(purrr)
library(DT)


# Helper Functions ----

summarize_timeline <- function(df, timeline_field) {
  timeline_field <- enquo(timeline_field)
  
  df %>% 
    summarize(N = n(), 
              Mean = round(mean(!!timeline_field), 2), 
              SD = round(sd(!!timeline_field), 2), 
              Min = min(!!timeline_field), 
              Median = round(median(!!timeline_field), 2), 
              Max = max(!!timeline_field)) %>% 
    datatable(options = list(paging    = FALSE,
                             searching = FALSE,
                             ordering  = FALSE,
                             info      = FALSE),
              rownames  = FALSE)
}

# # 
# # Accepts a dataframe and an arbritrary number of filtering expressions and 
# # grouping variables, then applies the filters and tallies the number of cases
# # in the dataframe by the grouping variables provided.
# # Ex:
# # filter_group_tally(my_dataframe, sex, race, uds_dx == "AD")
# filter_group_tally <- function(df, ...) {
#   args <- enexprs(...)
#   group_args <- args[map_lgl(args, is_symbol)]
#   filter_args <- args[map_lgl(args, is_call)]
#   
#   df %>% 
#     filter(!!!filter_args) %>%  # filtering faster on ungrouped data
#     group_by(!!!group_args) %>% 
#     tally()
# }
# 
# 
# vctr_filter_group_tally <- function(df, groups) {
#   groups <- syms(groups)
#   df %>%
#     dplyr::group_by(!!!groups) %>%
#     dplyr::tally()
# }
# vctr_filter_group_tally(df, groups = c("b", "c"))
# 
# 
# # df <- tibble::tibble(
# #   a = 1:5,
# #   b = c("a", "a", "b", "a", "b"),
# #   c = c("c", "d", "c", "c", "d")
# # )
# # list("race_value", "sex_value")
# # c("race_value", "sex_value")
# # cat(c("race_value", "sex_value"))



