# helper_fxns_get_data.R

# Helper functions for building UDS report summary table

# Load Libraries ----

suppressMessages( library(dplyr) )
suppressMessages( library(rlang) )
suppressMessages( library(purrr) )
suppressMessages( library(DT) )


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
