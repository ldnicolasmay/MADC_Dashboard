# helper_fxns_get_data.R

# Helper functions for building UDS report summary table

DT_OPTIONS <- list(paging    = FALSE,
                   searching = FALSE,
                   ordering  = FALSE,
                   info      = FALSE)

# Helper Functions ----

summarize_timeline <- function(df, timeline_field) {
  timeline_field <- enquo(timeline_field)
  
  df %>% 
    summarize(
      N = n(), 
      # Mean   = round(mean(!!timeline_field), 2), 
      # SD     = round(sd(!!timeline_field), 2), 
      Min    = min(!!timeline_field, na.rm = TRUE), 
      Q1     = round(quantile(!!timeline_field, probs = 0.25, na.rm = TRUE), 2),
      Median = round(median(!!timeline_field, na.rm = TRUE), 2),
      Q3     = round(quantile(!!timeline_field, probs = 0.75, na.rm = TRUE), 2),
      Max    = max(!!timeline_field, na.rm = TRUE)
    ) %>% 
    datatable(options = DT_OPTIONS,
              rownames  = FALSE)
}
