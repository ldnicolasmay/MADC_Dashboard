## Helper functions for building UDS report summary table

# # # # # 
## Load libraries ----

library(dplyr)
library(tidyr)

# # # # # 
## Define helper functions ----

## Fxn for outputting tables with one group variable
single_grp_table <- function(x, group_var) {
  distinct_grp_vals <- distinct(x, !!group_var)
  x %>% 
    group_by(!!group_var) %>% 
    summarize(Total = n()) %>%
    right_join(distinct_grp_vals, by = rlang::quo_text(group_var)) %>%
    arrange(!!group_var)
}

## Fxn for outputting tables with one group variable and one filter variable
single_grp_filter_table <- function(x, group_var, 
                                    filter_var, filter_var_string) {
  distinct_grp_vals <- distinct(x, !!group_var)
  x %>% 
    group_by(!!group_var) %>% 
    filter(!!filter_var == filter_var_string) %>% 
    summarize(Total = n()) %>% 
    right_join(distinct_grp_vals, by = rlang::quo_text(group_var)) %>% 
    arrange(!!group_var)
}

## Fxn for outputting tables with two group variables
double_grp_table <- function(x, group_var_1, group_var_2) {
  distinct_grp_vals <- distinct(x, !!group_var_1)
  x %>% 
    group_by(!!group_var_1, !!group_var_2) %>% 
    summarize(Total = n()) %>% 
    spread(!!group_var_2, Total) %>% 
    right_join(distinct_grp_vals, by = rlang::quo_text(group_var_1)) %>%
    arrange(!!group_var_1)
}

## Fxn for outputting tables with two group variables and one filter variable
double_grp_filter_table <- function(x, group_var_1, group_var_2, 
                                    filter_var, filter_var_string) {
  distinct_grp_vals <- distinct(x, !!group_var_1)
  x %>% 
    group_by(!!group_var_1, !!group_var_2) %>% 
    filter(!!filter_var == filter_var_string) %>% 
    summarize(Total = n()) %>% 
    spread(!!group_var_2, Total) %>%
    right_join(distinct_grp_vals, by = rlang::quo_text(group_var_1)) %>%
    arrange(!!group_var_1)
}

## Fxn for outputting tables with three group variables
triple_grp_table <- function(x, group_var_1, group_var_2, group_var_3) {
  distinct_grp_vals <- distinct(x, !!group_var_1)
  x %>% 
    group_by(!!group_var_1, !!group_var_2, !!group_var_3) %>% 
    summarize(Total = n()) %>% 
    unite(col = United, !!group_var_2, !!group_var_3, sep = "_") %>% 
    spread(United, Total) %>% 
    right_join(distinct_grp_vals, by = rlang::quo_text(group_var_1)) %>%
    arrange(!!group_var_1)
}

## Fxn for outputting tables with two group variables and one filter variable
triple_grp_filter_table <- function(x, group_var_1, group_var_2, group_var_3, 
                                    filter_var, filter_var_string) {
  distinct_grp_vals <- distinct(x, !!group_var_1)
  x %>% 
    group_by(!!group_var_1, !!group_var_2, !!group_var_3) %>% 
    filter(!!filter_var == filter_var_string) %>% 
    summarize(Total = n()) %>% 
    right_join(distinct_grp_vals, by = rlang::quo_text(group_var_1)) %>% 
    arrange(!!group_var_1)
}

## Fxn to append 'totals' row to the bottom of a data frame (table) 
add_totals_row <- function(data_tbl) {
  totals <- vapply(X = data_tbl[, 2:ncol(data_tbl)],
                   FUN = sum, na.rm = TRUE,
                   FUN.VALUE = numeric(1))
  totals_row <- as_tibble(
    as.data.frame(
      matrix(c("Totals", totals), nrow = 1, byrow = TRUE)
    )
  )
  names(totals_row) <- names(data_tbl)
  ## Attach totals row
  data_tbl <- rbind(data_tbl, totals_row)
  data_tbl[2:ncol(data_tbl)] <- lapply(X = data_tbl[2:ncol(data_tbl)],
                                       FUN = as.integer)
  return(data_tbl)
}

## Fxn to append 'proportions' row to the bottom of a data frame (table) 
add_proportions_row <- function(data_tbl) {
  # pt_sum <- as.integer(data_tbl[data_tbl$uds_dx == "Totals", "Total"])
  pt_sum <- data_tbl %>% 
    filter(uds_dx == "Totals") %>% 
    .[, "Total"] %>% 
    as.integer(.)
  get_proportion <- function(x) {
    round(sum(x, na.rm = TRUE) / pt_sum, 2)
  }
  proportions <- vapply(X = data_tbl[1:(nrow(data_tbl)-1), 2:ncol(data_tbl)],
                        FUN = get_proportion, FUN.VALUE = numeric(1))
  proportions_row <- as_tibble(
    as.data.frame(
      matrix(c("Proportions", proportions), nrow = 1, byrow = TRUE)
    )
  )
  names(proportions_row) <- names(data_tbl)
  ## Attach proportions row
  data_tbl <- rbind(data_tbl, proportions_row)
  return(data_tbl)
}




