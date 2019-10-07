# helpers_plots.R

# DEFINE GLOBALS ----

CUSTOM_LINE_SIZE <- 0.8
TODAY_COLOR <- "#888888"

## Vertical line on today
TODAY_LINE <-
  geom_vline(aes(xintercept = as.integer(as.Date(Sys.Date()))),
             color = TODAY_COLOR, linetype = "dashed")

## Axis theme
CUSTOM_THEME <- theme(#text = element_text(size = 15), 
                      axis.text.x = element_text(angle = 45, hjust = 1))


## DEFINE HELPER FUNCTIONS ----

pie_graph_fast <- function(data, 
                           condx, 
                           dx = NULL,
                           combn_vctr,
                           combn_vctr_rgx) {
  # print(combn_vctr)
  par(mar = rep(0, 4))
  combn_vctr[1] <- "remaining"
  select_condx_combn_cnt <- c()
  select_condx_combn_rows <- list()
  
  if (!is.null(dx)) {
    data_cp <- data[data$`MADC Dx` == dx, ]
    # data_cp <- data[data$madc_dx == dx, ]
  } else {
    data_cp <- data
  }
  dx_sum <- nrow(data_cp)
  
  for (i in length(combn_vctr_rgx):1) {
    select_condx_combn_rows[[i]] <-
      stringr::str_detect(string = data_cp$condx_combn_name,
                          pattern = combn_vctr_rgx[i])
    select_condx_combn_cnt[i] <- sum(select_condx_combn_rows[[i]])
    data_cp <- data_cp[!select_condx_combn_rows[[i]], ]
  }
  
  pie(x = select_condx_combn_cnt[as.logical(select_condx_combn_cnt)],
      labels = 
        sprintf("%.2f", 
                select_condx_combn_cnt[as.logical(select_condx_combn_cnt)] / 
                dx_sum),
      col = rainbow(
        n = length(
          select_condx_combn_cnt[as.logical(select_condx_combn_cnt)]), s = 0.7),
      clockwise = TRUE,
      init.angle = 180)
  legend("topleft",
         legend = 
           paste0(combn_vctr[as.logical(select_condx_combn_cnt)], " n=",
                  select_condx_combn_cnt[as.logical(select_condx_combn_cnt)]),
         fill = rainbow(
           n = length(
             select_condx_combn_cnt[as.logical(select_condx_combn_cnt)]), s = 0.7),
         cex = 1, bty = "n"
  )
}

# Propograte a value across rows where value is missing for a given
# participant ID
propagate_value <- function(df, id_field, visit_num_field, value_field) {
  
  if (nrow(df) < 2) {
    stop("passed dataframe needs at least 2 rows")
  }
  
  enquo_id_field <- enquo(id_field)
  enquo_visit_num_field <- enquo(visit_num_field)
  # enquo_value_field <- enquo(value_field)
  
  quo_name_enquo_id_field <- quo_name(enquo_id_field)
  quo_name_enquo_value_field <- quo_name(enquo(value_field))
  
  df = df %>% 
    arrange(!!enquo_id_field, !!enquo_visit_num_field)
  
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
    arrange(!!enquo_id_field, desc(!!enquo_visit_num_field))
  
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
    arrange(!!enquo_id_field, !!enquo_visit_num_field)
  
  df
}

## Fxn to abstract away work of adding diagnosis target rows
add_dx_target_rows <- function(df, dx, dx_target, annual_targets) {
  # create empty tibble to hold dx target numbers
  target_df <- 
    matrix(rep(NA, ncol(df) * 6), nrow = 6, byrow = TRUE,
           dimnames = list(NULL, names(df))) %>% 
    as_tibble()
  # fill `target_df` with appropriate data
  target_df$ptid <- paste0("UM0000XXX", 0:5)
  target_df$`Visit Date` <- lubridate::as_date(paste0(2017:2022,"-03-01"))
  target_df$`MADC Dx` <- rep(dx_target, 6)
  target_df$`madc_dx_cumsum` <- annual_targets
  # return the original `df` with `target_df` attached
  bind_rows(df, target_df)
}

## Fxn for cumulative plot (no groups)
cum_plot <- function(df, x, y, plot_title, start_date, end_date) {
  df %>%
    filter(!stringr::str_detect(`MADC Dx`, "target")) %>%
    ggplot(data = ., aes_string(x = x, y = y)) +
    # geom_line() +
    geom_line(size = CUSTOM_LINE_SIZE) +
    TODAY_LINE +
    scale_x_date(name = "Visit Date",
                 date_labels = "%b %Y",
                 date_breaks = "3 months",
                 date_minor_breaks = "1 month",
                 limits = c(start_date, end_date)
    ) +
    scale_y_continuous(name = "Cumulative Participants",
                       breaks = seq(0, nrow(df) + 10, by = 20)) +
    CUSTOM_THEME +
    ggtitle(label = plot_title)
}

## Fxn for cumulative plot with single group
cum_plot_single_grp <- function(df, x, y, group_var, 
                                plot_title, start_date, end_date) {
  df %>% 
    filter(!stringr::str_detect(`MADC Dx`, "target")) %>% 
    ggplot(data = ., 
           aes_string(x = x, y = y,
                      group = group_var, color = group_var), size = 2) +
    # geom_line() +
    geom_line(size = CUSTOM_LINE_SIZE) +
    TODAY_LINE +
    scale_x_date(name = "Visit Date",
                 date_labels = "%b %Y",
                 date_breaks = "3 months",
                 date_minor_breaks = "1 month",
                 limits = c(start_date, end_date)
    ) +
    scale_y_continuous(name = "Cumulative Participants",
                       breaks = seq(0, nrow(df) + 10, by = 20)) + 
    CUSTOM_THEME +
    ggtitle(label = plot_title)
}

## Fxn for cumulative plot with dx vs. dx target
cum_plot_dx_target_dx <- function(df, x, y, group_var, 
                                  dx, dx_target, 
                                  plot_title, start_date, end_date) {
  df %>% 
    filter(`MADC Dx` == dx | `MADC Dx` == dx_target) %>% 
    # filter(madc_dx == dx | madc_dx == dx_target) %>% 
    ggplot(data = ., 
           aes_string(x = x, y = y, 
                      group = group_var, 
                      color = group_var, 
                      linetype = group_var)) +
    # geom_line() +
    geom_line(size = CUSTOM_LINE_SIZE) +
    TODAY_LINE +
    scale_x_date(name = "Visit Date",
                 date_labels = "%b %Y",
                 date_breaks = "3 months",
                 date_minor_breaks = "1 month",
                 limits = c(start_date, end_date)
    ) +
    scale_y_continuous(name = "Cumulative Participants",
                       breaks = seq(0, nrow(df) + 10, by = 20)) +
    CUSTOM_THEME +
    ggtitle(label = plot_title)
}







