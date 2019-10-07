#!/usr/bin/env Rscript

# build_data_plots.R


# HANDLE SCRIPT EXEC ARGS ----
# Simplifies local-testing v. server-deployed execution of this script
args <- commandArgs(trailingOnly = TRUE)

DEPLOYED <- logical(length = 1L)

if (length(args) != 1) {
  stop(paste0("\nSupply an argument of either `local` or `server` ",
              "after executing `build_data_plots.R`\n", call. = TRUE))
} else if (args[1] == "local") {
  DEPLOYED <- FALSE
} else if (args[1] == "server") {
  DEPLOYED <- TRUE
}


# USEFUL LIBRARIES ----
suppressMessages( library(dplyr) )
suppressMessages( library(ggplot2) )

# GET SOURCE DATA & HELPER FUNCTIONS ----
if (DEPLOYED) {
  path_to_app <- # Michigan Medicine R Shiny server
    "~/ShinyApps/MADCDashboard_rf/" 
} else {
  path_to_app <- # local
    "/Box/Documents/MADC_Dashboard/MADCDashboard_rf/"
}
df_u3_ms <- readRDS(paste0(path_to_app, "rds/df_u3_ms.Rds"))
source(paste0(path_to_app, "helpers_plots.R"), local = TRUE)

# PROCES DATA ----

df_u3_ms_plot <- df_u3_ms %>% 
  # Keep relevant fields
  select(ptid, form_date, `Visit Num`, `Sex`, `Race`, # `Hispanic`
         `MADC Dx`, `MADC Dx`, `UDS Primary Etiology`) %>% 
  # Propogate most recent `madc_dx` to all visits
  group_by(ptid) %>% 
  mutate(max_visit_num = max(`Visit Num`)) %>% 
  # mutate(max_visit_num = max(visit_num)) %>% 
  mutate(`MADC Dx` = case_when(
    `Visit Num` == max_visit_num ~ `MADC Dx`,
    TRUE ~ NA_character_
  )) %>%  
  # mutate(madc_dx = case_when(
  #   visit_num == max_visit_num ~ madc_dx,
  #   TRUE ~ NA_character_
  # )) %>%
  ungroup() %>% 
  select(-max_visit_num) %>%
  propagate_value(ptid, `Visit Num`, `MADC Dx`) %>% # fxn in `helpers_plots.R`
  # propagate_value(ptid, visit_num, madc_dx) %>% # fxn in `helpers_plots.R`
  # Keep only first visit
  filter(`Visit Num` == 1L)
  # filter(visit_num == 1L)

df_u3_ms_plot <- df_u3_ms_plot %>% 
  arrange(form_date) %>% 
  mutate(unit = 1L) %>% 
  # total cumsum field
  mutate(total_cumsum = cumsum(unit)) %>% 
  # sex cumsum field
  group_by(`Sex`) %>% mutate(sex_cumsum = cumsum(unit)) %>% ungroup() %>%
  # group_by(sex) %>% mutate(sex_cumsum = cumsum(unit)) %>% ungroup() %>% 
  # race cumsum_field
  group_by(`Race`) %>% mutate(race_cumsum = cumsum(unit)) %>% ungroup() %>% 
  # group_by(race) %>% mutate(race_cumsum = cumsum(unit)) %>% ungroup() %>% 
  # `madc_dx` cumsum field %>%
  group_by(`MADC Dx`) %>%
  # group_by(madc_dx) %>%
  mutate(madc_dx_cumsum = cumsum(unit)) %>%
  ungroup() %>% 
  # add dx target rows
  add_dx_target_rows("NL",  "NL target",  
                     c(0, 63, 125, 125, 125, 125)) %>% 
  add_dx_target_rows("MCI", "MCI target", 
                     c(0, 50, 100, 100, 100, 100)) %>% 
  add_dx_target_rows("AD", "AD target", 
                     c(0, 18, 23, 36, 47, 58)) %>% 
  add_dx_target_rows("LBD", "LBD target", 
                     c(0, 10, 19, 38, 40, 37)) # %>% 
  # add_dx_target_rows("FTD", "FTD target", c(0, 5, 19, 22, 35, 36))

# WRITE DATA ----
saveRDS(df_u3_ms_plot, paste0(path_to_app, "rds/df_u3_ms_plot.Rds"))

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
