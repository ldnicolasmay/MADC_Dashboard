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
    "~/Box Sync/Documents/MADC_Dashboard/MADCDashboard_rf/"
}
df_u3_ms <- readRDS(paste0(path_to_app, "rds/df_u3_ms.Rds"))
source(paste0(path_to_app, "helpers_plots.R"), local = TRUE)

# PROCES DATA ----

df_u3_ms_plot <- df_u3_ms %>% 
  # Keep relevant fields
  select(ptid, form_date, visit_num, sex, race, 
         madc_dx, uds_dx_der, uds_prim_etio) %>% 
  # # Collapse uds_dx_der + uds_prim_etio => simple_dx
  # mutate(simple_dx = case_when(
  #   uds_dx_der    == "Normal"  ~ "Normal",
  #   uds_prim_etio == "AD"  ~ "AD",
  #   uds_prim_etio == "LBD" ~ "LBD",
  #   uds_prim_etio == "FTLD NOS" | 
  #     uds_prim_etio == "FTLD with motor neuron disease" ~ "FTD",
  #   uds_dx_der == "MCI" & 
  #     (is.na(uds_prim_etio) |
  #        uds_prim_etio != "AD" |
  #        uds_prim_etio != "LBD" |
  #        uds_prim_etio != "FTLD NOS" |
  #        uds_prim_etio != "FTLD with motor neuron disease") ~ "MCI",
  #   TRUE ~ NA_character_
  # )) %>% 
  # select(-uds_dx_der, -uds_prim_etio) %>%
  # Propogate most recent `uds_dx_der` to all visits
  group_by(ptid) %>% 
  mutate(max_visit_num = max(visit_num)) %>% 
  mutate(madc_dx = case_when(
    visit_num == max_visit_num ~ madc_dx,
    TRUE ~ NA_character_
  )) %>%
  # mutate(uds_dx_der = case_when(
  #   visit_num == max_visit_num ~ uds_dx_der,
  #   TRUE ~ NA_character_
  # )) %>% 
  ungroup() %>% 
  select(-max_visit_num) %>%
  propagate_value(ptid, visit_num, madc_dx) %>% # fxn in `helpers_plots.R`
  # propagate_value(ptid, visit_num, uds_dx_der) %>% # fxn in `helpers_plots.R`
  # Keep only first visit
  filter(visit_num == 1L)

df_u3_ms_plot <- df_u3_ms_plot %>% 
  arrange(form_date) %>% 
  mutate(unit = 1L) %>% 
  # total cumsum field
  mutate(total_cumsum = cumsum(unit)) %>% 
  # sex cumsum field
  group_by(sex) %>% mutate(sex_cumsum = cumsum(unit)) %>% ungroup() %>% 
  # race cumsum_field
  group_by(race) %>% mutate(race_cumsum = cumsum(unit)) %>% ungroup() %>% 
  # `madc_dx` cumsum field %>%
  group_by(madc_dx) %>%
  mutate(madc_dx_cumsum = cumsum(unit)) %>%
  # # `uds_dx_der` cumsum field %>% 
  # group_by(uds_dx_der) %>% 
  # mutate(uds_dx_der_cumsum = cumsum(unit)) %>% 
  ungroup() %>% 
  # add dx target rows
  add_dx_target_rows("Normal",  "Normal target",  
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
