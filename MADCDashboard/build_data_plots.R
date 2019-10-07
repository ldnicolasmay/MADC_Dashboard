#!/usr/bin/env Rscript

# build_data_plots.R


# USEFUL LIBRARIES ----

suppressMessages( library(dplyr)   )
suppressMessages( library(crayon)  )
suppressMessages( library(ggplot2) )



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
                    "Use either `--dev=local` or `--env=docker`")))
  }
)


# USEFUL GLOBALS & FUNCTIONS ----

if (ENV == "server") { # ignore `DEV` constant
  # Michigan Medicine R Shiny server
  path_to_app <- "~/ShinyApps/MADCDashboard_rf/"
} else if (ENV == "local" && DEV == "local") {
  # Local development w/o Docker container
  warning(
    red(paste("Running this script in a local environment without Docker is",
              bold(underline("NOT")), "recommended!"))
  )
  path_to_app <- "~/Box/Documents/MADC_Dashboard/MADCDashboard_interact/"
} else if (ENV == "local" && DEV == "docker") {
  # Local development on Docker container
  path_to_app <- "/Box/Documents/MADC_Dashboard/MADCDashboard_interact/"
}

source(paste0(path_to_app, "helpers.R"), local = TRUE)
source(paste0(path_to_app, "helpers_plots.R"), local = TRUE)



# GET DATA ----
if (ENV == "local") { cat(cyan("Retrieving data\n")) }

df_u3_ms <- readRDS(paste0(path_to_app, "rds/df_u3_ms.Rds"))



# PROCES DATA ----
if (ENV == "local") cat(cyan("Processing data\n"))

df_u3_ms_plot <- df_u3_ms %>% 
  # Keep relevant fields
  select(
    ptid
    , `Visit Date`
    , `Visit Num`
    , `Sex`
    , `Race` 
    , `MADC Dx`
    , `UDS Primary Etiology`
  ) %>% 
  # Propogate most recent `madc_dx` to all visits
  group_by(ptid) %>% 
  mutate(max_visit_num = max(`Visit Num`)) %>% 
  mutate(`MADC Dx` = case_when(
    `Visit Num` == max_visit_num ~ `MADC Dx`,
    TRUE ~ NA_character_
  )) %>%  
  ungroup() %>% 
  select(-max_visit_num) %>%
  propagate_value(ptid, `Visit Num`, `MADC Dx`) 

# df_u3_ms_plot <- df_u3_ms_plot %>% 
df_u3_ms_targdx_plot <- df_u3_ms_plot %>%
  get_visit_n(ptid, `Visit Date`, n = -Inf) %>% 
  arrange(`Visit Date`) %>% 
  mutate(unit = 1L) %>% 
  # total cumsum field
  mutate(total_cumsum = cumsum(unit)) %>% 
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
if (ENV == "local") cat(cyan("Writing data to Rds\n"))

saveRDS(df_u3_ms_plot, 
        paste0(path_to_app, "rds/df_u3_ms_plot.Rds"))
saveRDS(df_u3_ms_targdx_plot, 
        paste0(path_to_app, "rds/df_u3_ms_targdx_plot.Rds"))


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
