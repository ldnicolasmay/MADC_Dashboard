#!/usr/bin/env Rscript

# Build list of count dfs for maps (county, zip)

library(dplyr)
library(ggmap)
library(maps)
library(mapdata)
library(zipcode)

deployed <- TRUE
# deployed <- FALSE

if (deployed) {
  path_to_app <- # Michigan Medicine R Shiny server
    "~/ShinyApps/MADCDashboard/" 
} else {
  path_to_app <- # local
    "~/Box Sync/Documents/MADC_Dashboard/MADCDashboard/"
}

data <- readRDS(paste0(path_to_app, "rds/df_ms_xfrm.Rds"))
load(paste0(path_to_app, "mi_counties_list.Rdata"))

# build_lst_map_dfs <- function(data) {

# # # # # 
## Prepare input data ----

## Create list that bundles county map df and zip map df
# lst_map_dfs <- list()

## Select only relevant fields
data_maps <- data %>% 
  select(subject_id, exam_date, county, zip_code)

## Clean up county names in data_maps
data_maps$county <- gsub(pattern = "^Genessee$", replacement = "Genesee",
                         x = data_maps$county)
data_maps$county <- gsub(pattern = "^Oakand$", replacement = "Oakland",
                         x = data_maps$county)
data_maps$county <- gsub(pattern = "^Eaton $", replacement = "Eaton",
                         x = data_maps$county)
data_maps$county <- gsub(pattern = "St.", replacement = "St",
                         x = data_maps$county)
data_maps$county <- tolower(data_maps$county)

## Clean up zip codes in data_maps
data_maps$zip_code <- 
  stringr::str_sub(string = data_maps$zip_code, start = 0, end = 5)

# # # # # 
## Prepare maps ----

## Michigan state map
mi_state <- map_data("state") %>% 
  filter(region == "michigan")

## Michigan counties map
mi_counties <- map_data("county") %>% 
  filter(region == "michigan")

# # # # # 
## Prepare Michigan base map ----

mi_base_map <- ggplot(data = mi_state, aes(x = long, y = lat, group = group)) +
  coord_fixed(ratio = 1.3) +
  geom_polygon(color = "black", fill = "gray", size = 0.25) +
  geom_polygon(data = mi_counties, fill = NA, color = "black", size = 0.25) +
  theme_nothing()
  # geom_polygon(fill = NA, color = "black", size = 0.25)

# # # # # 
## Build county map df ----

## Participant counts by county table
data_map_county <- data_maps %>% 
  rename(subregion = county) %>% 
  group_by(subregion) %>% 
  summarize(Count = n()) %>% 
  filter(!(subregion == ""))

## Left join `mi_counties` with `data_map_county`
map_df_partic_ct_mi_county <-
  left_join(mi_counties, 
            data_map_county,
            by = "subregion") %>% 
  na.omit(.)

# # # # # 
## Build zip map df ----

## Get Michigan zip codes
data("zipcode")
mi_zips <- zipcode %>%
  filter(state == "MI")

## Participant counts by zips table
data_map_zip <- data_maps %>% 
  rename(zip = zip_code) %>% 
  group_by(zip) %>% 
  summarize(Count = n())

## Left join `mi_zips` with `data_map_zip`
map_df_partic_ct_mi_zip <- 
  left_join(mi_zips,
            data_map_zip,
            by = "zip") %>% 
  na.omit(.) %>% 
  arrange(Count)

# # # # # 
## Build output list
lst_map_dfs <- list()
lst_map_dfs$mi_base_map <- mi_base_map 
lst_map_dfs$map_df_partic_ct_mi_county <- map_df_partic_ct_mi_county
lst_map_dfs$map_df_partic_ct_mi_zip <- map_df_partic_ct_mi_zip

# # # # # 
## Return `data_plots`
# return(lst_map_dfs)
# }

saveRDS(lst_map_dfs, paste0(path_to_app, "rds/lst_map_dfs.Rds"))




