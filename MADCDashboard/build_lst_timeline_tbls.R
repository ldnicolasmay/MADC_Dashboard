#!/usr/bin/env Rscript

# Build list of participant timeline tables

`%>%` <- magrittr::`%>%`

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
source(paste0(path_to_app, "helper_fxns_summ_tbls.R"))
# source(paste0(path_to_app, "helper_fxns_timeline_tbls.R"))

lst_timeline_tbls <- list()

# # # # # 
## Participant Timelines table ----

wkdy_fctr <- 5/7 # to account for work only being down 5 out of 7 days

exam_scored_dur_df <- data %>%
  dplyr::filter(!is.na(exam_scored_dur)) %>%
  dplyr::summarize(N = n(),
                   Mean = round(mean(exam_scored_dur * wkdy_fctr), 2),
                   SD =   round(sd(exam_scored_dur * wkdy_fctr), 2),
                   SEM =  round(sd(exam_scored_dur * wkdy_fctr) / sqrt(n()), 2))
exam_dbl_scored_df <- data %>%
  dplyr::filter(!is.na(exam_dbl_scored_dur)) %>%
  dplyr::summarize(N = n(),
                   Mean = round(mean(exam_dbl_scored_dur * wkdy_fctr), 2),
                   SD = round(sd(exam_dbl_scored_dur * wkdy_fctr), 2),
                   SEM = round(sd(exam_dbl_scored_dur * wkdy_fctr) / sqrt(n()), 2))
exam_consensus_dur_df <- data %>%
  dplyr::filter(!is.na(exam_consensus_dur)) %>%
  dplyr::summarize(N = n(),
                   Mean = round(mean(exam_consensus_dur * wkdy_fctr), 2),
                   SD = round(sd(exam_consensus_dur * wkdy_fctr), 2),
                   SEM = round(sd(exam_consensus_dur * wkdy_fctr) / sqrt(n()), 2))
final_consensus_fb_dur <- data %>%
  dplyr::filter(!is.na(final_consensus_fb_dur)) %>%
  dplyr::summarize(N = n(),
                   Mean = round(mean(final_consensus_fb_dur * wkdy_fctr), 2),
                   SD = round(sd(final_consensus_fb_dur * wkdy_fctr), 2),
                   SEM = round(sd(final_consensus_fb_dur * wkdy_fctr) / sqrt(n()), 2))
timeline_tbl <- dplyr::bind_rows(exam_scored_dur_df, 
                                 exam_dbl_scored_df,
                                 exam_consensus_dur_df,
                                 final_consensus_fb_dur)
timeline_tbl <- dplyr::bind_cols(data.frame(
  "Duration" = c("Visit to Scored", 
                 "Visit to Double Scored", 
                 "Visit to First Consensus", 
                 "Final Consensus to Feedback")),
  timeline_tbl)
names(timeline_tbl) <- c("Duration", "N", "Mean", "SD", "SEM")

lst_timeline_tbls[["timeline_tbl"]] <- timeline_tbl

# # # # #
## Save `lst_timeline_tbls` list as .Rds ----

saveRDS(lst_timeline_tbls, paste0(path_to_app, "rds/lst_timeline_tbls.Rds"))



