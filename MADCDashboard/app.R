# app.R

# # # # #
# Copy all files to R Shiny server ----
# scp ~/"Box Sync"/Documents/MADC_Dashboard/MADCDashboard/* 
#   ldmay@rshiny.umms.med.umich.edu:~/ShinyApps/MADCDashboard/

# # # # #
# Load libraries ----

# package.list <- c("shiny", "shinydashboard", "DT", "ggplot2", "RCurl",
#                   "jsonlite", "dplyr", "tidyr", "lubridate", "forcats",
#                   "ggmap", "maps", "mapdata", "zipcode")
# new.package.list <-
#   package.list[!(package.list %in% installed.packages()[, "Package"])]
# if (length(new.package.list)) {
#   install.packages(new.package.list,
#                    repos = "https://cloud.r-project.org/",
#                    verbose = TRUE)
# }

library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(RCurl)
library(jsonlite)
library(dplyr)
library(tidyr)
library(lubridate)
library(forcats)
library(ggmap)
library(maps)
library(mapdata)
library(zipcode)

deployed <- TRUE
# deployed <- FALSE
dt_options <- list(paging = FALSE,
                   searching = FALSE,
                   ordering = FALSE,
                   info = FALSE)

# # # # #
## Source files ----

if (deployed) {
  path_to_app <- # Michigan Medicine R Shiny server
    "~/ShinyApps/MADCDashboard/" 
} else {
  path_to_app <- # local
    "~/Box Sync/Documents/MADC_Dashboard/MADCDashboard/"
}

source(paste0(path_to_app, "helper_fxns_plots.R"), local = TRUE)


## ui ----
ui <- dashboardPage(

  ## Dashboard skin color
  skin = "blue",

  ## _ Header ----
  dashboardHeader(title = "MADC Dashboard"),

  ## _ Sidebar ----
  dashboardSidebar(
    tags$style("ul {margin:0px;padding-left:12px;}"),
    tags$style("p {padding:0px;line-height:0.8;}"),
    sidebarMenu(
      menuItem(text = "Summary", tabName = "summary",
               icon = icon("table")),
      menuItem(text = "Timelines", tabName = "timelines",
               icon = icon("clock-o")),
      menuItem(text = "Plots", tabName = "plots",
               icon = icon("signal")),
      menuItem(text = "Maps", tabName = "maps",
               icon = icon("map")),
      menuItem(text = "Conditions", tabName = "condx",
               icon = icon("medkit"))
    ), # end sidebarMenu
    box(
      tags$div(class = "header", checked = NA,
               tags$p("Links"),
               tags$ul(
                 tags$li(
                   tags$a(href="http://alzheimers.med.umich.edu/",
                          "Michigan Alzheimer's Disease Center",
                          target="_blank")),
                 tags$li(
                   tags$a(href="https://redcapproduction.umms.med.umich.edu/surveys/?s=HH9NTXAWPY", 
                          "Data Request Form",
                          target="_blank")) #,
                 # tags$li(
                 #   tags$a(href="http://alzheimers.med.umich.edu/",
                 #          "Michigan Alzheimer's Disease Center"))
               )
      ),
      width = 12, background = "black"
    )

  ), # end dashboardSidebar

  ## _ Body ----
  dashboardBody(

    ## Set colors of font awesome icons
    # tags$style(".fa-dashboard {color:#064193}"),
    # tags$style(".fa-tint {color:#064193}"),
    # tags$style(".fa-magnet {color:#064193}"),
    ## Set h2, h6 css style
    tags$style("h2 {padding-top:0px;margin-top:0px;}"),
    tags$style("h6 {padding:0px;margin:0px;}"),

    # _ _ Tab container ----
    tabItems(
      # _ _ _ Tab for summary tables ----
      tabItem(
        tabName = "summary",
        h2("Summary Tables"),
        fluidRow(
          box(width = 12, h3("Visits"),
              DT::dataTableOutput("visit"),
              fluidRow(
                box(
                  downloadButton(outputId = "visit_dl",
                                 label = "Download"),
                  width = 12))) ),
        fluidRow(
          box(width = 12, h3("UDS Version"),
              DT::dataTableOutput("uds_vers"),
              fluidRow(
                box(
                  downloadButton(outputId = "uds_vers_dl",
                                 label = "Download"),
                  width = 12))) ),
        fluidRow(
          box(width = 12, h3("Sex"),
              DT::dataTableOutput("sex"),
              fluidRow(
                box(
                  downloadButton(outputId = "sex_dl",
                                 label = "Download"),
                  width = 12))) ),
        fluidRow(
          box(width = 12, h3("Race"),
              DT::dataTableOutput("race"),
              fluidRow(
                box(
                  downloadButton(outputId = "race_dl",
                                 label = "Download"),
                  width = 12))) ),
        fluidRow(
          box(width = 12, h3("Research"),
              DT::dataTableOutput("rsrch"),
              fluidRow(
                box(
                  downloadButton(outputId = "rsrch_dl",
                                 label = "Download"),
                  width = 12))) ),
        fluidRow(
          box(width = 12, h3("Sex + Race"),
              DT::dataTableOutput("sex_race"),
              fluidRow(
                box(
                  downloadButton(outputId = "sex_race_dl",
                                 label = "Download"),
                  width = 12))) ),
        fluidRow(
          box(width = 12, h3("UDS Version + Research"),
              DT::dataTableOutput("uds_rsrch"),
              fluidRow(
                box(
                  downloadButton(outputId = "uds_rsrch_dl",
                                 label = "Download"),
                  width = 12))) ),
        fluidRow(
          box(width = 12, h3("Sex + MRI Yes"),
              DT::dataTableOutput("sex_mri_yes"),
              fluidRow(
                box(
                  downloadButton(outputId = "sex_mri_yes_dl",
                                 label = "Download"),
                  width = 12))) ),
        fluidRow(
          box(width = 12, h3("Race + MRI Yes"),
              DT::dataTableOutput("race_mri_yes"),
              fluidRow(
                box(
                  downloadButton(outputId = "race_mri_yes_dl",
                                 label = "Download"),
                  width = 12))) )
      ),
      # _ _ _ Tab for timeline tables/plots ----
      tabItem(
        tabName = "timelines",
        h2("Timelines"),
        fluidRow(
          box(width = 12, h3("Participant Timelines"),
              DT::dataTableOutput("timeline"),
              fluidRow(
                box(
                  tags$h6("* These values do not include weekend days."),
                  width = 12
                )
              )
          )
        ),
        fluidRow(
          box(width = 6, height = 350, h3("Visit to Scored"),
              plotOutput(outputId = "plot_timeline_exam_scored_hist")),
          box(width = 6, height = 350, h3("Visit to Double Scored"),
              plotOutput(outputId = "plot_timeline_exam_dbl_scored_hist"))
        ),
        fluidRow(
          box(width = 6, height = 350, h3("Visit to First Consensus"),
              plotOutput(outputId = "plot_timeline_exam_consensus_dur_hist")),
          box(width = 6, height = 350, h3("Final Consensus to Feedback"),
              plotOutput(outputId = "plot_timeline_final_consensus_fb_hist"))
        )
      ),
      # _ _ _ Tab for plots ----
      tabItem(
        tabName = "plots",
        h2("Cumulative Enrollments"),
        fluidRow(
          tabBox( # _ _ _ _ tabBox 1 for cumulative plots ----
                  width = 12,
                  title = "Cumulative Enrollments",
                  id = "tabset_cumenroll",
                  height = "550px",
                  tabPanel( # _ _ _ _ _ tabPanel 1 -- Total plot ----
                            title = "Total",
                            box(width = 12,
                                plotOutput(outputId = "plot_cum_total"))
                  ),
                  tabPanel( # _ _ _ _ _ tabPanel 2 -- Sex plot ----
                            title = "Sex",
                            box(width = 12,
                                plotOutput(outputId = "plot_cum_sex"))
                  ),
                  tabPanel( # _ _ _ _ _ tabPanel 3 -- Race plot ----
                            title = "Race",
                            box(width = 12,
                                plotOutput(outputId = "plot_cum_race"))
                  )
          )
        ),
        fluidRow(
          box( # _ _ _ _ box 1 for date range input (cumulative) ----
               width = 12,
               dateRangeInput(inputId = "dateRange1",
                              label = "Date range input: yyyy-mm-dd",
                              start = as.Date("2017-03-01"), end = Sys.Date())
          )
        ),
        fluidRow(
          tabBox( # _ _ _ _ tabBox 2 for target diagnosis plots ----
                  width = 12,
                  title = "Target Enrollment by Diagnosis",
                  id = "tabset_targenroll",
                  height = "550px",
                  tabPanel("NL", # _ _ _ _ _ tabPanel 1 -- NL ----
                           box(width = 12,
                               plotOutput(outputId = "plot_cum_dx_target_nl"))),
                  tabPanel("MCI", # _ _ _ _ _ tabPanel 1 -- MCI ----
                           box(width = 12,
                               plotOutput(outputId = "plot_cum_dx_target_mci"))),
                  tabPanel("AD", # _ _ _ _ _ tabPanel 1 -- AD ----
                           box(width = 12,
                               plotOutput(outputId = "plot_cum_dx_target_ad"))),
                  tabPanel("LBD", # _ _ _ _ _ tabPanel 1 -- LBD ----
                           box(width = 12,
                               plotOutput(outputId = "plot_cum_dx_target_lbd"))),
                  tabPanel("FTD", # _ _ _ _ _ tabPanel 1 -- FTD ----
                           box(width = 12,
                               plotOutput(outputId = "plot_cum_dx_target_ftd")))
          )
        ),
        fluidRow(
          box( # _ _ _ _ box 2 for date range input (diagnosis) ----
               width = 12,
               dateRangeInput(inputId = "dateRange2",
                              label = "Date range input: yyyy-mm-dd",
                              start = as.Date("2017-03-01"),
                              end = as.Date("2022-03-01")
               )
          )
        )
      ),
      # _ _ _ Tab for maps ----
      tabItem(
        tabName = "maps",
        h2("Maps"),
        fluidRow(
          tabBox(
            width = 12,
            title = "",
            id = "tabset_map",
            height = "700px",
            side = "left",
            tabPanel( # _ _ _ _ tabPanel 1 -- county map ----
                      title = "County",
                      box(
                        width = 12,
                        height = "625px",
                        plotOutput(outputId = "map_partic_by_county")
                      )
            ),
            tabPanel( # _ _ _ _ tabPanel 2 -- ZIP map ----
                      title = "ZIP",
                      box(
                        width = 12,
                        height = "625px",
                        plotOutput(outputId = "map_partic_by_zip")
                      )
            )
          ) # end tabBox
        ) # end fluidRow
      ), # end tabItem for Maps
      # _ _ _ Tab for condx ----
      tabItem(
        tabName = "condx",
        h2("Conditions by Diagnosis"),
        fluidRow(
          box(
            checkboxGroupInput(
              inputId = "condxCheckboxesFast",
              label = h3("Conditions"),
              inline = TRUE,
              choices = list("Cancer" = "cancer",
                             "Diabetes" = "diabet",
                             "Myocardial infarction" = "myoinf",
                             "Congestive heart failure" = "conghrt",
                             "Hypertension" = "hypert",
                             "Hypercholesterolemia" = "hypchol",
                             "Arthritis" = "arth",
                             "Sleep apnea" = "sleepap",
                             "REM disorder" = "remdis",
                             "Hyposomnia/Insomnia" = "hyposom")),
            width = 12)
        ),
        fluidRow(
          box( plotOutput(outputId = "select_condx_combn_pie"),
               width = 12, title = h2("All Diagnoses"))
        ),
        fluidRow(
          box( plotOutput(outputId = "select_condx_combn_pie_nl"),
               width = 6, title = h2("NL")),
          box( plotOutput(outputId = "select_condx_combn_pie_mci"),
               width = 6, title = h2("MCI"))
        ),
        fluidRow(
          box( plotOutput(outputId = "select_condx_combn_pie_ad"),
               width = 6, title = h2("AD")),
          box( plotOutput(outputId = "select_condx_combn_pie_imp"),
               width = 6, title = h2("Impaired, not MCI"))
        ),
        fluidRow(
          box( plotOutput(outputId = "select_condx_combn_pie_ftd"),
               width = 6, title = h2("FTD")),
          box( plotOutput(outputId = "select_condx_combn_pie_lbd"),
               width = 6, title = h2("LBD"))
        ),
        fluidRow(
          box( plotOutput(outputId = "select_condx_combn_pie_oth"),
               width = 6, title = h2("Other")),
          box( plotOutput(outputId = "select_condx_combn_pie_pnd"),
               width = 6, title = h2("Pending Consensus"))
        )
      ) # end tabItem for Condx Fast
    ) # end tabItems
  ) # end dashboardBody
) # end dashboardPage

server <- function(input, output, session) {

  invalidation_time <- 1000 * 60 * 60 * 6 # 6-hour refresh
  #                    ^      ^    ^    ^
  #                    |      |    |    |> 6 hr
  #                    |      |    |> 60 min / hr
  #                    |      |> 60 sec / min
  #                    |> 1000 ms / sec
  # invalidation_time <- 1000 * 60 * 5 # 5-minute refresh (debug)

  # # # # #
  ## Get data ----

  ## Raw MiNDSet data
  data <-
    reactiveFileReader(intervalMillis = invalidation_time,
                       filePath = "./rds/df_ms_xfrm.Rds",
                       readFunc = readRDS,
                       session = NULL)

  ## List for summary tables
  lst_summ_tbls <-
    reactiveFileReader(intervalMillis = invalidation_time,
                       filePath = "./rds/lst_summ_tbls.Rds",
                       readFunc = readRDS,
                       session = NULL)

  ## List for timeline tables
  lst_timeline_tbls <-
    reactiveFileReader(intervalMillis = invalidation_time,
                       filePath = "./rds/lst_timeline_tbls.Rds",
                       readFunc = readRDS,
                       session = NULL)

  ## df for plots
  data_plots <-
    reactiveFileReader(intervalMillis = invalidation_time,
                       filePath = "./rds/data_plots.Rds",
                       readFunc = readRDS,
                       session = NULL)

  ## List for maps
  lst_map_dfs <-
    reactiveFileReader(intervalMillis = invalidation_time,
                       filePath = "./rds/lst_map_dfs.Rds",
                       readFunc = readRDS,
                       session = NULL)

  ## df for condx fast
  data_condx <-
    reactiveFileReader(intervalMillis = invalidation_time,
                       filePath = "./rds/data_condx.Rds",
                       readFunc = readRDS,
                       session = NULL)


  # # # # #
  ## Render summary tables ----

  ## Use `observe` + `lapply` to render all the summary tables
  summ_tbl_names <- c("visit", "uds_vers", "sex", "race", "sex_race",
                      "rsrch", "uds_rsrch", "sex_mri_yes", "race_mri_yes")
  observe({
    lapply(summ_tbl_names, function(tbl_name) {
      output[[tbl_name]] <- DT::renderDataTable({
        DT::datatable( lst_summ_tbls()[[paste0(tbl_name, "_tbl")]],
                       options = dt_options )
      })
    }) # end `lapply`
  }) # end `observe`
  ## Example of a single table render w/o observe+lapply
  # output$data_mindset_tbl <- DT::renderDataTable({
  #   DT::datatable( data_mindset_rctv(), options = dt_options )
  # })

  # # # # #
  ## Link summary table download buttons ----
  observe({
    lapply(summ_tbl_names, function(tbl_name) {
      output[[paste0(tbl_name, "_dl")]] <- downloadHandler(
        filename = function() { paste0(tbl_name, "_dl.csv") },
        content = function(file) {
          write.table(lst_summ_tbls()[[paste0(tbl_name, "_tbl")]],
                      file, sep = ",", row.names = FALSE, na = "")
        }
      )
    })
  })
  ## Example of single download button w/o observe+lapply
  # output$uds_vers_dl <- downloadHandler(
  #   filename = function() { paste("uds_vers_dl.csv") },
  #   content = function(file) {
  #     write.table(lst_summ_tbls()$uds_vers, file, sep = ",", row.names = FALSE)
  #   }
  # )


  # # # # #
  ## Render timeline tables / plots ----

  ## Use `observe` + `lapply` to render all the summary tables
  timeline_tbl_names <- c("timeline")
  observe({
    lapply(timeline_tbl_names, function(tbl_name) {
      output[[tbl_name]] <- DT::renderDataTable({
        DT::datatable( lst_timeline_tbls()[[paste0(tbl_name, "_tbl")]],
                       options = dt_options )
      })
    }) # end `lapply`
  }) # end `observe`

  # # # # #
  ## Render enrollment plots ----

  ## Timeline - Visit to Score plot
  output$plot_timeline_exam_scored_hist <- renderPlot({
    ggplot(data = data(), aes(x = exam_scored_dur * 5/7)) +
      geom_histogram(binwidth = 1, center = 0.5,
                     color = "#000000", fill = "#3885B7",
                     na.rm = TRUE) +
      scale_x_continuous(name = "Days")
  }, height = 275)

  ## Timeline - Visit to Double Score plot
  output$plot_timeline_exam_dbl_scored_hist <- renderPlot({
    ggplot(data = data(), aes(x = exam_dbl_scored_dur * 5/7)) +
      geom_histogram(binwidth = 1, center = 0.5,
                     color = "#000000", fill = "#3885B7",
                     na.rm = TRUE) +
      scale_x_continuous(name = "Days")
  }, height = 275)

  ## Timeline - Visit to First Consensus plot
  output$plot_timeline_exam_consensus_dur_hist <- renderPlot({
    ggplot(data = data(), aes(x = exam_consensus_dur * 5/7)) +
      geom_histogram(binwidth = 1, center = 0.5,
                     color = "#000000", fill = "#3885B7",
                     na.rm = TRUE) +
      scale_x_continuous(name = "Days")
  }, height = 275)

  ## Timeline - Final Consensus to Feedback plot
  output$plot_timeline_final_consensus_fb_hist <- renderPlot({
    ggplot(data = data(), aes(x = final_consensus_fb_dur * 5/7)) +
      geom_histogram(binwidth = 1, center = 0.5,
                     color = "#000000", fill = "#3885B7",
                     na.rm = TRUE) +
      scale_x_continuous(name = "Days")
  }, height = 275)

  ## Cumulative enrollment totals plot
  output$plot_cum_total <- renderPlot({
    cum_plot(df = data_plots(),
             x = "exam_date", y = "total_cum_sum",
             plot_title = "Total Participants Over Time",
             start_date = as.Date(input$dateRange1[1]),
             end_date = as.Date(input$dateRange1[2]))
  })

  ## Cumulative enrollment by sex plot
  output$plot_cum_sex <- renderPlot({
    cum_plot_single_grp(df = data_plots(),
                        x = "exam_date", y = "sex_cum_sum",
                        group_var = "sex_value",
                        plot_title = "Participants Over Time by Sex",
                        start_date = as.Date(input$dateRange1[1]),
                        end_date = as.Date(input$dateRange1[2]))
  })

  ## Cumulative enrollment by race plot
  output$plot_cum_race <- renderPlot({
    cum_plot_single_grp(df = data_plots(),
                        x = "exam_date", y = "race_cum_sum",
                        group_var = "race_value",
                        plot_title = "Participants Over Time by Race",
                        start_date = as.Date(input$dateRange1[1]),
                        end_date = as.Date(input$dateRange1[2]))
  })

  ## Cumulative enrollment by diagnosis vs. diagnosis targets
  # Use `observe` + `lapply` to render all the target diagnosis plots
  diagnosis_abbrevs <- c("AD", "FTD", "LBD", "MCI", "NL")
  observe({
    lapply(diagnosis_abbrevs, function(dx_abrv) {
      output[[paste0("plot_cum_dx_target_", tolower(dx_abrv))]] <-
        renderPlot({
          cum_plot_dx_target_dx(df = data_plots(),
                                x = "exam_date", y = "dx_cum_sum",
                                group_var = "uds_dx",
                                dx = dx_abrv,
                                dx_target = paste0(dx_abrv, " target"),
                                plot_title = paste0(dx_abrv, " vs. ",
                                                    dx_abrv, " Target"),
                                start_date = as.Date(input$dateRange2[1]),
                                end_date = as.Date(input$dateRange2[2]))
        }) # end renderPlot
    }) # end lapply
  }) # end observe

  # # # # #
  ## Render enrollment maps ----

  ## Participation by county map
  county_max <- reactive({
    max(lst_map_dfs()$map_df_partic_ct_mi_county$Count, na.rm = TRUE)
  })
  output$map_partic_by_county <- renderPlot({
    lst_map_dfs()$mi_base_map +
      geom_polygon(data = lst_map_dfs()$map_df_partic_ct_mi_county,
                   aes(fill = Count),
                   color = "black", size = 0.25) +
      geom_polygon(color = "black", size = 0.25, fill = NA) +
      theme_bw() +
      theme(
        axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank()
      ) +
      scale_fill_continuous(low = "#eeeeee", high = "royalblue",
                            breaks = seq(1, county_max(),
                                         by = round((county_max()-1)/5))) +
      ggtitle(label = "Participant Counts by County",
              subtitle = "March 2017 to Present")
  }, height = 600)

  ## Participation by ZIP map
  zip_max <- reactive({
    max(lst_map_dfs()$map_df_partic_ct_mi_zip$Count, na.rm = TRUE)
  })
  output$map_partic_by_zip <- renderPlot({
    lst_map_dfs()$mi_base_map +
      geom_point(data = lst_map_dfs()$map_df_partic_ct_mi_zip,
                 aes(x = longitude, y = latitude, group = zip,
                     size = Count,
                     fill = Count),
                 color = "black", pch = 21) +
      geom_polygon(color = "black", size = 0.25, fill = NA) +
      theme_bw() +
      theme(
        axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank()
      ) +
      scale_fill_continuous(low = "#eeeeee", high = "royalblue",
                            breaks = seq(1, zip_max(),
                                         by = round((zip_max()-1)/5))) +
      guides(fill = guide_legend(), size = guide_legend()) +
      scale_size_continuous(limits = c(1, zip_max()),
                            breaks = seq(1, zip_max(),
                                         by = round((zip_max()-1)/5))) +
      ggtitle(label = "Participant Counts by ZIP Code",
              subtitle = "March 2017 to Present")
  }, height = 600)


  # # # # #
  # Render condx plots ----
  select_condx <- reactive({ as.character(unlist(input$condxCheckboxesFast)) })

  # Build list with nCk select_condx condx using `combn()`
  select_condx_combn <- reactive({
    purrr::map(
      0:length(select_condx()),
      function(x) {
        combn(select_condx(), x, simplify = TRUE)
      })
  })

  # Build container list for all condx / all dx
  select_condx_combn_lst <- reactive({
    select_condx_combn_vctr <- c()
    select_condx_combn_vctr_rgx <- c()

    for (i in 1:length(select_condx_combn())) {
      for (j in 1:ncol(select_condx_combn()[[i]])) {
        col_name <- c(select_condx_combn()[[i]][, j])
        select_condx_combn_vctr <- # display names of condx combn.s
          c(select_condx_combn_vctr,
            paste0(col_name, collapse = " + "))
        select_condx_combn_vctr_rgx <- # find all selected condx
          c(select_condx_combn_vctr_rgx,
            paste0("(?=.*", col_name, ")", collapse = ""))
      }
    }

    return(list(select_condx_combn_vctr = select_condx_combn_vctr,
                select_condx_combn_vctr_rgx = select_condx_combn_vctr_rgx))
  })

  # Output condx fast
  # output$select_condx_combn <- renderPrint({
  #   select_condx_combn()
  # })
  output$select_condx_combn_lst <- renderPrint({
    select_condx_combn_lst()
  })
  # Pie graphs
  output$select_condx_combn_pie <- renderPlot({
    pie_graph_fast(
      data = data_condx(),
      condx = select_condx(),
      dx = NULL,
      combn_vctr = select_condx_combn_lst()$select_condx_combn_vctr,
      combn_vctr_rgx = select_condx_combn_lst()$select_condx_combn_vctr_rgx)
  })
  output$select_condx_combn_pie_nl <- renderPlot({
    pie_graph_fast(
      data = data_condx(),
      condx = select_condx(),
      dx = "NL",
      combn_vctr = select_condx_combn_lst()$select_condx_combn_vctr,
      combn_vctr_rgx = select_condx_combn_lst()$select_condx_combn_vctr_rgx)
  })
  output$select_condx_combn_pie_mci <- renderPlot({
    pie_graph_fast(
      data = data_condx(),
      condx = select_condx(),
      dx = "MCI",
      combn_vctr = select_condx_combn_lst()$select_condx_combn_vctr,
      combn_vctr_rgx = select_condx_combn_lst()$select_condx_combn_vctr_rgx)
  })
  output$select_condx_combn_pie_ad <- renderPlot({
    pie_graph_fast(
      data = data_condx(),
      condx = select_condx(),
      dx = "AD",
      combn_vctr = select_condx_combn_lst()$select_condx_combn_vctr,
      combn_vctr_rgx = select_condx_combn_lst()$select_condx_combn_vctr_rgx)
  })
  output$select_condx_combn_pie_imp <- renderPlot({
    pie_graph_fast(
      data = data_condx(),
      condx = select_condx(),
      dx = "Impaired, not MCI",
      combn_vctr = select_condx_combn_lst()$select_condx_combn_vctr,
      combn_vctr_rgx = select_condx_combn_lst()$select_condx_combn_vctr_rgx)
  })
  output$select_condx_combn_pie_ftd <- renderPlot({
    pie_graph_fast(
      data = data_condx(),
      condx = select_condx(),
      dx = "FTD",
      combn_vctr = select_condx_combn_lst()$select_condx_combn_vctr,
      combn_vctr_rgx = select_condx_combn_lst()$select_condx_combn_vctr_rgx)
  })
  output$select_condx_combn_pie_lbd <- renderPlot({
    pie_graph_fast(
      data = data_condx(),
      condx = select_condx(),
      dx = "LBD",
      combn_vctr = select_condx_combn_lst()$select_condx_combn_vctr,
      combn_vctr_rgx = select_condx_combn_lst()$select_condx_combn_vctr_rgx)
  })
  output$select_condx_combn_pie_oth <- renderPlot({
    pie_graph_fast(
      data = data_condx(),
      condx = select_condx(),
      dx = "Other",
      combn_vctr = select_condx_combn_lst()$select_condx_combn_vctr,
      combn_vctr_rgx = select_condx_combn_lst()$select_condx_combn_vctr_rgx)
  })
  output$select_condx_combn_pie_pnd <- renderPlot({
    pie_graph_fast(
      data = data_condx(),
      condx = select_condx(),
      dx = "Pending consensus",
      combn_vctr = select_condx_combn_lst()$select_condx_combn_vctr,
      combn_vctr_rgx = select_condx_combn_lst()$select_condx_combn_vctr_rgx)
  })

}

shinyApp(ui, server)
