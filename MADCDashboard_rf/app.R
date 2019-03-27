# app.R

# LOAD LIBRARIES ----

suppressMessages( library(shiny) )
suppressMessages( library(shinydashboard) )
suppressMessages( library(DT) )
suppressMessages( library(dplyr) )
suppressMessages( library(tidyr) )
suppressMessages( library(stringr) )
suppressMessages( library(purrr) )
suppressMessages( library(rlang) )
suppressMessages( library(ggplot2) )
suppressMessages( library(plotly) )
suppressMessages( library(lubridate) )

DEPLOYED <- FALSE
# DEPLOYED <- TRUE

if (DEPLOYED) {
  path_to_app <- # Michigan Medicine R Shiny server
    "~/ShinyApps/MADCDashboard_rf/" 
} else {
  path_to_app <- # local
    "~/Box Sync/Documents/MADC_Dashboard/MADCDashboard_rf/"
}
source(paste0(path_to_app, "helpers.R"), local = TRUE)
source(paste0(path_to_app, "helpers_app.R"), local = TRUE)
source(paste0(path_to_app, "helpers_plots.R"), local = TRUE)

DT_OPTIONS <- list(paging = FALSE,
                   searching = TRUE,
                   ordering = TRUE,
                   info = FALSE)

SUMMARY_TBL_GROUPS <-
  list(
    "UDS Dx"            = "uds_dx_der"
    , "UDS Prim Etio"   = "uds_prim_etio"
    , "UDS Condition"   = "uds_condition"
    , "Race"            = "race"
    , "Sex"             = "sex"
    , "County"          = "county"
    , "ZIP"             = "zip_code"
    , "Blood"           = "blood_drawn"
    , "Saliva"          = "sample_given"
    , "MRI"             = "mri_completed"
    , "Autopsy"         = "consent_to_autopsy"
    , "Visit Num"       = "visit_num"
    , "Milestoned"      = "milestone"
  )

SUMMARY_TBL_VISITS <- 
  list(
    "Most Recent" = "most_recent"
    , "1st" = "v1"
    , "2nd" = "v2"
    , "3rd" = "v3"
    , "4th" = "v4"
    , "5th" = "v5"
    , "All" = "all"
  )



# Define UI for application that draws a histogram
ui <- dashboardPage(
  
  # Header ----
  dashboardHeader(title = "MADC Dashboard"),
  
  # Sidebar ----
  dashboardSidebar(
    sidebarMenu(
      menuItem(text = "Summary", tabName = "summary",
               icon = icon("table"))
      , menuItem(text = "Timelines", tabName = "timelines",
                 icon = icon("clock-o"))
      , menuItem(text = "Plots", tabName = "plots",
               icon = icon("signal"))
      , menuItem(text = "Conditions", tabName = "condx",
               icon = icon("medkit"))
    )
  ),
  
  # Body ----
  dashboardBody(
    # Tab container ----
    tabItems(
      # tabItem for "summary" ----
      tabItem(
        tabName = "summary",
        h2("Summary Table"),
        fluidRow(
          box(width = 12,
              radioButtons(
                inputId = "visit_filter",
                label = "Participant Visit(s)",
                inline = TRUE,
                choices = SUMMARY_TBL_VISITS,
                selected = "most_recent"))),
        fluidRow(
          box(width = 12,
              checkboxGroupInput(
                inputId = "field_groups",
                label = "Group By",
                inline = TRUE,
                choices = SUMMARY_TBL_GROUPS))),
        fluidRow(
          box(width = 12,
              textInput(
                inputId = "field_filters",
                label = "R Filters",
                placeholder = 
                  paste("Enter valid R conditional expression:",
                        paste0("uds_dx_der == \"MCI\""))
              )
          )
        ),
        fluidRow(
          box(width = 12,
              dataTableOutput("summary")
          )
        )
      ) # end tabItem "summary
      # tabItem for "timelines" ----
      , tabItem(
        tabName = "timelines",
        h2("Timelines"),
        fluidRow(
          box(width = 12,
              sliderInput("date_slider", "Date range:",
                          min = as.Date("2017-03-01"),
                          max = today(),
                          value = c(today() - 365, today()),
                          dragRange = TRUE,
                          ticks = FALSE 
              )
          )
        ),
        fluidRow(
          box(width = 6, h4("Visit - Scored"),
              plotOutput(outputId = "plot_timeline_exam_scrd_hist"),
              dataTableOutput(outputId = "table_timeline_exam_scrd")),
          box(width = 6, h4("Visit - Double Scored"),
              plotOutput(outputId = "plot_timeline_exam_dblscrd_hist"),
              dataTableOutput(outputId = "table_timeline_exam_dblscrd"))
        ),
        fluidRow(
          box(width = 6, h4("Visit - Consensus"),
              plotOutput(outputId = "plot_timeline_exam_cons_hist"),
              dataTableOutput(outputId = "table_timeline_exam_cons")),
          box(width = 6, h4("Final Consensus - Feedback"),
              plotOutput(outputId = "plot_timeline_fincons_fdbk_hist"),
              dataTableOutput(outputId = "table_timeline_fincons_fdbk"))
        ),
        fluidRow(
          box(width = 12, h4("Timeline Comparison"),
              plotOutput(outputId = "plot_timelines"))
        )
      ) # end tabItem "timelines"
      # tabItem for "plots" ----
      , tabItem(
        tabName = "plots",
        h2("Cumulative Enrollments"),
        fluidRow(
          tabBox(
            width = 12,
            title = "Cumulative Enrollments",
            id = "tabset_cumenroll",
            height = "550px",
            tabPanel(
              title = "Total",
              box(width = 12,
                  plotOutput(outputId = "plot_cum_total"))
            ),
            tabPanel(
              title = "Sex",
              box(width = 12,
                  plotOutput(outputId = "plot_cum_sex"))
            ),
            tabPanel(
              title = "Race",
              box(width = 12,
                  plotOutput(outputId = "plot_cum_race"))
            )
          )
        ),
        fluidRow(
          tabBox(
            width = 12,
            title = "Target Enrollment by Diagnosis",
            id = "tabset_targenroll",
            height = "550px",
            tabPanel("Normal",
                     box(width = 12,
                         plotOutput(outputId = "plot_cum_dx_target_normal"))),
            tabPanel("MCI",
                     box(width = 12,
                         plotOutput(outputId = "plot_cum_dx_target_mci"))),
            tabPanel("LBD",
                     box(width = 12,
                         plotOutput(outputId = "plot_cum_dx_target_lbd")))
          )
        )
      ) # end tabItem for "plots"
      # tabItem for "condx" ----
      , tabItem(
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
        # # # # #
        # fluidRow(
        #   box( verbatimTextOutput("select_condx_combn"))
        #   ),
        # fluidRow(
        #   box( verbatimTextOutput("select_condx_combn_lst") )
        # ),
        # # # # #
        fluidRow(
          box( plotOutput(outputId = "select_condx_combn_pie"),
               width = 12, title = h2("All Diagnoses"))
        ),
        fluidRow(
          box( plotOutput(outputId = "select_condx_combn_pie_mci"),
               width = 6, title = h2("MCI")),
          
          box( plotOutput(outputId = "select_condx_combn_pie_normal"),
               width = 6, title = h2("Normal"))
        ),
        fluidRow(
          box( plotOutput(outputId = "select_condx_combn_pie_lbd"),
               width = 6, title = h2("LBD"))
        )
      ) # end tabItem for Condx Fast
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Read Data ----
  
  invalidation_time <- 1000 * 60 * 60 * 6 # 6-hour refresh
  #                    ^      ^    ^    ^
  #                    |      |    |    |> 6 hr
  #                    |      |    |> 60 min / hr
  #                    |      |> 60 sec / min
  #                    |> 1000 ms / sec
  # invalidation_time <- 1000 * 60 * 5 # 5-minute refresh (debug)
  
  df_u3_ms <-
    reactiveFileReader(intervalMillis = invalidation_time,
                       filePath = "rds/df_u3_ms.Rds",
                       readFunc = readRDS,
                       session = NULL)
  
  df_u3_ms_plot <-
    reactiveFileReader(intervalMillis = invalidation_time,
                       filePath = "rds/df_u3_ms_plot.Rds",
                       readFunc = readRDS,
                       session = NULL)
  
  # Summary Table ----
  
  # Reactive radio button switch
  visit_switch <- reactive({
    switch(input$visit_filter,
           most_recent = Inf,
           v1  = 1L,
           v2  = 2L,
           v3  = 3L,
           v4  = 4L,
           v5  = 5L,
           all = NULL)
  })
  
  field_filters <- reactive({
    str_trim(unlist(str_split(input$field_filters, ",")))
  })
  
  # Summary Table try-catch
  summary_table <- reactive({
    # Prevent invalid "Field Filters" from showing nasty errors in app
    ret_df <- tryCatch(
      {
        # message("try")
        df_u3_ms() %>%
          filter(!!!parse_exprs(field_filters())) %>%
          get_visit_n(ptid, form_date, visit_switch()) %>%
          group_by(!!!syms(input$field_groups)) %>%
          tally() %>%
          mutate(prop = format(round(n / sum(.$n), 2), nsmall = 2)) %>% 
          datatable(options = DT_OPTIONS)
      },
      error = function(cond) {
        # message("error")
        df_u3_ms() %>%
          get_visit_n(ptid, form_date, visit_switch()) %>%
          group_by(!!!syms(input$field_groups)) %>%
          tally() %>% 
          mutate(prop = format(round(n / sum(.$n), 2), nsmall = 2)) %>% 
          datatable(options = DT_OPTIONS)
      }
    )
    ret_df
  })
  
  # Summary Table output
  output$summary <-
    renderDataTable({
      summary_table()
    })
  
  # Timeline Plots & Tables ----
  
  # Reactive to capture date range slider
  df_u3_ms_date_fltr <- reactive({
    df_u3_ms() %>% 
      select(ptid, 
             form_date,
             dur_exam_scrd, 
             dur_exam_dblscrd, 
             dur_exam_cons, 
             dur_fincons_fdbk) %>% 
      filter(input$date_slider[1] <= form_date,
             form_date <= input$date_slider[2])
  })
  
  # Reactive to filter out NAs: dur_exam_scrd
  df_u3_ms_date_fltr_exam_scrd <- reactive({
    df_u3_ms_date_fltr() %>% 
      filter(!is.na(dur_exam_scrd))
  })
  
  # Reactive to filter out NAs: dur_exam_dblscrd
  df_u3_ms_date_fltr_exam_dblscrd <- reactive({
    df_u3_ms_date_fltr() %>% 
      filter(!is.na(dur_exam_dblscrd))
  })
  
  # Reactive to filter out NAs: dur_exam_cons
  df_u3_ms_date_fltr_exam_cons <- reactive({
    df_u3_ms_date_fltr() %>% 
      filter(!is.na(dur_exam_cons))
  })
  
  # Reactive to filter out NAs: dur_fins_fdbk
  df_u3_ms_date_fltr_fincons_fdbk <- reactive({
    df_u3_ms_date_fltr() %>% 
      filter(!is.na(dur_fincons_fdbk))
  })
  
  # Timeline table: dur_exam_scrd
  output$table_timeline_exam_scrd <- renderDataTable({
    df_u3_ms_date_fltr_exam_scrd() %>% 
      summarize_timeline(dur_exam_scrd)
  })
  
  # Timeline table: dur_exam_scrd
  output$table_timeline_exam_dblscrd <- renderDataTable({
    df_u3_ms_date_fltr_exam_dblscrd() %>% 
      summarize_timeline(dur_exam_dblscrd)
  })
  
  # Timeline table: dur_exam_cons
  output$table_timeline_exam_cons <- renderDataTable({
    df_u3_ms_date_fltr_exam_cons() %>% 
      summarize_timeline(dur_exam_cons)
  })
  
  # Timeline table: dur_fincons_fdbk
  output$table_timeline_fincons_fdbk <- renderDataTable({
    df_u3_ms_date_fltr_fincons_fdbk() %>% 
      summarize_timeline(dur_fincons_fdbk)
  })
  
  # Timeline plot: dur_exam_scrd
  output$plot_timeline_exam_scrd_hist <- renderPlot({
    df_u3_ms_date_fltr_exam_scrd() %>%
      ggplot(data = ., aes(x = dur_exam_scrd)) +
      geom_histogram(binwidth = 1, center = 0.5,
                     color = "#000000", fill = "#3885B7") +
      scale_x_continuous(limits = c(0, NA_integer_), name = "Days")
  })
  
  # Timeline plot: dur_exam_dblscrd
  output$plot_timeline_exam_dblscrd_hist <- renderPlot({
    df_u3_ms_date_fltr_exam_dblscrd() %>% 
      ggplot(data = ., aes(x = dur_exam_dblscrd)) +
      geom_histogram(binwidth = 1, center = 0.5,
                     color = "#000000", fill = "#3885B7") +
      scale_x_continuous(limits = c(0, NA_integer_), name = "Days")
  })
  
  # Timeline plot: dur_exam_cons
  output$plot_timeline_exam_cons_hist <- renderPlot({
    df_u3_ms_date_fltr_exam_cons() %>% 
      filter(!is.na(dur_exam_cons)) %>% 
      ggplot(data = ., aes(x = dur_exam_cons)) +
      geom_histogram(binwidth = 1, center = 0.5,
                     color = "#000000", fill = "#3885B7") +
      scale_x_continuous(limits = c(0, NA_integer_), name = "Days")
  })
  
  # Timeline plot: dur_exam_cons
  output$plot_timeline_fincons_fdbk_hist <- renderPlot({
    df_u3_ms_date_fltr_fincons_fdbk() %>% 
      filter(!is.na(dur_fincons_fdbk)) %>% 
      ggplot(data = ., aes(x = dur_fincons_fdbk)) +
      geom_histogram(binwidth = 1, center = 0.5,
                     color = "#000000", fill = "#3885B7") +
      scale_x_continuous(limits = c(0, NA_integer_), name = "Days")
  })
  
  # Timeline plot: all four dur_*
  output$plot_timelines <- renderPlot({
    df_u3_ms_date_fltr() %>% 
      select(-form_date) %>%
      gather(-ptid, key = "duration", value = "Days") %>%
      filter(!is.na(Days)) %>% 
      mutate(duration = case_when(
        duration == "dur_exam_scrd"    ~ "1 Visit - Scored",
        duration == "dur_exam_dblscrd" ~ "2 Visit - Double Scored",
        duration == "dur_exam_cons"    ~ "3 Visit - Consensus",
        duration == "dur_fincons_fdbk" ~ "4 FinalConsensus - Feedback"
      )) %>% 
      ggplot(aes(Days)) +
      geom_density(aes(fill=factor(duration)), alpha = 0.4) +
      labs(fill = "") +
      theme(legend.position = "bottom")
  })
  
  # Cumulative Enrollment Plots ----
  
  # Cumulative enrollment total plot
  output$plot_cum_total <- renderPlot({
    cum_plot(df = df_u3_ms_plot(),
             x = "form_date", y = "total_cumsum",
             plot_title = "Total Participants Over Time",
             start_date = as.Date("2017-03-01"),
             end_date = as.Date("2022-03-01"))
  })
  
  # Cumulative enrollment by sex plot
  output$plot_cum_sex <- renderPlot({
    cum_plot_single_grp(df = df_u3_ms_plot(),
                        x = "form_date", y = "sex_cumsum",
                        group_var = "sex",
                        plot_title = "Participants Over Time by Sex",
                        start_date = as.Date("2017-03-01"),
                        end_date = as.Date("2022-03-01"))
  })
  
  # Cumulative enrollment by race plot
  output$plot_cum_race <- renderPlot({
    cum_plot_single_grp(df = df_u3_ms_plot(),
                        x = "form_date", y = "race_cumsum",
                        group_var = "race",
                        plot_title = "Participants Over Time by Race",
                        start_date = as.Date("2017-03-01"),
                        end_date = as.Date("2022-03-01"))
  })
  
  # Cumulative enrollment by diagnosis vs. diagnosis targets
  # Use `observe` + `lapply` to render all the target diagnosis plots
  diagnosis_abbrevs <- c("Normal", "MCI", "LBD")
  observe({
    lapply(diagnosis_abbrevs, function(dx_abrv) {
      output[[paste0("plot_cum_dx_target_", tolower(dx_abrv))]] <-
        renderPlot({
          cum_plot_dx_target_dx(df = df_u3_ms_plot(),
                                x = "form_date", y = "uds_dx_der_cumsum",
                                group_var = "uds_dx_der",
                                dx = dx_abrv,
                                dx_target = paste0(dx_abrv, " target"),
                                plot_title = paste0(dx_abrv, " vs. ",
                                                    dx_abrv, " Target"),
                                start_date = as.Date("2017-03-01"),
                                end_date = as.Date("2022-03-01"))
        }) # end renderPlot
    }) # end lapply
  }) # end observe
  
  
  # Condx Plots ----
  data_condx <- reactive({
    df_u3_ms() %>% 
      filter(!is.na(uds_dx_der)) %>% 
      get_visit_n(ptid, form_date, Inf) %>%
      select(ptid, uds_dx_der, condx_combn_name)
  })
  
  select_condx <- reactive({ as.character(unlist(input$condxCheckboxesFast)) })
  
  # Build list with nCk select_condx condx using `combn()`
  select_condx_combn <- reactive({
    map(
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
  
  # # Output condx fast
  # output$select_condx_combn <- renderPrint({
  #   select_condx_combn()
  # })
  # output$select_condx_combn_lst <- renderPrint({
  #   select_condx_combn_lst()
  # })
  
  # Pie graphs
  output$select_condx_combn_pie <- renderPlot({
    pie_graph_fast(
      data = data_condx(),
      condx = select_condx(),
      dx = NULL,
      combn_vctr = select_condx_combn_lst()$select_condx_combn_vctr,
      combn_vctr_rgx = select_condx_combn_lst()$select_condx_combn_vctr_rgx)
  })
  output$select_condx_combn_pie_normal <- renderPlot({
    pie_graph_fast(
      data = data_condx(),
      condx = select_condx(),
      dx = "Normal",
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
  output$select_condx_combn_pie_lbd <- renderPlot({
    pie_graph_fast(
      data = data_condx(),
      condx = select_condx(),
      dx = "LBD",
      combn_vctr = select_condx_combn_lst()$select_condx_combn_vctr,
      combn_vctr_rgx = select_condx_combn_lst()$select_condx_combn_vctr_rgx)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)


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
