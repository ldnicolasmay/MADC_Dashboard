# app.R

# LOAD LIBRARIES ----

library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(rlang)
library(ggplot2)
library(plotly)
library(lubridate)

DEPLOYED <- FALSE
# DEPLOYED <- TRUE

if (DEPLOYED) {
  path_to_app <- # Michigan Medicine R Shiny server
    "~/ShinyApps/MADCDashboard/" 
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
    # , "Withdrawn/Completed" = "comp_withd"
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
      # , menuItem(text = "Plots", tabName = "plots",
      #          icon = icon("signal"))
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
              # h3("Visit Number"),
              radioButtons(
                inputId = "visit_filter",
                label = "Participant Visit(s)",
                inline = TRUE,
                choices = SUMMARY_TBL_VISITS,
                selected = "most_recent"))),
        fluidRow(
          box(width = 12,
              # h3("Groups"),
              checkboxGroupInput(
                inputId = "field_groups",
                label = "Group By",
                inline = TRUE,
                choices = SUMMARY_TBL_GROUPS))),
        fluidRow(
          box(width = 12,
              # h3("Filters"),
              textInput(
                inputId = "field_filters",
                label = "R Filters",
                placeholder = 
                  paste("Enter valid R conditional expression:",
                        paste0("uds_dx_der <= \"MCI\" & ",
                               "!is.na(uds_prim_etio)",
                               "sex == \"Female\" & "))
              )
          )
        ),
        fluidRow(
          box(width = 12,
              # h3("Summary"),
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
        # fluidRow(
        #   box( verbatimTextOutput("select_condx_combn"))
        #   ),
        # fluidRow(
        #   box( verbatimTextOutput("select_condx_combn_lst") )
        # ),
        fluidRow(
          box( plotOutput(outputId = "select_condx_combn_pie"),
               width = 12, title = h2("All Diagnoses"))
        ),
        fluidRow(
          box( plotOutput(outputId = "select_condx_combn_pie_nl"),
               width = 6, title = h2("NL")),
          box( plotOutput(outputId = "select_condx_combn_pie_mci"),
               width = 6, title = h2("MCI"))
        )# ,
        # fluidRow(
        #   box( plotOutput(outputId = "select_condx_combn_pie_ad"),
        #        width = 6, title = h2("AD")),
        #   box( plotOutput(outputId = "select_condx_combn_pie_imp"),
        #        width = 6, title = h2("Impaired, not MCI"))
        # ),
        # fluidRow(
        #   box( plotOutput(outputId = "select_condx_combn_pie_ftd"),
        #        width = 6, title = h2("FTD")),
        #   box( plotOutput(outputId = "select_condx_combn_pie_lbd"),
        #        width = 6, title = h2("LBD"))
        # ),
        # fluidRow(
        #   box( plotOutput(outputId = "select_condx_combn_pie_oth"),
        #        width = 6, title = h2("Other")),
        #   box( plotOutput(outputId = "select_condx_combn_pie_pnd"),
        #        width = 6, title = h2("Pending Consensus"))
        # )
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
          datatable(options = DT_OPTIONS)
      },
      error = function(cond) {
        # message("error")
        df_u3_ms() %>%
          get_visit_n(ptid, form_date, visit_switch()) %>%
          group_by(!!!syms(input$field_groups)) %>%
          tally() %>% 
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
  
  # Condx Plots ----
  data_condx <- reactive({
    df_u3_ms() %>% 
      filter(!is.na(uds_dx_der)) %>% 
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
