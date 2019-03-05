# app.R

# LOAD LIBRARIES ----

library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(stringr)
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
      # , menuItem(text = "Maps", tabName = "maps",
      #          icon = icon("map"))
      # , menuItem(text = "Conditions", tabName = "condx",
      #          icon = icon("medkit"))
    )
  ),
  
  # Body ----
  dashboardBody(
    # _ Tab container ----
    tabItems(
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
                        "uds_dx == \"AD\" & sex_value == \"Female\"")
              )
          )
        ),
        fluidRow(
          box(width = 12,
              # h3("Summary"),
              dataTableOutput("summary")
          )
        )
      ), # end tabItem "summary
      tabItem(
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
        fluidRow(hr()),
        fluidRow(
          box(width = 12, h4("Timeline Comparison"),
              plotOutput(outputId = "plot_timelines"))
        )
      ) # end tabItem "timelines"
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
      error=function(cond) {
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
  
  # Timeline Plots ----
  
  # Reactive to capture date range slider
  df_u3_ms_date_fltr <- reactive({
    df_u3_ms() %>% 
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
      scale_x_continuous(name = "Days")
  })
  
  # Timeline plot: dur_exam_dblscrd
  output$plot_timeline_exam_dblscrd_hist <- renderPlot({
    df_u3_ms_date_fltr_exam_dblscrd() %>% 
      ggplot(data = ., aes(x = dur_exam_dblscrd)) +
      geom_histogram(binwidth = 1, center = 0.5,
                     color = "#000000", fill = "#3885B7") +
      scale_x_continuous(name = "Days")
  })
  
  # Timeline plot: dur_exam_cons
  output$plot_timeline_exam_cons_hist <- renderPlot({
    df_u3_ms_date_fltr_exam_cons() %>% 
      filter(!is.na(dur_exam_cons)) %>% 
      ggplot(data = ., aes(x = dur_exam_cons)) +
      geom_histogram(binwidth = 1, center = 0.5,
                     color = "#000000", fill = "#3885B7") +
      scale_x_continuous(name = "Days")
  })
  
  # Timeline plot: dur_exam_cons
  output$plot_timeline_fincons_fdbk_hist <- renderPlot({
    df_u3_ms_date_fltr_fincons_fdbk() %>% 
      filter(!is.na(dur_fincons_fdbk)) %>% 
      ggplot(data = ., aes(x = dur_fincons_fdbk)) +
      geom_histogram(binwidth = 1, center = 0.5,
                     color = "#000000", fill = "#3885B7") +
      scale_x_continuous(name = "Days")
  })
  
  # Timeline plot: all four dur_*
  output$plot_timelines <- renderPlot({
    df_u3_ms_date_fltr() %>% 
      select(ptid, 
             dur_exam_scrd, dur_exam_dblscrd, dur_exam_cons, dur_fincons_fdbk) %>% 
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
