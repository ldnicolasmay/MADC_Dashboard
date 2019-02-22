# app.R

# LOAD LIBRARIES ----

library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(stringr)
library(rlang)

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

DT_OPTIONS <- list(paging = FALSE,
                   searching = TRUE,
                   ordering = TRUE,
                   info = FALSE)

SUMMARY_TBL_GROUPS <-
  list(
    "UDS Dx"    = "uds_dx"
    , "Race"    = "race"
    , "Sex"     = "sex"
    , "County"  = "county"
    , "ZIP"     = "zip_code"
    , "Blood"   = "blood_drawn"
    , "Saliva"  = "sample_given"
    , "MRI"     = "mri_completed"
    , "Autopsy" = "consent_to_autopsy"
    , "Visit #" = "visit_num"
    , "Withdrawn/Completed" = "comp_withd"
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
      # , menuItem(text = "Timelines", tabName = "timelines",
      #          icon = icon("clock-o"))
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
        # , box(width = 12,
        #       verbatimTextOutput(outputId = "filters"))
        # , box(width = 12,
        #       verbatimTextOutput(outputId = "filters_len"))
        # , box(width = 12,
        #       verbatimTextOutput(outputId = "filters_nchar"))
        # , box(width = 12,
        #       verbatimTextOutput(outputId = "filters_test"))
      )
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
  
  # _ Summary Table
  
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
          # filter(!!!parse_exprs(field_filters())) %>%
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
  
  # output$filters <- renderPrint(input$field_filters)
  # output$filters_len <- renderPrint(length(input$field_filters))
  # output$filters_nchar <- renderPrint(nchar(input$field_fielters))
  # output$filters_test <- renderPrint(input$field_filters == "")
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
