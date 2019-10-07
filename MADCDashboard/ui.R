################################################################################

# ui.R ##

suppressMessages( library(shiny) )
suppressMessages( library(shinydashboard) )

suppressMessages( library(DT) )
suppressMessages( library(dplyr) )
suppressMessages( library(tidyr) )
suppressMessages( library(purrr) )
suppressMessages( library(rlang) )
suppressMessages( library(plotly) )
suppressMessages( library(stringr) )
suppressMessages( library(ggplot2) )
suppressMessages( library(lubridate) )

library(profvis)


SUMMARY_TBL_GROUPS <-
  list(
    "Visit Num"       = "Visit Num"
    , "MADC Dx"           = "MADC Dx" 
    , "UDS Dx"          = "UDS Dx"
    , "UDS Prim Etio"   = "UDS Primary Etiology"
    # , "UDS Condition"   = "UDS Condition"
    , "Race"            = "Race"
    # , "Hispanic"        = "Hispanic"
    , "Sex"             = "Sex"
    , "County"          = "County"
    # , "ZIP"             = "ZIP Code"
    , "Blood"           = "Blood Drawn"
    # , "Saliva"          = "Saliva Given"
    , "MRI"             = "MRI Completed"
    , "Autopsy Consent" = "Autopsy Consent"
    , "Milestoned"      = "Milestoned"
  )

PLOT_GROUPS <- SUMMARY_TBL_GROUPS

SUMMARY_TBL_VISITS <- 
  list(
    "Most Recent" = "most_recent"
    , "1st" = "v1"
    , "2nd" = "v2"
    , "3rd" = "v3"
    , "4th" = "v4"
    # , "5th" = "v5"
    , "All" = "all"
  )


shinyUI(
  dashboardPage(
    
    # Header ---- 
    dashboardHeader(title = "UM MAP Dashboard"),
    
    # Sidebar ----
    dashboardSidebar(
      sidebarMenu(
        menuItem("Summary", tabName = "summary", icon = icon("table")),
        menuItem(
          "Plots", icon = icon("signal"),
          menuSubItem("Groups", tabName = "plot_g"),
          menuSubItem("Groups over Time", tabName = "plot_got"),
          menuSubItem("Groups × Clusters", tabName = "plot_gxg"),
          menuSubItem("Enroll Targets", tabName = "plot_enrolltarg")
        ),
        menuItem("Conditions", tabName = "condx", icon = icon("medkit")),
        menuItem("Timelines",  tabName = "timelines", icon = icon("clock-o"))
      )
    ),
    
    # Body ----
    dashboardBody(
      # _ Tab container ----
      tabItems(
        # _ _ tabItem for "summary" ----
        tabItem(
          tabName = "summary",
          h2("Summary Table"),
          fluidRow(
            box(width = 12,
                checkboxGroupInput(
                  inputId = "field_groups",
                  label   = "Group By",
                  inline  = TRUE,
                  choices = SUMMARY_TBL_GROUPS))),
          fluidRow(
            box(width = 6,
                height = 100,
                radioButtons(
                  inputId = "visit_filter_summ",
                  label = "Visit Filter",
                  inline = TRUE,
                  choices = SUMMARY_TBL_VISITS,
                  selected = "most_recent")),
            box(width = 6,
                height = 100,
                textInput(
                  inputId = "field_filters_summ",
                  label = "Filter(s)",
                  placeholder = 
                    paste("Enter valid R conditional expression:",
                          paste0("`MADC Dx` == \"MCI\""))))),
          fluidRow(
            box(width = 12,
                dataTableOutput("summary")))
        ), # end tabItem "summary"
        tabItem( # _ _ tabItem for "plot_g" ----
                 tabName = "plot_g",
                 h2("Groups"),
                 fluidRow(
                   box(width = 12,
                       radioButtons(
                         inputId  = "group_plot_g",
                         label    = "Group By",
                         inline   = TRUE,
                         choices  = PLOT_GROUPS,
                         selected = "Visit Num"
                       )),
                   box(width = 6,
                       radioButtons(
                         inputId = "visit_filter_plot_g",
                         label = "Visit Filter",
                         inline = TRUE,
                         choices = SUMMARY_TBL_VISITS,
                         selected = "all")),
                   box(width = 6,
                       height = 100,
                       textInput(
                         inputId = "field_filters_plot_g",
                         label = "Filter(s)",
                         placeholder =
                           paste("Enter valid R conditional expression:",
                                 paste0("`MADC Dx` == \"MCI\"")))),
                   box(width = 12,
                       plotlyOutput(outputId = "plot_g"))
                 )
        ), # end tabItem "plot_g"
        tabItem( # _ _ tabItem for "plot_got" ----
                 tabName = "plot_got",
                 h2("Groups over Time"),
                 fluidRow(
                   box(width = 12,
                       radioButtons(
                         inputId  = "group_plot_got",
                         label    = "Group By",
                         inline   = TRUE,
                         choices  = PLOT_GROUPS,
                         selected = "Visit Num"
                       )),
                   box(width = 6,
                       radioButtons(
                         inputId = "visit_filter_plot_got",
                         label = "Visit Filter",
                         inline = TRUE,
                         choices = SUMMARY_TBL_VISITS,
                         selected = "all")),
                   box(width = 6,
                       height = 100,
                       textInput(
                         inputId = "field_filters_plot_got",
                         label = "Filter(s)",
                         placeholder = 
                           paste("Enter valid R conditional expression:",
                                 paste0("`MADC Dx` == \"MCI\"")))),
                   box(width = 12,
                       plotlyOutput(outputId = "plot_got"))
                 )
        ), # end tabItem "plot_got"
        tabItem( # _ _ tabItem for "plot_gxg" ----
                 tabName = "plot_gxg",
                 h2("Groups × Clusters"),
                 fluidRow(
                   box(width = 12,
                       radioButtons(
                         inputId  = "group_plot_gxg_group",
                         label    = "Group By",
                         inline   = TRUE,
                         choices  = PLOT_GROUPS,
                         selected = "Visit Num"
                       ),
                       radioButtons(
                         inputId  = "group_plot_gxg_clust",
                         label    = "Cluster By",
                         inline   = TRUE,
                         choices  = PLOT_GROUPS,
                         selected = "MADC Dx"
                       )),
                   box(width = 6,
                       radioButtons(
                         inputId = "visit_filter_plot_gxg",
                         label = "Visit Filter",
                         inline = TRUE,
                         choices = SUMMARY_TBL_VISITS,
                         selected = "all")),
                   box(width = 6,
                       height = 100,
                       textInput(
                         inputId = "field_filters_plot_gxg",
                         label = "Filter(s)",
                         placeholder = 
                           paste("Enter valid R conditional expression:",
                                 paste0("`MADC Dx` == \"MCI\"")))),
                   box(width = 12,
                       plotlyOutput(outputId = "plot_gxg"))
                 )
        ), # end tabItem "plot_gxg"
        tabItem( # _ _ tabItem for "plot_enrolltarg" ----
          tabName = "plot_enrolltarg",
          h2("Enrollment Targets"),
          fluidRow(
            tabBox(
              width = 12,
              id = "tabset_enrolltarg",
              height = "550px",
              tabPanel("NL",
                       box(width = 12,
                           plotlyOutput(outputId = "plot_cume_dx_target_nl"))),
              tabPanel("MCI",
                       box(width = 12,
                           plotlyOutput(outputId = "plot_cume_dx_target_mci"))),
              tabPanel("AD",
                       box(width = 12,
                           plotlyOutput(outputId = "plot_cume_dx_target_ad"))),
              tabPanel("LBD",
                       box(width = 12,
                           plotlyOutput(outputId = "plot_cume_dx_target_lbd")))
            )
          )
        ), 
        
        # _ _ tabItem for "condx" ----
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
              width = 12)),
          fluidRow(
            box( plotOutput(outputId = "select_condx_combn_pie"),
                 width = 12, title = h2("All Diagnoses"))),
          fluidRow(
            box( plotOutput(outputId = "select_condx_combn_pie_normal"),
                 width = 6, title = h2("NL")),
            box( plotOutput(outputId = "select_condx_combn_pie_mci"),
                 width = 6, title = h2("MCI"))),
          fluidRow(
            box( plotOutput(outputId = "select_condx_combn_pie_ad"),
                 width = 6, title = h2("AD")),
            box( plotOutput(outputId = "select_condx_combn_pie_lbd"),
                 width = 6, title = h2("LBD")))
        ), # end tabItem for "condx"
        
        # _ _ tabItem for "timelines" ----
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
                            ticks = FALSE))),
          fluidRow(
            box(width = 6, h4("Visit - Scored"),
                plotlyOutput(outputId = "plot_timeline_exam_scrd_hist"),
                dataTableOutput(outputId = "table_timeline_exam_scrd")),
            box(width = 6, h4("Visit - Double Scored"),
                plotlyOutput(outputId = "plot_timeline_exam_dblscrd_hist"),
                dataTableOutput(outputId = "table_timeline_exam_dblscrd"))),
          fluidRow(
            box(width = 6, h4("Visit - Consensus"),
                plotlyOutput(outputId = "plot_timeline_exam_cons_hist"),
                dataTableOutput(outputId = "table_timeline_exam_cons")),
            box(width = 6, h4("Final Consensus - Feedback"),
                plotlyOutput(outputId = "plot_timeline_fincons_fdbk_hist"),
                dataTableOutput(outputId = "table_timeline_fincons_fdbk"))) #,
        ) # end tabItem "timelines"
        
      ) # end tabItems
      
      ### Profiler ----
      # , profvis_ui("profiler")
      ### 
      
    ) # end dashboardBody
  ) # end dashboardPage
  
) # end shinyUI

