

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

# library(profvis)



SUMMARY_TBL_GROUPS <-
  list(
    "Visit Num"         = "Visit Num"
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






DEPLOYED <- TRUE
DOCKER_DEV <- FALSE

if (DEPLOYED) {
  path_to_app <- # Michigan Medicine R Shiny server
    "~/ShinyApps/MADCDashboard/" 
} else if (DOCKER_DEV) {
  path_to_app <- # local via docker
    "/Box/Documents/MADC_Dashboard/MADCDashboard/"
} else {
  path_to_app <- # local
    "~/Box/Documents/MADC_Dashboard/MADCDashboard/"
}

source(paste0(path_to_app, "helpers.R"), local = TRUE)
source(paste0(path_to_app, "helpers_app.R"), local = TRUE)
source(paste0(path_to_app, "helpers_plots.R"), local = TRUE)

DT_OPTIONS <- list(paging    = FALSE,
                   searching = TRUE,
                   ordering  = TRUE,
                   info      = FALSE)





# shinyUI(
ui <- dashboardPage(
  
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

# ) # end shinyUI





# shinyServer(function(input, output, session) {
server <- function(input, output, session) {
  
  ### Profiler ----
  # callModule(profvis_server, "profiler")
  ###
  
  
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
                       filePath       = "rds/df_u3_ms.Rds",
                       readFunc       = readRDS,
                       session        = NULL)
  
  df_u3_ms_plot <-
    reactiveFileReader(intervalMillis = invalidation_time,
                       filePath       = "rds/df_u3_ms_plot.Rds",
                       readFunc       = readRDS,
                       session        = NULL)
  
  df_u3_ms_targdx_plot <-
    reactiveFileReader(intervalMillis = invalidation_time,
                       filePath       = "rds/df_u3_ms_targdx_plot.Rds",
                       readFunc       = readRDS,
                       session        = NULL)
  
  
  # Reactive radio button switch
  visit_switch <- reactive({
    switch(input$visit_filter_summ,
           most_recent = Inf,
           v1 = 1L,
           v2 = 2L,
           v3 = 3L,
           v4 = 4L,
           # v5 = 5L,
           # v6 = 6L,
           # v7 = 7L,
           # v8 = 8L,
           # v9 = 9L,
           all = NULL)
  })
  
  visit_switch_plot_g <- reactive({
    switch(input$visit_filter_plot_g,
           most_recent = Inf,
           v1 = 1L,
           v2 = 2L,
           v3 = 3L,
           v4 = 4L,
           # v5 = 5L,
           # v6 = 6L,
           # v7 = 7L,
           # v8 = 8L,
           # v9 = 9L,
           all = NULL)
  })
  
  visit_switch_plot_got <- reactive({
    switch(input$visit_filter_plot_got,
           most_recent = Inf,
           v1 = 1L,
           v2 = 2L,
           v3 = 3L,
           v4 = 4L,
           # v5 = 5L,
           # v6 = 6L,
           # v7 = 7L,
           # v8 = 8L,
           # v9 = 9L,
           all = NULL)
  })
  
  visit_switch_plot_gxg <- reactive({
    switch(input$visit_filter_plot_gxg,
           most_recent = Inf,
           v1 = 1L,
           v2 = 2L,
           v3 = 3L,
           v4 = 4L,
           # v5 = 5L,
           # v6 = 6L,
           # v7 = 7L,
           # v8 = 8L,
           # v9 = 9L,
           all = NULL)
  })
  
  
  field_filters_summ <- reactive({
    str_trim(unlist(str_split(input$field_filters_summ, ",")))
  })
  
  field_filters_plot_g <- reactive({
    str_trim(unlist(str_split(input$field_filters_plot_g, ",")))
  })
  
  field_filters_plot_got <- reactive({
    str_trim(unlist(str_split(input$field_filters_plot_got, ",")))
  })
  
  field_filters_plot_gxg <- reactive({
    str_trim(unlist(str_split(input$field_filters_plot_gxg, ",")))
  })
  
  
  # Summary Table ----
  
  # Summary Table try-catch
  summary_table <- reactive({
    # Prevent invalid "Field Filters" from showing nasty errors in app
    tryCatch(
      { # try
        df_u3_ms() %>%
          filter(!!!parse_exprs(field_filters_summ())) %>%
          get_visit_n(ptid, `Visit Date`, visit_switch()) %>%
          group_by(!!!syms(input$field_groups)) %>%
          tally() %>%
          mutate(prop = format(round(n / sum(.$n), 2), nsmall = 2)) %>%
          # filter(!!!parse_exprs(field_filters_summ())) %>%
          datatable(options = DT_OPTIONS)
      },
      error = function(e) { # error
        df_u3_ms() %>%
          get_visit_n(ptid, `Visit Date`, visit_switch()) %>%
          group_by(!!!syms(input$field_groups)) %>%
          tally() %>% 
          mutate(prop = format(round(n / sum(.$n), 2), nsmall = 2)) %>% 
          datatable(options = DT_OPTIONS)
      })
  })
  
  # Summary Table output
  output$summary <-
    renderDataTable({
      summary_table()
    })
  
  
  # Plots ----
  
  # _ Groups ----
  
  sym_plot_g <- reactive({ sym(input$group_plot_g) })
  
  df_u3_ms_data_plot_g <- reactive({
    tryCatch(
      { # try
        df_u3_ms() %>% 
          filter(!!!parse_exprs(field_filters_plot_g())) %>% 
          select(ptid, `Visit Date`, !!sym_plot_g()) %>% 
          get_visit_n(ptid, `Visit Date`, visit_switch_plot_g()) %>%
          arrange(!!sym_plot_g(), `Visit Date`, ptid)
      },
      error = function(e) { # error
        df_u3_ms() %>% 
          select(ptid, `Visit Date`, !!sym_plot_g()) %>% 
          get_visit_n(ptid, `Visit Date`, visit_switch_plot_g()) %>%
          arrange(!!sym_plot_g(), `Visit Date`, ptid)
      })
  })
  
  output$plot_g <-
    renderPlotly({
      eval_tidy(quo_squash(quo( # for mixing quasiquotation (!!) & formulas (~)
        df_u3_ms_data_plot_g() %>%
          count(!!sym_plot_g()) %>%
          plot_ly(x = ~!!sym_plot_g(), y = ~n) %>%
          add_bars(color = ~ordered(!!sym_plot_g()), colors = "Dark2")
      )))
    })
  
  # _ Groups over Time ----
  
  sym_plot_got <- reactive({ sym(input$group_plot_got) })
  sym_visit_cnt <- sym("Visit Count")
  
  df_u3_ms_data_plot_got <- reactive({
    # df_tryCatch <- tryCatch(
    tryCatch(
      { # try
        df_u3_ms() %>% 
          filter(!!!parse_exprs(field_filters_plot_got())) %>% 
          select(ptid, `Visit Date`, !!sym_plot_got()) %>% 
          # filter for visit
          get_visit_n(ptid, `Visit Date`, visit_switch_plot_got()) %>% 
          arrange(!!sym_plot_got(), `Visit Date`, ptid) %>% 
          mutate(
            !!sym_visit_cnt := 
              ave(!!sym_plot_got() == !!sym_plot_got(), 
                  !!sym_plot_got(), 
                  FUN = cumsum)
          )
      },
      error = function(e) {
        df_u3_ms() %>% 
          select(ptid, `Visit Date`, !!sym_plot_got()) %>% 
          # filter for visit
          get_visit_n(ptid, `Visit Date`, visit_switch_plot_got()) %>% 
          arrange(!!sym_plot_got(), `Visit Date`, ptid) %>% 
          mutate(
            !!sym_visit_cnt := 
              ave(!!sym_plot_got() == !!sym_plot_got(), 
                  !!sym_plot_got(), 
                  FUN = cumsum)
          )
      })
  })
  
  output$plot_got <-
    renderPlotly({
      eval_tidy(quo_squash(quo( # for mixing quasiquotation (!!) & formulas (~)
        plot_ly(
          df_u3_ms_data_plot_got(),
          x = ~`Visit Date`, y = ~!!sym_visit_cnt
        ) %>% 
          add_lines(color = ~ordered(!!sym_plot_got()), colors = "Dark2") %>% 
          layout(title = paste(sym_plot_got(), "- Visit Count"), 
                 hovermode = "compare")
      )))
    })
  
  # _ Groups by Groups ----
  
  sym_plot_gxg_group <- reactive({ sym(input$group_plot_gxg_group) })
  sym_plot_gxg_clust <- reactive({ sym(input$group_plot_gxg_clust) })
  
  df_u3_ms_data_plot_gxg <- reactive({
    tryCatch(
      { # try
        df_u3_ms() %>% 
          filter(!!!parse_exprs(field_filters_plot_gxg())) %>%
          # filter for visit
          get_visit_n(ptid, `Visit Date`, visit_switch_plot_gxg())
      },
      error = function(e) { # error
        df_u3_ms() %>% 
          # filter for visit
          get_visit_n(ptid, `Visit Date`, visit_switch_plot_gxg())
      })
  })
  
  tbl <- reactive({
    table(x = df_u3_ms_data_plot_gxg()[[input$group_plot_gxg_group]],
          y = df_u3_ms_data_plot_gxg()[[input$group_plot_gxg_clust]]) %>%
      as_tibble() %>% 
      select(!!sym_plot_gxg_group() := x,
             !!sym_plot_gxg_clust() := y,
             n)
  })
  
  output$plot_gxg <-
    renderPlotly({
      eval_tidy(quo_squash(quo( # for mixing quasiquotation (!!) & formulas (~)
        tbl() %>% 
          plot_ly(x = ~!!sym_plot_gxg_group(), 
                  y = ~n, 
                  color = ~!!sym_plot_gxg_clust(), 
                  colors = "Accent") %>%
          add_bars() %>% 
          layout(hovermode = "compare")
      )))
    })
  
  # _ Enrollment Targets ----
  
  # Cumulative enrollment by diagnosis vs. diagnosis targets
  # Use `observe` + `lapply` to render all the target diagnosis plots
  diagnosis_abbrevs <- c("NL", "MCI", "AD", "LBD")
  observe({
    lapply(diagnosis_abbrevs, function(dx_abrv) {
      output[[paste0("plot_cume_dx_target_", tolower(dx_abrv))]] <-
        # renderPlot({
        renderPlotly({
          cum_plot_dx_target_dx(df = df_u3_ms_targdx_plot(),
                                x = "`Visit Date`", y = "madc_dx_cumsum",
                                group_var = "`MADC Dx`",
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
      filter(!is.na(`MADC Dx`)) %>% 
      # filter(!is.na(`UDS Dx`)) %>% 
      get_visit_n(ptid, `Visit Date`, Inf) %>%
      select(ptid, `MADC Dx`, condx_combn_name)
    # select(ptid, `UDS Dx`, condx_combn_name)
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
    select_condx_combn_vctr <- 
      character(length = length(select_condx_combn()))
    select_condx_combn_vctr_rgx <- 
      character(length = length(select_condx_combn()))
    
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
  output$select_condx_combn_pie_lbd <- renderPlot({
    pie_graph_fast(
      data = data_condx(),
      condx = select_condx(),
      dx = "LBD",
      combn_vctr = select_condx_combn_lst()$select_condx_combn_vctr,
      combn_vctr_rgx = select_condx_combn_lst()$select_condx_combn_vctr_rgx)
  })
  
  
  # Timeline Plots & Tables ----
  
  # Reactive to capture date range slider
  df_u3_ms_date_fltr <- reactive({
    df_u3_ms() %>% 
      select(ptid, 
             `Visit Date`,
             dur_exam_scrd, 
             dur_exam_dblscrd, 
             dur_exam_cons, 
             dur_fincons_fdbk) %>% 
      filter(input$date_slider[1] <= `Visit Date`,
             `Visit Date` <= input$date_slider[2])
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
  output$plot_timeline_exam_scrd_hist <- renderPlotly({
    ggplotly(
      df_u3_ms_date_fltr_exam_scrd() %>%
        ggplot(data = ., aes(x = dur_exam_scrd)) +
        geom_histogram(binwidth = 1, # center = 0.5,
                       color = "#000000", fill = "#3885B7") +
        scale_x_continuous(limits = c(-0.5, NA_integer_), name = "Days")
    )
  })
  
  # Timeline plot: dur_exam_dblscrd
  output$plot_timeline_exam_dblscrd_hist <- renderPlotly({
    ggplotly(
      df_u3_ms_date_fltr_exam_dblscrd() %>% 
        ggplot(data = ., aes(x = dur_exam_dblscrd)) +
        geom_histogram(binwidth = 1, # center = 0.5,
                       color = "#000000", fill = "#3885B7") +
        scale_x_continuous(limits = c(-0.5, NA_integer_), name = "Days")
    )
  })
  
  # Timeline plot: dur_exam_cons
  output$plot_timeline_exam_cons_hist <- renderPlotly({
    ggplotly(
      df_u3_ms_date_fltr_exam_cons() %>% 
        filter(!is.na(dur_exam_cons)) %>% 
        ggplot(data = ., aes(x = dur_exam_cons)) +
        geom_histogram(binwidth = 1, # center = 0.5,
                       color = "#000000", fill = "#3885B7") +
        scale_x_continuous(limits = c(-0.5, NA_integer_), name = "Days")
    )
  })
  
  # Timeline plot: dur_exam_cons
  output$plot_timeline_fincons_fdbk_hist <- renderPlotly({
    ggplotly(
      df_u3_ms_date_fltr_fincons_fdbk() %>% 
        filter(!is.na(dur_fincons_fdbk)) %>% 
        ggplot(data = ., aes(x = dur_fincons_fdbk)) +
        geom_histogram(binwidth = 1, # center = 0.5,
                       color = "#000000", fill = "#3885B7") +
        scale_x_continuous(limits = c(-0.5, NA_integer_), name = "Days")
    )
  })
  
  # })
}

shinyApp(ui = ui, server = server)

