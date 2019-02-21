# app.R

# LOAD LIBRARIES ----

library(shiny)
library(shinydashboard)
library(DT)

DT_OPTIONS <- list(paging = FALSE,
                   searching = FALSE,
                   ordering = FALSE,
                   info = FALSE)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  
  # Header ----
  dashboardHeader(title = "MADC Dashboard"),
  
  # Sidebar ----
  dashboardSidebar(
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
              h3("Groups"),
              checkboxGroupInput(
                inputId = "field_groups",
                label = "Field Groups",
                inline = TRUE,
                choices = list("race_value" = "race_value",
                               "sex_value"  = "sex_value")
              )
          )
        ),
        fluidRow(
          box(width = 12,
              verbatimTextOutput("blah"))
        ),
        fluidRow(
          box(width = 12,
              h3("Summary"),
              dataTableOutput("summary")
          )
        )
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
  
  select_groups <- reactive({ input$field_groups })

  output$summary <-
    renderDataTable({
      datatable(df_u3_ms(), options = DT_OPTIONS)
    })
  output$blah <- renderText(select_groups())
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
