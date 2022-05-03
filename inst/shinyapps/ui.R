library(shiny)
library(shinydashboard)
library(pedbp)

ui <-
dashboardPage(
  dashboardHeader(
                    title = "pedbp"
                  #, disable = TRUE
                  )
  ,
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar"
      , menuItem("Overview",       tabName = "overview", icon = icon("fas fa-home"))
      , menuItem("Blood Pressure", tabName = "bp",       icon = icon("fas fa-chart-line"))
      , conditionalPanel( #{{{
            condition = "input.sidebar == 'bp'"
          , numericInput(
              inputId = "sbp"
            , label = "Systolic"
            , value = 102
            , min = 0
            )
          , numericInput(
              inputId = "dbp"
            , label = "Diastolic"
            , value = 58
            , min = 0
            )
          , numericInput(
              inputId = "age_mo"
            , label = "Age (months)"
            , value = 8 * 12
            , min = 0
            , max = 18 * 12
            )
          , radioButtons(
              inputId = "sex"
            , label = "Sex"
            , choices = list("Male" = 1, "Female" = 0)
            , inline = TRUE
            )
          , radioButtons(
              inputId = "height_known"
            , label = "Is patient height (length) known?"
            , choices = list("Yes" = 1, "No" = 0)
            , selected = 0
            , inline = TRUE
            )
          , conditionalPanel(
                condition = "input.height_known == 1"
              , numericInput(
                    inputId = "height_cm"
                  , label = "Height (cm)"
                  , value = 123
                  , min = 0
                  )
              )
          ) # end of conditionalPanel
      , menuItem("Batch Processing", tabName = "batch",  icon = icon("fas fa-file"))
    )#}}}
  )
  ,
  dashboardBody(
    tabItems(
        tabItem("overview",
          fluidRow(column(width = 6, includeMarkdown("overview.md"))
                   , column(width = 6, p("Flowchart for selection of data set to build blood pressure percentiles one."), plotOutput("flowchart"))))
      , tabItem("bp" #{{{
          ,
          h2("Blood Pressure Percentiles")
          ,
          fluidRow(
              box(title = "Patient Notes", width = 3, tableOutput("patient_notes"))
            , box(width = 9, plotOutput("bp_cdf"))
          )
          ,
          fluidRow(
                   column(width = 9, offset = 4,
                          box(title = "Blood Pressure CDF Notes", width = 7, tableOutput("bp_notes"))
                          )
          )
      ) #}}}
      , tabItem("batch" # {{{
          , h2("Batch Processing")
          , fluidRow(column(width = 12, "You may use this page to get the blood pressure percentiles for several patients at one time. The expected format the data in the uploaded file is as follows:"))
          , fluidRow(column(width = 5, plotOutput("csv_for_batch", height = "200px")))
          , fluidRow(column(width = 12, "That is, each row represents a patient, a column for age in months, sex indicated as a 0 = female, 1 = male, height in centimeters; leave empty cell for unknown height, systolic blood pressure in mmHg, and diastolic blood pressure in mmHg. The actual column names are not important.  The order of the columns is critically important as the batch process code assumes the shown sequence of columns."))
          , fluidRow(column(width = 12, "You may upload a file here and you'll get an output table to explore in this app along with the option to download a csv file with the blood pressure percentiles."))
      ) #}}}
    )
  )
)
