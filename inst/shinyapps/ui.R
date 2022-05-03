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
      , tabItem("bp",
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
      )
      , tabItem("batch", h3("hi"))
    )
  )
)
