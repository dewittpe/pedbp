library(shiny)
library(shinydashboard)

ui <-
dashboardPage(
  dashboardHeader(title = "Pediatric Blood Pressure Distributions")
  , 
  dashboardSidebar(
    sidebarMenu( #{{{
      id = "sidebar"
      , menuItem("Overview", tabName = "overview", icon = icon("fas fa-home"))
      , menuItem("BP",       tabName = "bp",       icon = icon("fas fa-chart-line"))
      , conditionalPanel(
            condition = "input.sidebar == 'bp'"
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
          ) # end of conditionalPanel 
    )#}}}
  ) 
  ,
  dashboardBody(
    tabItems(
        tabItem("overview", "Overview tab content")
      , tabItem("bp", 
          textOutput("age_string")
          , textOutput("sex_string")
      )
    )
  )
)
