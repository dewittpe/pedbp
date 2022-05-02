server <- function(input, output, session) {
  output$age_string <- renderText({paste0("Patient Age: ", input$age_mo, " months (", round(input$age_mo / 12, 2), " years)")})
  output$sex_string <- renderText({paste0("Patient Sex: ", ifelse(input$sex == 0, "Female", "Male"))})
}

