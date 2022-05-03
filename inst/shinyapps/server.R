# input <- list()
# input$age_mo <- 96
# input$sex <- 1L
# input$sbp <- 102
# input$dbp <- 58
# input$height_cm <- 102
# input$height_known <- 0

server <- function(input, output, session) {

  output$flowchart <- renderImage({
    list(src = normalizePath(system.file("images", "flowchart.png", package = "pedbp")))
  })

  bp <- reactive({
    bp <- p_bp(input$sbp, input$dbp, age = input$age_mo, male = input$sex,
               height = ifelse(input$height_known == 0, NA, input$height_cm))
    bp$sbp_percentile <- paste0(round(bp$sbp_percentile * 100, 2), " percentile")
    bp$dbp_percentile <- paste0(round(bp$dbp_percentile * 100, 2), " percentile")
    bp
  })

  output$bp_notes <- renderTable({ 
    d <- attr(bp(), "bp_params") 
    names(d) <-
      c("Source", "Male", "Age (months)", "SBP Mean", "SBP SD", "DBP Mean", "DBP SD", "Height Percentile")
    d
  })

  output$patient_notes <- renderTable({
    hp <-
      ifelse(input$height_known == 0, "",
             ifelse(input$age_mo < 36,
                    paste(round(p_length_for_age_inf(input$height_cm, input$age_mo, input$sex) * 100, 2), " percentile"),
                    paste(round(p_stature_for_age(input$height_cm, input$age_mo, input$sex) * 100, 2), " percentile")
             )
      )

    data.frame(V1 = c("Age", "Sex", "Height", "SBP", "DBP"),
               V2 = c(paste0(input$age_mo, " months"),
                      ifelse(input$sex == 0, "Female", "Male"),
                      ifelse(input$height_known == 0, "Unknown", paste0(round(input$height_cm, 1), " cm")),
                      paste0(input$sbp, "  mmHg"),
                      paste0(input$dbp, "  mmHg")),
               V3 = c(paste0("(", round(input$age_mo / 12, 2), " years)"),
                      "",
                      ifelse(input$height_known == 0, "", paste0("(", (input$height_cm * 0.393701) %/% 12, "' ", round((input$height_cm * 0.393701) %% 12, 1), "'')")),
                      "",
                      ""),
               V4 = c("", "", hp, bp()$sbp_percentile, bp()$dbp_percentile)
               )
  }
  , colnames = FALSE
  )

  output$bp_cdf <- renderPlot({
    bp_cdf(input$age_mo, male = input$sex, height = ifelse(input$height_known == 0, NA, input$height_cm), sbp = input$sbp, dbp = input$dbp) +
      ggplot2::scale_x_continuous(breaks = seq(30, 140, by = 10))
  })
}



