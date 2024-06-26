library(shiny)
library(shinydashboard)
library(shinyBS)
library(data.table)
library(markdown)
library(ggplot2)
library(DT)
library(pedbp)

server <- function(input, output, session) {

  output$flowchart <- renderImage({
    list(src = normalizePath(system.file("images", "flowchart.png", package = "pedbp")))
  }, deleteFile = FALSE)

  output$citation_research_letter <- renderPrint({
    x <- citation(package = "pedbp")
    attr(x, "mheader") <- "Cite the research letter"
    x
  })
  output$citation_package <- renderPrint({
    citation(package = "pedbp", auto = TRUE)
  })

  ##############################################################################
  # Blood Pressure
  bp <- reactive({
    other_args <-
      list(source = input$bp_source,
           age = ifelse(input$bp_age_units == "months", input$bp_age_months, input$bp_age_years * 12),
           male = as.integer(input$bp_sex == "Male"),
           height = ifelse(input$bp_height_status == "Known (cm)",
                           input$bp_height_cm,
                           ifelse(input$bp_height_status == "Known (inches)",
                                  input$bp_height_inch * 2.54,
                                  NA_real_)),
           height_percentile = ifelse(input$bp_height_status == "Known (percentile)",
                                      input$bp_height_percentile / 100, NA_real_)
      )

    if (input$bp_sbp_status == "mmHg" & input$bp_dbp_status == "mmHg") {
      cl <- list(quote(p_bp))
      cl[["q_sbp"]] <- input$bp_sbp_mmHg
      cl[["q_dbp"]] <- input$bp_dbp_mmHg
      cl <- c(cl, other_args)
      x <- eval(as.call(cl))

      rtn <- list(sbp_mmHg = input$bp_sbp_mmHg,
                  sbp_p = x$sbp_p,
                  dbp_mmHg = input$bp_dbp_mmHg,
                  dbp_p = x$dbp_p,
                  bp_params = attr(x, "bp_params"))

      print(rtn)

    } else if (input$bp_sbp_status == "mmHg" & input$bp_dbp_status == "percentile") {
      scl <- list(quote(p_bp))
      scl[["q_sbp"]] <- input$bp_sbp_mmHg
      scl[["q_dbp"]] <- NA_real_
      dcl <- list(quote(q_bp))
      dcl[["p_sbp"]] <- NA_real_
      dcl[["p_dbp"]] <- input$bp_dbp_percentile / 100
      scl <- c(scl, other_args)
      dcl <- c(dcl, other_args)

      scl <- eval(as.call(scl))
      dcl <- eval(as.call(dcl))

      rtn <- list(sbp_mmHg = input$bp_sbp_mmHg,
                  sbp_p = scl$sbp_p,
                  dbp_mmHg = dcl$dbp,
                  dbp_p = input$bp_dbp_percentile / 100,
                  bp_params = attr(scl, "bp_params")
                     )

    } else if (input$bp_sbp_status == "percentile" & input$bp_dbp_status == "mmHg") {
      scl <- list(quote(q_bp))
      scl[["p_sbp"]] <- input$bp_sbp_percentile / 100
      scl[["p_dbp"]] <- NA_real_
      dcl <- list(quote(p_bp))
      dcl[["q_sbp"]] <- NA_real_
      dcl[["q_dbp"]] <- input$bp_dbp_mmHg
      scl <- c(scl, other_args)
      dcl <- c(dcl, other_args)

      scl <- eval(as.call(scl))
      dcl <- eval(as.call(dcl))

      rtn <- list(sbp_mmHg = scl$sbp,
                     sbp_p = input$bp_sbp_percentile / 100,
                     dbp_mmHg = input$bp_dbp_mmHg,
                     dbp_p = dcl$dbp_p,
                     bp_params = attr(scl, "bp_params")
                     )

    } else if (input$bp_sbp_status == "percentile" & input$bp_dbp_status == "percentile") {
      cl <- list(quote(q_bp))
      cl[["p_sbp"]] <- input$bp_sbp_percentile / 100
      cl[["p_dbp"]] <- input$bp_dbp_percentile / 100
      cl <- c(cl, other_args)
      x <- eval(as.call(cl))

      rtn <- list(sbp_mmHg = x$sbp,
                     sbp_p = input$bp_sbp_percentile / 100,
                     dbp_mmHg = x$dbp,
                     dbp_percentile = input$bp_dbp_percentile / 100,
                     bp_params = attr(x, "bp_params"))
    }

    od <- data.frame(mmHg = c(rtn$sbp_mmHg, rtn$dbp_mmHg),
                     bp   = gl(n = 2, k = 1, labels = c("Systolic", "Diastolic")),
                     p    = c(rtn$sbp_p, rtn$dbp_percentile))
    dseg <-
      data.frame(
          bp   = gl(n = 2, k = 2, labels = c('Systolic', 'Diastolic')),
          p    = c(rtn$sbp_p, rtn$sbp_p, rtn$dbp_p, rtn$dbp_p),
          pend = c(rtn$sbp_p, -Inf, rtn$dbp_p, -Inf),
          mmHg = c(-Inf, rtn$sbp_mmHg, -Inf, rtn$dbp_mmHg),
          mmHgend = c(rtn$sbp_mmHg, rtn$sbp_mmHg, rtn$dbp_mmHg, rtn$dbp_mmHg)
    )

    rtn$plot <- pedbp:::bpcdfplot(od, dseg, rtn$bp_params)

    od2 <- data.frame(age = other_args$age, bp = factor(c("Systolic", "Diastolic"), levels = c("Systolic", "Diastolic")), male = other_args$male, mmHg = od$mmHg)

    rtn$bp_chart <-
      do.call(bp_chart, c(other_args[c("male", "height", "height_percentile", "source")], list(p = c(0.05, 0.25, 0.5, 0.75, 0.95)))
              ) +
      ggplot2::geom_point(data = od2, mapping = ggplot2::aes(x = age, y = mmHg))
    rtn
  })

  output$bp_chart <- renderPlot({
    bp()$bp_chart
  })

  output$bp_cdf <- renderPlot({
    bp()$plot
  })

  output$bp_params <- renderTable({
    bp()$bp_params
  })

  output$bp_mmHg_percentile <- renderTable({
    x <- bp()
    data.table("bp" = c("Systolic", "Diastolic"),
               "mmHg" = c(x$sbp_mmHg, x$dbp_mmHg),
               "%itle" = 100*c(x$sbp_p, x$dbp_p))
  })

  ##############################################################################
  # Growth Standards

  gs_male <- reactive({as.integer(input$gs_sex == "Male")})
  gs_age  <- reactive({
    switch(input$gs_age_units,
           "days"   = input$gs_age_days * 365.25 / 12,
           "months" = input$gs_age_months,
           "years"  = input$gs_age_years / 12)
  })
  gs_stature <- reactive({
    if (grepl("Height", input$gs_stature_units)) {
      ifelse(grepl("inches", input$gs_stature_units), input$gs_stature_height_inches * 2.54, input$gs_stature_height_cm)
    } else {
      ifelse(grepl("inches", input$gs_stature_units), input$gs_stature_length_inches * 2.54, input$gs_stature_length_cm)
    }
  })
  gs_weight <- reactive({
    ifelse(grepl("kg", input$gs_weight_units), input$gs_weight_kg, input$gs_weight_lbs / 2.205)
  })
  gs_metric <- reactive({
    if (input$gs_standard == "BMI for Age") {
      "bmi_for_age"
    } else if (input$gs_standard == "Head Circumference for Age") {
     "head_circumference_for_age"
    } else if (input$gs_standard == "Weight for Age") {
      "weight_for_age"
    } else if (input$gs_standard == "Stature for Age") {
      if (grepl("Height", input$gs_stature_units)) {
        "height_for_age"
      } else {
        "length_for_age"
      }
    } else if (input$gs_standard == "Weight for Stature") {
      if (grepl("Height", input$gs_stature_units)) {
        "weight_for_height"
      } else {
        "weight_for_length"
      }
    } else {
      stop("Unknown input$gs_standard")
    }
  })

  gs <- reactive({
    cdf_data <- data.table(p = seq(0.001, 0.999, length.out = 200))

    if (input$gs_standard == "BMI for Age") {
      observed <- data.table(q = input$gs_bmi, p = p_bmi_for_age(q = input$gs_bmi, male = gs_male(), age = gs_age(), source = input$gs_source))
      cdf_data[, q := q_bmi_for_age(p = p, male = gs_male(), gs_age(), source = input$gs_source)]
      qplabel <- "BMI"
    } else if (input$gs_standard == "Head Circumference for Age") {
      hc <- ifelse(input$gs_head_circ_units == "cm", input$gs_head_circ_cm, input$gs_head_circ_inches * 2.54)
      observed <- data.table(q = hc, p = p_head_circumference_for_age(q = hc, male = gs_male(), age = gs_age(), source = input$gs_source))
      cdf_data[, q := q_head_circumference_for_age(p = p, male = gs_male(), gs_age(), source = input$gs_source)]
      qplabel <- "Head Circumference (cm)"
    } else if (input$gs_standard == "Stature for Age") {
      if (grepl("Height", input$gs_stature_units)) {
        observed <- data.table(q = gs_stature(), p = p_height_for_age(q = gs_stature(), male = gs_male(), age = gs_age(), source = input$gs_source))
        cdf_data[, q := q_height_for_age(p = p, male = gs_male(), gs_age(), source = input$gs_source)]
        qplabel <- "Height (cm)"
      } else {
        observed <- data.table(q = gs_stature(), p = p_length_for_age(q = gs_stature(), male = gs_male(), age = gs_age(), source = input$gs_source))
        cdf_data[, q := q_length_for_age(p = p, male = gs_male(), gs_age(), source = input$gs_source)]
        qplabel <- "Length (cm)"
      }
    } else if (input$gs_standard == "Weight for Age") {
        observed <- data.table(q = gs_weight(), p = p_weight_for_age(q = gs_weight(), male = gs_male(), age = gs_age(), source = input$gs_source))
        cdf_data[, q := q_weight_for_age(p = p, male = gs_male(), gs_age(), source = input$gs_source)]
        qplabel <- "Weight (kg)"
    } else if (input$gs_standard == "Weight for Stature") {
      if (grepl("Height", input$gs_stature_units)) {
        observed <- data.table(q = gs_weight(), p = p_weight_for_height(q = gs_weight(), male = gs_male(), height = gs_stature(), source = input$gs_source))
        cdf_data[, q := q_weight_for_height(p = p, male = gs_male(), gs_stature(), source = input$gs_source)]
      } else {
        observed <- data.table(q = gs_weight(), p = p_weight_for_length(q = gs_weight(), male = gs_male(), length = gs_stature(), source = input$gs_source))
        cdf_data[, q := q_weight_for_length(p = p, male = gs_male(), gs_stature(), source = input$gs_source)]
      }
      qplabel <- "Weight (kg)"
    } else {
      stop("Unknown gs_standard")
    }

    obs_seg <- data.table(p = c(observed$p, observed$p, 0), q = c(-Inf, observed$q, observed$q))

    cdf <- ggplot2::ggplot() +
      ggplot2::aes(x = q, y = p) +
      ggplot2::geom_line(data = cdf_data) +
      ggplot2::geom_point(data = observed) +
      ggplot2::geom_line(data = obs_seg) +
      ggplot2::scale_y_continuous(name = "Percentile", labels = scales::label_percent(suffix = "th")) +
      ggplot2::xlab(qplabel)

    chart_point <- switch(input$gs_standard,
                  "Weight for Height" = ggplot2::geom_point(x = gs_stature(), y = observed$q),
                  "Weight for Length" = ggplot2::geom_point(x = gs_stature(), y = observed$q),
                  ggplot2::geom_point(x = gs_age(), y = observed$q))

    chart <- gs_chart(metric = gs_metric(), male = gs_male()) + chart_point

    list(observed = observed, cdf = cdf, chart = chart, qplabel = qplabel)

  })

  output$gs_cdf_plot <- renderPlot({ gs()$cdf })
  output$gs_chart_plot <- renderPlot({ gs()$chart })
  output$gs_cdf_table <- renderTable({
    DT <- data.table::copy(gs()$observed)
    DT[, p := paste0(qwraps2::frmt(p*100, digits = 1), "th")]
    setnames(DT, old = c("q", "p"), new = c(gs()$qplabel, "%ile"))
    DT
  })

  ##############################################################################
  # Batch processing

  batch_data <- reactive({
    data.table::fread(input$bpfile$datapath)
  })

  batch_method <- reactive({

    if (input$batch_method1 == "Blood Pressure") {
      m <- "bp"
    } else {
      m <- input$batch_method1
    }

    sub("^d", "p", gsub(" ", "_", tolower(paste(substr(input$batch_method2, 1, 1), m))))


  })

  output$batch_name_mapping <- renderUI({
    req(input$bpfile)
    data_names <- names(batch_data())

    map <- list()

    if ("q_sbp" %in% data_names) {
      map$q_sbp <- "q_sbp"
    } else if (length(i <- grep("^sbp", data_names)) > 0) {
      map$q_sbp <- data_names[min(i)]
    } else {
      map$q_sbp <- "_ignore_"
    }

    if ("q_sbp" %in% data_names) {
      map$q_sbp <- "q_sbp"
    } else if (length(i <- grep("^sbp", data_names)) > 0) {
      map$q_sbp <- data_names[min(i)]
    } else {
      map$q_sbp <- "_ignore_"
    }

    if ("q_dbp" %in% data_names) {
      map$q_dbp <- "q_dbp"
    } else if (length(i <- grep("^dbp", data_names)) > 0) {
      map$q_dbp <- data_names[min(i)]
    } else {
      map$q_dbp <- "_ignore_"
    }

    if ("q_dbp" %in% data_names) {
      map$q_dbp <- "q_dbp"
    } else if (length(i <- grep("^dbp", data_names)) > 0) {
      map$q_dbp <- data_names[min(i)]
    } else {
      map$q_dbp <- "_ignore_"
    }

    if ("age" %in% data_names) {
      map$age <- "age"
    } else if (length(i <- grep("^age", data_names)) > 0) {
      map$age <- data_names[min(i)]
    } else {
      map$age <- "_Select Default_"
    }

    if ("male" %in% data_names) {
      map$male <- "male"
    } else if (length(i <- grep("^male", data_names)) > 0) {
      map$male <- data_names[min(i)]
    } else {
      map$male <- "_Select Default_"
    }

    if ("height" %in% data_names) {
      map$height <- "height"
    } else if (length(i <- grep("^height", data_names)) > 0) {
      map$height <- data_names[min(i)]
    } else {
      map$height <- "_ignore_"
    }

    if ("height_percentile" %in% data_names) {
      map$height_percentile <- "height_percentile"
    } else if (length(i <- grep("^height_percentile", data_names)) > 0) {
      map$height_percentile <- data_names[min(i)]
    } else {
      map$height_percentile <- "_ignore_"
    }

    if ("source" %in% data_names) {
      map$source <- "source"
    } else if (length(i <- grep("^source", data_names)) > 0) {
      map$source <- data_names[min(i)]
    } else {
      map$source <- "_Select Default_"
    }

    if ("length" %in% data_names) {
      map$length <- "length"
    } else if (length(i <- grep("^length", data_names)) > 0) {
      map$length <- data_names[min(i)]
    } else {
      map$length <- "_Select Default_"
    }

    if ("weight" %in% data_names) {
      map$weight <- "weight"
    } else if (length(i <- grep("^weight", data_names)) > 0) {
      map$weight <- data_names[min(i)]
    } else {
      map$weight <- "_Select Default_"
    }

    if ("bmi" %in% data_names) {
      map$bmi <- "bmi"
    } else if (length(i <- grep("^bmi", data_names)) > 0) {
      map$bmi <- data_names[min(i)]
    } else {
      map$bmi <- "_ignore_"
    }

    if ("head_circumference" %in% data_names) {
      map$head_circumference <- "head_circumference"
    } else if (length(i <- grep("^head_circumference", data_names)) > 0) {
      map$head_circumference <- data_names[min(i)]
    } else {
      map$head_circumference <- "_ignore_"
    }

    inputs <- list(
      p_sbp = selectInput( inputId = "batch_p_sbp", label = "SBP Percentile (expected to be between 0 and 100)", choices = c("_ignore_", data_names), selected = map$p_sbp, multiple = FALSE),
      p_dbp = selectInput( inputId = "batch_p_dbp", label = "DBP Percentile (expected to be between 0 and 100)", choices = c("_ignore_", data_names), selected = map$p_dbp, multiple = FALSE),
      p_bmi = list(selectInput(inputId = "batch_p", label = "BMI Percentile (expected to be between 0 and 100)", choices = c("_ignore_", data_names), selected = map$bmi, multiple = FALSE)),
      p_head_circumference = list(selectInput(inputId = "batch_p", label = "Head Circumference Percentile (expected to be between 0 and 100)", choices = c("_ignore_", data_names), selected = map$head_circumference, multiple = FALSE)),
      p_height = list(selectInput(inputId = "batch_p", label = "Height Percentile (expected to be between 0 and 100)", choices = c("_ignore_", data_names), selected = map$height, multiple = FALSE)),
      p_length = list(selectInput(inputId = "batch_p", label = "Lenght Percentile (expected to be between 0 and 100)", choices = c("_ignore_", data_names), selected = map$length, multiple = FALSE)),
      p_weight = list(selectInput(inputId = "batch_p", label = "Weight Percentile (expected to be between 0 and 100)", choices = c("_ignore_", data_names), selected = map$weight, multiple = FALSE)),

      q_sbp = selectInput(inputId = "batch_q_sbp", label = "SBP (mmHg)", choices = c("_ignore_", data_names), selected = map$q_sbp, multiple = FALSE),
      q_dbp = selectInput(inputId = "batch_q_dbp", label = "DBP (mmHg)", choices = c("_ignore_", data_names), selected = map$q_dbp, multiple = FALSE),
      q_bmi = list(selectInput(inputId = "batch_q", label = "BMI", choices = c("_ignore_", data_names), selected = map$bmi, multiple = FALSE)),
      q_head_circumference = list(selectInput(inputId = "batch_q", label = "Head Circumference (cm)", choices = c("_ignore_", data_names), selected = map$head_circumference, multiple = FALSE)),
      q_height = list(selectInput(inputId = "batch_q", label = "Height (cm)", choices = c("_ignore_", data_names), selected = map$height, multiple = FALSE)),
      q_length = list(selectInput(inputId = "batch_q", label = "Lenght (cm)", choices = c("_ignore_", data_names), selected = map$length, multiple = FALSE)),
      q_weight = list(selectInput(inputId = "batch_q", label = "Weight (kg)", choices = c("_ignore_", data_names), selected = map$weight, multiple = FALSE)),

      age = list(selectInput( inputId = "batch_age", label = "Age (months)", choices = c("_Select Default_", data_names), selected = map$age, multiple = FALSE),
                 conditionalPanel( condition = "input.batch_age == '_Select Default_'", sliderInput(inputId = "batch_age_default", label = NULL, min = 1, max = 18*12, value = 8, step = 0.5))),
      male = list(selectInput( inputId = "batch_male", label = list("Male", bsButton(inputId = "batch_male_info", label = NULL, icon = icon("info"))), choices = c("_Select Default_", data_names), selected = map$male, multiple = FALSE),
                  bsPopover(id = "batch_male_info", title = "", content = "Column with 0 = female, 1 = male"),
                  conditionalPanel( condition = "input.batch_male == '_Select Default_'", radioButtons(inputId = "batch_male_default", label = NULL, choices = c("Female", "Male"), selected = "Female", inline = TRUE))),

      height = list(selectInput( inputId = "batch_height", label = "Height (cm)", choices = c("_ignore_", data_names), selected = map$height, multiple = FALSE)),
      height_percentile = list( selectInput( inputId = "batch_height_percentile", label = "Height Percentile", choices = c("_ignore_", data_names), selected = map$height_percentile, multiple = FALSE)),
      length = list(selectInput( inputId = "batch_length", label = "Length (cm)", choices = c("_ignore_", data_names), selected = map$length, multiple = FALSE)),

      height_with_deafult = list(selectInput( inputId = "batch_height", label = "Height (cm)", choices = c("_Select Default_", data_names), selected = map$height, multiple = FALSE),
                                 conditionalPanel( condition = "input.batch_height == '_Select Default_'", sliderInput(inputId = "batch_height_default", label = NULL, min = 0, max = 225, value = 100, step = 1))),

      length_with_deafult = list(selectInput( inputId = "batch_length", label = "Length (cm)", choices = c("_Select Default_", data_names), selected = map$length, multiple = FALSE),
                                 conditionalPanel( condition = "input.batch_length == '_Select Default_'", sliderInput(inputId = "batch_length_default", label = NULL, min = 0, max = 225, value = 100, step = 1))),


      bp_source = list(selectInput( inputId = "batch_source", label = "Data Source", choices = c("_Select Default_", data_names), selected = map$source, multiple = FALSE),
                       conditionalPanel( condition = "input.batch_source == '_Select Default_'", radioButtons(inputId = "batch_source_default", label = NULL, choices = c("martin2022", "gemelli1990", "lo2013", "nhlbi", "flynn2017"), selected = "martin2022", inline = TRUE))),
      gs_source = list(selectInput(inputId = "batch_source", label = "Data Source", choices = c("_Select Default_", data_names), selected = map$source, multiple = FALSE),
                       conditionalPanel(condition = "input.batch_source == '_Select Default_'", radioButtons(inputId = "batch_source_default", label = NULL, choices = c("CDC", "WHO"), selected = "CDC", inline = TRUE)))
    )

    if (input$batch_method1 == "Blood Pressure") {
      if (input$batch_method2 %in% c("Distribution", "Z-scores")) {
        inputs[c("q_sbp", "q_dbp", "age", "male", "height", "height_percentile", "bp_source")]
      } else {
        inputs[c("p_sbp", "p_dbp", "age", "male", "height", "height_percentile", "bp_source")]
      }
    } else if (input$batch_method1 == "BMI for Age") {
      if (input$batch_method2 %in% c("Distribution", "Z-scores")) {
        inputs[c("q_bmi", "male", "age", "gs_source")]
      } else {
        inputs[c("p_bmi", "male", "age", "gs_source")]
      }
    } else if (input$batch_method1 == "Head Circumference for Age") {
      if (input$batch_method2 %in% c("Distribution", "Z-scores")) {
        inputs[c("q_head_circumference", "male", "age", "gs_source")]
      } else {
        inputs[c("p_head_circumference", "male", "age", "gs_source")]
      }
    } else if (input$batch_method1 == "Height for Age") {
      if (input$batch_method2 %in% c("Distribution", "Z-scores")) {
        inputs[c("q_height", "male", "age", "gs_source")]
      } else {
        inputs[c("p_height", "male", "age", "gs_source")]
      }
    } else if (input$batch_method1 == "Length for Age") {
      if (input$batch_method2 %in% c("Distribution", "Z-scores")) {
        inputs[c("q_length", "male", "age", "gs_source")]
      } else {
        inputs[c("p_length", "male", "age", "gs_source")]
      }
    } else if (input$batch_method1 == "Weight for Age") {
      if (input$batch_method2 %in% c("Distribution", "Z-scores")) {
        inputs[c("q_weight", "male", "age", "gs_source")]
      } else {
        inputs[c("p_weight", "male", "age", "gs_source")]
      }
    } else if (input$batch_method1 == "Weight for Length") {
      if (input$batch_method2 %in% c("Distribution", "Z-scores")) {
        inputs[c("q_weight", "male", "length_with_deafult", "gs_source")]
      } else {
        inputs[c("p_weight", "male", "length_with_deafult", "gs_source")]
      }
    } else if (input$batch_method1 == "Weight for Height") {
      if (input$batch_method2 %in% c("Distribution", "Z-scores")) {
        inputs[c("q_weight", "male", "height_with_deafult", "gs_source")]
      } else {
        inputs[c("p_weight", "male", "height_with_deafult", "gs_source")]
      }
    } else {
      p("not yet built")
    }
  })


  batch_results <- reactive({
    req(input$bpfile)
    d <- data.table::copy(batch_data())

    cl <- list()
    cl[[1]] <- as.name(batch_method())

    if (input$batch_male == "_Select Default_") {
      cl[["male"]] <- rep(input$batch_male_default, nrow(d))
    } else {
      cl[["male"]] <- d[[input$batch_male]]
    }

    if (input$batch_source == "_Select Default_") {
      cl[["source"]] <- input$batch_source_default
    } else {
      cl[["source"]] <- d[[input$batch_source]]
    }

    if (grepl("bp$", batch_method()) | grepl("age$", batch_method())) {
      if (input$batch_age == "_Select Default_") {
        cl[["age"]] <- rep(input$batch_age_default, nrow(d))
      } else {
        cl[["age"]] <- d[[input$batch_age]]
      }
    }

    if (grepl("length$", batch_method())) {
      if (input$batch_length == "_Select Default_") {
        cl[["length"]] <- rep(input$batch_length_default, nrow(d))
      } else {
        cl[["length"]] <- d[[input$batch_length]]
      }
    }

    if (grepl("height$", batch_method())) {
      if (input$batch_height == "_Select Default_") {
        cl[["height"]] <- rep(input$batch_height_default, nrow(d))
      } else {
        cl[["height"]] <- d[[input$batch_height]]
      }
    }

    if (batch_method() %in% c("p_bp", "z_bp")) {
      if (input$batch_q_sbp == "_ignore_") {
        cl[["q_sbp"]] <- rep(NA_real_, nrow(d))
      } else {
        cl[["q_sbp"]] <- d[[input$batch_q_sbp]]
      }
      if (input$batch_q_dbp == "_ignore_") {
        cl[["q_dbp"]] <- rep(NA_real_, nrow(d))
      } else {
        cl[["q_dbp"]] <- d[[input$batch_q_dbp]]
      }
      if (input$batch_height == "_ignore_") {
        cl[["height"]] <- rep(NA_real_, nrow(d))
      } else {
        cl[["height"]] <- d[[input$batch_height]]
      }
      if (input$batch_height_percentile == "_ignore_") {
        cl[["height_percentile"]] <- rep(NA_real_, nrow(d))
      } else {
        cl[["height_percentile"]] <- d[[input$batch_height_percentile]]
      }

      results <- eval(as.call(cl))
      if (batch_method() == "p_bp") {
        d[, pedbp_sbp_p := results$sbp_p]
        d[, pedbp_dbp_p := results$dbp_p]
      } else {
        d[, pedbp_sbp_z := results$sbp_z]
        d[, pedbp_dbp_z := results$dbp_z]
      }

    } else if (batch_method() == "q_bp") {
      if (input$batch_p_sbp == "_ignore_") {
        cl[["p_sbp"]] <- rep(NA_real_, nrow(d))
      } else {
        cl[["p_sbp"]] <- d[[input$batch_p_sbp]] / 100 # expected inputs are 0 to 100
      }
      if (input$batch_p_dbp == "_ignore_") {
        cl[["p_dbp"]] <- rep(NA_real_, nrow(d))
      } else {
        cl[["p_dbp"]] <- d[[input$batch_p_dbp]] / 100 # expected inputs are 0 to 100
      }
      if (input$batch_height == "_ignore_") {
        cl[["height"]] <- rep(NA_real_, nrow(d))
      } else {
        cl[["height"]] <- d[[input$batch_height]]
      }
      if (input$batch_height_percentile == "_ignore_") {
        cl[["height_percentile"]] <- rep(NA_real_, nrow(d))
      } else {
        cl[["height_percentile"]] <- d[[input$batch_height_percentile]]
      }

      results <- eval(as.call(cl))
      d[, pedbp_sbp_mmHg := results$sbp]
      d[, pedbp_dbp_mmHg := results$dbp]

    } else if (grepl("^p_", batch_method()) | grepl("^z_", batch_method())) {
      if (input$batch_q == "_ignore_") {
        cl[["q"]] <- rep(NA_real_, nrow(d))
      } else {
        cl[["q"]] <- d[[input$batch_q]]
      }

      results <- eval(as.call(cl))
      data.table::set(d, j = batch_method(), value = results)

    } else if (grepl("^q_", batch_method())) {
      if (input$batch_p == "_ignore_") {
        cl[["p"]] <- rep(NA_real_, nrow(d))
      } else {
        cl[["p"]] <- d[[input$batch_p]]
      }

      results <- eval(as.call(cl))
      data.table::set(d, j = batch_method(), value = results)
    }

    d[]
  })

  output$batch_results <- DT::renderDataTable({
    d <- data.table::copy(batch_results())
    d[]
  })

  output$download_button <- renderUI({
    if(!is.null(input$bpfile)) {
      downloadButton("download_batch_results", "Download Results")
    }
  })

#  output$csv_for_batch <- renderImage({
#    list(height = "90%", src = normalizePath(system.file("images", "csv_for_batch.png", package = "pedbp")))
#  }, deleteFile = FALSE)
#
#  batch_results <- reactive({
#    req(input$bpfile)
#    d <- data.table::fread(input$bpfile$datapath)
#    names(d) <- c("pid", "age_months", "male", "height_cm", "sbp_mmHg", "dbp_mmHg")
#    d <- d[
#           , as.list(p_bp(sbp_mmHg, dbp_mmHg, age_months, male, height_cm))
#           , by = .(pid, age_months, male, height_cm, sbp_mmHg, dbp_mmHg)
#           ]
#   d
#  })
#
#
  output$download_batch_results <- downloadHandler(
    filename = function() {paste0(input$bpfile, "_with_percentiles.csv")},
    content  = function(file) {
      data.table::fwrite(batch_results(), file)
    }
  )
#
#
#  bp <- reactive({
#    bp <- p_bp(input$sbp, input$dbp, age = input$age_mo, male = input$sex,
#               height = ifelse(input$height_known == 0, NA, input$height_cm))
#    bp$sbp_p <- paste0(round(bp$sbp_p * 100, 2), " percentile")
#    bp$dbp_percentile <- paste0(round(bp$dbp_percentile * 100, 2), " percentile")
#    bp
#  })
#
#  output$bp_notes <- renderTable({
#    d <- attr(bp(), "bp_params")
#    names(d) <-
#      c("Source", "Male", "Age (months)", "SBP Mean", "SBP SD", "DBP Mean", "DBP SD", "Height Percentile")
#    d
#  })
#
#  output$patient_notes <- renderTable({
#    hp <-
#      ifelse(input$height_known == 0, "",
#             ifelse(input$age_mo < 36,
#                    paste(round(p_length_for_age_inf(input$height_cm, input$age_mo, input$sex) * 100, 2), " percentile"),
#                    paste(round(p_stature_for_age(input$height_cm, input$age_mo, input$sex) * 100, 2), " percentile")
#             )
#      )
#
#    data.frame(V1 = c("Age", "Sex", "Height", "SBP", "DBP"),
#               V2 = c(paste0(input$age_mo, " months"),
#                      ifelse(input$sex == 0, "Female", "Male"),
#                      ifelse(input$height_known == 0, "Unknown", paste0(round(input$height_cm, 1), " cm")),
#                      paste0(input$sbp, "  mmHg"),
#                      paste0(input$dbp, "  mmHg")),
#               V3 = c(paste0("(", round(input$age_mo / 12, 2), " years)"),
#                      "",
#                      ifelse(input$height_known == 0, "", paste0("(", (input$height_cm * 0.393701) %/% 12, "' ", round((input$height_cm * 0.393701) %% 12, 1), "'')")),
#                      "",
#                      ""),
#               V4 = c("", "", hp, bp()$sbp_p, bp()$dbp_percentile)
#               )
#  }
#  , colnames = FALSE
#  )
#
#  })
}



