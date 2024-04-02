library(shiny)
library(shinydashboard)
library(pedbp)
library(data.table)
library(markdown)
library(ggplot2)
library(DT)

server <- function(input, output, session) {

  output$flowchart <- renderImage({
    list(src = normalizePath(system.file("images", "flowchart.png", package = "pedbp")))
  }, deleteFile = FALSE)

  bp_calls <- reactive({

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

      output <- list(sbp_mmHg = input$bp_sbp_mmHg,
                     sbp_percentile = x$sbp_percentile,
                     dbp_mmHg = input$bp_dbp_mmHg,
                     dbp_percentile = x$dbp_percentile,
                     bp_params = attr(x, "bp_params"))

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

      print("\n\n......\n\n")
      print("attr(scl, 'bp_params')")
      print(attr(scl, 'bp_params'))
      print("attr(dcl, 'bp_params')")
      print(attr(dcl, 'bp_params'))


      stopifnot(isTRUE(all.equal(attr(scl, "bp_params"), attr(dcl, "bp_params"))))

      output <- list(sbp_mmHg = input$bp_sbp_mmHg,
                     sbp_percentile = scl$sbp_percentile / 100,
                     dbp_mmHg = dcl$dbp,
                     dbp_percentile = input$bp_dbp_percentile / 100,
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

      stopifnot(isTRUE(all.equal(attr(scl, "bp_params"), attr(dcl, "bp_params"))))

      output <- list(sbp_mmHg = scl$sbp,
                     sbp_percentile = input$bp_sbp_percentile / 100,
                     dbp_mmHg = input$bp_dbp_mmHg,
                     dbp_percentile = dcl$dbp_percentile,
                     bp_params = attr(scl, "bp_params")
                     )

    } else if (input$bp_sbp_status == "percentile" & input$bp_dbp_status == "percentile") {
      cl <- list(quote(q_bp))
      cl[["p_sbp"]] <- input$bp_sbp_percentile / 100
      cl[["p_dbp"]] <- input$bp_dbp_percentile / 100
      cl <- c(cl, other_args)
      x <- eval(as.call(cl))

      output <- list(sbp_mmHg = x$sbp,
                     sbp_percentile = input$bp_sbp_percentile / 100,
                     dbp_mmHg = x$dbp,
                     dbp_percentile = input$bp_dbp_percentile / 100,
                     bp_params = attr(x, "bp_params"))
    }

    od <- data.frame(mmHg = c(output$sbp_mmHg, output$dbp_mmHg),
                     bp   = gl(n = 2, k = 1, labels = c("Systolic", "Diastolic")),
                     p    = c(output$sbp_percentile, output$dbp_percentile))
    dseg <-
      data.frame(
          bp   = gl(n = 2, k = 2, labels = c('Systolic', 'Diastolic')),
          p    = c(output$sbp_percentile, output$sbp_percentile, output$dbp_percentile, output$dbp_percentile),
          pend = c(output$sbp_percentile, -Inf, output$dbp_percentile, -Inf),
          mmHg = c(-Inf, output$sbp_mmHg, -Inf, output$dbp_mmHg),
          mmHgend = c(output$sbp_mmHg, output$sbp_mmHg, output$dbp_mmHg, output$dbp_mmHg)
    )

    output$plot <- pedbp:::bpcdfplot(od, dseg, output$bp_params)
    output
  })

  output$bp_cdf <- renderPlot({
    bp_calls()$plot
  })


  output$bp_params <- renderTable({
    bp_calls()$bp_params
  })

  output$bp_mmHg_percentile <- renderTable({
    x <- bp_calls()
    data.table("bp" = c("Systolic", "Diastolic"),
               "mmHg" = c(x$sbp_mmHg, x$dbp_mmHg),
               "%itle" = 100*c(x$sbp_percentile, x$dbp_percentile))
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
#  output$batch_results <- DT::renderDataTable({
#    batch_results()
#  })
#
#  output$download_batch_results <- downloadHandler(
#    filename = function() {paste0(input$bpfile, "_with_percentiles.csv")},
#    content  = function(file) {
#      data.table::fwrite(batch_results(), file)
#    }
#  )
#
#  output$download_button <- renderUI({
#    if(!is.null(input$bpfile)) {
#      downloadButton("download_batch_results", "Download Data With Percentiles (.csv)")
#    }
#  })
#
#  bp <- reactive({
#    bp <- p_bp(input$sbp, input$dbp, age = input$age_mo, male = input$sex,
#               height = ifelse(input$height_known == 0, NA, input$height_cm))
#    bp$sbp_percentile <- paste0(round(bp$sbp_percentile * 100, 2), " percentile")
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
#               V4 = c("", "", hp, bp()$sbp_percentile, bp()$dbp_percentile)
#               )
#  }
#  , colnames = FALSE
#  )
#
#  output$bp_cdf <- renderPlot({
#    bp_cdf(input$age_mo, male = input$sex, height = ifelse(input$height_known == 0, NA, input$height_cm), sbp = input$sbp, dbp = input$dbp) +
#      ggplot2::scale_x_continuous(breaks = seq(30, 140, by = 10))
#  })
}



