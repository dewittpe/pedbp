library(shiny)
library(shinydashboard)
library(shinyBS)
library(data.table)
library(markdown)
library(ggplot2)
library(pedbp)

gs_age_box <-
  box(
    title = "Age",
    solidHeader = FALSE,
    width = 12,
    collapsible = TRUE,
    collapsed = FALSE,
    selectInput(
      inputId = "gs_age_units",
      label = NULL,
      choices = c("days", "months", "years"),
      selected = "months",
      multiple = FALSE
    ),
    conditionalPanel(
      condition = "input.gs_age_units == 'days'",
      sliderInput(
        inputId = "gs_age_days",
        label = NULL,
        min = 0,
        max = ceiling(18 * 365.25),
        value = 365,
        step = 1
      )
    ),
    conditionalPanel(
      condition = "input.gs_age_units == 'months'",
      sliderInput(
        inputId = "gs_age_months",
        label = NULL,
        min = 0,
        max = 18*12,
        value = 8 * 12,
        step = 0.25
      )
    ),
    conditionalPanel(
      condition = "input.gs_age_units == 'years'",
      sliderInput(
        inputId = "gs_age_years",
        label = NULL,
        min = 0,
        max = 18,
        value = 8,
        step = 0.1
      )
    )
  )

gs_bmi_box <-
  box(
    title = "BMI",
    solidHeader = FALSE,
    width = 12,
    collapsible = TRUE,
    collapsed = FALSE,
    sliderInput(
      inputId = "gs_bmi",
      label = NULL,
      min = 0,
      max = 50,
      value = 20,
      step = 0.1
    )
  )

gs_weight_box <-
  box(
    title = "Weight",
    solidHeader = FALSE,
    width = 12,
    collapsible = TRUE,
    collapsed = FALSE,
    selectInput(
      inputId = "gs_weight_units",
      label = NULL,
      choices = c("kg", "lbs"),
      selected = "kg",
      multiple = FALSE
    ),
    conditionalPanel(
      condition = "input.gs_weight_units == 'kg'",
      sliderInput(
        inputId = "gs_weight_kg",
        label = NULL,
        min = 0,
        max = 135, # about 300 lbs
        value = 45,
        step = 0.1
      )
    ),
    conditionalPanel(
      condition = "input.gs_weight_units == 'lbs'",
      sliderInput(
        inputId = "gs_weight_lbs",
        label = NULL,
        min = 0,
        max = 300,
        value = 100,
        step = 0.1
      )
    )
  )

gs_stature_box <-
  box(
    title = "Stature",
    solidHeader = FALSE,
    width = 12,
    collapsible = TRUE,
    collapsed = FALSE,
    selectInput(
      inputId = "gs_stature_units",
      label = NULL,
      choices = c("Height (cm)", "Height (inches)", "Length (cm)", "Length (inches)"),
      selected = "Height (cm)",
      multiple = FALSE
    ),
    conditionalPanel(
      condition = "input.gs_stature_units == 'Height (cm)'",
      sliderInput(
        inputId = "gs_stature_height_cm",
        label = NULL,
        min = 0,
        max = 225,
        value = 100,
        step = 1
      )
    ),
    conditionalPanel(
      condition = "input.gs_stature_units == 'Height (inches)'",
      sliderInput(
        inputId = "gs_stature_height_inches",
        label = NULL,
        min = 0,
        max = 100,
        value = 48,
        step = 1
      )
    ),
    conditionalPanel(
      condition = "input.gs_stature_units == 'Length (cm)'",
      sliderInput(
        inputId = "gs_stature_length_cm",
        label = NULL,
        min = 0,
        max = 225,
        value = 100,
        step = 1
      )
    ),
    conditionalPanel(
      condition = "input.gs_stature_units == 'Length (inches)'",
      sliderInput(
        inputId = "gs_stature_length_inches",
        label = NULL,
        min = 0,
        max = 100,
        value = 48,
        step = 1
      )
    )
  )

gs_head_circ_box <-
  box(
    title = "Head Circumference",
    solidHeader = FALSE,
    width = 12,
    collapsible = TRUE,
    collapsed = FALSE,
    selectInput(
      inputId = "gs_head_circ_units",
      label = NULL,
      choices = c("cm", "inches"),
      selected = "cm",
      multiple = FALSE
    ),
    conditionalPanel(
      condition = "input.gs_head_circ_units == 'cm'",
      sliderInput(
        inputId = "gs_head_circ_cm",
        label = NULL,
        min = 30,
        max = 60,
        value = 45,
        step = 0.1
      )
    ),
    conditionalPanel(
      condition = "input.gs_head_circ_units == 'inches'",
      sliderInput(
        inputId = "gs_head_circ_inches",
        label = NULL,
        min = floor(30 * 2.54),
        max = ceiling(60 * 2.54),
        value = 114,
        step = 0.25
      )
    )
  )

ui <-
  dashboardPage(
    dashboardHeader(title = paste0("pedbp v", packageVersion("pedbp"))),
    dashboardSidebar(
      sidebarMenu(
        id = "sidebar",
        menuItem("Overview",         tabName = "overview", icon = icon("fas fa-home")),
        menuItem("Blood Pressure",   tabName = "bp",       icon = icon("fas fa-chart-line")),
        menuItem("Growth Standards", tabName = "gs",       icon = icon("fas fa-chart-line")),
        menuItem("Batch Processing", tabName = "batch",    icon = icon("fas fa-file"))
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "overview",
          fluidRow(
            column(
              width = 6,
              includeMarkdown("overview.md")
            ),
            column(
              width = 6,
              p("Flowchart for selection of data set to build blood pressure percentiles one."),
              plotOutput("flowchart"),
              br(),
              h3("Cite this work:"),
              h4("Research Letter"),
              verbatimTextOutput("citation_research_letter"),
              h4("pedbp Package"),
              verbatimTextOutput("citation_package")
            )
          )
        ),
        tabItem(tabName = "bp",
          h1("Pediatric Blood Pressure"),
          box(
            title = "Inputs",
            solidHeader = TRUE,
            status = "primary",
            width = 3,
            box(
              title = "Source",
              width = 12,
              collapsible = TRUE,
              collapsed = FALSE,
              selectInput(
                inputId = "bp_source",
                label = NULL, #"Source/Method",
                choices = c("martin2022", "gemelli1990", "lo2013", "nhlbi", "flynn2017"),
                selected = "martin2022",
                multiple = FALSE
              )
            ),

            box(
              title = "Age",
              solidHeader = FALSE,
              width = 12,
              collapsible = TRUE,
              collapsed = FALSE,
              selectInput(
                inputId = "bp_age_units",
                label = NULL, #"Age",
                choices = c("months", "years"),
                selected = "months",
                multiple = FALSE
              ),
              conditionalPanel(
                condition = "input.bp_age_units == 'months'",
                sliderInput(
                  inputId = "bp_age_months",
                  label = NULL, #"Age (months)",
                  min = 1,
                  max = 18*12,
                  value = 8 * 12,
                  step = 0.5
                )
              ),
              conditionalPanel(
                condition = "input.bp_age_units == 'years'",
                sliderInput(
                  inputId = "bp_age_years",
                  label = "Age (years)",
                  min = 0,
                  max = 18,
                  value = 8,
                  step = 0.1
                )
              )
            ),

            box(
              title = "Sex",
              width = 12,
              collapsible = TRUE,
              collapsed = FALSE,
              selectInput(
                inputId = "bp_sex",
                label = NULL, #"Sex",
                choices = c("Male", "Female"),
                selected = "Male",
                multiple = FALSE
              )
            ),

            box(
              title = "Height",
              width = 12,
              collapsible = TRUE,
              collapsed = FALSE,
              selectInput(
                inputId = "bp_height_status",
                label = NULL, #"Height",
                choices = c("Unknown", "Known (cm)", "Known (inches)", "Known (percentile)"),
                selected = "Unknown",
                multiple = FALSE
              ),
              conditionalPanel(
                condition = "input.bp_height_status == 'Known (cm)'",
                sliderInput(
                  inputId = "bp_height_cm",
                  label = NULL, #"Height (cm)",
                  min = 10,
                  max = 225,
                  value = 100,
                  step = 1
                )
              ),
              conditionalPanel(
                condition = "input.bp_height_status == 'Known (inches)'",
                sliderInput(
                  inputId = "bp_height_inch",
                  label = NULL, #"Height (inches)",
                  min = 6,
                  max = 100,
                  value = 48,
                  step = 1
                )
              ),
              conditionalPanel(
                condition = "input.bp_height_status == 'Known (percentile)'",
                sliderInput(
                  inputId = "bp_height_percentile",
                  label = NULL, #"Height (percentile)",
                  min = 0,
                  max = 100,
                  value = 50,
                  step = 1
                )
              )
            ),

            box(
              title = "Systolic Blood Pressure",
              width = 12,
              collapsible = TRUE,
              collapsed = FALSE,
              selectInput(
                inputId = "bp_sbp_status",
                label = NULL, #"Systolic Blood Pressure",
                choices = c("mmHg", "percentile"),
                selected = "mmHg",
                multiple = FALSE
              ),
              conditionalPanel(
                condition = "input.bp_sbp_status == 'mmHg'",
                sliderInput(
                  inputId = "bp_sbp_mmHg",
                  label = NULL, #"SBP (mmHg)",
                  min = 60,
                  max = 160,
                  value = 100,
                  step = 1
                )
              ),
              conditionalPanel(
                condition = "input.bp_sbp_status == 'percentile'",
                sliderInput(
                  inputId = "bp_sbp_percentile",
                  label = NULL, #"SBP (percentile)",
                  min = 0,
                  max = 100,
                  value = 50,
                  step = 1
                )
              )
            ),

            box(
              title = "Diastolic Blood Pressure",
              width = 12,
              collapsible = TRUE,
              collapsed = FALSE,
              selectInput(
                inputId = "bp_dbp_status",
                label = NULL, #"Diastolic Blood Pressure",
                choices = c("mmHg", "percentile"),
                selected = "mmHg",
                multiple = FALSE
              ),
              conditionalPanel(
                condition = "input.bp_dbp_status == 'mmHg'",
                sliderInput(
                  inputId = "bp_dbp_mmHg",
                  label = NULL, #"DBP (mmHg)",
                  min = 20,
                  max = 90,
                  value = 60,
                  step = 1
                )
              ),
              conditionalPanel(
                condition = "input.bp_dbp_status == 'percentile'",
                sliderInput(
                  inputId = "bp_dbp_percentile",
                  label = NULL, #"DBP (percentile)",
                  min = 0,
                  max = 100,
                  value = 50,
                  step = 1
                )
              )
            )
          ),
          box(
            title = "Output",
            width = 9,
            solidHeader = TRUE,
            status = "info",
            box(title = "Blood Pressure by Age with Percentiles", width = 6, plotOutput("bp_chart")),
            box(title = "CDF", width = 6, plotOutput("bp_cdf")),
            box(title = "Distribution Parameters", width = 9, tableOutput(outputId = "bp_params")),
            box(title = NULL, width = 3, tableOutput(outputId = "bp_mmHg_percentile"))
          )
        ),
        tabItem(tabName = "gs",
          h1("Pediatric Growth Standards"),
          box(
            title = "Inputs",
            width = 3,
            solidHeader = TRUE,
            status = "primary",
            box(
              title = "Growth Standard",
              width = 12,
              collapsible = TRUE,
              collapsed = FALSE,
              selectInput(
                inputId = "gs_standard",
                label = NULL,
                choices = c("BMI for Age", "Head Circumference for Age", "Stature for Age", "Weight for Age", "Weight for Stature"),
                selected = "BMI for Age",
                multiple = FALSE
              )
            ),
            box(
              title = "Source",
              width = 12,
              collapsible = TRUE,
              collapsed = FALSE,
              selectInput(
                inputId = "gs_source",
                label = NULL,
                choices = c("CDC", "WHO"),
                selected = "CDC",
                multiple = FALSE
              )
            ),
            box(
              title = "Sex",
              width = 12,
              collapsible = TRUE,
              collapsed = FALSE,
              selectInput(
                inputId = "gs_sex",
                label = NULL, #"Sex",
                choices = c("Male", "Female"),
                selected = "Male",
                multiple = FALSE
              )
            ),
            conditionalPanel(
              condition = "input.gs_standard == 'BMI for Age' || input.gs_standard == 'Head Circumference for Age' || input.gs_standard == 'Stature for Age' || input.gs_standard == 'Weight for Age'",
              gs_age_box
            ),
            conditionalPanel(
              condition = "input.gs_standard == 'BMI for Age'",
              gs_bmi_box
            ),
            conditionalPanel(
              condition = "input.gs_standard == 'Head Circumference for Age'",
              gs_head_circ_box
            ),
            conditionalPanel(
              condition = "input.gs_standard == 'Stature for Age' || input.gs_standard == 'Weight for Stature'",
              gs_stature_box
            ),
            conditionalPanel(
              condition = "input.gs_standard == 'Weight for Age' || input.gs_standard == 'Weight for Stature'",
              gs_weight_box
            )
          ),
          box(
            title = "Output",
            width = 9,
            solidHeader = TRUE,
            status = "info",
            box(title = "Chart", width = 6, plotOutput("gs_chart_plot")),
            box(title = "CDF", width = 6, plotOutput("gs_cdf_plot")),
            tableOutput("gs_cdf_table")
          )
        ),
        tabItem(tabName = "batch"
            , h2("Batch Processing")
            #, fluidRow(column(width = 12, "You may use this page to get the blood pressure percentiles for several patients at one time. The expected format the data in the uploaded file is as follows:"))
            #, fluidRow(column(width = 5, plotOutput("csv_for_batch", height = "200px")))
            #, fluidRow(column(width = 12, "That is, each row represents a patient, a column for age in months, sex indicated as a 0 = female, 1 = male, height in centimeters; leave empty cell for unknown height, systolic blood pressure in mmHg, and diastolic blood pressure in mmHg. The actual column names are not important.  The order of the columns is critically important as the batch process code assumes the shown sequence of columns."))
            #, fluidRow(column(width = 12, "You may upload a file here and you'll get an output table to explore in this app along with the option to download a csv file with the blood pressure percentiles."))
            , box(
                title = "Inputs",
                width = 3,
                solidHeader = TRUE,
                status = "primary",
                fileInput(inputId = "bpfile"
                          , "Choose a File"
                          , multiple = FALSE
                          , accept   = c("text/plain")
                ),
                box(
                  title = "What to calculate?",
                  width = 12,
                  collapsible = TRUE,
                  selectInput(
                    inputId = "batch_method1",
                    label = "Blood Presure or Growth Standard",
                    choices = c("Blood Pressure", "BMI for Age", "Head Circumference for Age", "Length for Age", "Height for Age", "Weight for Age", "Weight for Length", "Weight for Height"),
                    selected = "Blood Pressure",
                    multiple = FALSE
                  ),
                  selectInput(
                    inputId = "batch_method2",
                    label = "Calculate",
                    choices = c("Distribution", "Quantiles", "Z-scores"),
                    selected = "Distribution",
                    multiple = FALSE
                  )
                ),
                box(
                  title = list("Inputs",
                  bsButton(inputId = "batch_inputs_info", label = NULL, icon = icon("info"))
                  ),
                  width = 12,
                  collapsible = TRUE,
                  bsPopover(id = "batch_inputs_info", title = "Info", content = "A guess has been made for the columns of the input data for each needed input.  Select the correct column name if needed.  Select '_ignore_' to use NA as the default value.  Select '_Default: ..._' to select data sources, etc, if not specified in the input data set."),
                  uiOutput(outputId = "batch_name_mapping")
                )
              )
            , box(
                title = "Output",
                width = 9,
                solidHeader = TRUE,
                status = "info",
                DT::dataTableOutput("batch_results")
              )
            , uiOutput("download_button")
        )
      )
    )
  )
