################################################################################
##           create a LMS data set for internal use in the package            ##

################################################################################
##                                  WHO Data                                  ##
who_lms_data <- list.files("./data-raw/who", pattern = ".*.xlsx", full.names = TRUE)
who_lms_data <- setNames(lapply(who_lms_data, readxl::read_excel), basename(who_lms_data))
who_lms_data <- lapply(who_lms_data, data.table::as.data.table)

who_downloads <- scan("./data-raw/who/download.sh", sep = "\n", what = character())

lapply(who_lms_data, names)

# set common names
for (i in 1:length(who_lms_data)) {
  if ("Age" %in% names(who_lms_data[[i]])) {
    data.table::setnames(who_lms_data[[i]], old = "Age", new = "age")
  }
  if ("Day" %in% names(who_lms_data[[i]])) {
    data.table::setnames(who_lms_data[[i]], old = "Day", new = "age")
  }
  if ("Height" %in% names(who_lms_data[[i]])) {
    data.table::setnames(who_lms_data[[i]], old = "Height", new = "stature")
  }
  if ("Length" %in% names(who_lms_data[[i]])) {
    data.table::setnames(who_lms_data[[i]], old = "Length", new = "stature")
  }
}

who_lms_data <- data.table::rbindlist(who_lms_data, idcol = "file", use.names = TRUE, fill = TRUE)

# WHO report age in days, convert to months
# One month = 365.25 / 12 = 30.4375 days
# conversion is also given in the instructions from the WHO on using there data
who_lms_data[, age := age / ( 365.25 / 12) ]
stopifnot(identical(who_lms_data[!is.na(age) & !is.na(Month), .N], 0L))
who_lms_data[is.na(age) & !is.na(Month), age := Month]
who_lms_data[, Month := NULL]

who_lms_data <- unique(who_lms_data)

# add a column for denoting male = 1, female = 0
who_lms_data[ , male := as.integer(grepl("boy", file))]

# define the "metric"
metric_by_file <-
  list('weight_for_age' =
       c(  "wfa-girls-zscore-expanded-tables.xlsx?sfvrsn=f01bc813_10"
         , "wfa-girls-percentiles-expanded-tables.xlsx?sfvrsn=54cfa5e8_9"
         , "wfa-boys-zscore-expanded-tables.xlsx?sfvrsn=65cce121_10"
         , "wfa-boys-percentiles-expanded-tables.xlsx?sfvrsn=c2f79259_11"
         # weight for age 5-19 years (file name is odd, but that is what the
         # link gave)
         , "hfa-girls-z-who-2007-exp_7ea58763-36a2-436d-bef0-7fcfbadd2820.xlsx?sfvrsn=6ede55a4_4"
         , "hfa-boys-z-who-2007-exp_0ff9c43c-8cc0-4c23-9fc6-81290675e08b.xlsx?sfvrsn=b3ca0d6f_4"
         , "hfa-girls-perc-who2007-exp_6040a43e-81da-48fa-a2d4-5c856fe4fe71.xlsx?sfvrsn=5c5825c4_4"
         , "hfa-boys-perc-who2007-exp_07eb5053-9a09-4910-aa6b-c7fb28012ce6.xlsx?sfvrsn=97ab852c_4"
       ),
       'stature_for_age' =
       c(  "lhfa-girls-zscore-expanded-tables.xlsx?sfvrsn=27f1e2cb_10"
         , "lhfa-girls-percentiles-expanded-tables.xlsx?sfvrsn=478569a5_9"
         , "lhfa-boys-zscore-expanded-tables.xlsx?sfvrsn=7b4a3428_12"
         , "lhfa-boys-percentiles-expanded-tables.xlsx?sfvrsn=bc36d818_9"
         # height for age 5-19 years
         , "hfa-girls-z-who-2007-exp.xlsx?sfvrsn=79d310ee_2"
         , "hfa-boys-z-who-2007-exp.xlsx?sfvrsn=7fa263d_2"
         , "hfa-girls-perc-who2007-exp.xlsx?sfvrsn=7a910e5d_2"
         , "hfa-boys-perc-who2007-exp.xlsx?sfvrsn=27f20eb1_2"
       ),
       'weight_for_stature' =
       c(  "wfl-girls-zscore-expanded-table.xlsx?sfvrsn=db7b5d6b_8"
         , "wfh-girls-zscore-expanded-tables.xlsx?sfvrsn=daac732c_8"
         , "wfl-girls-percentiles-expanded-tables.xlsx?sfvrsn=e50b7713_7"
         , "wfh-girls-percentiles-expanded-tables.xlsx?sfvrsn=eb27f3ad_7"
         , "wfl-boys-zscore-expanded-table.xlsx?sfvrsn=d307434f_8"
         , "wfh-boys-zscore-expanded-tables.xlsx?sfvrsn=ac60cb13_8"
         , "wfl-boys-percentiles-expanded-tables.xlsx?sfvrsn=41c436e1_7"
         , "wfh-boys-percentiles-expanded-tables.xlsx?sfvrsn=407ceb43_7"
       ),
       'bmi_for_age' =
       c(  "bfa-girls-zscore-expanded-tables.xlsx?sfvrsn=ae4cb8d1_12"
         , "bfa-girls-percentiles-expanded-tables.xlsx?sfvrsn=e9395fe_9"
         , "bfa-boys-zscore-expanded-tables.xlsx?sfvrsn=f8e1fbe2_10"
         , "bfa-boys-percentiles-expanded-tables.xlsx?sfvrsn=aec7ec8d_9"
         # bmi for age 5-19 years
         , "bmi-girls-z-who-2007-exp.xlsx?sfvrsn=79222875_2"
         , "bmi-boys-z-who-2007-exp.xlsx?sfvrsn=a84bca93_2"
         , "bmi-girls-perc-who2007-exp.xlsx?sfvrsn=e866c0a0_2"
         , "bmi-boys-perc-who2007-exp.xlsx?sfvrsn=28412fcf_2"
       )
      )
metric_by_file <-
  metric_by_file |>
  lapply(function(x) data.table::data.table(file = x)) |>
  data.table::rbindlist(idcol = "metric")

who_lms_data <-
  merge(x = who_lms_data, y = metric_by_file, all.x = TRUE, by = "file")


who_lms_data[, .N, keyby = .(file, metric)]  |> print(n = Inf)
who_lms_data[, .N, keyby = .(file, metric)][, .N, keyby = .(metric)] |> print(n = Inf)

who_lms_data[, file := NULL]

# because percentiles a zscores where provided in seperate files the metric,
# age, stature, L, M, S values are duplicated as there are columns for the
# percentiles and for the zscores.  Melt the data and recast it so that the
# percentiles and zscores for a set of the id.vars will be on one row instead of
# two.
who_lms_data <-
  data.table::melt(
    data = who_lms_data
  , id.vars = c("metric", "male", "age", "stature", "L", "M", "S")
  , na.rm = TRUE
  )

who_lms_data <- unique(who_lms_data)

who_lms_data <-
  data.table::dcast(
    data = who_lms_data
  , formula = metric + male + age + stature + L + M + S ~ variable
  , value.var = "value"
  )

# add a source column
who_lms_data[, source := "WHO"]

################################################################################
##                                  CDC Data                                  ##

cdc_lms_data <- list(
    "weight_for_age_inf"         = "./data-raw/cdc2000/wtageinf.csv"
  , "length_for_age_inf"         = "./data-raw/cdc2000/lenageinf.csv"
  , "weight_for_length_inf"      = "./data-raw/cdc2000/wtleninf.csv"
  , "head_circumference_for_age" = "./data-raw/cdc2000/hcageinf.csv"
  , "weight_for_height"          = "./data-raw/cdc2000/wtstat.csv"
  , "stature_for_age"            = "./data-raw/cdc2000/statage.csv"
  , "weight_for_age"             = "./data-raw/cdc2000/wtage.csv"
  , "bmi_for_age"                = "./data-raw/cdc2000/bmiagerev.csv"
) |>
lapply(data.table::fread) |>
data.table::rbindlist(idcol = "metric", use.names = TRUE, fill = TRUE)

data.table::setnames(cdc_lms_data, old = c("Sex", "Agemos"), new = c("male", "age"))
cdc_lms_data[, male := as.integer(male == 1)]

cdc_lms_data[, metric := sub("_inf", "", metric)]
cdc_lms_data[, metric := gsub("height", "stature", metric)]
cdc_lms_data[, metric := gsub("length", "stature", metric)]

cdc_lms_data[!is.na(Length), stature := Length]
cdc_lms_data[, Length := NULL]
cdc_lms_data[!is.na(Height), stature := Height]
cdc_lms_data[, Height := NULL]

cdc_lms_data[, .N, keyby = .(metric)]

cdc_lms_data[, source := "CDC"]

for (j in grep("Pub|Diff", names(cdc_lms_data), value = TRUE)) {
  data.table::set(cdc_lms_data, j = j, value = NULL)
}

# some of the files have the header coppied in a row in the middle of the file.
# Omit that row and coersce characters to numberic values
cdc_lms_data <- cdc_lms_data[L != "L"]

for (j in c("age", "L", "M", "S", grep("P\\d", names(cdc_lms_data), value = TRUE))) {
  data.table::set(cdc_lms_data, j = j, value = as.numeric(cdc_lms_data[[j]]))
}

################################################################################
##                       Put it all together and export                       ##

lms_data <- rbind(who_lms_data, cdc_lms_data, use.names = TRUE, fill = TRUE)
data.table::setkey(lms_data, source, metric, male, age, stature)
lms_data <- as.data.frame(lms_data)

lms_data <-
  lms_data |>
  split(f = lms_data$metric) |>
  lapply(function(x) { split(x, f = x$source) }) |>
  lapply(lapply, function(x) {setNames(split(x, f = x$male), c("Female", "Male"))}) |>
  lapply(lapply, lapply, Filter, f = function(x) !all(is.na(x))) |>
  I()
# lapply(lapply, lapply, Filter, f = function(x) length(unique(x)) > 1L)

str(lms_data, max.level = 0)
str(lms_data, max.level = 1)
str(lms_data, max.level = 2)
str(lms_data, max.level = 3)


################################################################################
##                            Export to .cpp files                            ##

cat("// Generated by data-raw/sysdata.R",
    "// Do not edit by hand",
    "#include <RcppArmadillo.h>",
    "#include <Rcpp.h>",
    sep = "\n",
    file = "src/lms.h",
    append = FALSE)

for (metric in names(lms_data)) {
  for (src in names(lms_data[[metric]])) {
    for (gndr in names(lms_data[[metric]][[src]])) {
      nm <- tolower(paste0(metric, "_", src, "_", gndr))
      d <- lms_data[[metric]][[src]][[gndr]]
      if (grepl("_for_age", metric)) {
        d <- d[, c("age", "L", "M", "S")]
      } else if (grepl("_for_stature", metric)) {
        d <- d[, c("stature", "L", "M", "S")]
      }
      d <- as.matrix(d)
      d <- apply(d, MARGIN = 1, function(x) paste("{", paste(x, collapse = ", "), "}"))
      d <- paste(d, collapse = ", \n")
      cat("// Generated by data-raw/sysdata.R",
          "// Do not edit by hand",
          "// [[Rcpp::depends(RcppArmadillo)]]",
          "#include <RcppArmadillo.h>",
          "#include <Rcpp.h>",
          "#include \"lms.h\"",
          paste0("arma::mat ", nm , "() {"),
          "\tarma::mat LMS = {",
          d,
          "\t};",
          "\treturn LMS;",
          "}",
          sep = "\n",
          file = paste0("src/", nm, ".cpp"),
          append = FALSE
          )
      cat(paste0("arma::mat ", nm , "();\n"), file = "src/lms.h", append = TRUE)
    }
  }
}

################################################################################
##                             Save Internal Data                             ##

usethis::use_data(lms_data, internal = TRUE, overwrite = TRUE)

################################################################################
##                                End of File                                 ##
################################################################################
