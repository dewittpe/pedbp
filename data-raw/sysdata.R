################################################################################
##           create a LMS data set for internal use in the package            ##

################################################################################
##                                  WHO Data                                  ##
who_lms_data <- list.files("./data-raw/who", pattern = ".*.xlsx", full.names = TRUE)
who_lms_data <- setNames(lapply(who_lms_data, readxl::read_excel), basename(who_lms_data))
who_lms_data <- lapply(who_lms_data, data.table::as.data.table)

# set common names
for (i in 1:length(who_lms_data)) {
  if ("Age" %in% names(who_lms_data[[i]])) {
    data.table::setnames(who_lms_data[[i]], old = "Age", new = "age")
  }
  if ("Day" %in% names(who_lms_data[[i]])) {
    data.table::setnames(who_lms_data[[i]], old = "Day", new = "age")
  }
  if ("Height" %in% names(who_lms_data[[i]])) {
    data.table::setnames(who_lms_data[[i]], old = "Height", new = "height")
  }
  if ("Length" %in% names(who_lms_data[[i]])) {
    data.table::setnames(who_lms_data[[i]], old = "Length", new = "length")
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
who_lms_data[, metric := data.table::tstrsplit(file, split = "-", keep = 1L)]

who_lms_data[, metric := data.table::fcase(metric == "bfa", "bmi_for_age",
                                           metric == "hfa", "height_for_age",
                                           metric == "lhfa", "length_for_age",
                                           metric == "wfa", "weight_for_age",
                                           metric == "wfh", "weight_for_height",
                                           metric == "wfl", "weight_for_length",
                                           metric == "hcfa", "head_circumference_for_age"
                                           )]

print(who_lms_data[, .N, by = .(metric, file)], n = Inf)

who_lms_data[, file := NULL]

# because percentiles a zscores where provided in seperate files the metric,
# age, stature, L, M, S values are duplicated as there are columns for the
# percentiles and for the zscores.  Melt the data and recast it so that the
# percentiles and zscores for a set of the id.vars will be on one row instead of
# two.
who_lms_data <-
  data.table::melt(
    data = who_lms_data
  , id.vars = c("metric", "male", "age", "height", "length", "L", "M", "S")
  , na.rm = TRUE
  )

who_lms_data <- unique(who_lms_data)

who_lms_data <-
  data.table::dcast(
    data = who_lms_data
  , formula = metric + male + age + height + length + L + M + S ~ variable
  , value.var = "value"
  )

# add a source column
who_lms_data[, source := "WHO"]

# verify that the metric, male, age/stature are unique
stopifnot( who_lms_data[, .N, by = .(metric, male, age, height, length)][, N == 1L])



################################################################################
##                                  CDC Data                                  ##

cdc_lms_data <- list(
    "weight_for_age_inf"         = "./data-raw/cdc2000/wtageinf.csv"
  , "length_for_age_inf"         = "./data-raw/cdc2000/lenageinf.csv"
  , "weight_for_length_inf"      = "./data-raw/cdc2000/wtleninf.csv"
  , "head_circumference_for_age" = "./data-raw/cdc2000/hcageinf.csv"
  , "weight_for_height"          = "./data-raw/cdc2000/wtstat.csv"
  , "height_for_age"             = "./data-raw/cdc2000/statage.csv"
  , "weight_for_age"             = "./data-raw/cdc2000/wtage.csv"
  , "bmi_for_age"                = "./data-raw/cdc2000/bmiagerev.csv"
) |>
lapply(data.table::fread) |>
data.table::rbindlist(idcol = "metric", use.names = TRUE, fill = TRUE)

data.table::setnames(cdc_lms_data, old = c("Sex", "Agemos", "Height", "Length"), new = c("male", "age", "height", "length"))
cdc_lms_data[, male := as.integer(male == 1)]

# clean up metric
# note from https://www.cdc.gov/growthcharts/growthchart_faq.htm
#
# > What growth charts are appropriate to use with exclusively breastfed babies?
#
# > In the United States, the WHO growth standard charts are recommended to use
# > with both breastfed and formula fed infants and children from birth to 2 years
# > of age (CDC, 2010). The WHO growth charts reflect growth patterns among
# > children who were predominantly breastfed for at least 4 months and still
# > breastfeeding at 12 months. These charts describe the growth of healthy
# > children living in well-supported environments in sites in six countries
# > throughout the world including the United States. The WHO growth charts show
# > how infants and children should grow rather than simply how they do grow in a
# > certain time and place and are therefore recommended for all infants (Dewey,
# > 2004; WHO Multicentre Growth Reference Study Group, 2006).
#
# > The WHO growth charts establish the growth of the breastfed infant as the norm
# > for growth. Healthy breastfed infants typically put on weight more slowly than
# > formula fed infants in the first year of life (Dewey, 1998). Formula fed
# > infants gain weight more rapidly after about 3 months of age. Differences in
# > weight patterns continue even after complementary foods are introduced (Dewey,
# > 1998).


# there are overlapping ages in the *_inf and non *_inf data sets.  omit ages 24
# months and over from the _inf sets
omit <- cdc_lms_data[, which(metric == "weight_for_age_inf" & age >= 24)]
cdc_lms_data <- cdc_lms_data[!omit]

cdc_lms_data[, metric := sub("_inf", "", metric)]

# verify that the metric, male, age/stature are unique
stopifnot(
          cdc_lms_data[, .N, by = .(metric, male, age, height, length)][, N == 1L]
)

cdc_lms_data[, .N, by = .(metric, male, age, height, length)]

# omit unwanted columns
for (j in grep("Pub|Diff", names(cdc_lms_data), value = TRUE)) {
  data.table::set(cdc_lms_data, j = j, value = NULL)
}

# some of the files have the header coppied in a row in the middle of the file.
# Omit that row and coersce characters to numberic values
cdc_lms_data <- cdc_lms_data[L != "L"]

for (j in c("age", "L", "M", "S", grep("P\\d", names(cdc_lms_data), value = TRUE))) {
  data.table::set(cdc_lms_data, j = j, value = as.numeric(cdc_lms_data[[j]]))
}

cdc_lms_data[, source := "CDC"]

################################################################################
##                       Put it all together and export                       ##

lms_data <- rbind(who_lms_data, cdc_lms_data, use.names = TRUE, fill = TRUE)
data.table::setkey(lms_data, source, metric, male, age, height, length)

lms_data[, .N, keyby = .(metric, source)] |> print(n = Inf)

lms_data <- as.data.frame(lms_data)


lms_data <-
  lms_data |>
  split(f = lms_data$metric) |>
  lapply(function(x) { split(x, f = x$source) }) |>
  lapply(lapply, function(x) {setNames(split(x, f = x$male), c("Female", "Male"))}) |>
  lapply(lapply, lapply, Filter, f = function(x) !all(is.na(x)))
# |> lapply(lapply, lapply, Filter, f = function(x) length(unique(x)) > 1L)

lms_data[["stature_for_age"]][["CDC"]][["Female"]][["age"]] |> duplicated()

str(lms_data, max.level = 0)
str(lms_data, max.level = 1)
str(lms_data, max.level = 2)
str(lms_data, max.level = 3)


################################################################################
##                            Export to .cpp files                            ##

cat("// Generated by data-raw/sysdata.R",
    "// Do not edit by hand",
    "// [[Rcpp::depends(RcppArmadillo)]]",
    "#include <RcppArmadillo.h>",
    "#include <Rcpp.h>",
    "#ifndef pedbp_lms_data_H",
    "#define pedbp_lms_data_H",
    sep = "\n",
    file = "src/lms_data.h",
    append = FALSE)

cat("// Generated by data-raw/sysdata.R",
    "// Do not edit by hand",
    "// [[Rcpp::depends(RcppArmadillo)]]",
    "#include <RcppArmadillo.h>",
    "#include <Rcpp.h>",
    "#include \"lms_data.h\"",
    sep = "\n",
    file = "src/lms_data.cpp",
    append = FALSE)

for (metric in names(lms_data)) {
  for (src in names(lms_data[[metric]])) {
    for (gndr in names(lms_data[[metric]][[src]])) {
      nm <- tolower(paste0(metric, "_", src, "_", gndr))
      d <- lms_data[[metric]][[src]][[gndr]]
      if (grepl("_for_age", metric)) {
        d <- d[, c("age", "L", "M", "S")]
      } else if (grepl("_for_length", metric)) {
        d <- d[, c("length", "L", "M", "S")]
      } else if (grepl("_for_height", metric)) {
        d <- d[, c("height", "L", "M", "S")]
      } else {
        stop("Unexpected _for_*** in metric")
      }
      d <- as.matrix(d)
      d <- apply(d, MARGIN = 1, function(x) paste("{", paste(x, collapse = ", "), "}"))
      d <- paste(d, collapse = ", \n")
      cat("",
          paste0("arma::mat ", nm , "() {"),
          "\tarma::mat LMS = {",
          d,
          "\t};",
          "\treturn LMS;",
          "}",
          sep = "\n",
          file = "src/lms_data.cpp",
          append = TRUE
          )
      cat(paste0("arma::mat ", nm , "();\n"), file = "src/lms_data.h", append = TRUE)
    }
  }
}

cat("#endif", sep = "\n", file = "src/lms_data.h", append = TRUE)

################################################################################
##                             Save Internal Data                             ##

usethis::use_data(lms_data, internal = TRUE, overwrite = TRUE)

################################################################################
##                                End of File                                 ##
################################################################################
