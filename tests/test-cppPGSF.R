# Testing the cppBP function
library(pedbp)
library(data.table)

################################################################################
# Verify error if any input is zero length
x <-
  tryCatch(
      pedbp:::cppPGSF(
          metric = character(0)
        , source = "CDC"
        , male   = 0
        , x      = 48
        , qp     = 15
        , type   = "quantile"
      )
    , error = function(e) e
  )
stopifnot(inherits(x, "error"))
stopifnot(identical(x$message, "zero length vector"))

x <-
  tryCatch(
      pedbp:::cppPGSF(
          metric = "bmi_for_age"
        , source = character(0)
        , male   = 0
        , x      = 48
        , qp     = 15
        , type   = "quantile"
      )
    , error = function(e) e
  )
stopifnot(inherits(x, "error"))
stopifnot(identical(x$message, "zero length vector"))

x <-
  tryCatch(
      pedbp:::cppPGSF(
          metric = "bmi_for_age"
        , source = "CDC"
        , male   = numeric(0)
        , x      = 48
        , qp     = 15
        , type   = "quantile"
      )
    , error = function(e) e
  )
stopifnot(inherits(x, "error"))
stopifnot(identical(x$message, "zero length vector"))

x <-
  tryCatch(
      pedbp:::cppPGSF(
          metric = 'bmi_for_age'
        , source = "CDC"
        , male   = 0
        , x      = numeric(0)
        , qp     = 15
        , type   = "quantile"
      )
    , error = function(e) e
  )
stopifnot(inherits(x, "error"))
stopifnot(identical(x$message, "zero length vector"))

x <-
  tryCatch(
      pedbp:::cppPGSF(
          metric = 'bmi_for_age'
        , source = "CDC"
        , male   = 0
        , x      = 48
        , qp     = numeric(0)
        , type   = "quantile"
      )
    , error = function(e) e
  )
stopifnot(inherits(x, "error"))
stopifnot(identical(x$message, "zero length vector"))

x <-
  tryCatch(
      pedbp:::cppPGSF(
          metric = 'bmi_for_age'
        , source = "CDC"
        , male   = 0
        , x      = 48
        , qp     = 15
        , type   = numeric(0)
      )
    , error = function(e) e
  )
stopifnot(inherits(x, "error"))
stopifnot(identical(x$message, "zero length vector"))

################################################################################
# If any length is > 1 but < max length, then error
x <-
  tryCatch(
      pedbp:::cppPGSF(
          metric = character(1)
        , source = character(1)
        , male   = integer(1)
        , x      = numeric(1)
        , qp     = numeric(4)
        , type   = numeric(5)
      )
    , error = function(e) e
  )
stopifnot(inherits(x, "error"))
stopifnot(identical(x$message, "all input vectors need to be of equal length, or length 1."))

x <-
  tryCatch(
      pedbp:::cppPGSF(
          metric = character(1)
        , source = character(1)
        , male   = integer(1)
        , x      = numeric(2)
        , qp     = numeric(5)
        , type   = numeric(5)
      )
    , error = function(e) e
  )
stopifnot(inherits(x, "error"))
stopifnot(identical(x$message, "all input vectors need to be of equal length, or length 1."))

x <-
  tryCatch(
      pedbp:::cppPGSF(
          metric = character(1)
        , source = character(1)
        , male   = integer(2)
        , x      = numeric(5)
        , qp     = numeric(5)
        , type   = numeric(5)
      )
    , error = function(e) e
  )
stopifnot(inherits(x, "error"))
stopifnot(identical(x$message, "all input vectors need to be of equal length, or length 1."))


x <-
  tryCatch(
      pedbp:::cppPGSF(
          metric = character(1)
        , source = character(2)
        , male   = integer(5)
        , x      = numeric(5)
        , qp     = numeric(5)
        , type   = numeric(5)
      )
    , error = function(e) e
  )
stopifnot(inherits(x, "error"))
stopifnot(identical(x$message, "all input vectors need to be of equal length, or length 1."))


x <-
  tryCatch(
      pedbp:::cppPGSF(
          metric = character(2)
        , source = character(5)
        , male   = integer(5)
        , x      = numeric(5)
        , qp     = numeric(5)
        , type   = numeric(5)
      )
    , error = function(e) e
  )
stopifnot(inherits(x, "error"))
stopifnot(identical(x$message, "all input vectors need to be of equal length, or length 1."))

################################################################################
# verify error if male is not 0 or 1
x <-
  tryCatch(
      pedbp:::cppPGSF(
          metric = character(1)
        , source = character(1)
        , male   = 2
        , x      = numeric(1)
        , qp     = numeric(1)
        , type   = numeric(1)
      )
    , error = function(e) e
  )
stopifnot(inherits(x, "error"))
stopifnot(identical(x$message, "male needs to be a 0 or 1"))

################################################################################
# verify error for unknown source
x <-
  tryCatch(
      pedbp:::cppPGSF(
          metric = "bmi_for_age"
        , source = "not-a-source"
        , male   = 0
        , x      = numeric(1)
        , qp     = numeric(1)
        , type   = numeric(1)
      )
    , error = function(e) e
  )
stopifnot(inherits(x, "error"))
stopifnot(identical(x$message, "Unknown source for bmi_for_age data"))

x <-
  tryCatch(
      pedbp:::cppPGSF(
          metric = "head_circumference_for_age"
        , source = "not-a-source"
        , male   = 0
        , x      = numeric(1)
        , qp     = numeric(1)
        , type   = numeric(1)
      )
    , error = function(e) e
  )
stopifnot(inherits(x, "error"))
stopifnot(identical(x$message, "Unknown source for head_circumference_for_age data"))

x <-
  tryCatch(
      pedbp:::cppPGSF(
          metric = "height_for_age"
        , source = "not-a-source"
        , male   = 0
        , x      = numeric(1)
        , qp     = numeric(1)
        , type   = numeric(1)
      )
    , error = function(e) e
  )
stopifnot(inherits(x, "error"))
stopifnot(identical(x$message, "Unknown source for height_for_age data"))

x <-
  tryCatch(
      pedbp:::cppPGSF(
          metric = "weight_for_age"
        , source = "not-a-source"
        , male   = 0
        , x      = numeric(1)
        , qp     = numeric(1)
        , type   = numeric(1)
      )
    , error = function(e) e
  )
stopifnot(inherits(x, "error"))
stopifnot(identical(x$message, "Unknown source for weight_for_age data"))

x <-
  tryCatch(
      pedbp:::cppPGSF(
          metric = "weight_for_height"
        , source = "not-a-source"
        , male   = 0
        , x      = numeric(1)
        , qp     = numeric(1)
        , type   = numeric(1)
      )
    , error = function(e) e
  )
stopifnot(inherits(x, "error"))
stopifnot(identical(x$message, "Unknown source for weight_for_height data"))

x <-
  tryCatch(
      pedbp:::cppPGSF(
          metric = "weight_for_length"
        , source = "not-a-source"
        , male   = 0
        , x      = numeric(1)
        , qp     = numeric(1)
        , type   = numeric(1)
      )
    , error = function(e) e
  )
stopifnot(inherits(x, "error"))
stopifnot(identical(x$message, "Unknown source for weight_for_length data"))

x <-
  tryCatch(
      pedbp:::cppPGSF(
          metric = "length_for_age"
        , source = "not-a-source"
        , male   = 0
        , x      = numeric(1)
        , qp     = numeric(1)
        , type   = numeric(1)
      )
    , error = function(e) e
  )
stopifnot(inherits(x, "error"))
stopifnot(identical(x$message, "Unknown source for length_for_age data"))

################################################################################
# verify error if unknown type
x <-
  tryCatch(
      pedbp:::cppPGSF(
          metric = "bmi_for_age"
        , source = "CDC"
        , male   = 0
        , x      = 48
        , qp     = 15
        , type   = "Not-a-type"
      )
    , error = function(e) e
  )
stopifnot(inherits(x, "error"))
stopifnot(identical(x$message, "type needs to be one of 'quantile', 'distribution', or 'zscore'"))

################################################################################
# verify warning if age is out of bounds
x <-
  tryCatch(
      pedbp:::cppPGSF(
          metric = "bmi_for_age"
        , source = "CDC"
        , male   = 0
        , x      = -1
        , qp     = 15
        , type   = "quantile"
      )
    , warning = function(w) w
  )
stopifnot(inherits(x, "warning"))
stopifnot(identical(x$message, "age/stature below lower limit"))

# verify warning if age is out of bounds
x <-
  tryCatch(
      pedbp:::cppPGSF(
          metric = "bmi_for_age"
        , source = "CDC"
        , male   = 0
        , x      = 1000
        , qp     = 15
        , type   = "quantile"
      )
    , warning = function(w) w
  )
stopifnot(inherits(x, "warning"))
stopifnot(identical(x$message, "age/stature above upper limit"))

################################################################################
# check output against published values
internal_lms_data <-
  pedbp:::lms_data |>
  lapply(lapply, data.table::rbindlist, use.names = TRUE, fill = TRUE) |>
  lapply(data.table::rbindlist, use.names = TRUE, fill = TRUE) |>
  data.table::rbindlist(use.names = TRUE, fill = TRUE)

internal_lms_data <-
  data.table::melt(
    internal_lms_data
  , id.vars = c("source", "metric", "male", "age", "height", "length", "L", "M", "S")
  , measure.vars = patterns("P")
  , variable.factor = FALSE
  , variable.name = "published_percentile"
  , value.name = "published_quantile"
  )

internal_lms_data[published_percentile == "P01",  published_percentile := 0.001]
internal_lms_data[published_percentile == "P1",   published_percentile := 0.010]
internal_lms_data[published_percentile == "P3",   published_percentile := 0.030]
internal_lms_data[published_percentile == "P5",   published_percentile := 0.050]
internal_lms_data[published_percentile == "P10",  published_percentile := 0.100]
internal_lms_data[published_percentile == "P15",  published_percentile := 0.150]
internal_lms_data[published_percentile == "P25",  published_percentile := 0.250]
internal_lms_data[published_percentile == "P50",  published_percentile := 0.500]
internal_lms_data[published_percentile == "P75",  published_percentile := 0.750]
internal_lms_data[published_percentile == "P85",  published_percentile := 0.850]
internal_lms_data[published_percentile == "P90",  published_percentile := 0.900]
internal_lms_data[published_percentile == "P95",  published_percentile := 0.950]
internal_lms_data[published_percentile == "P97",  published_percentile := 0.970]
internal_lms_data[published_percentile == "P99",  published_percentile := 0.990]
internal_lms_data[published_percentile == "P999", published_percentile := 0.999]
internal_lms_data[, published_percentile := as.numeric(published_percentile)]

internal_lms_data <- internal_lms_data[!is.na(published_quantile)]

internal_lms_data[, x := fcase(!is.na(age), age, !is.na(length), length, !is.na(height), height)]

internal_lms_data[,
    `:=`(
          test_quantile   = pedbp:::cppPGSF(metric = metric, source = source, male = male, x = x, qp = published_percentile, type = "quantile"),
          test_percentile = pedbp:::cppPGSF(metric = metric, source = source, male = male, x = x, qp = published_quantile,   type = "distribution"),
          test_zscore     = pedbp:::cppPGSF(metric = metric, source = source, male = male, x = x, qp = published_quantile,   type = "zscore")
    )
  ]

stopifnot(isTRUE(internal_lms_data[, max(abs(test_quantile - published_quantile)) <= 5.1e-04]))
stopifnot(isTRUE(internal_lms_data[, max(abs(test_percentile - published_percentile)) <= 0.001]))
stopifnot(isTRUE(internal_lms_data[, max(abs(test_percentile - published_percentile)) <= 0.001]))
stopifnot(isTRUE(all.equal(pnorm(internal_lms_data$test_zscore), internal_lms_data$test_percentile)))

################################################################################
# verify that the inputs will be extended as needed

stopifnot(
  internal_lms_data[metric == "bmi_for_age" & source == "CDC" & male == 0,
                    isTRUE(all.equal(
                    test_percentile,
                    pedbp:::cppPGSF(metric = "bmi_for_age", source = "CDC", male = 0, x = x, qp = published_quantile, type = "distribution")
                    ))]
  )

stopifnot(
  isTRUE(
    all.equal(
      pedbp:::cppPGSF(metric = "bmi_for_age", source = c("CDC", "WHO"), male = 0, x = 56, qp = 15, type = "distribution"),
      c(0.4390000, 0.4294214),
      tol = 1e-7
    )
  )
)


################################################################################
##                                End of file                                 ##
################################################################################
