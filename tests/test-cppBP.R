# Testing the cppBP function
library(pedbp)

################################################################################
# Verify error if more than one source
x <- tryCatch(pedbp:::cppBP(0.5, 0.5, 34, 0, NA, NA, 0.5, source = c("martin2022", "gemelli1990"), type = "percentile"), error = function(e) e)
stopifnot(identical(class(x), c("simpleError", "error", "condition")))
stopifnot(identical(x$message, "'source' should have length 1"))

x <- tryCatch(pedbp:::cppBP(0.5, 0.5, 34, 0, NA, NA, 0.5, source = c("martin2022", "not-a-source"), type = "percentile"), error = function(e) e)
stopifnot(identical(class(x), c("simpleError", "error", "condition")))
stopifnot(identical(x$message, "'source' should have length 1"))

################################################################################
# Verify error if more than one type
x <- tryCatch(pedbp:::cppBP(0.5, 0.5, 34, 0, NA, NA, 0.5, source = c("martin2022"), type = c("quantile", "percentile")), error = function(e) e)
stopifnot(identical(class(x), c("simpleError", "error", "condition")))
stopifnot(identical(x$message, "'type' should have length 1"))

################################################################################
# Verify error if source is not a known source
x <- tryCatch(pedbp:::cppBP(0.5, 0.5, 34, 0, NA, NA, 0.5, source = c("not-a-source"), type = "percentile"), error = function(e) e)
stopifnot(identical(class(x), c("simpleError", "error", "condition")))
stopifnot(identical(x$message, "Unknown source"))

################################################################################
# Verify error if type is not percentile or qualtile
x <- tryCatch(pedbp:::cppBP(0.5, 0.5, 34, 0, NA, NA, 0.5, source = c("martin2022"), type = "no"), error = function(e) e)
stopifnot(identical(class(x), c("simpleError", "error", "condition")))
stopifnot(identical(x$message, "type needs to be either 'percentile' or 'quantile'"))

################################################################################
# verify error if male is not 0 or 1
x <- tryCatch(pedbp:::cppBP(0.5, 0.5, 34, 2, NA, NA, 0.5, source = c("martin2022"), type = "no"), error = function(e) e)
stopifnot(identical(class(x), c("simpleError", "error", "condition")))
stopifnot(identical(x$message, "male needs to be a 0 or 1"))

################################################################################
# verify error if length of qp_sbp and qp_dbp differ
x <- tryCatch(pedbp:::cppBP(0.5, c(0.5, 0.5), 34, 0, NA, NA, 0.5, source = c("martin2022"), type = "no"), error = function(e) e)
stopifnot(identical(class(x), c("simpleError", "error", "condition")))
stopifnot(identical(x$message, "qp_sbp and qp_dbp lengths are not equal"))

x <- tryCatch(pedbp:::cppBP(c(0.5, 0.5), 0.5, 34, 0, NA, NA, 0.5, source = c("martin2022"), type = "no"), error = function(e) e)
stopifnot(identical(class(x), c("simpleError", "error", "condition")))
stopifnot(identical(x$message, "qp_sbp and qp_dbp lengths are not equal"))

################################################################################
# verify error if any of the inputs are zero length
x <- tryCatch(pedbp:::cppBP(
                qp_sbp = numeric(0),
                qp_dbp = numeric(0),
                age = 56,
                male = 0,
                height = NA,
                height_percentile = NA,
                default_height_percentile = 0.5,
                source = c("martin2022"),
                type = "no"),
              error = function(e) e)
stopifnot(identical(class(x), c("simpleError", "error", "condition")))
stopifnot(identical(x$message, "zero length vector"))

x <- tryCatch(pedbp:::cppBP(
                qp_sbp = 0.5,
                qp_dbp = 0.5,
                age = numeric(0),
                male = 0,
                height = NA,
                height_percentile = NA,
                default_height_percentile = 0.5,
                source = c("martin2022"),
                type = "no"),
              error = function(e) e)
stopifnot(identical(class(x), c("simpleError", "error", "condition")))
stopifnot(identical(x$message, "zero length vector"))

x <- tryCatch(pedbp:::cppBP(
                qp_sbp = 0.5,
                qp_dbp = 0.5,
                age = 54,
                male = numeric(0),
                height = NA,
                height_percentile = NA,
                default_height_percentile = 0.5,
                source = c("martin2022"),
                type = "no"),
              error = function(e) e)
stopifnot(identical(class(x), c("simpleError", "error", "condition")))
stopifnot(identical(x$message, "zero length vector"))

x <- tryCatch(pedbp:::cppBP(
                qp_sbp = 0.5,
                qp_dbp = 0.5,
                age = 54,
                male = 0,
                height = numeric(0),
                height_percentile = NA,
                default_height_percentile = 0.5,
                source = c("martin2022"),
                type = "no"),
              error = function(e) e)
stopifnot(identical(class(x), c("simpleError", "error", "condition")))
stopifnot(identical(x$message, "zero length vector"))

x <- tryCatch(pedbp:::cppBP(
                qp_sbp = 0.5,
                qp_dbp = 0.5,
                age = 54,
                male = 0,
                height = 0,
                height_percentile = numeric(0),
                default_height_percentile = 0.5,
                source = c("martin2022"),
                type = "no"),
              error = function(e) e)
stopifnot(identical(class(x), c("simpleError", "error", "condition")))
stopifnot(identical(x$message, "zero length vector"))

################################################################################
# test the expansion of vectors.
x <-pedbp:::cppBP(
                qp_sbp = 0.5,
                qp_dbp = 0.5,
                age = 54,
                male = 0:1,
                height = NA,
                height_percentile = NA,
                default_height_percentile = 0.5,
                source = c("martin2022"),
                type = "quantile")
stopifnot(identical(class(x), "list"))
stopifnot(identical(length(x), 2L))
stopifnot(identical(length(x[[1]]), 2L))
stopifnot(identical(length(x[[2]]), 2L))

x <-pedbp:::cppBP(
                qp_sbp = 0.5,
                qp_dbp = 0.5,
                age = 54,
                male = 0,
                height = c(NA, NA),
                height_percentile = NA,
                default_height_percentile = 0.5,
                source = c("martin2022"),
                type = "quantile")
stopifnot(identical(class(x), "list"))
stopifnot(identical(length(x), 2L))
stopifnot(identical(length(x[[1]]), 2L))
stopifnot(identical(length(x[[2]]), 2L))

################################################################################
# verify error if inputs are not lenght 1 or equal
x <- tryCatch(pedbp:::cppBP(
                qp_sbp = numeric(2),
                qp_dbp = numeric(2),
                age = numeric(2),
                male = numeric(2),
                height = numeric(2),
                height_percentile = numeric(5),
                default_height_percentile = 0.5,
                source = c("martin2022"),
                type = "percentile"),
              error = function(e) e)
stopifnot(identical(class(x), c("simpleError", "error", "condition")))
stopifnot(identical(x$message, "all input vectors need to be of equal length, or length 1."))

x <- tryCatch(pedbp:::cppBP(
                qp_sbp = numeric(2),
                qp_dbp = numeric(2),
                age = numeric(2),
                male = numeric(2),
                height = numeric(5),
                height_percentile = numeric(2),
                default_height_percentile = 0.5,
                source = c("martin2022"),
                type = "percentile"),
              error = function(e) e)
stopifnot(identical(class(x), c("simpleError", "error", "condition")))
stopifnot(identical(x$message, "all input vectors need to be of equal length, or length 1."))

x <- tryCatch(pedbp:::cppBP(
                qp_sbp = numeric(2),
                qp_dbp = numeric(2),
                age = numeric(2),
                male = numeric(5),
                height = numeric(2),
                height_percentile = numeric(2),
                default_height_percentile = 0.5,
                source = c("martin2022"),
                type = "percentile"),
              error = function(e) e)
stopifnot(identical(class(x), c("simpleError", "error", "condition")))
stopifnot(identical(x$message, "all input vectors need to be of equal length, or length 1."))

x <- tryCatch(pedbp:::cppBP(
                qp_sbp = numeric(2),
                qp_dbp = numeric(2),
                age = numeric(5),
                male = numeric(2),
                height = numeric(2),
                height_percentile = numeric(2),
                default_height_percentile = 0.5,
                source = c("martin2022"),
                type = "percentile"),
              error = function(e) e)
stopifnot(identical(class(x), c("simpleError", "error", "condition")))
stopifnot(identical(x$message, "all input vectors need to be of equal length, or length 1."))

################################################################################
# Verify expected output for gemelli1990
x <-
  pedbp:::cppBP(
      qp_sbp = gemelli1990$sbp_mean
    , qp_dbp = gemelli1990$dbp_mean
    , age    = gemelli1990$age
    , male   = gemelli1990$male
    , height = NA
    , height_percentile = NA
    , default_height_percentile = 0.5
    , source = "gemelli1990"
    , type = "percentile"
  )
stopifnot(identical(class(x), "list"))
stopifnot(identical(length(x), 2L))
stopifnot(identical(length(x[[1]]), nrow(gemelli1990)))
stopifnot(identical(length(x[[2]]), nrow(gemelli1990)))
stopifnot(identical(class(attr(x, "bp_params")), "data.frame"))
stopifnot(isTRUE(all.equal(unname(x), list(rep(0.5, nrow(gemelli1990)), rep(0.5, nrow(gemelli1990))), check.attributes = FALSE)))

x <-
  pedbp:::cppBP(
      qp_sbp = 0.5
    , qp_dbp = 0.5
    , age    = gemelli1990$age
    , male   = gemelli1990$male
    , height = NA
    , height_percentile = NA
    , default_height_percentile = 0.5
    , source = "gemelli1990"
    , type = "quantile"
  )
stopifnot(identical(class(x), "list"))
stopifnot(identical(length(x), 2L))
stopifnot(identical(length(x[[1]]), nrow(gemelli1990)))
stopifnot(identical(length(x[[2]]), nrow(gemelli1990)))
stopifnot(identical(class(attr(x, "bp_params")), "data.frame"))
stopifnot(isTRUE(all.equal(unname(x), as.list(gemelli1990[c("sbp_mean", "dbp_mean")]), check.attributes = FALSE)))

################################################################################
# Verify expected output for lo2013
x <-
  pedbp:::cppBP(
      qp_sbp = lo2013$sbp_mean
    , qp_dbp = lo2013$dbp_mean
    , age    = lo2013$age
    , male   = lo2013$male
    , height = NA
    , height_percentile = NA
    , default_height_percentile = 0.5
    , source = "lo2013"
    , type = "percentile"
  )
stopifnot(identical(class(x), "list"))
stopifnot(identical(length(x), 2L))
stopifnot(identical(length(x[[1]]), nrow(lo2013)))
stopifnot(identical(length(x[[2]]), nrow(lo2013)))
stopifnot(identical(class(attr(x, "bp_params")), "data.frame"))
stopifnot(isTRUE(all.equal(unname(x), list(rep(0.5, nrow(lo2013)), rep(0.5, nrow(lo2013))), check.attributes = FALSE)))

x <-
  pedbp:::cppBP(
      qp_sbp = 0.5
    , qp_dbp = 0.5
    , age    = lo2013$age
    , male   = lo2013$male
    , height = NA
    , height_percentile = NA
    , default_height_percentile = 0.5
    , source = "lo2013"
    , type = "quantile"
  )
stopifnot(identical(class(x), "list"))
stopifnot(identical(length(x), 2L))
stopifnot(identical(length(x[[1]]), nrow(lo2013)))
stopifnot(identical(length(x[[2]]), nrow(lo2013)))
stopifnot(identical(class(attr(x, "bp_params")), "data.frame"))
stopifnot(isTRUE(all.equal(unname(x), as.list(lo2013[c("sbp_mean", "dbp_mean")]), check.attributes = FALSE)))

################################################################################
# verify output for nhlbi
nq <-
  pedbp:::cppBP(
     qp_sbp = nhlbi_bp_norms$bp_percentile/100,
     qp_dbp = nhlbi_bp_norms$bp_percentile/100,
     male   = nhlbi_bp_norms$male,
     age    = nhlbi_bp_norms$age,
     height = NA,
     height_percentile = nhlbi_bp_norms$height_percentile,
     default_height_percentile = 0.5,
     source = "nhlbi",
     type = "quantile"
  )

np <-
  pedbp:::cppBP(
     qp_sbp = nhlbi_bp_norms$sbp,
     qp_dbp = nhlbi_bp_norms$dbp,
     male   = nhlbi_bp_norms$male,
     age    = nhlbi_bp_norms$age,
     height = NA,
     height_percentile = nhlbi_bp_norms$height_percentile,
     default_height_percentile = 0.5,
     source = "nhlbi",
     type = "percentile"
  )

nhlbi_bp <-
  cbind(nhlbi_bp_norms,
        pedbp_sbp = nq$sbp,
        pedbp_dbp = nq$dbp,
        pedbp_sbp_percentile = np$sbp_percentile * 100,
        pedbp_dbp_percentile = np$dbp_percentile * 100
  )


# All the quantile estimates are within 2 mmHg:
stopifnot(max(abs(nhlbi_bp$pedbp_sbp - nhlbi_bp$sbp)) < 2)
stopifnot(max(abs(nhlbi_bp$pedbp_dbp - nhlbi_bp$dbp)) < 2)

# All the percentiles are within 2 percentile points:
stopifnot(max(abs(nhlbi_bp$pedbp_sbp_percentile - nhlbi_bp$bp_percentile)) < 2)
stopifnot(max(abs(nhlbi_bp$pedbp_dbp_percentile - nhlbi_bp$bp_percentile)) < 2)

################################################################################
# verify output for flynn2017
nq <-
  pedbp:::cppBP(
     qp_sbp = flynn2017$bp_percentile/100,
     qp_dbp = flynn2017$bp_percentile/100,
     male   = flynn2017$male,
     age    = flynn2017$age,
     height = NA,
     height_percentile = flynn2017$height_percentile,
     default_height_percentile = 0.5,
     source = "flynn2017",
     type = "quantile"
  )

np <-
  pedbp:::cppBP(
     qp_sbp = flynn2017$sbp,
     qp_dbp = flynn2017$dbp,
     male   = flynn2017$male,
     age    = flynn2017$age,
     height = NA,
     height_percentile = flynn2017$height_percentile,
     default_height_percentile = 0.5,
     source = "flynn2017",
     type = "percentile"
  )

flynn2017 <-
  cbind(flynn2017,
        pedbp_sbp = nq$sbp,
        pedbp_dbp = nq$dbp,
        pedbp_sbp_percentile = np$sbp_percentile * 100,
        pedbp_dbp_percentile = np$dbp_percentile * 100
  )


# All the quantile estimates are within 2 mmHg:
stopifnot(max(abs(flynn2017$pedbp_sbp - flynn2017$sbp)) < 2)
stopifnot(max(abs(flynn2017$pedbp_dbp - flynn2017$dbp)) < 2)

# All the percentiles are within 2 percentile points:
stopifnot(max(abs(flynn2017$pedbp_sbp_percentile - flynn2017$bp_percentile)) < 2)
stopifnot(max(abs(flynn2017$pedbp_dbp_percentile - flynn2017$bp_percentile)) < 2)

################################################################################
# test output for martin2022
test_martin2022 <-
  expand.grid(age = seq(0, 217, by = 1),
              male = 0:1,
              height = c(NA, seq(75, 160, by = 10)),
              height_percentile = c(NA, seq(0.01, 0.99, by = 0.1)),
              source = NA_character_,
              stringsAsFactors = FALSE
  )
# build up the expected source
test_martin2022$source[test_martin2022$age < 12] <- "gemelli1990"

test_martin2022$source[(test_martin2022$age >= 12) &
                       (!is.na(test_martin2022$height) | !is.na(test_martin2022$height_percentile))
                      ] <- "nhlbi"

test_martin2022$source[(test_martin2022$age >= 12) &
                       (is.na(test_martin2022$height) & is.na(test_martin2022$height_percentile)) &
                       (test_martin2022$age < 36)
                      ] <- "nhlbi"

test_martin2022$source[(test_martin2022$age >= 12) &
                       (is.na(test_martin2022$height) & is.na(test_martin2022$height_percentile)) &
                       (test_martin2022$age >= 36)
                      ] <- "lo2013"

# clean up expected source
test_martin2022$source[test_martin2022$age <= 0] <- NA_character_
test_martin2022$source[test_martin2022$age > 216] <- NA_character_

original_hash <- digest::digest(test_martin2022)  # needed for testing against error seen in #18

x <-
  pedbp:::cppBP(
    qp_sbp = numeric(1),
    qp_dbp = numeric(1),
    age = test_martin2022$age,
    male = test_martin2022$male,
    height = test_martin2022$height,
    height_percentile = test_martin2022$height_percentile,
    default_height_percentile = 0.8,
    source = "martin2022",
    type = 'quantile')

new_hash <- digest::digest(test_martin2022)
stopifnot(identical(original_hash, new_hash))

x <- attr(x, 'bp_params')
stopifnot(identical(test_martin2022$source, x$source) )

x <-
  pedbp:::cppBP(
    qp_sbp = numeric(1),
    qp_dbp = numeric(1),
    age = test_martin2022$age,
    male = test_martin2022$male,
    height = test_martin2022$height,
    height_percentile = test_martin2022$height_percentile,
    default_height_percentile = 0.8,
    source = "martin2022",
    type = 'percentile')

new_hash <- digest::digest(test_martin2022)
stopifnot(identical(original_hash, new_hash))

x <- attr(x, 'bp_params')
stopifnot(identical(test_martin2022$source, x$source) )

################################################################################
##                                End of file                                 ##
################################################################################
