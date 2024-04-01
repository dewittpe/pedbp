library(pedbp)

################################################################################
# simple print test
print_test <-
  q_bp(p_sbp = c(0.5, 0.5), p_dbp = c(0.4, 0.32), age = 13, male = 0)

stopifnot(
  identical(
    capture.output(print_test)
  ,
    capture.output(print_test[1:2])
  )
)

################################################################################
# Verify error if Unknown source
x <- tryCatch(p_bp(q_sbp = 0.5, q_dbp = 0.5, male = 0, age = 55, source = c("not-a-source")), error = function(e) e)
stopifnot(identical(class(x), c("simpleError", "error", "condition")))
stopifnot(identical(x$message, "'arg' should be one of “martin2022”, “gemelli1990”, “nhlbi”, “lo2013”, “flynn2017”") |
          identical(x$message, "'arg' should be one of \"martin2022\", \"gemelli1990\", \"nhlbi\", \"lo2013\", \"flynn2017\""))

x <- tryCatch(q_bp(p_sbp = 0.5, p_dbp = 0.5, male = 0, age = 55, source = c("not-a-source")), error = function(e) e)
stopifnot(identical(class(x), c("simpleError", "error", "condition")))
stopifnot(identical(x$message, "'arg' should be one of “martin2022”, “gemelli1990”, “nhlbi”, “lo2013”, “flynn2017”") |
          identical(x$message, "'arg' should be one of \"martin2022\", \"gemelli1990\", \"nhlbi\", \"lo2013\", \"flynn2017\""))

x <- tryCatch(p_bp(q_sbp = 0.5, q_dbp = 0.5, male = 0, age = 55, source = character(0)), error = function(e) e)
stopifnot(identical(class(x), c("simpleError", "error", "condition")))
stopifnot(identical(x$message, "'arg' should be one of “martin2022”, “gemelli1990”, “nhlbi”, “lo2013”, “flynn2017”") |
          identical(x$message, "'arg' should be one of \"martin2022\", \"gemelli1990\", \"nhlbi\", \"lo2013\", \"flynn2017\""))

x <- tryCatch(q_bp(p_sbp = 0.5, p_dbp = 0.5, male = 0, age = 55, source = character(0)), error = function(e) e)
stopifnot(identical(class(x), c("simpleError", "error", "condition")))
stopifnot(identical(x$message, "'arg' should be one of “martin2022”, “gemelli1990”, “nhlbi”, “lo2013”, “flynn2017”") |
          identical(x$message, "'arg' should be one of \"martin2022\", \"gemelli1990\", \"nhlbi\", \"lo2013\", \"flynn2017\""))

################################################################################
# Verify error if more than one source
x <- tryCatch(p_bp(q_sbp = 0.5, q_dbp = 0.5, male = 0, age = 55, source = c("martin2022", "nhlbi")), error = function(e) e)
stopifnot(identical(class(x), c("simpleError", "error", "condition")))
stopifnot(identical(x$message, "'arg' must be of length 1"))

x <- tryCatch(q_bp(p_sbp = 0.5, p_dbp = 0.5, male = 0, age = 55, source = c("martin2022", "nhlbi")), error = function(e) e)
stopifnot(identical(class(x), c("simpleError", "error", "condition")))
stopifnot(identical(x$message, "'arg' must be of length 1"))

################################################################################
# Verify expected output for gemelli1990
x <-
  p_bp(
      q_sbp = gemelli1990$sbp_mean
    , q_dbp = gemelli1990$dbp_mean
    , age    = gemelli1990$age
    , male   = gemelli1990$male
    , height = NA
    , height_percentile = NA
    , default_height_percentile = 0.5
    , source = "gemelli1990"
  )
stopifnot(identical(class(x), c("pedbp_bp", "pedbp_p_bp")))
stopifnot(identical(length(x), 2L))
stopifnot(identical(length(x[[1]]), nrow(gemelli1990)))
stopifnot(identical(length(x[[2]]), nrow(gemelli1990)))
stopifnot(identical(class(attr(x, "bp_params")), "data.frame"))
stopifnot(isTRUE(all.equal(unname(x), list(rep(0.5, nrow(gemelli1990)), rep(0.5, nrow(gemelli1990))), check.attributes = FALSE)))

x <-
  q_bp(
      p_sbp = 0.5
    , p_dbp = 0.5
    , age    = gemelli1990$age
    , male   = gemelli1990$male
    , height = NA
    , height_percentile = NA
    , default_height_percentile = 0.5
    , source = "gemelli1990"
  )
stopifnot(identical(class(x), c("pedbp_bp", "pedbp_q_bp")))
stopifnot(identical(length(x), 2L))
stopifnot(identical(length(x[[1]]), nrow(gemelli1990)))
stopifnot(identical(length(x[[2]]), nrow(gemelli1990)))
stopifnot(identical(class(attr(x, "bp_params")), "data.frame"))
stopifnot(isTRUE(all.equal(unname(x), as.list(gemelli1990[c("sbp_mean", "dbp_mean")]), check.attributes = FALSE)))

################################################################################
# Verify expected output for lo2013
x <-
  p_bp(
      q_sbp = lo2013$sbp_mean
    , q_dbp = lo2013$dbp_mean
    , age    = lo2013$age
    , male   = lo2013$male
    , height = NA
    , height_percentile = NA
    , default_height_percentile = 0.5
    , source = "lo2013"
  )
stopifnot(identical(class(x), c('pedbp_bp', 'pedbp_p_bp')))
stopifnot(identical(length(x), 2L))
stopifnot(identical(length(x[[1]]), nrow(lo2013)))
stopifnot(identical(length(x[[2]]), nrow(lo2013)))
stopifnot(identical(class(attr(x, "bp_params")), "data.frame"))
stopifnot(isTRUE(all.equal(unname(x), list(rep(0.5, nrow(lo2013)), rep(0.5, nrow(lo2013))), check.attributes = FALSE)))

x <-
  q_bp(
      p_sbp = 0.5
    , p_dbp = 0.5
    , age    = lo2013$age
    , male   = lo2013$male
    , height = NA
    , height_percentile = NA
    , default_height_percentile = 0.5
    , source = "lo2013"
  )
stopifnot(identical(class(x), c('pedbp_bp', 'pedbp_q_bp')))
stopifnot(identical(length(x), 2L))
stopifnot(identical(length(x[[1]]), nrow(lo2013)))
stopifnot(identical(length(x[[2]]), nrow(lo2013)))
stopifnot(identical(class(attr(x, "bp_params")), "data.frame"))
stopifnot(isTRUE(all.equal(unname(x), as.list(lo2013[c("sbp_mean", "dbp_mean")]), check.attributes = FALSE)))

################################################################################
# verify output for nhlbi
nq <-
  q_bp(
     p_sbp = nhlbi_bp_norms$bp_percentile/100,
     p_dbp = nhlbi_bp_norms$bp_percentile/100,
     male   = nhlbi_bp_norms$male,
     age    = nhlbi_bp_norms$age,
     height = NA,
     height_percentile = nhlbi_bp_norms$height_percentile/100,
     default_height_percentile = 0.5,
     source = "nhlbi"
  )

np <-
  p_bp(
     q_sbp = nhlbi_bp_norms$sbp,
     q_dbp = nhlbi_bp_norms$dbp,
     male   = nhlbi_bp_norms$male,
     age    = nhlbi_bp_norms$age,
     height = NA,
     height_percentile = nhlbi_bp_norms$height_percentile/100,
     default_height_percentile = 0.5,
     source = "nhlbi"
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
  q_bp(
     p_sbp = flynn2017$bp_percentile/100,
     p_dbp = flynn2017$bp_percentile/100,
     male   = flynn2017$male,
     age    = flynn2017$age,
     height = NA,
     height_percentile = flynn2017$height_percentile/100,
     default_height_percentile = 0.5,
     source = "flynn2017"
  )

np <-
  p_bp(
     q_sbp = flynn2017$sbp,
     q_dbp = flynn2017$dbp,
     male   = flynn2017$male,
     age    = flynn2017$age,
     height = NA,
     height_percentile = flynn2017$height_percentile/100,
     default_height_percentile = 0.5,
     source = "flynn2017"
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
              height = c(NA, 120),
              height_percentile = c(NA, 0.2),
              source = NA_character_,
              stringsAsFactors = FALSE
  )

test_martin2022$source[ test_martin2022$age > 0 & test_martin2022$age < 12 & test_martin2022$age <= 216] <- "gemelli1990"
test_martin2022$source[
                       (test_martin2022$age > 0 & test_martin2022$age >= 12 & test_martin2022$age <= 216) &
                       (!is.na(test_martin2022$height))
                      ] <- "nhlbi"
test_martin2022$source[
                       (test_martin2022$age > 0 & test_martin2022$age >= 12 & test_martin2022$age <= 216) &
                       (is.na(test_martin2022$height)) &
                       (test_martin2022$age < 36)
                      ] <- "nhlbi"
test_martin2022$source[
                       (test_martin2022$age > 0 & test_martin2022$age >= 12 & test_martin2022$age <= 216) &
                       (is.na(test_martin2022$height)) &
                       (test_martin2022$age >= 36)
                      ] <- "lo2013"

x <-
  p_bp(
    q_sbp = numeric(1),
    q_dbp = numeric(1),
    age = test_martin2022$age,
    male = test_martin2022$male,
    height = test_martin2022$height,
    height_percentile = test_martin2022$height_percentile,
    default_height_percentile = 0.8,
    source = "martin2022"
    )

x <- attr(x, 'bp_params')
stopifnot(identical(test_martin2022$source, x$source) )

x <-
  q_bp(
    p_sbp = numeric(1),
    p_dbp = numeric(1),
    age = test_martin2022$age,
    male = test_martin2022$male,
    height = test_martin2022$height,
    height_percentile = test_martin2022$height_percentile,
    default_height_percentile = 0.8,
    source = "martin2022"
    )

x <- attr(x, 'bp_params')
stopifnot(identical(test_martin2022$source, x$source) )

################################################################################
##                                End of file                                 ##
################################################################################
