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
     height_percentile = nhlbi_bp_norms$height_percentile,
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
     height_percentile = nhlbi_bp_norms$height_percentile,
     default_height_percentile = 0.5,
     source = "nhlbi"
  )

nhlbi_bp <-
  cbind(nhlbi_bp_norms,
        pedbp_sbp = nq$sbp,
        pedbp_dbp = nq$dbp,
        pedbp_sbp_p = np$sbp_p * 100,
        pedbp_dbp_p = np$dbp_p * 100
  )

# All the quantile estimates are within 2 mmHg:
stopifnot(max(abs(nhlbi_bp$pedbp_sbp - nhlbi_bp$sbp)) < 2)
stopifnot(max(abs(nhlbi_bp$pedbp_dbp - nhlbi_bp$dbp)) < 2)

# All the percentiles are within 2 percentile points:
stopifnot(max(abs(nhlbi_bp$pedbp_sbp_p - nhlbi_bp$bp_percentile)) < 2)
stopifnot(max(abs(nhlbi_bp$pedbp_dbp_p - nhlbi_bp$bp_percentile)) < 2)

################################################################################
# verify output for flynn2017
nq <-
  q_bp(
     p_sbp = flynn2017$bp_percentile/100,
     p_dbp = flynn2017$bp_percentile/100,
     male   = flynn2017$male,
     age    = flynn2017$age,
     height = NA,
     height_percentile = flynn2017$height_percentile,
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
     height_percentile = flynn2017$height_percentile,
     default_height_percentile = 0.5,
     source = "flynn2017"
  )

flynn2017 <-
  cbind(flynn2017,
        pedbp_sbp = nq$sbp,
        pedbp_dbp = nq$dbp,
        pedbp_sbp_p = np$sbp_p * 100,
        pedbp_dbp_p = np$dbp_p * 100
  )


# All the quantile estimates are within 2 mmHg:
stopifnot(max(abs(flynn2017$pedbp_sbp - flynn2017$sbp)) < 2)
stopifnot(max(abs(flynn2017$pedbp_dbp - flynn2017$dbp)) < 2)

# All the percentiles are within 2 percentile points:
stopifnot(max(abs(flynn2017$pedbp_sbp_p - flynn2017$bp_percentile)) < 2)
stopifnot(max(abs(flynn2017$pedbp_dbp_p - flynn2017$bp_percentile)) < 2)

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

original_hash <- digest::digest(test_martin2022)

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

new_hash <- digest::digest(test_martin2022)
stopifnot(identical(original_hash, new_hash))

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

new_hash <- digest::digest(test_martin2022)
stopifnot(identical(original_hash, new_hash))

x <- attr(x, 'bp_params')
stopifnot(identical(test_martin2022$source, x$source) )

################################################################################
# Verify that p_bp and q_bp undo each other
x <-
  expand.grid(age = seq(1, 216, by = 1),
              male = 0:1,
              height = c(NA, seq(75, 160, by = 10)),
              height_percentile = c(NA, seq(0.01, 0.99, by = 0.1)),
              p = seq(0, 1, by = 0.05),
              stringsAsFactors = FALSE)

original_hash <- digest::digest(x)

yq <-
  q_bp(p_sbp = x$p,
       p_dbp = x$p,
       age = x$age,
       male = x$male,
       height = x$height,
       height_percentile = x$height_percentile,
       default_height_percentile = 0.8,
       source = "martin2022")

new_hash <- digest::digest(x)
stopifnot(identical(original_hash, new_hash)) # test needed re #18

yp <-
  p_bp(q_sbp = yq$sbp,
       q_dbp = yq$dbp,
       age = x$age,
       male = x$male,
       height = x$height,
       height_percentile = x$height_percentile,
       default_height_percentile = 0.8,
       source = "martin2022")

new_hash <- digest::digest(x)
stopifnot(identical(original_hash, new_hash)) # test needed re #18

bp_params1 <- attr(yq, "bp_params")
bp_params2 <- attr(yp, "bp_params")
stopifnot(isTRUE(all.equal(bp_params1, bp_params2))) # test needed re #18

x$q_sbp <- yq$sbp
x$q_dbp <- yq$dbp
x$p_sbp <- yp$sbp_p
x$p_dbp <- yp$dbp_p

#names(bp_params1) <- paste0(names(bp_params1), "1")
#names(bp_params2) <- paste0(names(bp_params2), "2")
x <- cbind(x, bp_params1)
#head(x)

stopifnot(isTRUE(with(x, all.equal(pnorm(q_sbp, mean = sbp_mean, sd = sbp_sd), p_sbp))))
stopifnot(isTRUE(with(x, all.equal(pnorm(q_dbp, mean = dbp_mean, sd = dbp_sd), p_dbp))))
stopifnot(isTRUE(with(x, all.equal(qnorm(p_sbp, mean = sbp_mean, sd = sbp_sd), q_sbp))))
stopifnot(isTRUE(with(x, all.equal(qnorm(p_dbp, mean = dbp_mean, sd = dbp_sd), q_dbp))))

################################################################################
# verify z_bp is as expected
yq <-
  z_bp(q_sbp = yq$sbp,
       q_dbp = yq$dbp,
       age = x$age,
       male = x$male,
       height = x$height,
       height_percentile = x$height_percentile,
       default_height_percentile = 0.8,
       source = "martin2022")

x$z_sbp <- yq$sbp_z
x$z_dbp <- yq$dbp_z

stopifnot(isTRUE(with(x, all.equal(pnorm(z_sbp), p_sbp))))
stopifnot(isTRUE(with(x, all.equal(pnorm(z_dbp), p_dbp))))
stopifnot(isTRUE(with(x, all.equal(qnorm(p_sbp, mean = sbp_mean, sd = sbp_sd), (z_sbp * sbp_sd) + sbp_mean))))
stopifnot(isTRUE(with(x, all.equal(qnorm(p_dbp, mean = dbp_mean, sd = dbp_sd), (z_dbp * dbp_sd) + dbp_mean))))

################################################################################
##                                End of file                                 ##
################################################################################
