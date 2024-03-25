# test that the p_bp and q_bp functions return as expected for all the different
# source options
library(pedbp)

# first, verify that the source input options are as expected
x <- tryCatch(q_bp(p_sbp = 1, p_dbp = 1, male = 1, age = numeric(1), source = "Not a real source"), error = function(e) e)
stopifnot(identical(class(x), c("simpleError", "error", "condition")))

# the message appears to print in one of two ways, using the unicode \u8220 and
# \u8221 for the double quotes when in an interactive session, and \u22 when not
# interactive
stopifnot(
          identical(x$message, "'arg' should be one of “martin2022”, “gemelli1990”, “nhlbi”, “lo2013”, “flynn2017”") |
          identical(x$message, "'arg' should be one of \"martin2022\", \"gemelli1990\", \"nhlbi\", \"lo2013\", \"flynn2017\"")
)

################################################################################
# gemelli1990

qbp_expected <- list(sbp = rep(NA_real_, 9), dbp = rep(NA_real_, 9))

# return NA for age out of bounds
# too young
stopifnot(
  isTRUE(
    all.equal(
      q_bp(p_sbp = 1:9/10, p_dbp = 1:9/10, male = 0, age = 0.9)
      ,
      qbp_expected
      ,
      check.attributes = FALSE
    )
  )
)

stopifnot(
  isTRUE(
    all.equal(
      q_bp(p_sbp = 1:9/10, p_dbp = 1:9/10, male = 0, age = 0.9, source = "martin2022")
      ,
      qbp_expected
      ,
      check.attributes = FALSE
    )
  )
)

stopifnot(
  isTRUE(
    all.equal(
      q_bp(p_sbp = 1:9/10, p_dbp = 1:9/10, male = 0, age = 0.9, source = "gemelli1990")
      ,
      qbp_expected
      ,
      check.attributes = FALSE
    )
  )
)

stopifnot(
  isTRUE(
    all.equal(
      q_bp(p_sbp = 1:9/10, p_dbp = 1:9/10, male = 0, age = 35.9, source = "lo2013")
      ,
      qbp_expected
      ,
      check.attributes = FALSE
    )
  )
)

stopifnot(
  isTRUE(
    all.equal(
      q_bp(p_sbp = 1:9/10, p_dbp = 1:9/10, male = 0, age = 11.9, source = "nhlbi")
      ,
      qbp_expected
      ,
      check.attributes = FALSE
    )
  )
)

stopifnot(
  isTRUE(
    all.equal(
      q_bp(p_sbp = 1:9/10, p_dbp = 1:9/10, male = 0, age = 11.9, source = "flynn2017")
      ,
      qbp_expected
      ,
      check.attributes = FALSE
    )
  )
)


#too old
stopifnot(
  isTRUE(
    all.equal(
      q_bp(p_sbp = 1:9/10, p_dbp = 1:9/10, male = 0, age = 19*12, source = "flynn2017")
      ,
      qbp_expected
      ,
      check.attributes = FALSE
    )
  )
)

stopifnot(
  isTRUE(
    all.equal(
      q_bp(p_sbp = 1:9/10, p_dbp = 1:9/10, male = 0, age = 19*12, source = "nhlbi")
      ,
      qbp_expected
      ,
      check.attributes = FALSE
    )
  )
)

stopifnot(
  isTRUE(
    all.equal(
      q_bp(p_sbp = 1:9/10, p_dbp = 1:9/10, male = 0, age = 19*12, source = "martin2022")
      ,
      qbp_expected
      ,
      check.attributes = FALSE
    )
  )
)

stopifnot(
  isTRUE(
    all.equal(
      q_bp(p_sbp = 1:9/10, p_dbp = 1:9/10, male = 0, age = 19*12)
      ,
      qbp_expected
      ,
      check.attributes = FALSE
    )
  )
)

stopifnot(
  isTRUE(
    all.equal(
      q_bp(p_sbp = 1:9/10, p_dbp = 1:9/10, male = 0, age = 19*12, source = "lo2013")
      ,
      qbp_expected
      ,
      check.attributes = FALSE
    )
  )
)

stopifnot(
  isTRUE(
    all.equal(
      q_bp(p_sbp = 1:9/10, p_dbp = 1:9/10, male = 0, age = 13, source = "gemelli1990")
      ,
      qbp_expected
      ,
      check.attributes = FALSE
    )
  )
)

################################################################################
# nhlbi and flynn2017 values will be different from the publications.
# The difference is due to having a Gaussian mean and variance estimated by the
# three or four percentiles published.  The tests here are to verify that the
# estimates are not too different.

# NOTE: IF THIS FAILS UPDATE THE VIGNETTE


fq <-
  q_bp(
     p_sbp = flynn2017$bp_percentile/100,
     p_dbp = flynn2017$bp_percentile/100,
     male  = flynn2017$male,
     age   = flynn2017$age,
     height_percentile = flynn2017$height_percentile/100,
     source = "flynn2017")

fp <-
  p_bp(
     q_sbp = flynn2017$sbp,
     q_dbp = flynn2017$dbp,
     male  = flynn2017$male,
     age   = flynn2017$age,
     height_percentile = flynn2017$height_percentile/100,
     source = "flynn2017")

nq <-
  q_bp(
     p_sbp = nhlbi_bp_norms$bp_percentile/100,
     p_dbp = nhlbi_bp_norms$bp_percentile/100,
     male  = nhlbi_bp_norms$male,
     age   = nhlbi_bp_norms$age,
     height_percentile = nhlbi_bp_norms$height_percentile/100,
     source = "nhlbi")

np <-
  p_bp(
     q_sbp = nhlbi_bp_norms$sbp,
     q_dbp = nhlbi_bp_norms$dbp,
     male  = nhlbi_bp_norms$male,
     age   = nhlbi_bp_norms$age,
     height_percentile = nhlbi_bp_norms$height_percentile/100,
     source = "nhlbi")

flynn2017 <-
  cbind(flynn2017,
        pedbp_sbp = fq$sbp,
        pedbp_dbp = fq$dbp,
        pedbp_sbp_percentile = fp$sbp_percentile * 100,
        pedbp_dbp_percentile = fp$dbp_percentile * 100
  )

nhlbi_bp_norms <-
  cbind(nhlbi_bp_norms,
        pedbp_sbp = nq$sbp,
        pedbp_dbp = nq$dbp,
        pedbp_sbp_percentile = np$sbp_percentile * 100,
        pedbp_dbp_percentile = np$dbp_percentile * 100
  )

# all estimates within 2 mmHg or 2 percentage points
stopifnot(max(abs(flynn2017$pedbp_sbp - flynn2017$sbp)) < 2)
stopifnot(max(abs(flynn2017$pedbp_dbp - flynn2017$dbp)) < 2)
stopifnot(max(abs(flynn2017$pedbp_sbp_percentile - flynn2017$bp_percentile)) < 2)
stopifnot(max(abs(flynn2017$pedbp_dbp_percentile - flynn2017$bp_percentile)) < 2)
stopifnot(max(abs(nhlbi_bp_norms$pedbp_sbp - nhlbi_bp_norms$sbp)) < 2)
stopifnot(max(abs(nhlbi_bp_norms$pedbp_dbp - nhlbi_bp_norms$dbp)) < 2)
stopifnot(max(abs(nhlbi_bp_norms$pedbp_sbp_percentile - nhlbi_bp_norms$bp_percentile)) < 2)
stopifnot(max(abs(nhlbi_bp_norms$pedbp_dbp_percentile - nhlbi_bp_norms$bp_percentile)) < 2)

if (interactive()) {
  par(mfrow = c(1, 2))
  plot(flynn2017$sbp, flynn2017$pedbp_sbp); abline(0, 1)
  plot(flynn2017$dbp, flynn2017$pedbp_dbp); abline(0, 1)
  summary(flynn2017$pedbp_sbp - flynn2017$sbp)
  summary(flynn2017$pedbp_dbp - flynn2017$dbp)
  summary(flynn2017$pedbp_sbp_percentile*100 - flynn2017$bp_percentile)
  summary(flynn2017$pedbp_dbp_percentile*100 - flynn2017$bp_percentile)

  qwraps2::qblandaltman(flynn2017[, c("sbp", "pedbp_sbp")])
  qwraps2::qblandaltman(flynn2017[, c("dbp", "pedbp_dbp")])
  qwraps2::qblandaltman(flynn2017[, c("bp_percentile", "pedbp_sbp_percentile")])
  qwraps2::qblandaltman(flynn2017[, c("bp_percentile", "pedbp_dbp_percentile")])

  par(mfrow = c(1, 2))
  plot(nhlbi_bp_norms$sbp, nhlbi_bp_norms$pedbp_sbp); abline(0, 1)
  plot(nhlbi_bp_norms$dbp, nhlbi_bp_norms$pedbp_dbp); abline(0, 1)
  summary(nhlbi_bp_norms$pedbp_sbp - nhlbi_bp_norms$sbp)
  summary(nhlbi_bp_norms$pedbp_dbp - nhlbi_bp_norms$dbp)
  summary(nhlbi_bp_norms$pedbp_sbp_percentile*100 - nhlbi_bp_norms$bp_percentile)
  summary(nhlbi_bp_norms$pedbp_dbp_percentile*100 - nhlbi_bp_norms$bp_percentile)

  qwraps2::qblandaltman(nhlbi_bp_norms[, c("sbp", "pedbp_sbp")])
  qwraps2::qblandaltman(nhlbi_bp_norms[, c("dbp", "pedbp_dbp")])
  qwraps2::qblandaltman(nhlbi_bp_norms[, c("bp_percentile", "pedbp_sbp_percentile")])
  qwraps2::qblandaltman(nhlbi_bp_norms[, c("bp_percentile", "pedbp_dbp_percentile")])
}

################################################################################
#                                 End of file                                  #
################################################################################
