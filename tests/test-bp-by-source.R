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
#                                 End of file                                  #
################################################################################
