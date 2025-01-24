# The following tests come from examples reported by end users which resulted in
# unexpected values.

library(pedbp)

################################################################################
# 23 January 2025
#
# The following record would return unexpectely high blood pressure percentiles.
#
pat1 <-
  data.frame(age = 16.8,    # months
             height = 79.8, # cm
             weight = 11.6, # kg
             male = 0,
             sbp = 92,      # mmHg
             dbp = 54       # mmHg
  )

# first, stature for age.  Expect that height_for_age will return NaN and a
# warning when source is CDC becuase the CDC starts start at 24 months, and WHO
# starts at 61 months
expr <- expression(with(pat1, p_height_for_age(q = height, male = male, age = age, source = "CDC")))
test1a <- tryCatch(eval(expr), warning = function(w) w)
stopifnot(inherits(test1a, "warning"))

test1b <- suppressWarnings(eval(expr))
stopifnot(is.na(test1b))

expr <- expression(with(pat1, p_height_for_age(q = height, male = male, age = age, source = "WHO")))
test1c <- tryCatch(eval(expr), warning = function(w) w)
stopifnot(inherits(test1c, "warning"))

test1d <- suppressWarnings(eval(expr))
stopifnot(is.na(test1d))

# second the length_for_age should return useful values for both the CDC and WHO
# sources
stopifnot(
  all.equal(
    with(pat1, p_length_for_age(age = age, male = male, q = height, source = "CDC")),
    0.6251524,
    tol = 1e-7
  )
)

stopifnot(
  all.equal(
    with(pat1, p_length_for_age(age = age, male = male, q = height, source = "WHO")),
    0.5493427,
    tol = 1e-7
  )
)

# The blood pressure precentiles are not as expected
# When using only age and sex then the flowchart for source = "martin2022" says
# that the percentiles should come from NHLBI
test2 <- with(pat1, p_bp(age = age, male = male, q_sbp = sbp, q_dbp = dbp))
stopifnot(identical(attr(test2, "bp_params")$source, "nhlbi"))
stopifnot(all.equal(attr(test2, "bp_params")$height_percentile, 50))
stopifnot(identical(attr(test2, "bp_params")$male, 0L))
stopifnot(all.equal(attr(test2, "bp_params")$age, 12))
stopifnot(all.equal(attr(test2, "bp_params")$sbp_mean, 86.00094, tol = 1e-6))
stopifnot(all.equal(attr(test2, "bp_params")$sbp_sd, 10.92093, tol = 1e-6))
stopifnot(all.equal(attr(test2, "bp_params")$dbp_mean, 40.00094, tol = 1e-6))
stopifnot(all.equal(attr(test2, "bp_params")$dbp_sd, 10.92093, tol = 1e-6))
stopifnot(all.equal(test2$sbp_p, 0.7086064, tol = 1e-6))
stopifnot(all.equal(test2$dbp_p, 0.9000536, tol = 1e-6))

# when height is given, again the NHLBI data should be used, and the height
# percentile should be calculated.
#
# The problem is that as of version 2.0.2 the height_percentile being used is 5,
# which suggests to me that the height_for_age method is being used instead of
# the length_for_age method for calculating the height_percentile.
#
# The problem was in src/blood_pressue.cpp where height_percentile needed to be
# multiplied by 100 before confronting the lookup table.
#
# given that the height percentile is 54, the look up table should use the
# median value and return the same thing as test2
test3 <- with(pat1, p_bp(age = age, male = male, height = height, q_sbp = sbp, q_dbp = dbp))
stopifnot(all.equal(test2, test3))
