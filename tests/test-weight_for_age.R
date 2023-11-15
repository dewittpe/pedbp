library(pedbp)

d <- pedbp:::lms_data
d <- d[d$metric == "weight_for_age", ]
dwho <- d[d$source == "WHO", ]
dcdc <- d[d$source == "CDC", ]

################################################################################
##                           Testing p_weight_for_age                            ##

# verify the 0.01 and 0.25 percentile is genereted as expected.  Assuming these
# tests pass there will be additional tests of the quantile, distribution, and
# zscore functions.

p01_who_test <-
  p_weight_for_age(  q      = dwho$P01
                , age    = dwho$age
                , male   = dwho$male
                , source = "WHO"
  )
p01_cdc_test <-
  p_weight_for_age(  q      = dwho$P01
                , age    = dwho$age
                , male   = dwho$male
                , source = "CDC"
  )
p01_cdc_who_test <-
  p_weight_for_age(  q      = dwho$P01
                , age    = dwho$age
                , male   = dwho$male
                , source = "CDC-WHO"
  )

# should return a vector the same length as the input
stopifnot(identical(length(p01_who_test), nrow(dwho)))
stopifnot(identical(length(p01_cdc_test), nrow(dwho)))
stopifnot(identical(length(p01_cdc_who_test), nrow(dwho)))

# 0.001 to three digits, even 4 digits, for the WHO data
stopifnot(identical(round(p01_who_test, digits = 3), rep(0.001, length = nrow(dwho))))
stopifnot(identical(round(p01_who_test, digits = 4), rep(0.001, length = nrow(dwho))))

# The weight for age data should be non-missing for all ages and both WHO and
# CDC (this is different from BMI for age)
stopifnot(isTRUE(all(!is.na( p01_cdc_test ))))
stopifnot(isTRUE(all(!is.na( p01_who_test ))))
stopifnot(isTRUE(all(!is.na( p01_cdc_who_test ))))

# the p01_cdc_who_test result should be the who value were p01_cdc_test is NA
# and cdc value otherwise
expected <- p01_cdc_test
expected[dwho$age < 24] <- p01_who_test[dwho$age < 24]
stopifnot(isTRUE(all.equal(p01_cdc_who_test, expected)))

# do the same test for the first quantile
p25_who_test <-
  p_weight_for_age(  q      = dwho$P25
                , age    = dwho$age
                , male   = dwho$male
                , source = "WHO"
  )
p25_cdc_test <-
  p_weight_for_age(  q      = dwho$P25
                , age    = dwho$age
                , male   = dwho$male
                , source = "CDC"
  )
p25_cdc_who_test <-
  p_weight_for_age(  q      = dwho$P25
                , age    = dwho$age
                , male   = dwho$male
                , source = "CDC-WHO"
  )

stopifnot(identical(length(p25_who_test), nrow(dwho)))
stopifnot(identical(length(p25_cdc_test), nrow(dwho)))
stopifnot(identical(length(p25_cdc_who_test), nrow(dwho)))

stopifnot(identical(round(p25_who_test, digits = 3), rep(0.25, length = nrow(dwho))))

stopifnot(isTRUE(all(!is.na( p25_cdc_test ))))
stopifnot(isTRUE(all(!is.na( p25_who_test ))))
stopifnot(isTRUE(all(!is.na( p25_cdc_who_test ))))

expected <- p25_cdc_test
expected[dwho$age < 24] <- p25_who_test[dwho$age < 24]
stopifnot(isTRUE(all.equal(p25_cdc_who_test, expected)))

# testing on the CDC data
p25_who_test <-
  p_weight_for_age(  q      = dcdc$P25
                , age    = dcdc$age
                , male   = dcdc$male
                , source = "WHO"
  )
p25_cdc_test <-
  p_weight_for_age(  q      = dcdc$P25
                , age    = dcdc$age
                , male   = dcdc$male
                , source = "CDC"
  )
p25_cdc_who_test <-
  p_weight_for_age(  q      = dcdc$P25
                , age    = dcdc$age
                , male   = dcdc$male
                , source = "CDC-WHO"
  )

stopifnot(identical(length(p25_who_test), nrow(dcdc)))
stopifnot(identical(length(p25_cdc_test), nrow(dcdc)))
stopifnot(identical(length(p25_cdc_who_test), nrow(dcdc)))

stopifnot(identical(round(p25_cdc_test, digits = 3), rep(0.25, length = nrow(dcdc))))

expected <- p25_cdc_test
expected[dcdc$age < 24] <- p25_who_test[dcdc$age < 24]
stopifnot(isTRUE(all.equal(p25_cdc_who_test, expected)))

################################################################################
##           Testing of quantile, distribution, and zscore methods            ##
set.seed(42)
ages    <- runif(1000, min = 0, max = 240.5)
genders <- as.integer(runif(1000) < 0.5)
ps      <- runif(1000)

test_df <-
  data.frame(
    age       = ages
  , male      = genders
  , p         = ps
  , z         = qnorm(ps)
  , q_cdc.who = suppressWarnings(q_weight_for_age(p = ps, male = genders, age = ages, source = "CDC-WHO"))
  , q_cdc     = suppressWarnings(q_weight_for_age(p = ps, male = genders, age = ages, source = "CDC"))
  , q_who     = suppressWarnings(q_weight_for_age(p = ps, male = genders, age = ages, source = "WHO"))
  )

test_df[["p_cdc.who"]] <- suppressWarnings(p_weight_for_age(q = test_df$q_cdc.who, male = genders, age = ages, source = "CDC-WHO"))
test_df[["p_cdc"]] <- suppressWarnings(p_weight_for_age(q = test_df$q_cdc, male = genders, age = ages, source = "CDC"))
test_df[["p_who"]] <- suppressWarnings(p_weight_for_age(q = test_df$q_who, male = genders, age = ages, source = "WHO"))

test_df[["z_cdc.who"]] <- suppressWarnings(z_weight_for_age(q = test_df$q_cdc.who, male = genders, age = ages, source = "CDC-WHO"))
test_df[["z_cdc"]] <- suppressWarnings(z_weight_for_age(q = test_df$q_cdc, male = genders, age = ages, source = "CDC"))
test_df[["z_who"]] <- suppressWarnings(z_weight_for_age(q = test_df$q_who, male = genders, age = ages, source = "WHO"))

head(test_df)

stopifnot(identical(test_df[test_df$age <  24, "q_cdc.who"], test_df[test_df$age <  24, "q_who"]))
stopifnot(identical(test_df[test_df$age >= 24, "q_cdc.who"], test_df[test_df$age >= 24, "q_cdc"]))

stopifnot(with(test_df[!is.na(test_df$z_cdc), ], isTRUE(all.equal(z, z_cdc))))
stopifnot(with(test_df[!is.na(test_df$z_who), ], isTRUE(all.equal(z, z_who))))
stopifnot(with(test_df[!is.na(test_df$z_cdc.who), ], isTRUE(all.equal(z, z_cdc.who))))

################################################################################
##                               End of center                                ##
################################################################################
