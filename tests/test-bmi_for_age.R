library(pedbp)

d <- pedbp:::lms_data
d <- d[d$metric == "bmi_for_age", ]
dwho <- d[d$source == "WHO", ]
dcdc <- d[d$source == "CDC", ]

# dwho <- Filter(function(x) {!all(is.na(x))}, d[d$source == "WHO", ])
# dcdc <- Filter(function(x) {!all(is.na(x))}, d[d$source == "CDC-2000", ])

################################################################################
##                           Testing p_bmi_for_age                            ##

p01_who_test <-
  p_bmi_for_age(  q      = dwho$P01
                , age    = dwho$age
                , male   = dwho$male
                , source = "WHO"
  )
p01_cdc_test <-
  p_bmi_for_age(  q      = dwho$P01
                , age    = dwho$age
                , male   = dwho$male
                , source = "CDC"
  )
p01_cdc_who_test <-
  p_bmi_for_age(  q      = dwho$P01
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

# for the pure cdc data, there should be NAs, for ages under 2 years = 24
# months, and non-NA for ages >= 2 years = 24 months.  The non-missing values
# should be near 0.001, but will not be there as the provided quantiles in the
# test are for the WHO data and corresponding values for the CDC are not
# available.
stopifnot(isTRUE(all(is.na( p01_cdc_test[dwho$age < 24] ))))
stopifnot(isTRUE(all(!is.na( p01_cdc_test[dwho$age >= 24] ))))

# the p01_cdc_who_test result should be the who value were p01_cdc_test is NA
# and cdc value otherwise
expected <- p01_cdc_test
expected[is.na(p01_cdc_test)] <- p01_who_test[is.na(p01_cdc_test)]
stopifnot(isTRUE(all.equal(p01_cdc_who_test, expected)))

# do the same test for the first quantile
p25_who_test <-
  p_bmi_for_age(  q      = dwho$P25
                , age    = dwho$age
                , male   = dwho$male
                , source = "WHO"
  )
p25_cdc_test <-
  p_bmi_for_age(  q      = dwho$P25
                , age    = dwho$age
                , male   = dwho$male
                , source = "CDC"
  )
p25_cdc_who_test <-
  p_bmi_for_age(  q      = dwho$P25
                , age    = dwho$age
                , male   = dwho$male
                , source = "CDC-WHO"
  )

stopifnot(identical(length(p25_who_test), nrow(dwho)))
stopifnot(identical(length(p25_cdc_test), nrow(dwho)))
stopifnot(identical(length(p25_cdc_who_test), nrow(dwho)))

stopifnot(identical(round(p25_who_test, digits = 3), rep(0.25, length = nrow(dwho))))

stopifnot(isTRUE(all(is.na( p25_cdc_test[dwho$age < 24] ))))
stopifnot(isTRUE(all(!is.na( p25_cdc_test[dwho$age >= 24] ))))

expected <- p25_cdc_test
expected[is.na(p25_cdc_test)] <- p25_who_test[is.na(p25_cdc_test)]
stopifnot(isTRUE(all.equal(p25_cdc_who_test, expected)))

# testing on the CDC data
p25_who_test <-
  p_bmi_for_age(  q      = dcdc$P25
                , age    = dcdc$age
                , male   = dcdc$male
                , source = "WHO"
  )
p25_cdc_test <-
  p_bmi_for_age(  q      = dcdc$P25
                , age    = dcdc$age
                , male   = dcdc$male
                , source = "CDC"
  )
p25_cdc_who_test <-
  p_bmi_for_age(  q      = dcdc$P25
                , age    = dcdc$age
                , male   = dcdc$male
                , source = "CDC-WHO"
  )

stopifnot(identical(length(p25_who_test), nrow(dcdc)))
stopifnot(identical(length(p25_cdc_test), nrow(dcdc)))
stopifnot(identical(length(p25_cdc_who_test), nrow(dcdc)))

stopifnot(identical(round(p25_cdc_test, digits = 3), rep(0.25, length = nrow(dcdc))))

stopifnot(isTRUE(all(is.na( p25_cdc_test[dcdc$age < 24] ))))
stopifnot(isTRUE(all(!is.na( p25_cdc_test[dcdc$age >= 24] ))))

expected <- p25_cdc_test
expected[is.na(p25_cdc_test)] <- p25_who_test[is.na(p25_cdc_test)]
stopifnot(isTRUE(all.equal(p25_cdc_who_test, expected)))




################################################################################
##                            Tesing q_bmi_for_age                            ##


################################################################################
##                            Tesing z_bmi_for_age                            ##

################################################################################
##                               End of center                                ##
################################################################################
