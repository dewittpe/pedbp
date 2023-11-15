# library(pedbp)

str(pedbp:::lms_data, max.level = 3)

d <- pedbp:::lms_data |> 
     lapply(lapply, data.table::rbindlist, use.names = TRUE, fill = TRUE) |>
     lapply(data.table::rbindlist, use.names = TRUE, fill = TRUE) |>
     data.table::rbindlist(use.names = TRUE, fill = TRUE)

d <- d[d$metric == "weight_for_stature"]

################################################################################
##                    Testing against the published values                    ##

published <-
  data.table::melt(
    d
  , id.vars = c("source", "metric", "male", "stature", "L", "M", "S")
  , measure.vars = patterns("P")
  , variable.factor = FALSE
  , variable.name = "published_percentile"
  , value.name = "published_quantile"
  )

published[published_percentile == "P01",  published_percentile := 0.001]
published[published_percentile == "P1",   published_percentile := 0.010]
published[published_percentile == "P3",   published_percentile := 0.030]
published[published_percentile == "P5",   published_percentile := 0.050]
published[published_percentile == "P10",  published_percentile := 0.100]
published[published_percentile == "P15",  published_percentile := 0.150]
published[published_percentile == "P25",  published_percentile := 0.250]
published[published_percentile == "P50",  published_percentile := 0.500]
published[published_percentile == "P75",  published_percentile := 0.750]
published[published_percentile == "P85",  published_percentile := 0.850]
published[published_percentile == "P90",  published_percentile := 0.900]
published[published_percentile == "P95",  published_percentile := 0.950]
published[published_percentile == "P97",  published_percentile := 0.970]
published[published_percentile == "P99",  published_percentile := 0.990]
published[published_percentile == "P999", published_percentile := 0.999]
published[, published_percentile := as.numeric(published_percentile)]


# published[, testing_cdc_who_percentile := p_weight_for_stature(q = published_quantile, male = male, stature = stature, source = "CDC-WHO")]
# published[, testing_cdc_percentile := p_weight_for_stature(q = published_quantile, male = male, stature = stature, source = "CDC")]
# published[, testing_who_percentile := p_weight_for_stature(q = published_quantile, male = male, stature = stature, source = "WHO")]
# 
# published[, testing_cdc_who_quantile := q_weight_for_stature(p = published_percentile, male = male, stature = stature, source = "CDC-WHO")]
# published[, testing_cdc_quantile := q_weight_for_stature(p = published_percentile, male = male, stature = stature, source = "CDC")]

tictoc::tic()
published[, testing_who_quantile := suppressWarnings(q_weight_for_stature(p = published_percentile, male = male, stature = stature, source = "WHO"))]
tictoc::toc()
# was around 49 seconds when lms_data as a data.frame in the package


stop()

debug(p_weight_for_stature)
debug(pedbp:::pgsf)
debug(pedbp:::get_lms)

undebug(p_weight_for_stature)
undebug(pedbp:::pgsf)
undebug(pedbp:::get_lms)

p_weight_for_stature(q = 1.888, male = 0, stature = 45, source = "CDC")
p_weight_for_stature(q = 1.888, male = 0, stature = 45, source = "WHO")
p_weight_for_stature(q = 1.888, male = 0, stature = 45, source = "CDC-WHO")

all.equal(published$testing_who_percentile, published$testing_cdc_percentile)
all.equal(published$testing_who_percentile, published$testing_cdc_who_percentile)

published[stature == 45 & male == 0]

stop()




# verify the 0.01 and 0.25 percentile is genereted as expected.  Assuming these
# tests pass there will be additional tests of the quantile, distribution, and
# zscore functions.

p01_who_test <-
  p_weight_for_stature(  q      = dwho$P01
                , stature    = dwho$stature
                , male   = dwho$male
                , source = "WHO"
  )
p01_cdc_test <-
  p_weight_for_stature(  q      = dwho$P01
                , stature    = dwho$stature
                , male   = dwho$male
                , source = "CDC"
  )
p01_cdc_who_test <-
  p_weight_for_stature(  q      = dwho$P01
                , stature    = dwho$stature
                , male   = dwho$male
                , source = "CDC-WHO"
  )

# should return a vector the same length as the input
stopifnot(identical(length(p01_who_test), nrow(dwho)))
stopifnot(identical(length(p01_cdc_test), nrow(dwho)))
stopifnot(identical(length(p01_cdc_who_test), nrow(dwho)))

# 0.001 to three digits, even 4 digits, for the WHO data
stopifnot(identical(round(p01_who_test, digits = 2), rep(0.00, length = nrow(dwho))))
stopifnot(identical(round(p01_who_test, digits = 3), rep(0.001, length = nrow(dwho))))
stopifnot(identical(round(p01_who_test, digits = 4), rep(0.001, length = nrow(dwho))))

stopifnot(isTRUE(all(is.na( p01_cdc_test[dwho$age < 24] ))))
stopifnot(isTRUE(all(!is.na( p01_cdc_test[dwho$age >= 24] ))))

# the p01_cdc_who_test result should be the who value were p01_cdc_test is NA
# and cdc value otherwise
expected <- p01_cdc_test
expected[is.na(p01_cdc_test)] <- p01_who_test[is.na(p01_cdc_test)]
stopifnot(isTRUE(all.equal(p01_cdc_who_test, expected)))

# do the same test for the first quantile
p25_who_test <-
  p_weight_for_stature(  q      = dwho$P25
                , stature    = dwho$stature
                , male   = dwho$male
                , source = "WHO"
  )
p25_cdc_test <-
  p_weight_for_stature(  q      = dwho$P25
                , stature    = dwho$stature
                , male   = dwho$male
                , source = "CDC"
  )
p25_cdc_who_test <-
  p_weight_for_stature(  q      = dwho$P25
                , stature    = dwho$stature
                , male   = dwho$male
                , source = "CDC-WHO"
  )

stopifnot(identical(length(p25_who_test), nrow(dwho)))
stopifnot(identical(length(p25_cdc_test), nrow(dwho)))
stopifnot(identical(length(p25_cdc_who_test), nrow(dwho)))

stopifnot(identical(round(p25_who_test, digits = 3), rep(0.25, length = nrow(dwho))))
stopifnot(identical(round(p25_cdc_test, digits = 3), rep(0.25, length = nrow(dwho))))

stopifnot(isTRUE(all(is.na( p25_cdc_test[dwho$age < 24] ))))
stopifnot(isTRUE(all(!is.na( p25_cdc_test[dwho$age >= 24] ))))

expected <- p25_cdc_test
expected[is.na(p25_cdc_test)] <- p25_who_test[is.na(p25_cdc_test)]
stopifnot(isTRUE(all.equal(p25_cdc_who_test, expected)))

# testing on the CDC data
p25_who_test <-
  p_weight_for_stature(  q      = dcdc$P25
                , stature    = dcdc$stature
                , male   = dcdc$male
                , source = "WHO"
  )
p25_cdc_test <-
  p_weight_for_stature(  q      = dcdc$P25
                , stature    = dcdc$stature
                , male   = dcdc$male
                , source = "CDC"
  )
p25_cdc_who_test <-
  p_weight_for_stature(  q      = dcdc$P25
                , stature    = dcdc$stature
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
##           Testing of quantile, distribution, and zscore methods            ##
set.seed(42)
statures <- runif(1000, min = 45.0, max = 121.5)
genders  <- as.integer(runif(1000) < 0.5)
ps       <- runif(1000)

test_df <-
  data.frame(
    stature   = statures
  , male      = genders
  , p         = ps
  , z         = qnorm(ps)
  , q_cdc.who = suppressWarnings(q_weight_for_stature(p = ps, male = genders, stature = statures, source = "CDC-WHO"))
  , q_cdc     = suppressWarnings(q_weight_for_stature(p = ps, male = genders, stature = statures, source = "CDC"))
  , q_who     = suppressWarnings(q_weight_for_stature(p = ps, male = genders, stature = statures, source = "WHO"))
  )

test_df[["p_cdc.who"]] <- suppressWarnings(p_weight_for_stature(q = test_df$q_cdc.who, male = genders, stature = statures, source = "CDC-WHO"))
test_df[["p_cdc"]] <- suppressWarnings(p_weight_for_stature(q = test_df$q_cdc, male = genders, stature = statures, source = "CDC"))
test_df[["p_who"]] <- suppressWarnings(p_weight_for_stature(q = test_df$q_who, male = genders, stature = statures, source = "WHO"))

test_df[["z_cdc.who"]] <- suppressWarnings(z_weight_for_stature(q = test_df$q_cdc.who, male = genders, stature = statures, source = "CDC-WHO"))
test_df[["z_cdc"]] <- suppressWarnings(z_weight_for_stature(q = test_df$q_cdc, male = genders, stature = statures, source = "CDC"))
test_df[["z_who"]] <- suppressWarnings(z_weight_for_stature(q = test_df$q_who, male = genders, stature = statures, source = "WHO"))

head(test_df)

stopifnot(with(test_df, isTRUE(all.equal(p, p_cdc))))
stopifnot(with(test_df, isTRUE(all.equal(p, p_who))))
stopifnot(with(test_df, isTRUE(all.equal(p, p_cdc.who))))

stopifnot(with(test_df[!is.na(test_df$z_cdc), ], isTRUE(all.equal(z, z_cdc))))
stopifnot(with(test_df[!is.na(test_df$z_who), ], isTRUE(all.equal(z, z_who))))
stopifnot(with(test_df[!is.na(test_df$z_cdc.who), ], isTRUE(all.equal(z, z_cdc.who))))

################################################################################
##                               End of center                                ##
################################################################################
