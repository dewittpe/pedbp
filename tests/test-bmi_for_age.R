library(pedbp)

d <- pedbp:::lms_data
d <- d[d$metric == "bmi_for_age", ]

dwho <- Filter(function(x) {!all(is.na(x))}, d[d$source == "WHO", ])
dcdc <- Filter(function(x) {!all(is.na(x))}, d[d$source == "CDC-2000", ])

################################################################################
##                           Testing p_bmi_for_age                            ##

debug(pvsd)
debug(v_get_lms)
debug(get_lms)

undebug(pvsd)
undebug(v_get_lms)
undebug(get_lms)

pvsd(x = dwho$P10
     , male = dwho$male
     , age = dwho$age
     , source = "WHO"
     , type = "distribution"
     ) |> round(digits = 3) 

pvsd(x = dcdc$P10
     , male = dcdc$male
     , age = dcdc$age
     , source = "CDC-2000"
     , type = "distribution"
     ) |> round(digits = 3) 

P01_test <- 
  p_bmi_for_age(q = dwho$P01
                , age = dwho$age
                , male = 1#d$male
                , source = "WHO"
  )


test_p03 <-
  p_bmi_for_age(
           q    = d[d$set == "bmi_for_age", "p03"]
         , age  = d[d$set == "bmi_for_age", "age"]
         , male = d[d$set == "bmi_for_age", "male"]
         )

test_p05 <-
  p_bmi_for_age(
           q    = d[d$set == "bmi_for_age", "p05"]
         , age  = d[d$set == "bmi_for_age", "age"]
         , male = d[d$set == "bmi_for_age", "male"]
         )

test_p10 <-
  p_bmi_for_age(
           q    = d[d$set == "bmi_for_age", "p10"]
         , age  = d[d$set == "bmi_for_age", "age"]
         , male = d[d$set == "bmi_for_age", "male"]
         )

test_p25 <-
  p_bmi_for_age(
           q    = d[d$set == "bmi_for_age", "p25"]
         , age  = d[d$set == "bmi_for_age", "age"]
         , male = d[d$set == "bmi_for_age", "male"]
         )

test_p50 <-
  p_bmi_for_age(
           q    = d[d$set == "bmi_for_age", "p50"]
         , age  = d[d$set == "bmi_for_age", "age"]
         , male = d[d$set == "bmi_for_age", "male"]
         )

test_p75 <-
  p_bmi_for_age(
           q    = d[d$set == "bmi_for_age", "p75"]
         , age  = d[d$set == "bmi_for_age", "age"]
         , male = d[d$set == "bmi_for_age", "male"]
         )

test_p85 <-
  p_bmi_for_age(
           q    = d[d$set == "bmi_for_age", "p85"]
         , age  = d[d$set == "bmi_for_age", "age"]
         , male = d[d$set == "bmi_for_age", "male"]
         )

test_p90 <-
  p_bmi_for_age(
           q    = d[d$set == "bmi_for_age", "p90"]
         , age  = d[d$set == "bmi_for_age", "age"]
         , male = d[d$set == "bmi_for_age", "male"]
         )

test_p95 <-
  p_bmi_for_age(
           q    = d[d$set == "bmi_for_age", "p95"]
         , age  = d[d$set == "bmi_for_age", "age"]
         , male = d[d$set == "bmi_for_age", "male"]
         )

test_p97 <-
  p_bmi_for_age(
           q    = d[d$set == "bmi_for_age", "p97"]
         , age  = d[d$set == "bmi_for_age", "age"]
         , male = d[d$set == "bmi_for_age", "male"]
         )

stopifnot(identical(round(test_p03, 2), rep(0.03, 438)))
stopifnot(identical(round(test_p05, 2), rep(0.05, 438)))
stopifnot(identical(round(test_p10, 2), rep(0.10, 438)))
stopifnot(identical(round(test_p25, 2), rep(0.25, 438)))
stopifnot(identical(round(test_p50, 2), rep(0.50, 438)))
stopifnot(identical(round(test_p75, 2), rep(0.75, 438)))
stopifnot(identical(round(test_p85, 2), rep(0.85, 438)))
stopifnot(identical(round(test_p90, 2), rep(0.90, 438)))
stopifnot(identical(round(test_p95, 2), rep(0.95, 438)))
stopifnot(identical(round(test_p97, 2), rep(0.97, 438)))

################################################################################
##                            Tesing q_bmi_for_age                            ##

test_q03 <-
  q_bmi_for_age(
           p    = c(0.03)
         , age  = d[d$set == "bmi_for_age", "age"]
         , male = d[d$set == "bmi_for_age", "male"]
         )

test_q05 <-
  q_bmi_for_age(
           p    = c(0.05)
         , age  = d[d$set == "bmi_for_age", "age"]
         , male = d[d$set == "bmi_for_age", "male"]
         )

test_q10 <-
  q_bmi_for_age(
           p    = c(0.10)
         , age  = d[d$set == "bmi_for_age", "age"]
         , male = d[d$set == "bmi_for_age", "male"]
         )

test_q25 <-
  q_bmi_for_age(
           p    = c(0.25)
         , age  = d[d$set == "bmi_for_age", "age"]
         , male = d[d$set == "bmi_for_age", "male"]
         )

test_q50 <-
  q_bmi_for_age(
           p    = c(0.50)
         , age  = d[d$set == "bmi_for_age", "age"]
         , male = d[d$set == "bmi_for_age", "male"]
         )

test_q75 <-
  q_bmi_for_age(
           p    = c(0.75)
         , age  = d[d$set == "bmi_for_age", "age"]
         , male = d[d$set == "bmi_for_age", "male"]
         )

test_q85 <-
  q_bmi_for_age(
           p    = c(0.85)
         , age  = d[d$set == "bmi_for_age", "age"]
         , male = d[d$set == "bmi_for_age", "male"]
         )

test_q90 <-
  q_bmi_for_age(
           p    = c(0.90)
         , age  = d[d$set == "bmi_for_age", "age"]
         , male = d[d$set == "bmi_for_age", "male"]
         )

test_q95 <-
  q_bmi_for_age(
           p    = c(0.95)
         , age  = d[d$set == "bmi_for_age", "age"]
         , male = d[d$set == "bmi_for_age", "male"]
         )

test_q97 <-
  q_bmi_for_age(
           p    = c(0.97)
         , age  = d[d$set == "bmi_for_age", "age"]
         , male = d[d$set == "bmi_for_age", "male"]
         )

stopifnot(all.equal(test_q03, d[d$set == "bmi_for_age", "p03"]))
stopifnot(all.equal(test_q05, d[d$set == "bmi_for_age", "p05"]))
stopifnot(all.equal(test_q10, d[d$set == "bmi_for_age", "p10"]))
stopifnot(all.equal(test_q25, d[d$set == "bmi_for_age", "p25"]))
stopifnot(all.equal(test_q50, d[d$set == "bmi_for_age", "p50"]))
stopifnot(all.equal(test_q75, d[d$set == "bmi_for_age", "p75"]))
stopifnot(all.equal(test_q85, d[d$set == "bmi_for_age", "p85"]))
stopifnot(all.equal(test_q90, d[d$set == "bmi_for_age", "p90"]))
stopifnot(all.equal(test_q95, d[d$set == "bmi_for_age", "p95"]))
stopifnot(all.equal(test_q97, d[d$set == "bmi_for_age", "p97"]))

# end testing q_bmi_for_age }}}

# Testing z_bmi_for_age {{{
test_z03 <-
  z_bmi_for_age(
           q    = d[d$set == "bmi_for_age", "p03"]
         , age  = d[d$set == "bmi_for_age", "age"]
         , male = d[d$set == "bmi_for_age", "male"]
         )

test_z05 <-
  z_bmi_for_age(
           q    = d[d$set == "bmi_for_age", "p05"]
         , age  = d[d$set == "bmi_for_age", "age"]
         , male = d[d$set == "bmi_for_age", "male"]
         )

test_z10 <-
  z_bmi_for_age(
           q    = d[d$set == "bmi_for_age", "p10"]
         , age  = d[d$set == "bmi_for_age", "age"]
         , male = d[d$set == "bmi_for_age", "male"]
         )

test_z25 <-
  z_bmi_for_age(
           q    = d[d$set == "bmi_for_age", "p25"]
         , age  = d[d$set == "bmi_for_age", "age"]
         , male = d[d$set == "bmi_for_age", "male"]
         )

test_z50 <-
  z_bmi_for_age(
           q    = d[d$set == "bmi_for_age", "p50"]
         , age  = d[d$set == "bmi_for_age", "age"]
         , male = d[d$set == "bmi_for_age", "male"]
         )

test_z75 <-
  z_bmi_for_age(
           q    = d[d$set == "bmi_for_age", "p75"]
         , age  = d[d$set == "bmi_for_age", "age"]
         , male = d[d$set == "bmi_for_age", "male"]
         )

test_z85 <-
  z_bmi_for_age(
           q    = d[d$set == "bmi_for_age", "p85"]
         , age  = d[d$set == "bmi_for_age", "age"]
         , male = d[d$set == "bmi_for_age", "male"]
         )

test_z90 <-
  z_bmi_for_age(
           q    = d[d$set == "bmi_for_age", "p90"]
         , age  = d[d$set == "bmi_for_age", "age"]
         , male = d[d$set == "bmi_for_age", "male"]
         )

test_z95 <-
  z_bmi_for_age(
           q    = d[d$set == "bmi_for_age", "p95"]
         , age  = d[d$set == "bmi_for_age", "age"]
         , male = d[d$set == "bmi_for_age", "male"]
         )

test_z97 <-
  z_bmi_for_age(
           q    = d[d$set == "bmi_for_age", "p97"]
         , age  = d[d$set == "bmi_for_age", "age"]
         , male = d[d$set == "bmi_for_age", "male"]
         )


stopifnot(all.equal(test_z03, rep(qnorm(0.03), length(test_z03))))
stopifnot(all.equal(test_z05, rep(qnorm(0.05), length(test_z05))))
stopifnot(all.equal(test_z10, rep(qnorm(0.10), length(test_z10))))
stopifnot(all.equal(test_z25, rep(qnorm(0.25), length(test_z25))))
stopifnot(all.equal(test_z50, rep(qnorm(0.50), length(test_z50))))
stopifnot(all.equal(test_z75, rep(qnorm(0.75), length(test_z75))))
stopifnot(all.equal(test_z85, rep(qnorm(0.85), length(test_z85))))
stopifnot(all.equal(test_z90, rep(qnorm(0.90), length(test_z90))))
stopifnot(all.equal(test_z95, rep(qnorm(0.95), length(test_z95))))
stopifnot(all.equal(test_z97, rep(qnorm(0.97), length(test_z97))))

# }}}

