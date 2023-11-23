library(pedbp)

l <- -0.1600954
m <-  9.476500305
s <-  0.11218624

stopifnot(isTRUE(
  all.equal(
    round(qlms(x = 0.05, l = l, m = m, s = s), digits = 2)
    ,
    7.9
  )
))

# What percentile is the value 8.2?
stopifnot(isTRUE(
  all.equal(
    round(plms(x = 8.2, l = l, m = m, s = s), digits = 3)
    ,
    0.096
  )
))

# What is the standard score for the value 8.2
stopifnot(isTRUE(
  all.equal(
    zlms(x = 8.2, l = l, m = m, s = s)
    , -1.304701
    , tol = 1e-6
  )
))

stopifnot(isTRUE(
  all.equal(
    zlms(x = 8.2, l = l, m = m, s = s)
    ,
    qnorm(plms(x = 8.2, l = l, m = m, s = s))
  )
))

# for multiple inputs
ps <- seq(0.05, 0.95, by = 0.05)
qs <- qlms(x = ps, l = l, m = m, s = s)
stopifnot(isTRUE(all.equal(plms(qs, l, m, s), ps)))
stopifnot(isTRUE(all.equal(zlms(x = qs, l = l, m = m, s = s), qnorm(ps))))

# one quantile or percentile for different LMS values:
d <- pedbp:::lms_data[["bmi_for_age"]][["CDC"]][["Female"]]
stopifnot(isTRUE(all.equal(qlms(x = 0.25, l = d$L, m = d$M, s = d$S), d$P25)))
stopifnot(isTRUE(all.equal(plms(x = d$P25, l = d$L, m = d$M, s = d$S), rep(0.25, nrow(d)))))

# make sure that the zeros are dealt with
stopifnot(isTRUE(
  all.equal(
    qlms(x = 0.25, l = 0, m = 4, s = 0.1)
    ,
    exp(0.1 * qnorm(0.25)) * 4
  )
))

stopifnot(isTRUE(
  all.equal(
    zlms(x = 4.3, l = 0, m = 4, s = 0.1)
    ,
    log(4.3 / 4) / 0.1
  )
))


################################################################################
##                                End of File                                 ##
################################################################################
