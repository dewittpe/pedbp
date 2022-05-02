library(pedbp)

d <- pedbp:::cdc_lms_data


# Testing p_length_for_age_inf {{{
test_p03 <-
  mapply(p_length_for_age_inf
         , q    = d[d$set == "length_for_age_inf", "p03"]
         , age  = d[d$set == "length_for_age_inf", "age"]
         , male = d[d$set == "length_for_age_inf", "male"]
         )

test_p05 <-
  mapply(p_length_for_age_inf
         , q    = d[d$set == "length_for_age_inf", "p05"]
         , age  = d[d$set == "length_for_age_inf", "age"]
         , male = d[d$set == "length_for_age_inf", "male"]
         )
test_p10 <-
  mapply(p_length_for_age_inf
         , q    = d[d$set == "length_for_age_inf", "p10"]
         , age  = d[d$set == "length_for_age_inf", "age"]
         , male = d[d$set == "length_for_age_inf", "male"]
         )

test_p25 <-
  mapply(p_length_for_age_inf
         , q    = d[d$set == "length_for_age_inf", "p25"]
         , age  = d[d$set == "length_for_age_inf", "age"]
         , male = d[d$set == "length_for_age_inf", "male"]
         )

test_p50 <-
  mapply(p_length_for_age_inf
         , q    = d[d$set == "length_for_age_inf", "p50"]
         , age  = d[d$set == "length_for_age_inf", "age"]
         , male = d[d$set == "length_for_age_inf", "male"]
         )

test_p75 <-
  mapply(p_length_for_age_inf
         , q    = d[d$set == "length_for_age_inf", "p75"]
         , age  = d[d$set == "length_for_age_inf", "age"]
         , male = d[d$set == "length_for_age_inf", "male"]
         )

test_p85 <-
  mapply(p_length_for_age_inf
         , q    = d[d$set == "length_for_age_inf", "p85"]
         , age  = d[d$set == "length_for_age_inf", "age"]
         , male = d[d$set == "length_for_age_inf", "male"]
         )

test_p90 <-
  mapply(p_length_for_age_inf
         , q    = d[d$set == "length_for_age_inf", "p90"]
         , age  = d[d$set == "length_for_age_inf", "age"]
         , male = d[d$set == "length_for_age_inf", "male"]
         )

test_p95 <-
  mapply(p_length_for_age_inf
         , q    = d[d$set == "length_for_age_inf", "p95"]
         , age  = d[d$set == "length_for_age_inf", "age"]
         , male = d[d$set == "length_for_age_inf", "male"]
         )

test_p97 <-
  mapply(p_length_for_age_inf
         , q    = d[d$set == "length_for_age_inf", "p97"]
         , age  = d[d$set == "length_for_age_inf", "age"]
         , male = d[d$set == "length_for_age_inf", "male"]
         )


stopifnot(all(round(test_p03, 2) == 0.03))
stopifnot(all(round(test_p05, 2) == 0.05))
stopifnot(all(round(test_p10, 2) == 0.10))
stopifnot(all(round(test_p25, 2) == 0.25))
stopifnot(all(round(test_p50, 2) == 0.50))
stopifnot(all(round(test_p75, 2) == 0.75))
# stopifnot(all(round(test_p85, 2) == 0.85))
stopifnot(all(is.na(test_p85)))
stopifnot(all(round(test_p90, 2) == 0.90))
stopifnot(all(round(test_p95, 2) == 0.95))
stopifnot(all(round(test_p97, 2) == 0.97))

# }}}

# Tesing q_length_for_age_inf {{{
test_q03 <-
  mapply(q_length_for_age_inf
         , p    = c(0.03)
         , age  = d[d$set == "length_for_age_inf", "age"]
         , male = d[d$set == "length_for_age_inf", "male"]
         )

test_q05 <-
  mapply(q_length_for_age_inf
         , p    = c(0.05)
         , age  = d[d$set == "length_for_age_inf", "age"]
         , male = d[d$set == "length_for_age_inf", "male"]
         )

test_q10 <-
  mapply(q_length_for_age_inf
         , p    = c(0.10)
         , age  = d[d$set == "length_for_age_inf", "age"]
         , male = d[d$set == "length_for_age_inf", "male"]
         )

test_q25 <-
  mapply(q_length_for_age_inf
         , p    = c(0.25)
         , age  = d[d$set == "length_for_age_inf", "age"]
         , male = d[d$set == "length_for_age_inf", "male"]
         )

test_q50 <-
  mapply(q_length_for_age_inf
         , p    = c(0.50)
         , age  = d[d$set == "length_for_age_inf", "age"]
         , male = d[d$set == "length_for_age_inf", "male"]
         )

test_q75 <-
  mapply(q_length_for_age_inf
         , p    = c(0.75)
         , age  = d[d$set == "length_for_age_inf", "age"]
         , male = d[d$set == "length_for_age_inf", "male"]
         )

# no need to test 85th as it is not in the provided data set
stopifnot(all(is.na(d[d$set == "length_for_age_inf", "p85"])))
# test_q85 <-
#   mapply(q_length_for_age_inf
#          , p    = c(0.85)
#          , age  = d[d$set == "length_for_age_inf", "age"]
#          , male = d[d$set == "length_for_age_inf", "male"]
#          )

test_q90 <-
  mapply(q_length_for_age_inf
         , p    = c(0.90)
         , age  = d[d$set == "length_for_age_inf", "age"]
         , male = d[d$set == "length_for_age_inf", "male"]
         )

test_q95 <-
  mapply(q_length_for_age_inf
         , p    = c(0.95)
         , age  = d[d$set == "length_for_age_inf", "age"]
         , male = d[d$set == "length_for_age_inf", "male"]
         )

test_q97 <-
  mapply(q_length_for_age_inf
         , p    = c(0.97)
         , age  = d[d$set == "length_for_age_inf", "age"]
         , male = d[d$set == "length_for_age_inf", "male"]
         )

stopifnot(all.equal(test_q03, d[d$set == "length_for_age_inf", "p03"], tol = 1e-6))
stopifnot(all.equal(test_q05, d[d$set == "length_for_age_inf", "p05"], tol = 1e-6))
stopifnot(all.equal(test_q10, d[d$set == "length_for_age_inf", "p10"], tol = 1e-6))
stopifnot(all.equal(test_q25, d[d$set == "length_for_age_inf", "p25"], tol = 1e-6))
stopifnot(all.equal(test_q50, d[d$set == "length_for_age_inf", "p50"], tol = 1e-6))
stopifnot(all.equal(test_q75, d[d$set == "length_for_age_inf", "p75"], tol = 1e-6))
# stopifnot(all.equal(test_q85, d[d$set == "length_for_age_inf", "p85"], tol = 1e-6))
stopifnot(all.equal(test_q90, d[d$set == "length_for_age_inf", "p90"], tol = 1e-6))
stopifnot(all.equal(test_q95, d[d$set == "length_for_age_inf", "p95"], tol = 1e-6))
stopifnot(all.equal(test_q97, d[d$set == "length_for_age_inf", "p97"], tol = 1e-6))

# end testing q_length_for_age_inf }}}

# Testing z_length_for_age_inf {{{
test_z03 <-
  mapply(z_length_for_age_inf
         , q    = d[d$set == "length_for_age_inf", "p03"]
         , age  = d[d$set == "length_for_age_inf", "age"]
         , male = d[d$set == "length_for_age_inf", "male"]
         )

test_z05 <-
  mapply(z_length_for_age_inf
         , q    = d[d$set == "length_for_age_inf", "p05"]
         , age  = d[d$set == "length_for_age_inf", "age"]
         , male = d[d$set == "length_for_age_inf", "male"]
         )

test_z10 <-
  mapply(z_length_for_age_inf
         , q    = d[d$set == "length_for_age_inf", "p10"]
         , age  = d[d$set == "length_for_age_inf", "age"]
         , male = d[d$set == "length_for_age_inf", "male"]
         )

test_z25 <-
  mapply(z_length_for_age_inf
         , q    = d[d$set == "length_for_age_inf", "p25"]
         , age  = d[d$set == "length_for_age_inf", "age"]
         , male = d[d$set == "length_for_age_inf", "male"]
         )

test_z50 <-
  mapply(z_length_for_age_inf
         , q    = d[d$set == "length_for_age_inf", "p50"]
         , age  = d[d$set == "length_for_age_inf", "age"]
         , male = d[d$set == "length_for_age_inf", "male"]
         )

test_z75 <-
  mapply(z_length_for_age_inf
         , q    = d[d$set == "length_for_age_inf", "p75"]
         , age  = d[d$set == "length_for_age_inf", "age"]
         , male = d[d$set == "length_for_age_inf", "male"]
         )

stopifnot(all(is.na(d[d$set == "length_for_age_inf", "p85"])))
# test_z85 <-
#   mapply(z_length_for_age_inf
#          , q    = d[d$set == "length_for_age_inf", "p85"]
#          , age  = d[d$set == "length_for_age_inf", "age"]
#          , male = d[d$set == "length_for_age_inf", "male"]
#          )

test_z90 <-
  mapply(z_length_for_age_inf
         , q    = d[d$set == "length_for_age_inf", "p90"]
         , age  = d[d$set == "length_for_age_inf", "age"]
         , male = d[d$set == "length_for_age_inf", "male"]
         )

test_z95 <-
  mapply(z_length_for_age_inf
         , q    = d[d$set == "length_for_age_inf", "p95"]
         , age  = d[d$set == "length_for_age_inf", "age"]
         , male = d[d$set == "length_for_age_inf", "male"]
         )

test_z97 <-
  mapply(z_length_for_age_inf
         , q    = d[d$set == "length_for_age_inf", "p97"]
         , age  = d[d$set == "length_for_age_inf", "age"]
         , male = d[d$set == "length_for_age_inf", "male"]
         )


stopifnot(all.equal(test_z03, rep(qnorm(0.03), length(test_z03)), tol = 2.2e-6))
stopifnot(all.equal(test_z05, rep(qnorm(0.05), length(test_z05)), tol = 1e-6))
stopifnot(all.equal(test_z10, rep(qnorm(0.10), length(test_z10)), tol = 1e-6))
stopifnot(all.equal(test_z25, rep(qnorm(0.25), length(test_z25)), tol = 1e-6))
stopifnot(all.equal(test_z50, rep(qnorm(0.50), length(test_z50)), tol = 1e-6))
stopifnot(all.equal(test_z75, rep(qnorm(0.75), length(test_z75)), tol = 1e-6))
# stopifnot(all.equal(test_z85, rep(qnorm(0.85), length(test_z85)), tol = 1e-6))
stopifnot(all.equal(test_z90, rep(qnorm(0.90), length(test_z90)), tol = 1e-6))
stopifnot(all.equal(test_z95, rep(qnorm(0.95), length(test_z95)), tol = 1e-6))
stopifnot(all.equal(test_z97, rep(qnorm(0.97), length(test_z97)), tol = 2.2e-6))

# }}}

