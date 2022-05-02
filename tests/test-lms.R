library(pedbp)

d <- pedbp:::cdc_lms_data


# Testing p_bmi_for_age {{{
test_bmi_p03 <-
  mapply(p_bmi_for_age
         , q    = d[d$set == "bmi_for_age", "p03"]
         , age  = d[d$set == "bmi_for_age", "age"]
         , male = d[d$set == "bmi_for_age", "male"]
         )

test_bmi_p05 <-
  mapply(p_bmi_for_age
         , q    = d[d$set == "bmi_for_age", "p05"]
         , age  = d[d$set == "bmi_for_age", "age"]
         , male = d[d$set == "bmi_for_age", "male"]
         )
test_bmi_p10 <-
  mapply(p_bmi_for_age
         , q    = d[d$set == "bmi_for_age", "p10"]
         , age  = d[d$set == "bmi_for_age", "age"]
         , male = d[d$set == "bmi_for_age", "male"]
         )

test_bmi_p25 <-
  mapply(p_bmi_for_age
         , q    = d[d$set == "bmi_for_age", "p25"]
         , age  = d[d$set == "bmi_for_age", "age"]
         , male = d[d$set == "bmi_for_age", "male"]
         )

test_bmi_p50 <-
  mapply(p_bmi_for_age
         , q    = d[d$set == "bmi_for_age", "p50"]
         , age  = d[d$set == "bmi_for_age", "age"]
         , male = d[d$set == "bmi_for_age", "male"]
         )

test_bmi_p75 <-
  mapply(p_bmi_for_age
         , q    = d[d$set == "bmi_for_age", "p75"]
         , age  = d[d$set == "bmi_for_age", "age"]
         , male = d[d$set == "bmi_for_age", "male"]
         )

test_bmi_p85 <-
  mapply(p_bmi_for_age
         , q    = d[d$set == "bmi_for_age", "p85"]
         , age  = d[d$set == "bmi_for_age", "age"]
         , male = d[d$set == "bmi_for_age", "male"]
         )

test_bmi_p90 <-
  mapply(p_bmi_for_age
         , q    = d[d$set == "bmi_for_age", "p90"]
         , age  = d[d$set == "bmi_for_age", "age"]
         , male = d[d$set == "bmi_for_age", "male"]
         )

test_bmi_p95 <-
  mapply(p_bmi_for_age
         , q    = d[d$set == "bmi_for_age", "p95"]
         , age  = d[d$set == "bmi_for_age", "age"]
         , male = d[d$set == "bmi_for_age", "male"]
         )

test_bmi_p97 <-
  mapply(p_bmi_for_age
         , q    = d[d$set == "bmi_for_age", "p97"]
         , age  = d[d$set == "bmi_for_age", "age"]
         , male = d[d$set == "bmi_for_age", "male"]
         )


stopifnot(all(round(test_bmi_p03, 2) == 0.03))
stopifnot(all(round(test_bmi_p05, 2) == 0.05))
stopifnot(all(round(test_bmi_p10, 2) == 0.10))
stopifnot(all(round(test_bmi_p25, 2) == 0.25))
stopifnot(all(round(test_bmi_p50, 2) == 0.50))
stopifnot(all(round(test_bmi_p75, 2) == 0.75))
stopifnot(all(round(test_bmi_p85, 2) == 0.85))
stopifnot(all(round(test_bmi_p90, 2) == 0.90))
stopifnot(all(round(test_bmi_p95, 2) == 0.95))
stopifnot(all(round(test_bmi_p97, 2) == 0.97))

# }}}

# Tesing q_bmi_for_age {{{
test_bmi_q03 <-
  mapply(q_bmi_for_age
         , p    = c(0.03)
         , age  = d[d$set == "bmi_for_age", "age"]
         , male = d[d$set == "bmi_for_age", "male"]
         )

test_bmi_q05 <-
  mapply(q_bmi_for_age
         , p    = c(0.05)
         , age  = d[d$set == "bmi_for_age", "age"]
         , male = d[d$set == "bmi_for_age", "male"]
         )

test_bmi_q10 <-
  mapply(q_bmi_for_age
         , p    = c(0.10)
         , age  = d[d$set == "bmi_for_age", "age"]
         , male = d[d$set == "bmi_for_age", "male"]
         )

test_bmi_q25 <-
  mapply(q_bmi_for_age
         , p    = c(0.25)#, 0.25, 0.10, 0.25, 0.50, 0.75, 0.85, 0.90, 0.95, 0.97)
         , age  = d[d$set == "bmi_for_age", "age"]
         , male = d[d$set == "bmi_for_age", "male"]
         )

test_bmi_q50 <-
  mapply(q_bmi_for_age
         , p    = c(0.50)#, 0.50, 0.10, 0.25, 0.50, 0.75, 0.85, 0.90, 0.95, 0.97)
         , age  = d[d$set == "bmi_for_age", "age"]
         , male = d[d$set == "bmi_for_age", "male"]
         )

test_bmi_q75 <-
  mapply(q_bmi_for_age
         , p    = c(0.75)#, 0.75, 0.10, 0.25, 0.50, 0.75, 0.85, 0.90, 0.95, 0.97)
         , age  = d[d$set == "bmi_for_age", "age"]
         , male = d[d$set == "bmi_for_age", "male"]
         )

test_bmi_q85 <-
  mapply(q_bmi_for_age
         , p    = c(0.85)#, 0.85, 0.10, 0.25, 0.50, 0.75, 0.85, 0.90, 0.95, 0.97)
         , age  = d[d$set == "bmi_for_age", "age"]
         , male = d[d$set == "bmi_for_age", "male"]
         )

test_bmi_q90 <-
  mapply(q_bmi_for_age
         , p    = c(0.90)#, 0.90, 0.10, 0.25, 0.50, 0.75, 0.85, 0.90, 0.95, 0.97)
         , age  = d[d$set == "bmi_for_age", "age"]
         , male = d[d$set == "bmi_for_age", "male"]
         )

test_bmi_q95 <-
  mapply(q_bmi_for_age
         , p    = c(0.95)#, 0.95, 0.10, 0.25, 0.50, 0.75, 0.85, 0.90, 0.95, 0.97)
         , age  = d[d$set == "bmi_for_age", "age"]
         , male = d[d$set == "bmi_for_age", "male"]
         )

test_bmi_q97 <-
  mapply(q_bmi_for_age
         , p    = c(0.97)#, 0.97, 0.10, 0.25, 0.50, 0.75, 0.85, 0.90, 0.95, 0.97)
         , age  = d[d$set == "bmi_for_age", "age"]
         , male = d[d$set == "bmi_for_age", "male"]
         )

stopifnot(all.equal(test_bmi_q03, d[d$set == "bmi_for_age", "p03"]))
stopifnot(all.equal(test_bmi_q05, d[d$set == "bmi_for_age", "p05"]))
stopifnot(all.equal(test_bmi_q10, d[d$set == "bmi_for_age", "p10"]))
stopifnot(all.equal(test_bmi_q25, d[d$set == "bmi_for_age", "p25"]))
stopifnot(all.equal(test_bmi_q50, d[d$set == "bmi_for_age", "p50"]))
stopifnot(all.equal(test_bmi_q75, d[d$set == "bmi_for_age", "p75"]))
stopifnot(all.equal(test_bmi_q85, d[d$set == "bmi_for_age", "p85"]))
stopifnot(all.equal(test_bmi_q90, d[d$set == "bmi_for_age", "p90"]))
stopifnot(all.equal(test_bmi_q95, d[d$set == "bmi_for_age", "p95"]))
stopifnot(all.equal(test_bmi_q97, d[d$set == "bmi_for_age", "p97"]))

# end testing q_bmi_for_age }}}

# Testing z_bmi_for_age {{{
test_bmi_z03 <-
  mapply(z_bmi_for_age
         , q    = d[d$set == "bmi_for_age", "p03"]
         , age  = d[d$set == "bmi_for_age", "age"]
         , male = d[d$set == "bmi_for_age", "male"]
         )

test_bmi_z05 <-
  mapply(z_bmi_for_age
         , q    = d[d$set == "bmi_for_age", "p05"]
         , age  = d[d$set == "bmi_for_age", "age"]
         , male = d[d$set == "bmi_for_age", "male"]
         )

test_bmi_z10 <-
  mapply(z_bmi_for_age
         , q    = d[d$set == "bmi_for_age", "p10"]
         , age  = d[d$set == "bmi_for_age", "age"]
         , male = d[d$set == "bmi_for_age", "male"]
         )

test_bmi_z25 <-
  mapply(z_bmi_for_age
         , q    = d[d$set == "bmi_for_age", "p25"]
         , age  = d[d$set == "bmi_for_age", "age"]
         , male = d[d$set == "bmi_for_age", "male"]
         )

test_bmi_z50 <-
  mapply(z_bmi_for_age
         , q    = d[d$set == "bmi_for_age", "p50"]
         , age  = d[d$set == "bmi_for_age", "age"]
         , male = d[d$set == "bmi_for_age", "male"]
         )

test_bmi_z75 <-
  mapply(z_bmi_for_age
         , q    = d[d$set == "bmi_for_age", "p75"]
         , age  = d[d$set == "bmi_for_age", "age"]
         , male = d[d$set == "bmi_for_age", "male"]
         )

test_bmi_z85 <-
  mapply(z_bmi_for_age
         , q    = d[d$set == "bmi_for_age", "p85"]
         , age  = d[d$set == "bmi_for_age", "age"]
         , male = d[d$set == "bmi_for_age", "male"]
         )

test_bmi_z90 <-
  mapply(z_bmi_for_age
         , q    = d[d$set == "bmi_for_age", "p90"]
         , age  = d[d$set == "bmi_for_age", "age"]
         , male = d[d$set == "bmi_for_age", "male"]
         )

test_bmi_z95 <-
  mapply(z_bmi_for_age
         , q    = d[d$set == "bmi_for_age", "p95"]
         , age  = d[d$set == "bmi_for_age", "age"]
         , male = d[d$set == "bmi_for_age", "male"]
         )

test_bmi_z97 <-
  mapply(z_bmi_for_age
         , q    = d[d$set == "bmi_for_age", "p97"]
         , age  = d[d$set == "bmi_for_age", "age"]
         , male = d[d$set == "bmi_for_age", "male"]
         )


stopifnot(all.equal(test_bmi_z03, rep(qnorm(0.03), length(test_bmi_z03))))
stopifnot(all.equal(test_bmi_z05, rep(qnorm(0.05), length(test_bmi_z05))))
stopifnot(all.equal(test_bmi_z10, rep(qnorm(0.10), length(test_bmi_z10))))
stopifnot(all.equal(test_bmi_z25, rep(qnorm(0.25), length(test_bmi_z25))))
stopifnot(all.equal(test_bmi_z50, rep(qnorm(0.50), length(test_bmi_z50))))
stopifnot(all.equal(test_bmi_z75, rep(qnorm(0.75), length(test_bmi_z75))))
stopifnot(all.equal(test_bmi_z85, rep(qnorm(0.85), length(test_bmi_z85))))
stopifnot(all.equal(test_bmi_z90, rep(qnorm(0.90), length(test_bmi_z90))))
stopifnot(all.equal(test_bmi_z95, rep(qnorm(0.95), length(test_bmi_z95))))
stopifnot(all.equal(test_bmi_z97, rep(qnorm(0.97), length(test_bmi_z97))))

# }}}

# Testing p_length_for_age_inf {{{
test_length_p03 <-
  mapply(p_length_for_age_inf
         , q    = d[d$set == "length_for_age_inf", "p03"]
         , age  = d[d$set == "length_for_age_inf", "age"]
         , male = d[d$set == "length_for_age_inf", "male"]
         )

test_length_p05 <-
  mapply(p_length_for_age_inf
         , q    = d[d$set == "length_for_age_inf", "p05"]
         , age  = d[d$set == "length_for_age_inf", "age"]
         , male = d[d$set == "length_for_age_inf", "male"]
         )
test_length_p10 <-
  mapply(p_length_for_age_inf
         , q    = d[d$set == "length_for_age_inf", "p10"]
         , age  = d[d$set == "length_for_age_inf", "age"]
         , male = d[d$set == "length_for_age_inf", "male"]
         )

test_length_p25 <-
  mapply(p_length_for_age_inf
         , q    = d[d$set == "length_for_age_inf", "p25"]
         , age  = d[d$set == "length_for_age_inf", "age"]
         , male = d[d$set == "length_for_age_inf", "male"]
         )

test_length_p50 <-
  mapply(p_length_for_age_inf
         , q    = d[d$set == "length_for_age_inf", "p50"]
         , age  = d[d$set == "length_for_age_inf", "age"]
         , male = d[d$set == "length_for_age_inf", "male"]
         )

test_length_p75 <-
  mapply(p_length_for_age_inf
         , q    = d[d$set == "length_for_age_inf", "p75"]
         , age  = d[d$set == "length_for_age_inf", "age"]
         , male = d[d$set == "length_for_age_inf", "male"]
         )

test_length_p85 <-
  mapply(p_length_for_age_inf
         , q    = d[d$set == "length_for_age_inf", "p85"]
         , age  = d[d$set == "length_for_age_inf", "age"]
         , male = d[d$set == "length_for_age_inf", "male"]
         )

test_length_p90 <-
  mapply(p_length_for_age_inf
         , q    = d[d$set == "length_for_age_inf", "p90"]
         , age  = d[d$set == "length_for_age_inf", "age"]
         , male = d[d$set == "length_for_age_inf", "male"]
         )

test_length_p95 <-
  mapply(p_length_for_age_inf
         , q    = d[d$set == "length_for_age_inf", "p95"]
         , age  = d[d$set == "length_for_age_inf", "age"]
         , male = d[d$set == "length_for_age_inf", "male"]
         )

test_length_p97 <-
  mapply(p_length_for_age_inf
         , q    = d[d$set == "length_for_age_inf", "p97"]
         , age  = d[d$set == "length_for_age_inf", "age"]
         , male = d[d$set == "length_for_age_inf", "male"]
         )


stopifnot(all(round(test_length_p03, 2) == 0.03))
stopifnot(all(round(test_length_p05, 2) == 0.05))
stopifnot(all(round(test_length_p10, 2) == 0.10))
stopifnot(all(round(test_length_p25, 2) == 0.25))
stopifnot(all(round(test_length_p50, 2) == 0.50))
stopifnot(all(round(test_length_p75, 2) == 0.75))
# stopifnot(all(round(test_length_p85, 2) == 0.85))
stopifnot(all(is.na(test_length_p85)))
stopifnot(all(round(test_length_p90, 2) == 0.90))
stopifnot(all(round(test_length_p95, 2) == 0.95))
stopifnot(all(round(test_length_p97, 2) == 0.97))

# }}}}

# Testing p_height_for_age {{{
test_stature_p03 <-
  mapply(p_stature_for_age
         , q    = d[d$set == "stature_for_age", "p03"]
         , age  = d[d$set == "stature_for_age", "age"]
         , male = d[d$set == "stature_for_age", "male"]
         )

test_stature_p05 <-
  mapply(p_stature_for_age
         , q    = d[d$set == "stature_for_age", "p05"]
         , age  = d[d$set == "stature_for_age", "age"]
         , male = d[d$set == "stature_for_age", "male"]
         )
test_stature_p10 <-
  mapply(p_stature_for_age
         , q    = d[d$set == "stature_for_age", "p10"]
         , age  = d[d$set == "stature_for_age", "age"]
         , male = d[d$set == "stature_for_age", "male"]
         )

test_stature_p25 <-
  mapply(p_stature_for_age
         , q    = d[d$set == "stature_for_age", "p25"]
         , age  = d[d$set == "stature_for_age", "age"]
         , male = d[d$set == "stature_for_age", "male"]
         )

test_stature_p50 <-
  mapply(p_stature_for_age
         , q    = d[d$set == "stature_for_age", "p50"]
         , age  = d[d$set == "stature_for_age", "age"]
         , male = d[d$set == "stature_for_age", "male"]
         )

test_stature_p75 <-
  mapply(p_stature_for_age
         , q    = d[d$set == "stature_for_age", "p75"]
         , age  = d[d$set == "stature_for_age", "age"]
         , male = d[d$set == "stature_for_age", "male"]
         )

test_stature_p85 <-
  mapply(p_stature_for_age
         , q    = d[d$set == "stature_for_age", "p85"]
         , age  = d[d$set == "stature_for_age", "age"]
         , male = d[d$set == "stature_for_age", "male"]
         )

test_stature_p90 <-
  mapply(p_stature_for_age
         , q    = d[d$set == "stature_for_age", "p90"]
         , age  = d[d$set == "stature_for_age", "age"]
         , male = d[d$set == "stature_for_age", "male"]
         )

test_stature_p95 <-
  mapply(p_stature_for_age
         , q    = d[d$set == "stature_for_age", "p95"]
         , age  = d[d$set == "stature_for_age", "age"]
         , male = d[d$set == "stature_for_age", "male"]
         )

test_stature_p97 <-
  mapply(p_stature_for_age
         , q    = d[d$set == "stature_for_age", "p97"]
         , age  = d[d$set == "stature_for_age", "age"]
         , male = d[d$set == "stature_for_age", "male"]
         )


stopifnot(all(round(test_stature_p03, 2) == 0.03))
stopifnot(all(round(test_stature_p05, 2) == 0.05))
stopifnot(all(round(test_stature_p10, 2) == 0.10))
stopifnot(all(round(test_stature_p25, 2) == 0.25))
stopifnot(all(round(test_stature_p50, 2) == 0.50))
stopifnot(all(round(test_stature_p75, 2) == 0.75))
#stopifnot(all(round(test_stature_p85, 2) == 0.85))
stopifnot(all(is.na(test_stature_p85)))
stopifnot(all(round(test_stature_p90, 2) == 0.90))
stopifnot(all(round(test_stature_p95, 2) == 0.95))
stopifnot(all(round(test_stature_p97, 2) == 0.97))

# }}}}

