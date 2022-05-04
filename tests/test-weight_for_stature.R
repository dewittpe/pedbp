library(pedbp)

d <- pedbp:::cdc_lms_data


# Testing p_weight_for_stature {{{
test_p03 <-
  p_weight_for_stature(
           q    = d[d$set == "weight_for_stature", "p03"]
         , height  = d[d$set == "weight_for_stature", "height"]
         , male = d[d$set == "weight_for_stature", "male"]
         )

test_p05 <-
  p_weight_for_stature(
           q    = d[d$set == "weight_for_stature", "p05"]
         , height  = d[d$set == "weight_for_stature", "height"]
         , male = d[d$set == "weight_for_stature", "male"]
         )
test_p10 <-
  p_weight_for_stature(
           q    = d[d$set == "weight_for_stature", "p10"]
         , height  = d[d$set == "weight_for_stature", "height"]
         , male = d[d$set == "weight_for_stature", "male"]
         )

test_p25 <-
  p_weight_for_stature(
           q    = d[d$set == "weight_for_stature", "p25"]
         , height  = d[d$set == "weight_for_stature", "height"]
         , male = d[d$set == "weight_for_stature", "male"]
         )

test_p50 <-
  p_weight_for_stature(
           q    = d[d$set == "weight_for_stature", "p50"]
         , height  = d[d$set == "weight_for_stature", "height"]
         , male = d[d$set == "weight_for_stature", "male"]
         )

test_p75 <-
  p_weight_for_stature(
           q    = d[d$set == "weight_for_stature", "p75"]
         , height  = d[d$set == "weight_for_stature", "height"]
         , male = d[d$set == "weight_for_stature", "male"]
         )

test_p85 <-
  p_weight_for_stature(
           q    = d[d$set == "weight_for_stature", "p85"]
         , height  = d[d$set == "weight_for_stature", "height"]
         , male = d[d$set == "weight_for_stature", "male"]
         )

test_p90 <-
  p_weight_for_stature(
           q    = d[d$set == "weight_for_stature", "p90"]
         , height  = d[d$set == "weight_for_stature", "height"]
         , male = d[d$set == "weight_for_stature", "male"]
         )

test_p95 <-
  p_weight_for_stature(
           q    = d[d$set == "weight_for_stature", "p95"]
         , height  = d[d$set == "weight_for_stature", "height"]
         , male = d[d$set == "weight_for_stature", "male"]
         )

test_p97 <-
  p_weight_for_stature(
           q    = d[d$set == "weight_for_stature", "p97"]
         , height  = d[d$set == "weight_for_stature", "height"]
         , male = d[d$set == "weight_for_stature", "male"]
         )


stopifnot(all(round(test_p03, 2) == 0.03))
stopifnot(all(round(test_p05, 2) == 0.05))
stopifnot(all(round(test_p10, 2) == 0.10))
stopifnot(all(round(test_p25, 2) == 0.25))
stopifnot(all(round(test_p50, 2) == 0.50))
stopifnot(all(round(test_p75, 2) == 0.75))
stopifnot(all(round(test_p85, 2) == 0.85))
stopifnot(all(round(test_p90, 2) == 0.90))
stopifnot(all(round(test_p95, 2) == 0.95))
stopifnot(all(round(test_p97, 2) == 0.97))

# }}}

# Tesing q_weight_for_stature {{{
test_q03 <-
  q_weight_for_stature(
           p    = c(0.03)
         , height  = d[d$set == "weight_for_stature", "height"]
         , male = d[d$set == "weight_for_stature", "male"]
         )

test_q05 <-
  q_weight_for_stature(
           p    = c(0.05)
         , height  = d[d$set == "weight_for_stature", "height"]
         , male = d[d$set == "weight_for_stature", "male"]
         )

test_q10 <-
  q_weight_for_stature(
           p    = c(0.10)
         , height  = d[d$set == "weight_for_stature", "height"]
         , male = d[d$set == "weight_for_stature", "male"]
         )

test_q25 <-
  q_weight_for_stature(
           p    = c(0.25)#, 0.25, 0.10, 0.25, 0.50, 0.75, 0.85, 0.90, 0.95, 0.97)
         , height  = d[d$set == "weight_for_stature", "height"]
         , male = d[d$set == "weight_for_stature", "male"]
         )

test_q50 <-
  q_weight_for_stature(
           p    = c(0.50)#, 0.50, 0.10, 0.25, 0.50, 0.75, 0.85, 0.90, 0.95, 0.97)
         , height  = d[d$set == "weight_for_stature", "height"]
         , male = d[d$set == "weight_for_stature", "male"]
         )

test_q75 <-
  q_weight_for_stature(
           p    = c(0.75)#, 0.75, 0.10, 0.25, 0.50, 0.75, 0.85, 0.90, 0.95, 0.97)
         , height  = d[d$set == "weight_for_stature", "height"]
         , male = d[d$set == "weight_for_stature", "male"]
         )

test_q85 <-
  q_weight_for_stature(
           p    = c(0.85)#, 0.85, 0.10, 0.25, 0.50, 0.75, 0.85, 0.90, 0.95, 0.97)
         , height  = d[d$set == "weight_for_stature", "height"]
         , male = d[d$set == "weight_for_stature", "male"]
         )

test_q90 <-
  q_weight_for_stature(
           p    = c(0.90)#, 0.90, 0.10, 0.25, 0.50, 0.75, 0.85, 0.90, 0.95, 0.97)
         , height  = d[d$set == "weight_for_stature", "height"]
         , male = d[d$set == "weight_for_stature", "male"]
         )

test_q95 <-
  q_weight_for_stature(
           p    = c(0.95)#, 0.95, 0.10, 0.25, 0.50, 0.75, 0.85, 0.90, 0.95, 0.97)
         , height  = d[d$set == "weight_for_stature", "height"]
         , male = d[d$set == "weight_for_stature", "male"]
         )

test_q97 <-
  q_weight_for_stature(
           p    = c(0.97)#, 0.97, 0.10, 0.25, 0.50, 0.75, 0.85, 0.90, 0.95, 0.97)
         , height  = d[d$set == "weight_for_stature", "height"]
         , male = d[d$set == "weight_for_stature", "male"]
         )

stopifnot(all.equal(test_q03, d[d$set == "weight_for_stature", "p03"]))
stopifnot(all.equal(test_q05, d[d$set == "weight_for_stature", "p05"]))
stopifnot(all.equal(test_q10, d[d$set == "weight_for_stature", "p10"]))
stopifnot(all.equal(test_q25, d[d$set == "weight_for_stature", "p25"]))
stopifnot(all.equal(test_q50, d[d$set == "weight_for_stature", "p50"]))
stopifnot(all.equal(test_q75, d[d$set == "weight_for_stature", "p75"]))
stopifnot(all.equal(test_q85, d[d$set == "weight_for_stature", "p85"]))
stopifnot(all.equal(test_q90, d[d$set == "weight_for_stature", "p90"]))
stopifnot(all.equal(test_q95, d[d$set == "weight_for_stature", "p95"]))
stopifnot(all.equal(test_q97, d[d$set == "weight_for_stature", "p97"]))

# end testing q_weight_for_stature }}}

# Testing z_weight_for_stature {{{
test_z03 <-
  z_weight_for_stature(
           q    = d[d$set == "weight_for_stature", "p03"]
         , height  = d[d$set == "weight_for_stature", "height"]
         , male = d[d$set == "weight_for_stature", "male"]
         )

test_z05 <-
  z_weight_for_stature(
           q    = d[d$set == "weight_for_stature", "p05"]
         , height  = d[d$set == "weight_for_stature", "height"]
         , male = d[d$set == "weight_for_stature", "male"]
         )

test_z10 <-
  z_weight_for_stature(
           q    = d[d$set == "weight_for_stature", "p10"]
         , height  = d[d$set == "weight_for_stature", "height"]
         , male = d[d$set == "weight_for_stature", "male"]
         )

test_z25 <-
  z_weight_for_stature(
           q    = d[d$set == "weight_for_stature", "p25"]
         , height  = d[d$set == "weight_for_stature", "height"]
         , male = d[d$set == "weight_for_stature", "male"]
         )

test_z50 <-
  z_weight_for_stature(
           q    = d[d$set == "weight_for_stature", "p50"]
         , height  = d[d$set == "weight_for_stature", "height"]
         , male = d[d$set == "weight_for_stature", "male"]
         )

test_z75 <-
  z_weight_for_stature(
           q    = d[d$set == "weight_for_stature", "p75"]
         , height  = d[d$set == "weight_for_stature", "height"]
         , male = d[d$set == "weight_for_stature", "male"]
         )

test_z85 <-
  z_weight_for_stature(
           q    = d[d$set == "weight_for_stature", "p85"]
         , height  = d[d$set == "weight_for_stature", "height"]
         , male = d[d$set == "weight_for_stature", "male"]
         )

test_z90 <-
  z_weight_for_stature(
           q    = d[d$set == "weight_for_stature", "p90"]
         , height  = d[d$set == "weight_for_stature", "height"]
         , male = d[d$set == "weight_for_stature", "male"]
         )

test_z95 <-
  z_weight_for_stature(
           q    = d[d$set == "weight_for_stature", "p95"]
         , height  = d[d$set == "weight_for_stature", "height"]
         , male = d[d$set == "weight_for_stature", "male"]
         )

test_z97 <-
  z_weight_for_stature(
           q    = d[d$set == "weight_for_stature", "p97"]
         , height  = d[d$set == "weight_for_stature", "height"]
         , male = d[d$set == "weight_for_stature", "male"]
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

