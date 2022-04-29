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
