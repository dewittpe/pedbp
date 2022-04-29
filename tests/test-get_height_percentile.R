library(pedbp)

utils::data(list = "cdc_length_for_age", package = "pedbp")

head(cdc_length_for_age)

approx_p3 <-
  apply(cdc_length_for_age[, c("age", "male", "p3")], 1,
        function(x) {
          get_height_percentile(x[1], x[2], x[3])
        })
approx_p5 <-
  apply(cdc_length_for_age[, c("age", "male", "p5")], 1,
        function(x) {
          get_height_percentile(x[1], x[2], x[3])
        })
approx_p10 <-
  apply(cdc_length_for_age[, c("age", "male", "p10")], 1,
        function(x) {
          get_height_percentile(x[1], x[2], x[3])
        })
approx_p25 <-
  apply(cdc_length_for_age[, c("age", "male", "p25")], 1,
        function(x) {
          get_height_percentile(x[1], x[2], x[3])
        })
approx_p50 <-
  apply(cdc_length_for_age[, c("age", "male", "p50")], 1,
        function(x) {
          get_height_percentile(x[1], x[2], x[3])
        })
approx_p75 <-
  apply(cdc_length_for_age[, c("age", "male", "p75")], 1,
        function(x) {
          get_height_percentile(x[1], x[2], x[3])
        })
approx_p90 <-
  apply(cdc_length_for_age[, c("age", "male", "p90")], 1,
        function(x) {
          get_height_percentile(x[1], x[2], x[3])
        })
approx_p95 <-
  apply(cdc_length_for_age[, c("age", "male", "p95")], 1,
        function(x) {
          get_height_percentile(x[1], x[2], x[3])
        })
approx_p97 <-
  apply(cdc_length_for_age[, c("age", "male", "p97")], 1,
        function(x) {
          get_height_percentile(x[1], x[2], x[3])
        })

stopifnot(all(approx_p3 > 0.01) & all(approx_p3 < 0.05))
stopifnot(all(approx_p5 > 0.03) & all(approx_p5 < 0.07))
stopifnot(all(approx_p10 > 0.067) & all(approx_p10 < 0.12))
stopifnot(all(approx_p25 > 0.18) & all(approx_p25 < 0.27))
stopifnot(all(approx_p50 > 0.41) & all(approx_p50 < 0.51))
stopifnot(all(approx_p75 > 0.67) & all(approx_p75 < 0.76))
stopifnot(all(approx_p90 > 0.85) & all(approx_p90 < 0.92))
stopifnot(all(approx_p95 > 0.91) & all(approx_p95 < 0.97))
stopifnot(all(approx_p97 > 0.948) & all(approx_p97 < 0.99))
