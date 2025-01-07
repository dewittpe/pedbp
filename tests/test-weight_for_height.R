library(pedbp)
library(data.table)
setDTthreads(threads = 1L) # to prevent CPU time exceeding elapsed time on CRAN

################################################################################
##                    Testing against the published values                    ##
internal_lms_data <-
  pedbp:::lms_data |>
  lapply(lapply, data.table::rbindlist, use.names = TRUE, fill = TRUE) |>
  lapply(data.table::rbindlist, use.names = TRUE, fill = TRUE) |>
  data.table::rbindlist(use.names = TRUE, fill = TRUE)

internal_lms_data <-
  data.table::melt(
    internal_lms_data
  , id.vars = c("source", "metric", "male", "age", "height", "length", "L", "M", "S")
  , measure.vars = patterns("P")
  , variable.factor = FALSE
  , variable.name = "published_percentile"
  , value.name = "published_quantile"
  )

internal_lms_data <- internal_lms_data[metric == "weight_for_height"]
internal_lms_data <- internal_lms_data[!is.na(published_quantile) & !is.na(published_percentile)]

internal_lms_data[published_percentile == "P01",  published_percentile := 0.001]
internal_lms_data[published_percentile == "P1",   published_percentile := 0.010]
internal_lms_data[published_percentile == "P3",   published_percentile := 0.030]
internal_lms_data[published_percentile == "P5",   published_percentile := 0.050]
internal_lms_data[published_percentile == "P10",  published_percentile := 0.100]
internal_lms_data[published_percentile == "P15",  published_percentile := 0.150]
internal_lms_data[published_percentile == "P25",  published_percentile := 0.250]
internal_lms_data[published_percentile == "P50",  published_percentile := 0.500]
internal_lms_data[published_percentile == "P75",  published_percentile := 0.750]
internal_lms_data[published_percentile == "P85",  published_percentile := 0.850]
internal_lms_data[published_percentile == "P90",  published_percentile := 0.900]
internal_lms_data[published_percentile == "P95",  published_percentile := 0.950]
internal_lms_data[published_percentile == "P97",  published_percentile := 0.970]
internal_lms_data[published_percentile == "P99",  published_percentile := 0.990]
internal_lms_data[published_percentile == "P999", published_percentile := 0.999]
internal_lms_data[, published_percentile := as.numeric(published_percentile)]

internal_lms_data[, test_percentile := p_weight_for_height(q = published_quantile, male = male, height = height, source = source)]
internal_lms_data[, test_quantile   := q_weight_for_height(p = published_percentile, male = male, height = height, source = source)]
internal_lms_data[, test_zscore     := z_weight_for_height(q = published_quantile, male = male, height = height, source = source)]

stopifnot(internal_lms_data[, round(test_percentile, 3) == published_percentile])

#internal_lms_data[ round(test_percentile, 3) != published_percentile]
#pedbp:::plms(x = 8.972919, l = -0.9992942, m = 10.27441, s = 0.07711584)
#p_weight_for_height(q = 8.972919, male = 1, height = 77.0)# = -0.9992942, m = 10.27441, s = 0.07711584)


# arma::mat weight_for_height_cdc_male() {
# 	arma::mat LMS = {
# { 77, -0.999294215, 10.27440527, 0.077115837 },
# { 77.5, -0.979897716, 10.38901871, 0.076995353 },
# { 78.5, -0.943555181, 10.61724901, 0.076769511 },

# arma::mat weight_for_height_cdc_female() {
# 	arma::mat LMS = {
# { 77, -0.957840869, 10.08653219, 0.081713853 },
# { 77.5, -0.935908436, 10.19868351, 0.081394448 },
# { 78.5, -0.89621042, 10.42217324, 0.080780644 },

#stop()

quantile_tests_3 <-
  Map(function(x, y) { isTRUE(all.equal(x, y, tol = 1e-3)) }
      , x = internal_lms_data[["test_quantile"]]
      , y = internal_lms_data[["published_quantile"]]) |>
  do.call(c, args = _)

quantile_tests_4 <-
  Map(function(x, y) { isTRUE(all.equal(x, y, tol = 1e-4)) }
      , x = internal_lms_data[["test_quantile"]]
      , y = internal_lms_data[["published_quantile"]]) |>
  do.call(c, args = _)

stopifnot(quantile_tests_3)
stopifnot(quantile_tests_4)

################################################################################
# Test that the default will return the value based on the floor of the height #

# take sequential heights known exactly for CDC data
cdc <- internal_lms_data[height > 86 & height < 88 & source == "CDC"]
stopifnot(length(unique(cdc$height)) == 2L)

# define testing heights between the two values above
testing_heights <- seq(from = min(cdc$height), to = max(cdc$height), length.out = 7)

d <- data.table::CJ(male = 0:1, height = testing_heights)

d[, p := p_weight_for_height(q = 16.19, male = male, height = height, source = "CDC")]
d[, q := q_weight_for_height(p = 0.42, male = male, height = height, source = "CDC")]
d[, z := z_weight_for_height(q = 15.26, male = male, height = height, source = "CDC")]

# Expected behavior is that ignoring height should result in four unique rows,
# male/female by range(height)
d[, dup := duplicated(d, by = c("male", "p", "q", "z"))]
stopifnot(
          d[!(dup), identical(male, c(0L, 0L, 1L, 1L))],
          d[!(dup), all.equal(height, rep(range(testing_heights), 2))]
)

################################################################################
##                               End of center                                ##
################################################################################
