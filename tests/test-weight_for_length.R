library(pedbp)
library(data.table)

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

internal_lms_data <- internal_lms_data[metric == "weight_for_length"]
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

internal_lms_data[, test_percentile := p_weight_for_length(q = published_quantile, male = male, length = length, source = source)]
internal_lms_data[, test_quantile   := q_weight_for_length(p = published_percentile, male = male, length = length, source = source)]
internal_lms_data[, test_zscore     := z_weight_for_length(q = published_quantile, male = male, length = length, source = source)]

stopifnot(internal_lms_data[, round(test_percentile, 3) == published_percentile])

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
# Test that the default will return the value based on the floor of the length #

cdc <- internal_lms_data[length > 56 & length < 58 & source == "CDC"]
stopifnot(length(unique(cdc$length)) == 2L)

# define testing lengths between the two values above
testing_lengths <- seq(from = min(cdc$length), to = max(cdc$length), length.out = 7)

d <- data.table::CJ(male = 0:1, length = testing_lengths)

d[, p := p_weight_for_length(q = 16.19, male = male, length = length, source = "CDC")]
d[, q := q_weight_for_length(p = 0.42, male = male, length = length, source = "CDC")]
d[, z := z_weight_for_length(q = 15.26, male = male, length = length, source = "CDC")]

# Expected behavior is that ignoring length should result in four unique rows,
# male/female by range(length)
d[, dup := duplicated(d, by = c("male", "p", "q", "z"))]
stopifnot(
          d[!(dup), identical(male, c(0L, 0L, 1L, 1L))],
          d[!(dup), all.equal(length, rep(range(testing_lengths), 2))]
)

################################################################################
##                               End of center                                ##
################################################################################
