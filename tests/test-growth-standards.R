# library(pedbp)
devtools::load_all()

d <- pedbp:::lms_data |>
     lapply(lapply, data.table::rbindlist, use.names = TRUE, fill = TRUE) |>
     lapply(data.table::rbindlist, use.names = TRUE, fill = TRUE) |>
     data.table::rbindlist(use.names = TRUE, fill = TRUE)

################################################################################
##                    Testing against the published values                    ##

published <-
  data.table::melt(
    d
  , id.vars = c("source", "metric", "male", "age", "height", "length", "L", "M", "S")
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

published <- split(published, published$metric)


# Time R version
# tictoc::tic()
# published |>
#   lapply(function(x) {x
#          x[, q_cdc_test_r := pgsf(x = published_percentile, male = male, age = age, stature = stature, source = "CDC", metric = unique(metric), type = "quantile") ]
#   })
# tictoc::toc()
# 168.441 sec elapsed
# 179.438 sec elapsed

# time for cpp version - no limit on the metric, nor the source
tictoc::tic()
published |>
  lapply(function(x) {x
         x[, q_test_rcpp := cppPGSF(qp = published_percentile, male = male, x = na.omit(c(age, length, height)), source = source, metric = metric, type = "quantile") ]
         x[, p_test_rcpp := cppPGSF(qp = published_quantile,   male = male, x = na.omit(c(age, length, height)), source = source, metric = metric, type = "distribution") ]
         x[, z_test_rcpp := cppPGSF(qp = published_quantile,   male = male, x = na.omit(c(age, length, height)), source = source, metric = metric, type = "zscore") ]
  })
tictoc::toc()
# about 40 seconds

published <- data.table::rbindlist(published)

published[!is.na(p_test_rcpp) & abs(published_percentile - p_test_rcpp) > 0.0001 ] [, .N, by = metric ]
published[!is.na(p_test_rcpp) & abs(published_percentile - p_test_rcpp) > 0.001 ] [, .N, by = metric]
published[!is.na(p_test_rcpp) & abs(published_percentile - p_test_rcpp) > 0.01 ]# [, .N, by = metric]  all weight_for_height and weight_for_stature
published[!is.na(p_test_rcpp) & abs(published_percentile - p_test_rcpp) > 0.1 ] #[, .N, by = metric]

d[metric == "weight_for_height" & height == 77.0 & source == "CDC"] 
d[metric == "weight_for_height" & height == 77.0 & source == "WHO"] 

d[, .N, by = .(metric, source, is.na(age), is.na(height), is.na(length))] |> print(n = Inf)

published[!is.na(p_test_rcpp), mean(abs(published_percentile - p_test_rcpp) > 0.0001) ]
published[!is.na(p_test_rcpp), mean(abs(published_percentile - p_test_rcpp) > 0.001) ]
published[!is.na(p_test_rcpp), mean(abs(published_percentile - p_test_rcpp) > 0.01) ]
published[!is.na(p_test_rcpp), mean(abs(published_percentile - p_test_rcpp) > 0.1) ]

plms(8.972919, -0.9992942, 10.27441, 0.07711584)  # cdc male
zlms(8.972919, -0.9992942, 10.27441, 0.07711584)  # cdc male
qlms(0.03, -0.9992942, 10.27441, 0.07711584)      # cdc male

plms(8.972919, -0.9992942, 10.27441, 0.07711584) 
zlms(8.972919, -0.9992942, 10.27441, 0.07711584)
qlms(0.03, -0.9992942, 10.27441, 0.07711584)

#cppPGSF(metric = "stature_for_age", source = "CDC", x = 24.5, qp = 0.03, type = "quantile", male = 0)

#{ 24.5, 1.051272912, 86.1973169, 0.040859727 }, 

#pedbp:::lms_data[["stature_for_age"]][["CDC"]][["Female"]]


#all.equal(
#published[[1]][["q_cdc_test_r"]],
#published[[1]][["q_cdc_test_rcpp"]])
#
#published[[1]][is.na(q_cdc_test_r)]


#stop()

