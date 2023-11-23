library(pedbp)

################################################################################
# The data set bp_parameters should be lazy loaded when the the package is
# loaded
stopifnot(identical(class(bp_parameters), "data.frame"))

# verify some basic features of the data.frame
stopifnot(identical(nrow(bp_parameters), 276L))
stopifnot(identical(ncol(bp_parameters),   8L))

stopifnot(identical(
          sort(unique(bp_parameters[["source"]]))
          , c("gemelli1990", "lo2013", "nhlbi")
          ))

stopifnot(identical(
          names(bp_parameters)
          , c("source", "male", "age", "sbp_mean", "sbp_sd", "dbp_mean", "dbp_sd", "height_percentile")
          ))

################################################################################
##                                End of file                                 ##
################################################################################
