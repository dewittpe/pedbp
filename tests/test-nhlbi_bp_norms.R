library(pedbp)

# the data should be lazy loaded, so this will error if the data doesn't load
stopifnot(identical(class(nhlbi_bp_norms), "data.frame"))

stopifnot(identical(names(nhlbi_bp_norms), c("male", "age", "height_percentile", "bp_percentile", "sbp", "dbp")))

stopifnot(identical(dim(nhlbi_bp_norms), c(952L, 6L)))
stopifnot(isTRUE(all(nhlbi_bp_norms$male %in% c(0, 1))))

################################################################################
##                                End of file                                 ##
################################################################################
