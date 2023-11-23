library(pedbp)

# the data should be lazy loaded so this will fail if that assumtion isn't valid
stopifnot(identical(class(gemelli1990), "data.frame"))

# expected names:
stopifnot(identical(names(gemelli1990), c("male", "age", "sbp_mean", "sbp_sd", "dbp_mean", "dbp_sd")))

# testing charateristics of the data set
stopifnot(identical(dim(gemelli1990), c(8L, 6L)))

stopifnot(isTRUE(all(gemelli1990$male %in% c(0, 1))))

################################################################################
##                                End of file                                 ##
################################################################################
